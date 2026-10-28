(in-package #:llh2)

;;;; -- Limits --

(defparameter *hpack-maximum-integer* (1- (ash 1 32))
  "The largest integer a header block may encode; larger ones are decoding errors.")

(defparameter *hpack-default-table-size* 4096
  "The dynamic table size RFC 9113 assumes before SETTINGS_HEADER_TABLE_SIZE changes it.")

(defparameter *hpack-default-header-list-limit* 262144
  "The default limit on a decoded header list, counted as RFC 9113 counts it.")

(defparameter *hpack-never-indexed-names*
  '("authorization" "cookie" "proxy-authorization" "set-cookie")
  "Header names the encoder marks never-indexed, so intermediaries do not store them.")


;;;; -- Dynamic Table --

(defclass hpack-table ()
  ((entries
    :initform      (make-array 16 :adjustable t :fill-pointer 0)
    :reader        hpack-table-entries
    :documentation "The (NAME . VALUE) entries, oldest first.")
   (size
    :initform      0
    :accessor      hpack-table-size
    :type          (integer 0 *)
    :documentation "The size of the entries as RFC 7541 Section 4.1 counts it.")
   (maximum-size
    :initarg       :maximum-size
    :initform      *hpack-default-table-size*
    :accessor      hpack-table-maximum-size
    :type          (integer 0 *)
    :documentation "The size the entries may not exceed."))
  (:documentation "The dynamic header table of one direction of a connection."))

(-> hpack-table--entry-size (string string) (integer 0 *))
(defun hpack-table--entry-size (name value)
  "Return the size of the entry NAME and VALUE."
  (+ (length name) (length value) 32))

(-> hpack-table--evict (hpack-table (integer 0 *)) null)
(defun hpack-table--evict (table limit)
  "Remove the oldest entries of TABLE until its size is at most LIMIT."
  (let* ((entries (hpack-table-entries table))
         (count   (loop for entry across entries
                        while (> (hpack-table-size table) limit)
                        do (decf (hpack-table-size table)
                                 (hpack-table--entry-size (first entry) (rest entry)))
                        count t)))
    (when (plusp count)
      (replace entries entries :start2 count)
      (setf (fill-pointer entries) (- (fill-pointer entries) count))))
  nil)

(-> hpack-table-add (hpack-table string string) null)
(defun hpack-table-add (table name value)
  "Insert NAME and VALUE as the newest entry of TABLE, evicting old entries to fit.

An entry larger than the whole table empties it and is not stored."
  (let ((size (hpack-table--entry-size name value)))
    (hpack-table--evict table (max 0 (- (hpack-table-maximum-size table) size)))
    (when (<= size (hpack-table-maximum-size table))
      (vector-push-extend (cons name value) (hpack-table-entries table))
      (incf (hpack-table-size table) size)))
  nil)

(-> hpack-table-resize (hpack-table (integer 0 *)) null)
(defun hpack-table-resize (table maximum-size)
  "Set the maximum size of TABLE, evicting entries that no longer fit."
  (setf (hpack-table-maximum-size table) maximum-size)
  (hpack-table--evict table maximum-size)
  nil)

(-> hpack-table-lookup (hpack-table (integer 0 *)) cons)
(defun hpack-table-lookup (table index)
  "Return the (NAME . VALUE) entry at INDEX of the static and dynamic address space."
  (let ((static-count (length *hpack-static-table*))
        (entries      (hpack-table-entries table)))
    (cond
      ((zerop index)
       (error 'hpack-error :message "Header index 0 is not valid."))
      ((<= index static-count)
       (svref *hpack-static-table* (1- index)))
      ((<= (- index static-count) (length entries))
       (aref entries (- (length entries) (- index static-count))))
      (t
       (error 'hpack-error :message (format nil "Header index ~D is outside the table." index))))))


;;;; -- Huffman Code --

(-> hpack--build-huffman-tree () simple-vector)
(defun hpack--build-huffman-tree ()
  "Return the decoding tree of the Huffman code: nodes are two-element vectors, leaves symbols."
  (let ((root (vector nil nil)))
    (loop for symbol from 0
          for (code length) across *hpack-huffman-codes*
          do (let ((node root))
               (loop for bit-index from (1- length) downto 1
                     for bit = (ldb (byte 1 bit-index) code)
                     do (setf node (or (svref node bit)
                                       (setf (svref node bit) (vector nil nil)))))
               (setf (svref node (ldb (byte 1 0) code)) symbol)))
    root))

(defparameter *hpack-huffman-tree* (hpack--build-huffman-tree)
  "The decoding tree of the Huffman code of RFC 7541 Appendix B.")

(-> hpack--huffman-length (octet-vector) (integer 0 *))
(defun hpack--huffman-length (octets)
  "Return the number of octets OCTETS occupies once Huffman encoded."
  (ceiling (loop for octet across octets
                 sum (second (svref *hpack-huffman-codes* octet)))
           8))

(-> hpack--huffman-encode ((vector octet) octet-vector) null)
(defun hpack--huffman-encode (buffer octets)
  "Append the Huffman encoding of OCTETS to BUFFER, padded with the EOS prefix."
  (let ((accumulator 0)
        (bits        0))
    (loop for octet across octets
          do (destructuring-bind (code length) (svref *hpack-huffman-codes* octet)
               (setf accumulator (logior (ash accumulator length) code))
               (incf bits length)
               (loop while (>= bits 8)
                     do (decf bits 8)
                        (vector-push-extend (ldb (byte 8 bits) accumulator) buffer))
               (setf accumulator (ldb (byte bits 0) accumulator))))
    (when (plusp bits)
      (vector-push-extend (logior (ash accumulator (- 8 bits))
                                  (1- (ash 1 (- 8 bits))))
                          buffer)))
  nil)

(-> hpack--huffman-decode (octet-vector (integer 0 *) (integer 0 *)) simple-string)
(defun hpack--huffman-decode (octets start end)
  "Return the string the Huffman-coded OCTETS between START and END encode."
  (let ((output     (make-array (* 2 (- end start)) :element-type 'character
                                                    :adjustable   t
                                                    :fill-pointer 0))
        (node       *hpack-huffman-tree*)
        (depth      0)
        (all-ones-p t))
    (loop for index from start below end
          for octet = (aref octets index)
          do (loop for bit-index from 7 downto 0
                   for bit = (ldb (byte 1 bit-index) octet)
                   for child = (svref node bit)
                   do (incf depth)
                      (setf all-ones-p (and all-ones-p (= bit 1)))
                      (cond
                        ((not (integerp child))
                         (setf node child))
                        ((= child 256)
                         (error 'hpack-error :message "A Huffman string contains EOS."))
                        (t
                         (vector-push-extend (code-char child) output)
                         (setf node       *hpack-huffman-tree*
                               depth      0
                               all-ones-p t)))))
    (unless (and (< depth 8) all-ones-p)
      (error 'hpack-error :message "A Huffman string has invalid padding."))
    (coerce output 'simple-string)))


;;;; -- Primitive Coding --

(-> hpack--octet (octet-vector (integer 0 *)) octet)
(defun hpack--octet (octets position)
  "Return the octet at POSITION, signaling a decoding error past the end of the block."
  (if (< position (length octets))
      (aref octets position)
      (error 'hpack-error :message "The header block ends inside a representation.")))

(-> hpack--encode-integer ((vector octet) (integer 0 *) &key (:prefix-bits (integer 1 8)) (:flags octet))
    null)
(defun hpack--encode-integer (buffer value &key prefix-bits (flags 0))
  "Append VALUE with a PREFIX-BITS prefix to BUFFER, setting FLAGS in the first octet."
  (let ((limit (1- (ash 1 prefix-bits))))
    (if (< value limit)
        (vector-push-extend (logior flags value) buffer)
        (progn
          (vector-push-extend (logior flags limit) buffer)
          (loop for remainder = (- value limit) then (ash remainder -7)
                while (>= remainder 128)
                do (vector-push-extend (logior #x80 (logand remainder #x7f)) buffer)
                finally (vector-push-extend remainder buffer)))))
  nil)

(-> hpack--decode-integer (octet-vector (integer 0 *) (integer 1 8))
    (values (integer 0 *) (integer 0 *)))
(defun hpack--decode-integer (octets position prefix-bits)
  "Decode the integer with a PREFIX-BITS prefix at POSITION; return it and the next position."
  (let* ((limit (1- (ash 1 prefix-bits)))
         (value (logand (hpack--octet octets position) limit)))
    (incf position)
    (if (< value limit)
        (values value position)
        (loop for shift from 0 by 7
              for octet = (hpack--octet octets position)
              do (incf position)
                 (incf value (ash (logand octet #x7f) shift))
                 (when (> value *hpack-maximum-integer*)
                   (error 'hpack-error :message "A header block integer is too large."))
              until (zerop (logand octet #x80))
              finally (return (values value position))))))

(-> hpack--encode-string ((vector octet) string) null)
(defun hpack--encode-string (buffer string)
  "Append STRING to BUFFER, Huffman coded when that is shorter."
  (let* ((octets         (latin1-string->octets string))
         (huffman-length (hpack--huffman-length octets)))
    (if (< huffman-length (length octets))
        (progn
          (hpack--encode-integer buffer huffman-length :prefix-bits 7 :flags #x80)
          (hpack--huffman-encode buffer octets))
        (progn
          (hpack--encode-integer buffer (length octets) :prefix-bits 7)
          (octet-buffer-append buffer octets))))
  nil)

(-> hpack--decode-string (octet-vector (integer 0 *)) (values simple-string (integer 0 *)))
(defun hpack--decode-string (octets position)
  "Decode the string literal at POSITION; return it and the next position."
  (let ((huffman-p (logbitp 7 (hpack--octet octets position))))
    (multiple-value-bind (length start) (hpack--decode-integer octets position 7)
      (let ((end (+ start length)))
        (when (> end (length octets))
          (error 'hpack-error :message "A header string runs past the header block."))
        (values (if huffman-p
                    (hpack--huffman-decode octets start end)
                    (octets->latin1-string octets :start start :end end))
                end)))))


;;;; -- Decoder --

(defclass hpack-decoder ()
  ((table
    :initform      (make-instance 'hpack-table)
    :reader        hpack-decoder-table
    :documentation "The dynamic table the peer's encoder maintains for this decoder.")
   (table-limit
    :initarg       :table-limit
    :initform      *hpack-default-table-size*
    :reader        hpack-decoder-table-limit
    :type          (integer 0 *)
    :documentation "The SETTINGS_HEADER_TABLE_SIZE this side advertised.")
   (header-list-limit
    :initarg       :header-list-limit
    :initform      *hpack-default-header-list-limit*
    :reader        hpack-decoder-header-list-limit
    :type          (integer 0 *)
    :documentation "The largest decoded header list accepted."))
  (:documentation "Decoding state for the header blocks one peer sends."))

(-> hpack--decode-literal (hpack-table octet-vector (integer 0 *) &key (:prefix-bits (integer 1 8)))
    (values string string (integer 0 *)))
(defun hpack--decode-literal (table octets position &key prefix-bits)
  "Decode a literal representation at POSITION; return its name, value, and next position."
  (multiple-value-bind (index next) (hpack--decode-integer octets position prefix-bits)
    (multiple-value-bind (name after-name)
        (if (zerop index)
            (hpack--decode-string octets next)
            (values (first (hpack-table-lookup table index)) next))
      (multiple-value-bind (value after-value) (hpack--decode-string octets after-name)
        (values name value after-value)))))

(-> hpack-decode (hpack-decoder octet-vector) list)
(defun hpack-decode (decoder octets)
  "Decode the complete header block OCTETS and return its (NAME . VALUE) fields in order.

Decoding errors leave the dynamic table unusable, so they end the connection."
  (let ((table          (hpack-decoder-table decoder))
        (position       0)
        (headers        nil)
        (list-size      0)
        (header-seen-p nil))
    (loop while (< position (length octets))
          do (let ((octet (aref octets position)))
               (if (= (logand octet #xe0) #x20)
                   (multiple-value-bind (size next) (hpack--decode-integer octets position 5)
                     (when header-seen-p
                       (error 'hpack-error
                              :message "A table size update follows a header field."))
                     (when (> size (hpack-decoder-table-limit decoder))
                       (error 'hpack-error
                              :message "A table size update exceeds the advertised limit."))
                     (hpack-table-resize table size)
                     (setf position next))
                   (multiple-value-bind (name value next)
                       (cond
                         ((logbitp 7 octet)
                          (multiple-value-bind (index after) (hpack--decode-integer octets position 7)
                            (let ((entry (hpack-table-lookup table index)))
                              (values (first entry) (rest entry) after))))
                         ((= (logand octet #xc0) #x40)
                          (multiple-value-bind (name value after)
                              (hpack--decode-literal table octets position :prefix-bits 6)
                            (hpack-table-add table name value)
                            (values name value after)))
                         (t
                          (hpack--decode-literal table octets position :prefix-bits 4)))
                     (setf header-seen-p t
                           position      next)
                     (incf list-size (hpack-table--entry-size name value))
                     (when (> list-size (hpack-decoder-header-list-limit decoder))
                       (error 'hpack-error
                              :code    ':enhance-your-calm
                              :message "A header list exceeds the size limit."))
                     (push (cons name value) headers)))))
    (nreverse headers)))


;;;; -- Encoder --

;; The encoder never inserts into the dynamic table. It announces a zero table
;; size once, so the peer never has to keep entries for it.

(-> hpack--static-indexes () (values hash-table hash-table))
(defun hpack--static-indexes ()
  "Return hash tables from (NAME . VALUE) and from NAME to their first static index."
  (let ((exact (make-hash-table :test 'equal))
        (names (make-hash-table :test 'equal)))
    (loop for entry across *hpack-static-table*
          for index from 1
          do (unless (gethash entry exact)
               (setf (gethash entry exact) index))
             (unless (gethash (first entry) names)
               (setf (gethash (first entry) names) index)))
    (values exact names)))

(defparameter *hpack-static-exact-indexes* (nth-value 0 (hpack--static-indexes))
  "The first static table index of each (NAME . VALUE) entry.")

(defparameter *hpack-static-name-indexes* (nth-value 1 (hpack--static-indexes))
  "The first static table index of each header name.")

(defclass hpack-encoder ()
  ((size-update-pending-p
    :initform      t
    :accessor      hpack-encoder-size-update-pending-p
    :type          boolean
    :documentation "Whether the next header block must start with a zero table size."))
  (:documentation "Encoding state for the header blocks this side sends."))

(-> hpack-encoder-reset-table (hpack-encoder) null)
(defun hpack-encoder-reset-table (encoder)
  "Announce the zero table size again, after the peer changes its table size setting."
  (setf (hpack-encoder-size-update-pending-p encoder) t)
  nil)

(-> hpack-encode (hpack-encoder list) octet-vector)
(defun hpack-encode (encoder headers)
  "Return the header block encoding the (NAME . VALUE) fields of HEADERS in order."
  (let ((buffer (make-octet-buffer)))
    (when (hpack-encoder-size-update-pending-p encoder)
      (hpack--encode-integer buffer 0 :prefix-bits 5 :flags #x20)
      (setf (hpack-encoder-size-update-pending-p encoder) nil))
    (loop for (name . value) in headers
          for exact-index = (gethash (cons name value) *hpack-static-exact-indexes*)
          for name-index = (gethash name *hpack-static-name-indexes*)
          for flags = (if (member name *hpack-never-indexed-names* :test #'string=) #x10 #x00)
          do (cond
               ((and exact-index (zerop flags))
                (hpack--encode-integer buffer exact-index :prefix-bits 7 :flags #x80))
               (name-index
                (hpack--encode-integer buffer name-index :prefix-bits 4 :flags flags)
                (hpack--encode-string buffer value))
               (t
                (vector-push-extend flags buffer)
                (hpack--encode-string buffer name)
                (hpack--encode-string buffer value))))
    (octet-buffer->vector buffer)))
