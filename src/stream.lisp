(in-package #:llh2)

;;;; -- Streams --

(defclass h2-stream ()
  ((connection
    :initarg       :connection
    :reader        h2-stream-connection
    :documentation "The connection carrying the stream.")
   (id
    :initarg       :id
    :reader        h2-stream-id
    :type          (unsigned-byte 31)
    :documentation "The stream identifier; odd for client-initiated streams.")
   (headers
    :initform      nil
    :accessor      h2-stream-headers
    :type          list
    :documentation "The (NAME . VALUE) fields the peer sent first: the request on a server, the final response on a client.")
   (headers-p
    :initform      nil
    :accessor      h2-stream--headers-p
    :type          boolean
    :documentation "Whether the first header block from the peer arrived.")
   (trailers
    :initform      nil
    :accessor      h2-stream-trailers
    :type          list
    :documentation "The trailing (NAME . VALUE) fields the peer sent after its data.")
   (incoming
    :initform      nil
    :accessor      h2-stream--incoming
    :type          list
    :documentation "Received data chunks the application has not read, oldest first.")
   (expected-length
    :initform      nil
    :accessor      h2-stream--expected-length
    :type          (or null (integer 0 *))
    :documentation "The content-length the peer announced, checked when its data ends.")
   (received-length
    :initform      0
    :accessor      h2-stream--received-length
    :type          (integer 0 *)
    :documentation "The data octets received from the peer.")
   (remote-closed-p
    :initform      nil
    :accessor      h2-stream--remote-closed-p
    :type          boolean
    :documentation "Whether the peer ended its side with END_STREAM.")
   (headers-sent-p
    :initform      nil
    :accessor      h2-stream--headers-sent-p
    :type          boolean
    :documentation "Whether this side sent its header block.")
   (local-closed-p
    :initform      nil
    :accessor      h2-stream--local-closed-p
    :type          boolean
    :documentation "Whether this side ended its side with END_STREAM.")
   (reset-code
    :initform      nil
    :accessor      h2-stream-reset-code
    :type          (or null keyword)
    :documentation "The error code of the RST_STREAM that ended the stream, from either side.")
   (send-window
    :initarg       :send-window
    :accessor      h2-stream--send-window
    :type          integer
    :documentation "The data octets the peer currently accepts on this stream; negative after a settings change.")
   (receive-window
    :initarg       :receive-window
    :accessor      h2-stream--receive-window
    :type          integer
    :documentation "The data octets the peer may still send before this side opens the window.")
   (consumed
    :initform      0
    :accessor      h2-stream--consumed
    :type          (integer 0 *)
    :documentation "Octets the application read that no WINDOW_UPDATE returned yet."))
  (:documentation "One HTTP/2 request and response exchange on a connection.

Every state change happens under the connection's lock, and every waiter
waits on the connection's condition variable."))


;;;; -- Header Fields --

(-> h2-stream-header (h2-stream string) (or null string))
(defun h2-stream-header (stream name)
  "Return the value of header NAME the peer sent first, or NIL.

Repeated fields are joined with commas, cookies with semicolons."
  (let ((values (loop for (field . value) in (h2-stream-headers stream)
                      when (string-equal field name)
                        collect value)))
    (when values
      (format nil (if (string-equal name "cookie") "~{~A~^; ~}" "~{~A~^, ~}") values))))

(-> h2-stream-method (h2-stream) (or null string))
(defun h2-stream-method (stream)
  "Return the request method of STREAM."
  (h2-stream-header stream ":method"))

(-> h2-stream-path (h2-stream) (or null string))
(defun h2-stream-path (stream)
  "Return the request path of STREAM, query included."
  (h2-stream-header stream ":path"))

(-> h2-stream-authority (h2-stream) (or null string))
(defun h2-stream-authority (stream)
  "Return the request authority of STREAM."
  (h2-stream-header stream ":authority"))

(-> h2-stream-scheme (h2-stream) (or null string))
(defun h2-stream-scheme (stream)
  "Return the request scheme of STREAM."
  (h2-stream-header stream ":scheme"))

(-> h2-stream-status (h2-stream) (or null integer))
(defun h2-stream-status (stream)
  "Return the final response status of STREAM as an integer."
  (let ((status (h2-stream-header stream ":status")))
    (and status (parse-integer status :junk-allowed t))))


;;;; -- Stream State --

(-> h2-stream-closed-p (h2-stream) boolean)
(defun h2-stream-closed-p (stream)
  "Return whether STREAM can carry nothing more: reset, finished both ways, or disconnected."
  (let ((connection (h2-stream-connection stream)))
    (bt2:with-lock-held ((h2-connection--lock connection))
      (and (or (h2-stream-reset-code stream)
               (and (h2-stream--local-closed-p stream) (h2-stream--remote-closed-p stream))
               (h2-connection--closed-p connection))
           t))))

(-> h2-stream--check-open (h2-stream) null)
(defun h2-stream--check-open (stream)
  "Signal H2-STREAM-CLOSED when STREAM was reset or lost its connection; hold the lock."
  (let ((code (h2-stream-reset-code stream)))
    (cond
      (code
       (error 'h2-stream-closed :stream-id (h2-stream-id stream)
                                :message   (format nil "Stream ~D was reset (~(~A~))."
                                                   (h2-stream-id stream) code)))
      ((h2-connection--closed-p (h2-stream-connection stream))
       (error 'h2-stream-closed :stream-id (h2-stream-id stream)
                                :message   "The connection closed."))))
  nil)

(-> h2-stream--check-writable (h2-stream) null)
(defun h2-stream--check-writable (stream)
  "Signal H2-STREAM-CLOSED unless this side of STREAM may still send; hold the lock."
  (h2-stream--check-open stream)
  (when (h2-stream--local-closed-p stream)
    (error 'h2-stream-closed :stream-id (h2-stream-id stream)
                             :message   (format nil "Stream ~D already ended its output."
                                                (h2-stream-id stream))))
  nil)


;;;; -- Reading --

(-> h2-stream-wait-headers (h2-stream &key (:timeout (or null real))) list)
(defun h2-stream-wait-headers (stream &key timeout)
  "Wait for the peer's first header block on STREAM and return it.

Return NIL when TIMEOUT seconds pass first. Signal H2-STREAM-CLOSED when the
stream or connection ends without one."
  (let ((connection (h2-stream-connection stream)))
    (bt2:with-lock-held ((h2-connection--lock connection))
      (if (h2-connection--wait connection
                               (lambda ()
                                 (or (h2-stream--headers-p stream)
                                     (h2-stream-reset-code stream)
                                     (h2-connection--closed-p connection)))
                               :timeout timeout)
          (progn
            (unless (h2-stream--headers-p stream)
              (h2-stream--check-open stream))
            (h2-stream-headers stream))
          nil))))

(-> h2-stream-read-data (h2-stream &key (:timeout (or null real)))
    (values (or null octet-vector) boolean))
(defun h2-stream-read-data (stream &key timeout)
  "Return the next chunk of data the peer sent on STREAM, or NIL once its data ended.

The second value is true when TIMEOUT seconds passed without data. Reading
opens the stream's flow-control window again. Signal H2-STREAM-CLOSED when
the stream was reset or the connection ended before the peer finished."
  (let ((connection (h2-stream-connection stream))
        (chunk      nil)
        (increment  0))
    (bt2:with-lock-held ((h2-connection--lock connection))
      (unless (h2-connection--wait connection
                                   (lambda ()
                                     (or (h2-stream--incoming stream)
                                         (h2-stream--remote-closed-p stream)
                                         (h2-stream-reset-code stream)
                                         (h2-connection--closed-p connection)))
                                   :timeout timeout)
        (return-from h2-stream-read-data (values nil t)))
      (cond
        ((h2-stream--incoming stream)
         (setf chunk (pop (h2-stream--incoming stream)))
         (incf (h2-stream--consumed stream) (length chunk))
         (when (and (not (h2-stream--remote-closed-p stream))
                    (>= (h2-stream--consumed stream)
                        (floor (h2-connection--local-setting connection ':initial-window-size) 2)))
           (setf increment                         (h2-stream--consumed stream)
                 (h2-stream--consumed stream)       0)
           (incf (h2-stream--receive-window stream) increment)))
        ((h2-stream--remote-closed-p stream)
         nil)
        (t
         (h2-stream--check-open stream))))
    (when (plusp increment)
      (h2-connection--send-window-update connection (h2-stream-id stream) increment))
    (values chunk nil)))

(-> h2-stream-read-body (h2-stream &key (:limit (or null (integer 0 *)))) octet-vector)
(defun h2-stream-read-body (stream &key limit)
  "Read the rest of the peer's data on STREAM and return it as one octet vector.

A body longer than LIMIT octets resets the stream and signals H2-STREAM-ERROR."
  (let ((buffer (make-octet-buffer)))
    (loop for chunk = (h2-stream-read-data stream)
          while chunk
          do (when (and limit (> (+ (length buffer) (length chunk)) limit))
               (h2-stream-reset stream :code ':cancel)
               (error 'h2-stream-error :stream-id (h2-stream-id stream)
                                       :code      ':cancel
                                       :message   (format nil "The body exceeds ~D octets." limit)))
             (octet-buffer-append buffer chunk))
    (octet-buffer->vector buffer)))


;;;; -- Writing --

(-> h2-stream-send-headers (h2-stream list &key (:end-stream-p boolean)) null)
(defun h2-stream-send-headers (stream headers &key end-stream-p)
  "Send the (NAME . VALUE) fields of HEADERS on STREAM; END-STREAM-P ends this side.

Names are sent in lower case. Pseudo-header fields must come first."
  (let ((connection (h2-stream-connection stream)))
    (bt2:with-lock-held ((h2-connection--lock connection))
      (h2-stream--check-writable stream))
    (h2-connection--send-headers connection stream headers :end-stream-p end-stream-p))
  nil)

(-> h2-stream-write-data (h2-stream (vector octet) &key (:end-stream-p boolean)) null)
(defun h2-stream-write-data (stream data &key end-stream-p)
  "Send the octets of DATA on STREAM, waiting for flow-control credit as needed.

END-STREAM-P ends this side after the last octet. Signal H2-STREAM-CLOSED
when the stream was reset or the connection ended."
  (let* ((connection (h2-stream-connection stream))
         (octets     (coerce data 'octet-vector))
         (total      (length octets))
         (position   0))
    (loop
      (let ((chunk 0))
        (bt2:with-lock-held ((h2-connection--lock connection))
          (h2-stream--check-writable stream)
          (when (< position total)
            (h2-connection--wait connection
                                 (lambda ()
                                   (or (h2-stream-reset-code stream)
                                       (h2-connection--closed-p connection)
                                       (and (plusp (h2-stream--send-window stream))
                                            (plusp (h2-connection--send-window connection))))))
            (h2-stream--check-writable stream)
            (setf chunk (min (- total position)
                             (h2-stream--send-window stream)
                             (h2-connection--send-window connection)
                             (h2-connection--peer-setting connection ':max-frame-size)))
            (decf (h2-stream--send-window stream) chunk)
            (decf (h2-connection--send-window connection) chunk)))
        (let ((last-p (= (+ position chunk) total)))
          (when (or (plusp chunk) (and last-p end-stream-p))
            (h2-connection--send-data connection stream octets
                                      :start        position
                                      :end          (+ position chunk)
                                      :end-stream-p (and last-p end-stream-p)))
          (incf position chunk)
          (when last-p
            (return))))))
  nil)

(-> h2-stream-finish (h2-stream) null)
(defun h2-stream-finish (stream)
  "End this side of STREAM with an empty DATA frame."
  (h2-stream-write-data stream (make-octet-vector 0) :end-stream-p t))

(-> h2-stream-respond (h2-stream (integer 100 999) &key (:headers list) (:body (or null (vector octet))))
    null)
(defun h2-stream-respond (stream status &key headers body)
  "Send a complete response with STATUS, HEADERS, and the optional octet BODY."
  (let ((fields (cons (cons ":status" (princ-to-string status)) headers)))
    (if (and body (plusp (length body)))
        (progn
          (h2-stream-send-headers stream
                                  (append fields
                                          (list (cons "content-length"
                                                      (princ-to-string (length body))))))
          (h2-stream-write-data stream body :end-stream-p t))
        (h2-stream-send-headers stream fields :end-stream-p t)))
  nil)

(-> h2-stream-reset (h2-stream &key (:code keyword)) null)
(defun h2-stream-reset (stream &key (code ':cancel))
  "End STREAM at once in both directions with a RST_STREAM frame carrying CODE."
  (let ((connection (h2-stream-connection stream))
        (send-p     nil))
    (bt2:with-lock-held ((h2-connection--lock connection))
      (unless (or (h2-stream-reset-code stream)
                  (h2-connection--closed-p connection)
                  (and (h2-stream--local-closed-p stream) (h2-stream--remote-closed-p stream)))
        (setf send-p t))
      (h2-connection--mark-reset connection stream code))
    (when send-p
      (h2-connection--send-reset connection (h2-stream-id stream) code)))
  nil)
