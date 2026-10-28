(in-package #:llh2)

;;;; -- Frame Constants --

(defparameter *h2-connection-preface*
  (latin1-string->octets (format nil "PRI * HTTP/2.0~C~C~C~CSM~C~C~C~C"
                                 #\Return #\Newline #\Return #\Newline
                                 #\Return #\Newline #\Return #\Newline))
  "The 24 octets a client sends before its first frame.")

(defparameter *h2-frame-types*
  '((:data          . #x0)
    (:headers       . #x1)
    (:priority      . #x2)
    (:rst-stream    . #x3)
    (:settings      . #x4)
    (:push-promise  . #x5)
    (:ping          . #x6)
    (:goaway        . #x7)
    (:window-update . #x8)
    (:continuation  . #x9))
  "The frame types of RFC 9113 Section 6 by keyword.")

(defparameter *h2-flag-end-stream* #x1
  "The END_STREAM flag of DATA and HEADERS frames.")

(defparameter *h2-flag-ack* #x1
  "The ACK flag of SETTINGS and PING frames.")

(defparameter *h2-flag-end-headers* #x4
  "The END_HEADERS flag of HEADERS and CONTINUATION frames.")

(defparameter *h2-flag-padded* #x8
  "The PADDED flag of DATA and HEADERS frames.")

(defparameter *h2-flag-priority* #x20
  "The PRIORITY flag of HEADERS frames.")

(defparameter *h2-settings-identifiers*
  '((:header-table-size       . #x1)
    (:enable-push             . #x2)
    (:max-concurrent-streams  . #x3)
    (:initial-window-size     . #x4)
    (:max-frame-size          . #x5)
    (:max-header-list-size    . #x6))
  "The settings of RFC 9113 Section 6.5.2 by keyword.")

(defparameter *h2-default-frame-size* 16384
  "The frame payload size every endpoint accepts before settings say otherwise.")

(defparameter *h2-largest-frame-size* (1- (ash 1 24))
  "The largest frame payload size SETTINGS_MAX_FRAME_SIZE may announce.")

(defparameter *h2-largest-window* (1- (ash 1 31))
  "The largest flow-control window RFC 9113 allows.")


;;;; -- Frames --

(defclass h2-frame ()
  ((type
    :initarg       :type
    :reader        h2-frame-type
    :type          (unsigned-byte 8)
    :documentation "The frame type number; types this library does not know are kept.")
   (flags
    :initarg       :flags
    :initform      0
    :reader        h2-frame-flags
    :type          (unsigned-byte 8)
    :documentation "The eight flag bits.")
   (stream-id
    :initarg       :stream-id
    :initform      0
    :reader        h2-frame-stream-id
    :type          (unsigned-byte 31)
    :documentation "The stream the frame belongs to; 0 for the connection.")
   (payload
    :initarg       :payload
    :initform      (make-octet-vector 0)
    :reader        h2-frame-payload
    :type          octet-vector
    :documentation "The payload octets, padding included."))
  (:documentation "One HTTP/2 frame as it appears on the wire."))

(-> h2-frame-type-number (keyword) (unsigned-byte 8))
(defun h2-frame-type-number (type)
  "Return the wire number of the frame TYPE keyword."
  (or (rest (assoc type *h2-frame-types*))
      (error "Unknown HTTP/2 frame type ~S." type)))

(-> h2-frame-type-keyword ((unsigned-byte 8)) (or null keyword))
(defun h2-frame-type-keyword (number)
  "Return the keyword of frame type NUMBER, or NIL for an extension type."
  (first (rassoc number *h2-frame-types*)))

(-> h2-frame-flag-p (h2-frame (unsigned-byte 8)) boolean)
(defun h2-frame-flag-p (frame flag)
  "Return whether FRAME has FLAG set."
  (logtest flag (h2-frame-flags frame)))

(-> make-h2-frame (keyword &key (:flags (unsigned-byte 8)) (:stream-id (unsigned-byte 31))
                           (:payload (vector octet)))
    h2-frame)
(defun make-h2-frame (type &key (flags 0) (stream-id 0) (payload (make-octet-vector 0)))
  "Return a frame of TYPE with FLAGS, STREAM-ID, and PAYLOAD."
  (make-instance 'h2-frame :type      (h2-frame-type-number type)
                           :flags     flags
                           :stream-id stream-id
                           :payload   (coerce payload 'octet-vector)))


;;;; -- Frame Input and Output --

(-> h2--read-octets (stream (integer 0 *)) (or null octet-vector))
(defun h2--read-octets (stream count)
  "Read exactly COUNT octets from STREAM; return NIL when input ends before the first."
  (let* ((octets (make-octet-vector count))
         (end    (read-sequence octets stream)))
    (cond
      ((= end count)
       octets)
      ((zerop end)
       nil)
      (t
       (error 'h2-connection-error :code    ':protocol-error
                                   :message "The connection ended inside a frame.")))))

(-> h2-read-frame (stream (integer 0 *)) (or null h2-frame))
(defun h2-read-frame (stream maximum-size)
  "Read one frame from the octet STREAM, or return NIL when input ends between frames.

A payload longer than MAXIMUM-SIZE is a FRAME_SIZE_ERROR."
  (let ((header (h2--read-octets stream 9)))
    (when header
      (let ((length (logior (ash (aref header 0) 16) (ash (aref header 1) 8) (aref header 2))))
        (when (> length maximum-size)
          (error 'h2-connection-error
                 :code    ':frame-size-error
                 :message (format nil "A ~D-octet frame exceeds the ~D-octet limit." length maximum-size)))
        (make-instance 'h2-frame
                       :type      (aref header 3)
                       :flags     (aref header 4)
                       :stream-id (logand (h2-decode-u32 header 5) *h2-largest-window*)
                       :payload   (if (zerop length)
                                      (make-octet-vector 0)
                                      (or (h2--read-octets stream length)
                                          (error 'h2-connection-error
                                                 :code    ':protocol-error
                                                 :message "The connection ended inside a frame."))))))))

(-> h2-write-frame (stream h2-frame) null)
(defun h2-write-frame (stream frame)
  "Write FRAME to the octet STREAM without forcing output."
  (let ((header  (make-octet-vector 9))
        (payload (h2-frame-payload frame)))
    (setf (aref header 0) (ldb (byte 8 16) (length payload))
          (aref header 1) (ldb (byte 8 8) (length payload))
          (aref header 2) (ldb (byte 8 0) (length payload))
          (aref header 3) (h2-frame-type frame)
          (aref header 4) (h2-frame-flags frame))
    (h2-encode-u32 header 5 (h2-frame-stream-id frame))
    (write-sequence header stream)
    (write-sequence payload stream))
  nil)


;;;; -- Payload Fields --

(-> h2-decode-u32 ((vector octet) (integer 0 *)) (unsigned-byte 32))
(defun h2-decode-u32 (octets position)
  "Return the big-endian 32-bit integer at POSITION of OCTETS."
  (logior (ash (aref octets position) 24)
          (ash (aref octets (+ position 1)) 16)
          (ash (aref octets (+ position 2)) 8)
          (aref octets (+ position 3))))

(-> h2-encode-u32 ((vector octet) (integer 0 *) (unsigned-byte 32)) null)
(defun h2-encode-u32 (octets position value)
  "Store VALUE as a big-endian 32-bit integer at POSITION of OCTETS."
  (loop for index from 0 below 4
        do (setf (aref octets (+ position index)) (ldb (byte 8 (* 8 (- 3 index))) value)))
  nil)

(-> h2-u32-payload (&rest (unsigned-byte 32)) octet-vector)
(defun h2-u32-payload (&rest values)
  "Return a payload holding VALUES as consecutive big-endian 32-bit integers."
  (let ((octets (make-octet-vector (* 4 (length values)))))
    (loop for value in values
          for position from 0 by 4
          do (h2-encode-u32 octets position value))
    octets))

(-> h2-frame-content-bounds (h2-frame) (values (integer 0 *) (integer 0 *)))
(defun h2-frame-content-bounds (frame)
  "Return the start and end of FRAME's content once padding and priority fields are removed.

Applies to DATA and HEADERS frames. Padding as long as the payload is a
PROTOCOL_ERROR."
  (let* ((payload (h2-frame-payload frame))
         (start   0)
         (end     (length payload)))
    (when (h2-frame-flag-p frame *h2-flag-padded*)
      (when (zerop end)
        (error 'h2-connection-error :code    ':frame-size-error
                                    :message "A padded frame has no pad length."))
      (let ((padding (aref payload 0)))
        (setf start 1)
        (when (>= padding end)
          (error 'h2-connection-error :code    ':protocol-error
                                      :message "The padding fills the whole frame."))
        (decf end padding)))
    (when (and (= (h2-frame-type frame) (h2-frame-type-number ':headers))
               (h2-frame-flag-p frame *h2-flag-priority*))
      (incf start 5))
    (when (> start end)
      (error 'h2-connection-error :code    ':frame-size-error
                                  :message "A frame is too short for its fields."))
    (values start end)))


;;;; -- Settings --

(-> h2-decode-settings ((vector octet)) list)
(defun h2-decode-settings (payload)
  "Return the (IDENTIFIER . VALUE) pairs of a SETTINGS PAYLOAD in order.

Known identifiers are keywords; unknown ones stay numbers for the caller to ignore."
  (unless (zerop (mod (length payload) 6))
    (error 'h2-connection-error :code    ':frame-size-error
                                :message "A SETTINGS payload is not a multiple of six octets."))
  (loop for position from 0 below (length payload) by 6
        for number = (logior (ash (aref payload position) 8) (aref payload (1+ position)))
        collect (cons (or (first (rassoc number *h2-settings-identifiers*)) number)
                      (h2-decode-u32 payload (+ position 2)))))

(-> h2-encode-settings (list) octet-vector)
(defun h2-encode-settings (settings)
  "Return the SETTINGS payload for the (KEYWORD . VALUE) pairs of SETTINGS."
  (let ((payload (make-octet-vector (* 6 (length settings)))))
    (loop for (identifier . value) in settings
          for position from 0 by 6
          for number = (or (rest (assoc identifier *h2-settings-identifiers*))
                           (error "Unknown HTTP/2 setting ~S." identifier))
          do (setf (aref payload position)      (ldb (byte 8 8) number)
                   (aref payload (1+ position)) (ldb (byte 8 0) number))
             (h2-encode-u32 payload (+ position 2) value))
    payload))

(-> h2-decode-settings-header (string) list)
(defun h2-decode-settings-header (text)
  "Return the settings pairs of an HTTP2-Settings header value, base64url without padding."
  (let* ((standard (map 'string
                        (lambda (character)
                          (case character
                            (#\- #\+)
                            (#\_ #\/)
                            (t character)))
                        text))
         (padding  (mod (- 4 (mod (length standard) 4)) 4))
         (payload  (handler-case
                       (cl-base64:base64-string-to-usb8-array
                        (concatenate 'string standard (make-string padding :initial-element #\=)))
                     (error ()
                       (error 'h2-connection-error
                              :code    ':protocol-error
                              :message "The HTTP2-Settings header is not base64url.")))))
    (h2-decode-settings payload)))
