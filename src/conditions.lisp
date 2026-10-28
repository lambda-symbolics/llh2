(in-package #:llh2)

;;;; -- Error Codes --

(defparameter *h2-error-codes*
  '((:no-error            . #x0)
    (:protocol-error      . #x1)
    (:internal-error      . #x2)
    (:flow-control-error  . #x3)
    (:settings-timeout    . #x4)
    (:stream-closed       . #x5)
    (:frame-size-error    . #x6)
    (:refused-stream      . #x7)
    (:cancel              . #x8)
    (:compression-error   . #x9)
    (:connect-error       . #xa)
    (:enhance-your-calm   . #xb)
    (:inadequate-security . #xc)
    (:http-1-1-required   . #xd))
  "The HTTP/2 error codes of RFC 9113 Section 7 by keyword.")

(-> h2-error-code-number (keyword) (unsigned-byte 32))
(defun h2-error-code-number (code)
  "Return the wire number of the error CODE keyword."
  (or (rest (assoc code *h2-error-codes*))
      (error "Unknown HTTP/2 error code ~S." code)))

(-> h2-error-code-keyword ((unsigned-byte 32)) keyword)
(defun h2-error-code-keyword (number)
  "Return the keyword for wire error NUMBER; unknown codes read as :INTERNAL-ERROR."
  (or (first (rassoc number *h2-error-codes*))
      ':internal-error))


;;;; -- Conditions --

(define-condition h2-error (error)
  ((code
    :initarg       :code
    :initform      ':internal-error
    :reader        h2-error-code
    :documentation "The HTTP/2 error code keyword that describes the failure.")
   (message
    :initarg       :message
    :initform      "HTTP/2 failed."
    :reader        h2-error-message
    :documentation "A sentence describing the failure."))
  (:documentation "The base of every llh2 failure.")
  (:report (lambda (condition stream)
             (format stream "~A (~(~A~))"
                     (h2-error-message condition)
                     (h2-error-code condition)))))

(define-condition h2-connection-error (h2-error)
  ()
  (:documentation "A failure that ends the whole connection with a GOAWAY frame."))

(define-condition h2-stream-error (h2-error)
  ((stream-id
    :initarg       :stream-id
    :reader        h2-stream-error-stream-id
    :documentation "The stream the failure ends with a RST_STREAM frame."))
  (:documentation "A failure confined to one stream."))

(define-condition h2-stream-closed (h2-error)
  ((stream-id
    :initarg       :stream-id
    :reader        h2-stream-closed-stream-id
    :documentation "The stream that can no longer carry the operation."))
  (:default-initargs :code ':stream-closed)
  (:documentation "An operation on a stream that was reset, finished, or lost its connection."))

(define-condition hpack-error (h2-connection-error)
  ()
  (:default-initargs :code ':compression-error)
  (:documentation "A header block that does not decode under RFC 7541."))
