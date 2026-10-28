(in-package #:llh2)

;;;; -- Frame Codec Tests --

(-> test-frame-octets (h2-frame) octet-vector)
(defun test-frame-octets (frame)
  "Return the wire octets of FRAME."
  (test-call-with-octet-stream (lambda (stream) (h2-write-frame stream frame))
                               (lambda (stream)
                                 (let ((octets (make-octet-vector (file-length stream))))
                                   (read-sequence octets stream)
                                   octets))))

(define-test frame-write-and-read
  (let ((frame (make-h2-frame ':headers :flags     (logior *h2-flag-end-headers* *h2-flag-end-stream*)
                                        :stream-id 5
                                        :payload   (test-text-octets "block"))))
    (test-equal (test-hex "000005 01 05 00000005 626c6f636b") (test-frame-octets frame) :test #'equalp)
    (let ((read (test-call-with-octet-stream (lambda (stream) (h2-write-frame stream frame))
                                             (lambda (stream) (h2-read-frame stream 16384)))))
      (test-equal 1 (h2-frame-type read))
      (test-equal 5 (h2-frame-flags read))
      (test-equal 5 (h2-frame-stream-id read))
      (test-equal "block" (octets->latin1-string (h2-frame-payload read))))))

(define-test frame-read-limits
  (let ((frame (make-h2-frame ':data :stream-id 1 :payload (make-octet-vector 100))))
    (test-equal ':frame-size-error
                (h2-error-code
                 (test-signals h2-connection-error
                   (test-call-with-octet-stream (lambda (stream) (h2-write-frame stream frame))
                                                (lambda (stream) (h2-read-frame stream 99)))))))
  (test-equal nil (test-call-with-octet-stream (lambda (stream) (declare (ignore stream)))
                                               (lambda (stream) (h2-read-frame stream 16384))))
  (test-signals h2-connection-error
    (test-call-with-octet-stream (lambda (stream) (write-sequence (test-hex "000004 00 00 00000001 00") stream))
                                 (lambda (stream) (h2-read-frame stream 16384)))))

(define-test frame-content-bounds
  (dolist (case '((:data 0 "616263" 0 3)
                  (:data 8 "02 616263 0000" 1 4)
                  (:headers #x20 "80000003 10 616263" 5 8)
                  (:headers #x28 "01 80000003 10 6162 00" 6 8)))
    (destructuring-bind (type flags hex start end) case
      (multiple-value-bind (actual-start actual-end)
          (h2-frame-content-bounds (make-h2-frame type :flags flags :stream-id 1 :payload (test-hex hex)))
        (test-equal start actual-start)
        (test-equal end actual-end))))
  (dolist (hex '("04 616263" "05 6162" ""))
    (test-signals h2-connection-error
      (h2-frame-content-bounds (make-h2-frame ':data :flags 8 :stream-id 1 :payload (test-hex hex))))))

(define-test frame-settings
  (let ((settings '((:header-table-size . 4096)
                    (:enable-push . 0)
                    (:initial-window-size . 1048576)
                    (:max-frame-size . 16384))))
    (test-equal settings (h2-decode-settings (h2-encode-settings settings))))
  (test-equal '((:max-concurrent-streams . 100) (#x99 . 7))
              (h2-decode-settings (test-hex "0003 00000064 0099 00000007")))
  (test-signals h2-connection-error
    (h2-decode-settings (test-hex "0003 000000")))
  (test-equal '((:max-concurrent-streams . 100) (:initial-window-size . 33554432) (:enable-push . 0))
              (h2-decode-settings-header "AAMAAABkAAQCAAAAAAIAAAAA"))
  (test-signals h2-connection-error
    (h2-decode-settings-header "!!!")))
