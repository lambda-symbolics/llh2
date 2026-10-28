(in-package #:llh2)

;;;; -- Loopback Fixtures --

(-> test-shut-down-socket (t) null)
(defun test-shut-down-socket (socket)
  "Shut SOCKET down in both directions, waking threads blocked reading it."
  (handler-case (usocket:socket-shutdown socket ':io)
    (error ()
      nil))
  nil)

(-> test-call-with-server (function function) t)
(defun test-call-with-server (handler function)
  "Serve HANDLER on a loopback port and call FUNCTION with a client connection to it."
  (let* ((listener (ls-compat.tcp:tcp-listen "127.0.0.1" 0))
         (port     (ls-compat.tcp:tcp-local-port listener))
         (server   (bt2:make-thread
                    (lambda ()
                      (let ((socket (ls-compat.tcp:tcp-accept listener)))
                        (h2-connection-serve
                         (make-h2-server-connection (ls-compat.tcp:tcp-stream socket)
                                                    :handler        handler
                                                    :shutdown-function (partial #'test-shut-down-socket socket)))))
                    :name "llh2 test server"))
         (socket   (ls-compat.tcp:tcp-connect "127.0.0.1" port))
         (client   (make-h2-client-connection (ls-compat.tcp:tcp-stream socket)
                                              :shutdown-function (partial #'test-shut-down-socket socket))))
    (unwind-protect (funcall function client)
      (h2-connection-close client)
      (bt2:join-thread server)
      (ls-compat.tcp:close-tcp listener))))

(-> test-call-with-raw-server (function function &key (:upgrade-headers list)) t)
(defun test-call-with-raw-server (handler function &key upgrade-headers)
  "Serve HANDLER on a loopback port and call FUNCTION with a raw octet stream to it."
  (let* ((listener (ls-compat.tcp:tcp-listen "127.0.0.1" 0))
         (port     (ls-compat.tcp:tcp-local-port listener))
         (server   (bt2:make-thread
                    (lambda ()
                      (let ((socket (ls-compat.tcp:tcp-accept listener)))
                        (h2-connection-serve
                         (make-h2-server-connection (ls-compat.tcp:tcp-stream socket)
                                                    :handler        handler
                                                    :shutdown-function (partial #'test-shut-down-socket socket))
                         :upgrade-headers  upgrade-headers
                         :upgrade-settings (and upgrade-headers "AAMAAABkAAQCAAAAAAIAAAAA"))))
                    :name "llh2 raw test server"))
         (socket   (ls-compat.tcp:tcp-connect "127.0.0.1" port)))
    (unwind-protect (funcall function (ls-compat.tcp:tcp-stream socket))
      (ls-compat.tcp:close-tcp socket)
      (bt2:join-thread server)
      (ls-compat.tcp:close-tcp listener))))

(-> test-random-octets ((integer 0 *)) octet-vector)
(defun test-random-octets (count)
  "Return COUNT pseudo-random octets."
  (let ((octets (make-octet-vector count))
        (state  (sb-ext:seed-random-state 42)))
    (dotimes (index count octets)
      (setf (aref octets index) (random 256 state)))))

(-> test-echo-handler (h2-stream) null)
(defun test-echo-handler (stream)
  "Answer GET /hello with a greeting and POST /echo with the request body."
  (let ((path (h2-stream-path stream)))
    (cond
      ((string= path "/hello")
       (h2-stream-respond stream 200 :headers '(("content-type" . "text/plain"))
                                     :body    (test-text-octets "hello")))
      ((string= path "/echo")
       (h2-stream-respond stream 200 :body (h2-stream-read-body stream)))
      ((string= path "/fail")
       (error "The handler fails on purpose."))
      (t
       (h2-stream-respond stream 404))))
  nil)


;;;; -- Connection Tests --

(define-test connection-request-and-response
  (test-call-with-server
   #'test-echo-handler
   (lambda (client)
     (let ((stream (h2-request client :path "/hello")))
       (h2-stream-wait-headers stream :timeout 10)
       (test-equal 200 (h2-stream-status stream))
       (test-equal "text/plain" (h2-stream-header stream "content-type"))
       (test-equal "hello" (octets->latin1-string (h2-stream-read-body stream))))
     (let ((stream (h2-request client :path "/missing")))
       (h2-stream-wait-headers stream :timeout 10)
       (test-equal 404 (h2-stream-status stream))
       (test-equal 0 (length (h2-stream-read-body stream)))))))

(define-test connection-flow-control-large-bodies
  (test-call-with-server
   #'test-echo-handler
   (lambda (client)
     (let* ((body   (test-random-octets (* 3 1048576)))
            (stream (h2-request client :method "POST" :path "/echo" :body body)))
       (h2-stream-wait-headers stream :timeout 30)
       (test-equal 200 (h2-stream-status stream))
       (test-equal body (h2-stream-read-body stream) :test #'equalp)))))

(define-test connection-concurrent-streams
  (test-call-with-server
   #'test-echo-handler
   (lambda (client)
     (let* ((results (make-array 16 :initial-element nil))
            (threads (loop for index from 0 below 16
                           collect (let ((index index))
                                     (bt2:make-thread
                                      (lambda ()
                                        (let* ((body   (test-text-octets (format nil "body ~D" index)))
                                               (stream (h2-request client :method "POST"
                                                                          :path   "/echo"
                                                                          :body   body)))
                                          (setf (aref results index)
                                                (octets->latin1-string (h2-stream-read-body stream)))))
                                      :name "llh2 test request")))))
       (mapc #'bt2:join-thread threads)
       (dotimes (index 16)
         (test-equal (format nil "body ~D" index) (aref results index)))))))

(define-test connection-streaming-response
  (test-call-with-server
   (lambda (stream)
     (h2-stream-send-headers stream '((":status" . "200") ("content-type" . "text/event-stream")))
     (dotimes (index 3)
       (h2-stream-write-data stream (test-text-octets (format nil "data: ~D~%~%" index)))
       (sleep 0.05))
     (h2-stream-finish stream))
   (lambda (client)
     (let ((stream (h2-request client :path "/events")))
       (test-equal "text/event-stream"
                   (rest (assoc "content-type" (h2-stream-wait-headers stream :timeout 10) :test #'string=)))
       (test-equal (loop for index from 0 below 3 collect (format nil "data: ~D~%~%" index))
                   (loop for chunk = (h2-stream-read-data stream :timeout 10)
                         while chunk
                         collect (octets->latin1-string chunk)))))))

(define-test connection-resets
  (let ((server-condition nil)
        (finished         (bt2:make-semaphore)))
    (test-call-with-server
     (lambda (stream)
       (if (string= (h2-stream-path stream) "/fail")
           (test-echo-handler stream)
           (unwind-protect
                (handler-case
                    (progn
                      (h2-stream-send-headers stream '((":status" . "200")))
                      (loop (h2-stream-write-data stream (test-text-octets "tick"))
                            (sleep 0.01)))
                  (h2-stream-closed (condition)
                    (setf server-condition condition)))
             (bt2:signal-semaphore finished))))
     (lambda (client)
       (let ((stream (h2-request client :path "/ticks")))
         (h2-stream-wait-headers stream :timeout 10)
         (test-assert (h2-stream-read-data stream :timeout 10))
         (h2-stream-reset stream :code ':cancel)
         (test-assert (bt2:wait-on-semaphore finished :timeout 10))
         (test-assert (typep server-condition 'h2-stream-closed) "the handler saw the client's reset"))
       (let ((stream (h2-request client :path "/fail")))
         (test-signals h2-stream-closed
           (h2-stream-wait-headers stream :timeout 10))
         (test-equal ':internal-error (h2-stream-reset-code stream)))))))

(define-test connection-upgraded-request
  (test-call-with-raw-server
   (lambda (stream)
     (h2-stream-respond stream 200 :body (test-text-octets (h2-stream-path stream))))
   (lambda (io)
     (write-sequence *h2-connection-preface* io)
     (h2-write-frame io (make-h2-frame ':settings))
     (force-output io)
     (let ((decoder (make-instance 'hpack-decoder))
           (fields  nil)
           (body    (make-octet-buffer)))
       (loop for frame = (h2-read-frame io 16384)
             while frame
             do (case (h2-frame-type-keyword (h2-frame-type frame))
                  (:headers
                   (test-equal 1 (h2-frame-stream-id frame))
                   (setf fields (hpack-decode decoder (h2-frame-payload frame))))
                  (:data
                   (test-equal 1 (h2-frame-stream-id frame))
                   (octet-buffer-append body (h2-frame-payload frame))
                   (when (h2-frame-flag-p frame *h2-flag-end-stream*)
                     (return)))))
       (test-equal "200" (rest (assoc ":status" fields :test #'string=)))
       (test-equal "/upgraded" (octets->latin1-string body))))
   :upgrade-headers '((":method" . "GET")
                      (":scheme" . "http")
                      (":authority" . "localhost")
                      (":path" . "/upgraded"))))

(define-test connection-protocol-errors
  (dolist (case '(("first frame is not SETTINGS" (:data 0 1 "00") :protocol-error)
                  ("HEADERS on stream 0" (:headers 4 0 "82") :protocol-error)
                  ("PING of the wrong size" (:ping 0 0 "0102") :frame-size-error)
                  ("WINDOW_UPDATE without increment" (:window-update 0 0 "00000000") :protocol-error)
                  ("even client stream" (:headers 5 2 "828684") :protocol-error)))
    (destructuring-bind (description (type flags stream-id hex) code) case
      (test-call-with-raw-server
       #'test-echo-handler
       (lambda (io)
         (write-sequence *h2-connection-preface* io)
         (unless (string= description "first frame is not SETTINGS")
           (h2-write-frame io (make-h2-frame ':settings)))
         (h2-write-frame io (make-h2-frame type :flags flags :stream-id stream-id :payload (test-hex hex)))
         (force-output io)
         (let ((goaway (loop for frame = (h2-read-frame io 16384)
                             while frame
                             when (eql (h2-frame-type frame) (h2-frame-type-number ':goaway))
                               return frame)))
           (test-assert goaway description)
           (test-equal code (h2-error-code-keyword (h2-decode-u32 (h2-frame-payload goaway) 4)))))))))
