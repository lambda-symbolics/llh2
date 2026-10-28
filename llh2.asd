(asdf:defsystem #:llh2
  :description "HTTP/2 over cleartext streams: frames, HPACK, flow control, and h2c."
  :author      "Lambda Symbolics OÜ"
  :license     "COLL-Attribution"
  :version     "0.1.0"
  :serial      t
  :depends-on  (#:bordeaux-threads
                #:cl-base64
                #:serapeum)
  :components  ((:module     "src"
                 :serial     t
                 :components ((:file "package")
                              (:file "conditions")
                              (:file "octets")
                              (:file "hpack-tables")
                              (:file "hpack")
                              (:file "frames")
                              (:file "stream")
                              (:file "connection"))))
  :in-order-to ((asdf:test-op (asdf:test-op #:llh2/tests))))

(asdf:defsystem #:llh2/tests
  :description "Tests for llh2."
  :depends-on  (#:llh2
                #:ls-compat/tcp
                #:usocket)
  :serial      t
  :components  ((:module     "tests"
                 :serial     t
                 :components ((:file "test-support")
                              (:file "hpack-tests")
                              (:file "frame-tests")
                              (:file "connection-tests")
                              (:file "tests"))))
  :perform     (asdf:test-op (operation component)
                 (declare (ignore operation component))
                 (uiop:symbol-call '#:llh2 '#:run-tests)))
