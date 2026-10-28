(in-package #:llh2)

;;;; -- RFC 7541 Examples --

(defparameter *hpack-test-series*
  '((4096
     ("828684410f7777772e6578616d706c652e636f6d"
      ((":method" . "GET") (":scheme" . "http") (":path" . "/") (":authority" . "www.example.com"))
      ((":authority" . "www.example.com"))
      57)
     ("828684be58086e6f2d6361636865"
      ((":method" . "GET") (":scheme" . "http") (":path" . "/") (":authority" . "www.example.com") ("cache-control" . "no-cache"))
      (("cache-control" . "no-cache") (":authority" . "www.example.com"))
      110)
     ("828785bf400a637573746f6d2d6b65790c637573746f6d2d76616c7565"
      ((":method" . "GET") (":scheme" . "https") (":path" . "/index.html") (":authority" . "www.example.com") ("custom-key" . "custom-value"))
      (("custom-key" . "custom-value") ("cache-control" . "no-cache") (":authority" . "www.example.com"))
      164))
    (4096
     ("828684418cf1e3c2e5f23a6ba0ab90f4ff"
      ((":method" . "GET") (":scheme" . "http") (":path" . "/") (":authority" . "www.example.com"))
      ((":authority" . "www.example.com"))
      57)
     ("828684be5886a8eb10649cbf"
      ((":method" . "GET") (":scheme" . "http") (":path" . "/") (":authority" . "www.example.com") ("cache-control" . "no-cache"))
      (("cache-control" . "no-cache") (":authority" . "www.example.com"))
      110)
     ("828785bf408825a849e95ba97d7f8925a849e95bb8e8b4bf"
      ((":method" . "GET") (":scheme" . "https") (":path" . "/index.html") (":authority" . "www.example.com") ("custom-key" . "custom-value"))
      (("custom-key" . "custom-value") ("cache-control" . "no-cache") (":authority" . "www.example.com"))
      164))
    (256
     ("4803333032580770726976617465611d4d6f6e2c203231204f637420323031332032303a31333a323120474d546e1768747470733a2f2f7777772e6578616d706c652e636f6d"
      ((":status" . "302") ("cache-control" . "private") ("date" . "Mon, 21 Oct 2013 20:13:21 GMT") ("location" . "https://www.example.com"))
      (("location" . "https://www.example.com") ("date" . "Mon, 21 Oct 2013 20:13:21 GMT") ("cache-control" . "private") (":status" . "302"))
      222)
     ("4803333037c1c0bf"
      ((":status" . "307") ("cache-control" . "private") ("date" . "Mon, 21 Oct 2013 20:13:21 GMT") ("location" . "https://www.example.com"))
      ((":status" . "307") ("location" . "https://www.example.com") ("date" . "Mon, 21 Oct 2013 20:13:21 GMT") ("cache-control" . "private"))
      222)
     ("88c1611d4d6f6e2c203231204f637420323031332032303a31333a323220474d54c05a04677a69707738666f6f3d4153444a4b48514b425a584f5157454f50495541585157454f49553b206d61782d6167653d333630303b2076657273696f6e3d31"
      ((":status" . "200") ("cache-control" . "private") ("date" . "Mon, 21 Oct 2013 20:13:22 GMT") ("location" . "https://www.example.com") ("content-encoding" . "gzip") ("set-cookie" . "foo=ASDJKHQKBZXOQWEOPIUAXQWEOIU; max-age=3600; version=1"))
      (("set-cookie" . "foo=ASDJKHQKBZXOQWEOPIUAXQWEOIU; max-age=3600; version=1") ("content-encoding" . "gzip") ("date" . "Mon, 21 Oct 2013 20:13:22 GMT"))
      215))
    (256
     ("488264025885aec3771a4b6196d07abe941054d444a8200595040b8166e082a62d1bff6e919d29ad171863c78f0b97c8e9ae82ae43d3"
      ((":status" . "302") ("cache-control" . "private") ("date" . "Mon, 21 Oct 2013 20:13:21 GMT") ("location" . "https://www.example.com"))
      (("location" . "https://www.example.com") ("date" . "Mon, 21 Oct 2013 20:13:21 GMT") ("cache-control" . "private") (":status" . "302"))
      222)
     ("4883640effc1c0bf"
      ((":status" . "307") ("cache-control" . "private") ("date" . "Mon, 21 Oct 2013 20:13:21 GMT") ("location" . "https://www.example.com"))
      ((":status" . "307") ("location" . "https://www.example.com") ("date" . "Mon, 21 Oct 2013 20:13:21 GMT") ("cache-control" . "private"))
      222)
     ("88c16196d07abe941054d444a8200595040b8166e084a62d1bffc05a839bd9ab77ad94e7821dd7f2e6c7b335dfdfcd5b3960d5af27087f3672c1ab270fb5291f9587316065c003ed4ee5b1063d5007"
      ((":status" . "200") ("cache-control" . "private") ("date" . "Mon, 21 Oct 2013 20:13:22 GMT") ("location" . "https://www.example.com") ("content-encoding" . "gzip") ("set-cookie" . "foo=ASDJKHQKBZXOQWEOPIUAXQWEOIU; max-age=3600; version=1"))
      (("set-cookie" . "foo=ASDJKHQKBZXOQWEOPIUAXQWEOIU; max-age=3600; version=1") ("content-encoding" . "gzip") ("date" . "Mon, 21 Oct 2013 20:13:22 GMT"))
      215)))
  "The header block series of RFC 7541 Appendix C.3 to C.6.

Each series starts with a table size; each step holds the encoded block, the
decoded fields, the dynamic table newest first, and the table size.")

(define-test hpack-rfc-integer-examples
  (dolist (case '((10 5 #x00 "0a")
                  (1337 5 #x00 "1f9a0a")
                  (42 8 #x00 "2a")
                  (31 5 #x00 "1f00")
                  (127 7 #x80 "ff00")))
    (destructuring-bind (value prefix-bits flags hex) case
      (let ((buffer (make-octet-buffer)))
        (hpack--encode-integer buffer value :prefix-bits prefix-bits :flags flags)
        (test-equal (test-hex hex) (octet-buffer->vector buffer) :test #'equalp)
        (test-equal value (hpack--decode-integer (test-hex hex) 0 prefix-bits))))))

(define-test hpack-rfc-decoding-series
  (dolist (series *hpack-test-series*)
    (destructuring-bind (table-size &rest steps) series
      (let ((decoder (make-instance 'hpack-decoder :table-limit table-size)))
        (hpack-table-resize (hpack-decoder-table decoder) table-size)
        (dolist (step steps)
          (destructuring-bind (hex fields dynamic size) step
            (test-equal fields (hpack-decode decoder (test-hex hex)))
            (test-equal dynamic
                        (reverse (coerce (hpack-table-entries (hpack-decoder-table decoder)) 'list)))
            (test-equal size (hpack-table-size (hpack-decoder-table decoder)))))))))

(define-test hpack-huffman-round-trip
  (dolist (text (list "" "a" "www.example.com" "no-cache" "custom-value"
                      (coerce (loop for code from 0 below 256 collect (code-char code)) 'string)))
    (let ((buffer (make-octet-buffer))
          (octets (test-text-octets text)))
      (hpack--huffman-encode buffer octets)
      (test-equal (hpack--huffman-length octets) (length buffer))
      (test-equal text (hpack--huffman-decode (octet-buffer->vector buffer) 0 (length buffer))))))

(define-test hpack-huffman-invalid-strings
  (dolist (hex '("ff"
                 "00"
                 "ffffffff"))
    (let ((octets (test-hex hex)))
      (test-signals hpack-error
        (hpack--huffman-decode octets 0 (length octets))))))

(define-test hpack-encoder-round-trip
  (let ((encoder (make-instance 'hpack-encoder))
        (decoder (make-instance 'hpack-decoder))
        (fields  '((":method" . "POST")
                   (":scheme" . "http")
                   (":path" . "/acp?token=x")
                   (":authority" . "127.0.0.1:8737")
                   ("content-type" . "application/json")
                   ("authorization" . "Bearer secret")
                   ("acp-connection-id" . "c-1")
                   ("x-empty" . ""))))
    (dotimes (repeat 3)
      (let ((block (hpack-encode encoder fields)))
        (test-equal fields (hpack-decode decoder block))
        (test-equal 0 (hpack-table-size (hpack-decoder-table decoder)))))
    (let ((block (hpack-encode (make-instance 'hpack-encoder) '(("authorization" . "Bearer secret")))))
      (test-equal #x10 (logand (aref block 1) #xf0) :test #'=))))

(define-test hpack-decoder-rejects-invalid-blocks
  (dolist (hex '("80"
                 "be"
                 "3fe21f"
                 "8220"
                 "0f"
                 "4005616263"
                 "ffffffffff0f"))
    (test-signals hpack-error
      (hpack-decode (make-instance 'hpack-decoder) (test-hex hex))))
  (test-signals hpack-error
    (hpack-decode (make-instance 'hpack-decoder :header-list-limit 40)
                  (test-hex "4001610a62626262626262626262"))))
