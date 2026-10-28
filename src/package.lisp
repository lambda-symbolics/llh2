(defpackage #:llh2
  (:documentation "HTTP/2 over cleartext octet streams, with HPACK and flow control.")
  (:use #:cl)
  (:import-from #:serapeum
                #:->
                #:partial)
  (:export
   ;; Conditions
   #:h2-error
   #:h2-error-code
   #:h2-error-message
   #:h2-connection-error
   #:h2-stream-error
   #:h2-stream-error-stream-id
   #:h2-stream-closed
   #:h2-stream-closed-stream-id
   #:hpack-error
   ;; Octets
   #:octet
   #:octet-vector
   ;; Header compression
   #:hpack-decoder
   #:hpack-decode
   #:hpack-encoder
   #:hpack-encode
   #:hpack-encoder-reset-table
   ;; Connections
   #:*h2-default-settings*
   #:h2-connection
   #:h2-connection-close
   #:h2-connection-closed-p
   #:h2-connection-serve
   #:h2-open-stream
   #:h2-request
   #:make-h2-client-connection
   #:make-h2-server-connection
   ;; Streams
   #:h2-stream
   #:h2-stream-authority
   #:h2-stream-closed-p
   #:h2-stream-finish
   #:h2-stream-header
   #:h2-stream-headers
   #:h2-stream-id
   #:h2-stream-method
   #:h2-stream-path
   #:h2-stream-read-body
   #:h2-stream-read-data
   #:h2-stream-reset
   #:h2-stream-reset-code
   #:h2-stream-respond
   #:h2-stream-scheme
   #:h2-stream-send-headers
   #:h2-stream-status
   #:h2-stream-trailers
   #:h2-stream-wait-headers
   #:h2-stream-write-data))
