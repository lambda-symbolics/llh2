(in-package #:llh2)

;;;; -- Connection Policy --

(defparameter *h2-default-settings*
  '((:max-concurrent-streams . 128)
    (:initial-window-size    . 1048576)
    (:max-header-list-size   . 262144))
  "The settings this side announces unless a constructor overrides them.")

(defparameter *h2-connection-receive-window* 16777216
  "The flow-control window this side opens for the whole connection.")

(defparameter *h2-maximum-header-block* 1048576
  "The largest compressed header block, CONTINUATION frames included, this side accepts.")

(defparameter *h2-recent-reset-limit* 256
  "How many recently reset stream identifiers keep late frames from being errors.")

(defparameter *h2-peer-setting-defaults*
  '((:header-table-size      . 4096)
    (:enable-push            . 1)
    (:max-concurrent-streams . nil)
    (:initial-window-size    . 65535)
    (:max-frame-size         . 16384)
    (:max-header-list-size   . nil))
  "The values a peer's settings have before its first SETTINGS frame; NIL means unlimited.")

(defparameter *h2-connection-specific-fields*
  '("connection" "keep-alive" "proxy-connection" "transfer-encoding" "upgrade")
  "Fields HTTP/2 forbids because they describe an HTTP/1.1 connection.")


;;;; -- Connections --

(defclass h2-connection ()
  ((io
    :initarg       :io
    :reader        h2-connection-io
    :type          stream
    :documentation "The bidirectional octet stream the connection owns and closes.")
   (role
    :initarg       :role
    :reader        h2-connection-role
    :type          (member :server :client)
    :documentation "Which side of the connection this endpoint is.")
   (handler
    :initarg       :handler
    :initform      nil
    :reader        h2-connection-handler
    :type          (or null function)
    :documentation "On a server, the function called with each new request stream in its own thread.")
   (preface
    :initarg       :preface
    :initform      ':full
    :reader        h2-connection--preface
    :type          (member :full :after-request-line)
    :documentation "On a server, how much of the client preface is still unread.")
   (shutdown-function
    :initarg       :shutdown-function
    :initform      nil
    :reader        h2-connection-shutdown-function
    :type          (or null function)
    :documentation "An optional function that wakes a thread blocked reading IO, such as a socket shutdown.")
   (log-function
    :initarg       :log-function
    :initform      nil
    :reader        h2-connection-log-function
    :type          (or null function)
    :documentation "An optional function called with a diagnostic line.")
   (lock
    :initform      (bt2:make-lock :name "llh2 connection state")
    :reader        h2-connection--lock
    :documentation "The lock guarding connection and stream state.")
   (condition
    :initform      (bt2:make-condition-variable :name "llh2 connection state")
    :reader        h2-connection--condition
    :documentation "Broadcast after every state change waiters may care about.")
   (write-lock
    :initform      (bt2:make-lock :name "llh2 connection output")
    :reader        h2-connection--write-lock
    :documentation "The lock serializing output; taken before the state lock, never after.")
   (encoder
    :initform      (make-instance 'hpack-encoder)
    :reader        h2-connection--encoder
    :documentation "The header compression state for blocks this side sends.")
   (decoder
    :accessor      h2-connection--decoder
    :documentation "The header compression state for blocks the peer sends.")
   (local-settings
    :initarg       :local-settings
    :reader        h2-connection-local-settings
    :type          list
    :documentation "The (KEYWORD . VALUE) settings this side announced.")
   (peer-settings
    :initform      (copy-alist *h2-peer-setting-defaults*)
    :accessor      h2-connection--peer-settings
    :type          list
    :documentation "The (KEYWORD . VALUE) settings the peer announced so far.")
   (peer-settings-p
    :initform      nil
    :accessor      h2-connection--peer-settings-p
    :type          boolean
    :documentation "Whether the peer's first SETTINGS frame arrived.")
   (streams
    :initform      (make-hash-table)
    :reader        h2-connection--streams
    :documentation "The streams that are not yet closed, by identifier.")
   (recent-resets
    :initform      nil
    :accessor      h2-connection--recent-resets
    :type          list
    :documentation "Recently reset stream identifiers, newest first.")
   (last-peer-stream-id
    :initform      0
    :accessor      h2-connection--last-peer-stream-id
    :type          (integer 0 *)
    :documentation "The highest stream identifier the peer opened.")
   (next-stream-id
    :initarg       :next-stream-id
    :accessor      h2-connection--next-stream-id
    :type          (integer 1 *)
    :documentation "The identifier of the next stream this side opens.")
   (send-window
    :initform      65535
    :accessor      h2-connection--send-window
    :type          integer
    :documentation "The data octets the peer currently accepts on the whole connection.")
   (receive-window
    :initform      65535
    :accessor      h2-connection--receive-window
    :type          integer
    :documentation "The data octets the peer may still send on the whole connection.")
   (header-block
    :initform      nil
    :accessor      h2-connection--header-block
    :type          list
    :documentation "While CONTINUATION frames are due: (STREAM-ID END-STREAM-P BUFFER).")
   (goaway-received-p
    :initform      nil
    :accessor      h2-connection--goaway-received-p
    :type          boolean
    :documentation "Whether the peer announced the end of the connection.")
   (goaway-sent-p
    :initform      nil
    :accessor      h2-connection--goaway-sent-p
    :type          boolean
    :documentation "Whether this side announced the end of the connection.")
   (closed-p
    :initform      nil
    :accessor      h2-connection--closed-p
    :type          boolean
    :documentation "Whether the connection ended.")
   (reader-thread
    :initform      nil
    :accessor      h2-connection--reader-thread
    :documentation "On a client, the thread reading frames from the server."))
  (:documentation "One HTTP/2 connection over a cleartext octet stream."))

(defmethod initialize-instance :after ((connection h2-connection) &key)
  "Size the header decoder from the settings this side announces."
  (setf (h2-connection--decoder connection)
        (make-instance 'hpack-decoder
                       :table-limit       (h2-connection--local-setting connection ':header-table-size)
                       :header-list-limit (h2-connection--local-setting connection ':max-header-list-size))))


;;;; -- Public Interface --

(-> make-h2-server-connection (stream &key (:handler function) (:settings list) (:preface keyword)
                                      (:shutdown-function (or null function)) (:log-function (or null function)))
    h2-connection)
(defun make-h2-server-connection (io &key handler settings (preface ':full) shutdown-function log-function)
  "Return a server connection over the octet stream IO; H2-CONNECTION-SERVE runs it.

HANDLER receives each request stream in a new thread and must respond on it.
SETTINGS override entries of *H2-DEFAULT-SETTINGS*. PREFACE is :FULL when the
client preface is still unread and :AFTER-REQUEST-LINE when the caller read its
first line, \"PRI * HTTP/2.0\", and the blank line after it.

The connection owns IO and closes it once its reader stops. Closing a stream
does not wake a thread blocked reading it, so a socket owner passes
SHUTDOWN-FUNCTION, which shuts the socket down when the connection ends."
  (make-instance 'h2-connection
                 :io             io
                 :role           ':server
                 :handler        handler
                 :preface        preface
                 :shutdown-function shutdown-function
                 :log-function   log-function
                 :next-stream-id 2
                 :local-settings (h2--merge-settings settings)))

(-> h2-connection-serve (h2-connection &key (:upgrade-headers list) (:upgrade-body (or null (vector octet)))
                                       (:upgrade-settings (or null string)))
    null)
(defun h2-connection-serve (connection &key upgrade-headers upgrade-body upgrade-settings)
  "Run the server CONNECTION in the calling thread until it ends, then close it.

For an HTTP/1.1 Upgrade to h2c, after the caller sent 101 Switching Protocols,
UPGRADE-HEADERS holds the request as HTTP/2 fields with pseudo-headers,
UPGRADE-BODY its body, and UPGRADE-SETTINGS the HTTP2-Settings header value.
The request becomes stream 1."
  (unwind-protect
       (handler-case
           (progn
             (when upgrade-settings
               (h2-connection--apply-settings connection (h2-decode-settings-header upgrade-settings)
                                              :acknowledge-p nil))
             (h2-connection--send-opening connection)
             (h2-connection--read-preface connection)
             (when upgrade-headers
               (h2-connection--accept-upgrade connection upgrade-headers (or upgrade-body (make-octet-vector 0))))
             (h2-connection--read-loop connection))
         (h2-connection-error (condition)
           (h2-connection--fail connection condition)))
    (h2-connection--release connection))
  nil)

(-> make-h2-client-connection (stream &key (:settings list) (:shutdown-function (or null function))
                                      (:log-function (or null function)))
    h2-connection)
(defun make-h2-client-connection (io &key settings shutdown-function log-function)
  "Return a client connection over the octet stream IO, with prior knowledge of HTTP/2.

The connection sends its preface at once and reads frames in its own thread.
SETTINGS override entries of *H2-DEFAULT-SETTINGS*. SHUTDOWN-FUNCTION is as
for MAKE-H2-SERVER-CONNECTION."
  (let ((connection (make-instance 'h2-connection
                                   :io             io
                                   :role           ':client
                                   :shutdown-function shutdown-function
                                   :log-function   log-function
                                   :next-stream-id 1
                                   :local-settings (h2--merge-settings
                                                    (cons (cons ':enable-push 0) settings)))))
    (bt2:with-lock-held ((h2-connection--write-lock connection))
      (write-sequence *h2-connection-preface* (h2-connection-io connection)))
    (h2-connection--send-opening connection)
    (setf (h2-connection--reader-thread connection)
          (bt2:make-thread (lambda ()
                             (unwind-protect
                                  (handler-case (h2-connection--read-loop connection)
                                    (h2-connection-error (condition)
                                      (h2-connection--fail connection condition)))
                               (h2-connection--release connection)))
                           :name "llh2 client reader"))
    connection))

(-> h2-open-stream (h2-connection list &key (:end-stream-p boolean) (:timeout (or null real))) h2-stream)
(defun h2-open-stream (connection headers &key end-stream-p timeout)
  "Open a client stream sending the (NAME . VALUE) request HEADERS and return it.

Wait up to TIMEOUT seconds while the server's concurrent stream limit is
reached. Signal H2-STREAM-CLOSED when the connection cannot open streams."
  (bt2:with-lock-held ((h2-connection--lock connection))
    (unless (h2-connection--wait connection
                                 (lambda ()
                                   (or (h2-connection--closed-p connection)
                                       (h2-connection--goaway-received-p connection)
                                       (h2-connection--stream-slot-p connection)))
                                 :timeout timeout)
      (error 'h2-stream-closed :stream-id 0 :message "No stream became available in time."))
    (when (or (h2-connection--closed-p connection)
              (h2-connection--goaway-received-p connection))
      (error 'h2-stream-closed :stream-id 0 :message "The connection accepts no new streams.")))
  (h2-connection--send-headers connection nil headers :end-stream-p end-stream-p))

(-> h2-request (h2-connection &key (:method string) (:path string) (:authority string) (:scheme string)
                                   (:headers list) (:body (or null (vector octet))))
    h2-stream)
(defun h2-request (connection &key (method "GET") (path "/") (authority "localhost") (scheme "http")
                                   headers body)
  "Send a request on a client CONNECTION and return its stream once the body is sent.

Read the response with H2-STREAM-WAIT-HEADERS and H2-STREAM-READ-DATA."
  (let* ((fields (append (list (cons ":method" method)
                               (cons ":scheme" scheme)
                               (cons ":authority" authority)
                               (cons ":path" path))
                         headers))
         (stream (h2-open-stream connection fields :end-stream-p (null body))))
    (when body
      (h2-stream-write-data stream body :end-stream-p t))
    stream))

(-> h2-connection-closed-p (h2-connection) boolean)
(defun h2-connection-closed-p (connection)
  "Return whether CONNECTION ended."
  (bt2:with-lock-held ((h2-connection--lock connection))
    (h2-connection--closed-p connection)))

(-> h2-connection-close (h2-connection &key (:code keyword)) null)
(defun h2-connection-close (connection &key (code ':no-error))
  "Announce the end of CONNECTION with GOAWAY carrying CODE, then close its stream."
  (unless (h2-connection-closed-p connection)
    (handler-case (h2-connection--send-goaway connection code "")
      (error ()
        nil)))
  (h2-connection--lose connection)
  (let ((thread (h2-connection--reader-thread connection)))
    (when (and thread (not (eq thread (bt2:current-thread))))
      (bt2:join-thread thread)))
  nil)


;;;; -- Settings --

(-> h2--merge-settings (list) list)
(defun h2--merge-settings (settings)
  "Return *H2-DEFAULT-SETTINGS* with the entries of SETTINGS taking precedence."
  (let ((merged (copy-alist settings)))
    (loop for (key . value) in *h2-default-settings*
          unless (assoc key merged)
            do (setf merged (append merged (list (cons key value)))))
    merged))

(-> h2-connection--local-setting (h2-connection keyword) t)
(defun h2-connection--local-setting (connection key)
  "Return the value of setting KEY this side announced, or the protocol default."
  (let ((entry (assoc key (h2-connection-local-settings connection))))
    (if entry
        (rest entry)
        (case key
          (:header-table-size 4096)
          (:initial-window-size 65535)
          (:max-frame-size *h2-default-frame-size*)
          (:max-header-list-size *hpack-default-header-list-limit*)
          (t nil)))))

(-> h2-connection--peer-setting (h2-connection keyword) t)
(defun h2-connection--peer-setting (connection key)
  "Return the value of the peer's setting KEY; NIL means unlimited."
  (rest (assoc key (h2-connection--peer-settings connection))))

(-> h2-connection--apply-settings (h2-connection list &key (:acknowledge-p boolean)) null)
(defun h2-connection--apply-settings (connection settings &key (acknowledge-p t))
  "Apply the peer's SETTINGS pairs and acknowledge them when ACKNOWLEDGE-P."
  (bt2:with-lock-held ((h2-connection--lock connection))
    (loop for (key . value) in settings
          do (case key
               (:enable-push
                (when (or (> value 1)
                          (and (eq (h2-connection-role connection) ':client) (= value 1)))
                  (error 'h2-connection-error :code    ':protocol-error
                                              :message "SETTINGS_ENABLE_PUSH has an invalid value.")))
               (:initial-window-size
                (when (> value *h2-largest-window*)
                  (error 'h2-connection-error :code    ':flow-control-error
                                              :message "SETTINGS_INITIAL_WINDOW_SIZE is too large."))
                (let ((delta (- value (h2-connection--peer-setting connection ':initial-window-size))))
                  (loop for stream being the hash-values of (h2-connection--streams connection)
                        do (when (> (incf (h2-stream--send-window stream) delta) *h2-largest-window*)
                             (error 'h2-connection-error
                                    :code    ':flow-control-error
                                    :message "A settings change overflows a stream window.")))))
               (:max-frame-size
                (unless (<= *h2-default-frame-size* value *h2-largest-frame-size*)
                  (error 'h2-connection-error :code    ':protocol-error
                                              :message "SETTINGS_MAX_FRAME_SIZE is out of range.")))
               (:header-table-size
                (hpack-encoder-reset-table (h2-connection--encoder connection))))
             (when (keywordp key)
               (setf (rest (assoc key (h2-connection--peer-settings connection))) value)))
    (setf (h2-connection--peer-settings-p connection) t)
    (bt2:condition-broadcast (h2-connection--condition connection)))
  (when acknowledge-p
    (h2-connection--send-frame connection ':settings :flags *h2-flag-ack*))
  nil)


;;;; -- Output --

(-> h2-connection--write-frames (h2-connection list) null)
(defun h2-connection--write-frames (connection frames)
  "Write FRAMES in order and force output; hold the write lock."
  (let ((io (h2-connection-io connection)))
    (handler-case
        (progn
          (dolist (frame frames)
            (h2-write-frame io frame))
          (force-output io))
      (stream-error ()
        (h2-connection--lose connection)
        (error 'h2-stream-closed :stream-id 0 :message "The connection failed while writing."))))
  nil)

(-> h2-connection--send-frame (h2-connection keyword &key (:flags (unsigned-byte 8))
                                             (:stream-id (unsigned-byte 31)) (:payload (vector octet)))
    null)
(defun h2-connection--send-frame (connection type &key (flags 0) (stream-id 0) (payload (make-octet-vector 0)))
  "Send one frame of TYPE with FLAGS, STREAM-ID, and PAYLOAD."
  (bt2:with-lock-held ((h2-connection--write-lock connection))
    (h2-connection--write-frames connection
                                 (list (make-h2-frame type :flags     flags
                                                           :stream-id stream-id
                                                           :payload   payload))))
  nil)

(-> h2-connection--send-opening (h2-connection) null)
(defun h2-connection--send-opening (connection)
  "Send this side's SETTINGS and open the connection receive window."
  (let ((increment (- *h2-connection-receive-window* 65535)))
    (h2-connection--send-frame connection ':settings
                               :payload (h2-encode-settings (h2-connection-local-settings connection)))
    (when (plusp increment)
      (bt2:with-lock-held ((h2-connection--lock connection))
        (incf (h2-connection--receive-window connection) increment))
      (h2-connection--send-window-update connection 0 increment)))
  nil)

(-> h2-connection--send-window-update (h2-connection (unsigned-byte 31) (integer 1 *)) null)
(defun h2-connection--send-window-update (connection stream-id increment)
  "Open the receive window of STREAM-ID, or of the connection for 0, by INCREMENT."
  (handler-case (h2-connection--send-frame connection ':window-update
                                           :stream-id stream-id
                                           :payload   (h2-u32-payload increment))
    (h2-stream-closed ()
      nil))
  nil)

(-> h2-connection--send-reset (h2-connection (unsigned-byte 31) keyword) null)
(defun h2-connection--send-reset (connection stream-id code)
  "Send RST_STREAM for STREAM-ID with CODE."
  (handler-case (h2-connection--send-frame connection ':rst-stream
                                           :stream-id stream-id
                                           :payload   (h2-u32-payload (h2-error-code-number code)))
    (h2-stream-closed ()
      nil))
  nil)

(-> h2-connection--send-goaway (h2-connection keyword string) null)
(defun h2-connection--send-goaway (connection code message)
  "Send GOAWAY with CODE and the debug MESSAGE, once."
  (let ((last-stream-id nil))
    (bt2:with-lock-held ((h2-connection--lock connection))
      (unless (h2-connection--goaway-sent-p connection)
        (setf (h2-connection--goaway-sent-p connection) t
              last-stream-id                            (h2-connection--last-peer-stream-id connection))))
    (when last-stream-id
      (let* ((debug   (latin1-string->octets (remove-if (lambda (character) (> (char-code character) 255))
                                                        message)))
             (payload (concatenate 'octet-vector
                                   (h2-u32-payload last-stream-id (h2-error-code-number code))
                                   debug)))
        (h2-connection--send-frame connection ':goaway :payload payload))))
  nil)

(-> h2-connection--send-headers (h2-connection (or null h2-stream) list &key (:end-stream-p boolean))
    h2-stream)
(defun h2-connection--send-headers (connection stream headers &key end-stream-p)
  "Send HEADERS on STREAM, or on a new client stream when STREAM is NIL, and return the stream.

The header block is encoded and written under the write lock, so blocks reach
the peer in the order their compression state assumes, split into
CONTINUATION frames as the peer's frame size requires."
  (let ((fields (loop for (name . value) in headers
                      collect (cons (string-downcase name) value))))
    (bt2:with-lock-held ((h2-connection--write-lock connection))
      (let* ((block      (hpack-encode (h2-connection--encoder connection) fields))
             (frame-size (bt2:with-lock-held ((h2-connection--lock connection))
                           (h2-connection--peer-setting connection ':max-frame-size))))
        (unless stream
          (setf stream (h2-connection--open-local-stream connection)))
        (let ((frames (loop for start from 0 by frame-size
                            for first-p = t then nil
                            for end = (min (length block) (+ start frame-size))
                            for last-p = (= end (length block))
                            collect (make-h2-frame (if first-p ':headers ':continuation)
                                                   :stream-id (h2-stream-id stream)
                                                   :flags     (logior (if last-p *h2-flag-end-headers* 0)
                                                                      (if (and first-p end-stream-p)
                                                                          *h2-flag-end-stream*
                                                                          0))
                                                   :payload   (subseq block start end))
                            until last-p)))
          (h2-connection--write-frames connection frames))))
    (bt2:with-lock-held ((h2-connection--lock connection))
      (setf (h2-stream--headers-sent-p stream) t)
      (when end-stream-p
        (setf (h2-stream--local-closed-p stream) t)
        (h2-connection--note-closed connection stream))
      (bt2:condition-broadcast (h2-connection--condition connection)))
    stream))

(-> h2-connection--send-data (h2-connection h2-stream octet-vector &key (:start (integer 0 *))
                                            (:end (integer 0 *)) (:end-stream-p boolean))
    null)
(defun h2-connection--send-data (connection stream octets &key start end end-stream-p)
  "Send OCTETS between START and END as one DATA frame; the caller holds the flow-control credit."
  (bt2:with-lock-held ((h2-connection--write-lock connection))
    (h2-connection--write-frames connection
                                 (list (make-h2-frame ':data
                                                      :stream-id (h2-stream-id stream)
                                                      :flags     (if end-stream-p *h2-flag-end-stream* 0)
                                                      :payload   (subseq octets start end)))))
  (when end-stream-p
    (bt2:with-lock-held ((h2-connection--lock connection))
      (setf (h2-stream--local-closed-p stream) t)
      (h2-connection--note-closed connection stream)
      (bt2:condition-broadcast (h2-connection--condition connection))))
  nil)


;;;; -- Stream Bookkeeping --

(-> h2-connection--wait (h2-connection function &key (:timeout (or null real))) boolean)
(defun h2-connection--wait (connection predicate &key timeout)
  "Wait until PREDICATE returns true or TIMEOUT seconds pass; hold the state lock.

Return T when PREDICATE became true and NIL on timeout."
  (let ((deadline (and timeout
                       (+ (get-internal-real-time)
                          (round (* timeout internal-time-units-per-second))))))
    (loop
      (when (funcall predicate)
        (return t))
      (let ((remaining (and deadline
                            (/ (- deadline (get-internal-real-time))
                               internal-time-units-per-second))))
        (when (and remaining (<= remaining 0))
          (return nil))
        (bt2:condition-wait (h2-connection--condition connection)
                            (h2-connection--lock connection)
                            :timeout remaining)))))

(-> h2-connection--make-stream (h2-connection (unsigned-byte 31)) h2-stream)
(defun h2-connection--make-stream (connection id)
  "Create and register stream ID with the current initial windows; hold the state lock."
  (setf (gethash id (h2-connection--streams connection))
        (make-instance 'h2-stream
                       :connection     connection
                       :id             id
                       :send-window    (h2-connection--peer-setting connection ':initial-window-size)
                       :receive-window (h2-connection--local-setting connection ':initial-window-size))))

(-> h2-connection--open-local-stream (h2-connection) h2-stream)
(defun h2-connection--open-local-stream (connection)
  "Allocate the next identifier for a stream this side opens; hold the write lock."
  (bt2:with-lock-held ((h2-connection--lock connection))
    (let ((id (h2-connection--next-stream-id connection)))
      (when (> id *h2-largest-window*)
        (error 'h2-stream-closed :stream-id 0 :message "The connection ran out of stream identifiers."))
      (incf (h2-connection--next-stream-id connection) 2)
      (h2-connection--make-stream connection id))))

(-> h2-connection--stream-slot-p (h2-connection) boolean)
(defun h2-connection--stream-slot-p (connection)
  "Return whether the peer's concurrent stream limit admits another local stream; hold the lock."
  (let ((limit (h2-connection--peer-setting connection ':max-concurrent-streams)))
    (or (null limit)
        (< (loop for stream being the hash-values of (h2-connection--streams connection)
                 count (h2-connection--local-stream-id-p connection (h2-stream-id stream)))
           limit))))

(-> h2-connection--local-stream-id-p (h2-connection (unsigned-byte 31)) boolean)
(defun h2-connection--local-stream-id-p (connection id)
  "Return whether stream ID has the parity of streams this side opens."
  (eq (oddp id) (eq (h2-connection-role connection) ':client)))

(-> h2-connection--note-closed (h2-connection h2-stream) null)
(defun h2-connection--note-closed (connection stream)
  "Forget STREAM once it can carry nothing more; hold the state lock."
  (when (or (h2-stream-reset-code stream)
            (and (h2-stream--local-closed-p stream) (h2-stream--remote-closed-p stream)))
    (remhash (h2-stream-id stream) (h2-connection--streams connection)))
  nil)

(-> h2-connection--mark-reset (h2-connection h2-stream keyword) null)
(defun h2-connection--mark-reset (connection stream code)
  "Record that STREAM was reset with CODE and wake its waiters; hold the state lock."
  (unless (h2-stream-reset-code stream)
    (setf (h2-stream-reset-code stream) code)
    (push (h2-stream-id stream) (h2-connection--recent-resets connection))
    (let ((excess (nthcdr *h2-recent-reset-limit* (h2-connection--recent-resets connection))))
      (when excess
        (setf (h2-connection--recent-resets connection)
              (ldiff (h2-connection--recent-resets connection) excess)))))
  (h2-connection--note-closed connection stream)
  (bt2:condition-broadcast (h2-connection--condition connection))
  nil)

(-> h2-connection--lose (h2-connection) null)
(defun h2-connection--lose (connection)
  "Mark CONNECTION ended, wake every waiter, and wake its reader.

The shutdown function wakes the reader; without one, closing the stream is
the best this side can do."
  (let ((close-p nil))
    (bt2:with-lock-held ((h2-connection--lock connection))
      (unless (h2-connection--closed-p connection)
        (setf (h2-connection--closed-p connection) t
              close-p                              t))
      (bt2:condition-broadcast (h2-connection--condition connection)))
    (when close-p
      (handler-case (let ((function (h2-connection-shutdown-function connection)))
                      (if function
                          (funcall function)
                          (close (h2-connection-io connection))))
        (error ()
          nil))))
  nil)

(-> h2-connection--release (h2-connection) null)
(defun h2-connection--release (connection)
  "End CONNECTION and close its stream; called by the reader once it stops."
  (h2-connection--lose connection)
  (handler-case (close (h2-connection-io connection))
    (error ()
      nil))
  nil)

(-> h2-connection--log (h2-connection string &rest t) null)
(defun h2-connection--log (connection control &rest arguments)
  "Pass the line CONTROL formatted with ARGUMENTS to the log function, when there is one."
  (let ((function (h2-connection-log-function connection)))
    (when function
      (funcall function (apply #'format nil control arguments))))
  nil)

(-> h2-connection--fail (h2-connection h2-connection-error) null)
(defun h2-connection--fail (connection condition)
  "End CONNECTION after the peer broke the protocol as CONDITION describes."
  (h2-connection--log connection "HTTP/2 connection error: ~A" condition)
  (handler-case (h2-connection--send-goaway connection (h2-error-code condition) (h2-error-message condition))
    (error ()
      nil))
  nil)


;;;; -- Input --

(-> h2-connection--read-preface (h2-connection) null)
(defun h2-connection--read-preface (connection)
  "Read and check the part of the client preface the caller left unread."
  (let* ((full     *h2-connection-preface*)
         (expected (if (eq (h2-connection--preface connection) ':after-request-line)
                       (subseq full (- (length full) 6))
                       full))
         (received (h2--read-octets (h2-connection-io connection) (length expected))))
    (unless (and received (equalp received expected))
      (error 'h2-connection-error :code    ':protocol-error
                                  :message "The client preface is missing.")))
  nil)

(-> h2-connection--read-loop (h2-connection) null)
(defun h2-connection--read-loop (connection)
  "Read and process frames until the input ends or the connection closes."
  (let ((io (h2-connection-io connection)))
    (handler-case
        (loop
          (let ((frame (h2-read-frame io (h2-connection--local-setting connection ':max-frame-size))))
            (unless frame
              (return))
            (handler-case (h2-connection--dispatch connection frame)
              (h2-stream-error (condition)
                (h2-connection--log connection "HTTP/2 stream error: ~A" condition)
                (let ((stream (bt2:with-lock-held ((h2-connection--lock connection))
                                (gethash (h2-stream-error-stream-id condition)
                                         (h2-connection--streams connection)))))
                  (when stream
                    (bt2:with-lock-held ((h2-connection--lock connection))
                      (h2-connection--mark-reset connection stream (h2-error-code condition)))))
                (h2-connection--send-reset connection
                                           (h2-stream-error-stream-id condition)
                                           (h2-error-code condition))))
            (when (h2-connection--finished-p connection)
              (return))))
      (stream-error ()
        nil)))
  nil)

(-> h2-connection--finished-p (h2-connection) boolean)
(defun h2-connection--finished-p (connection)
  "Return whether the peer announced the end of CONNECTION and no stream remains open."
  (bt2:with-lock-held ((h2-connection--lock connection))
    (and (h2-connection--goaway-received-p connection)
         (zerop (hash-table-count (h2-connection--streams connection))))))

(-> h2-connection--dispatch (h2-connection h2-frame) null)
(defun h2-connection--dispatch (connection frame)
  "Process one FRAME from the peer."
  (let ((type (h2-frame-type-keyword (h2-frame-type frame))))
    (when (and (h2-connection--header-block connection) (not (eq type ':continuation)))
      (error 'h2-connection-error :code    ':protocol-error
                                  :message "A header block was interrupted."))
    (unless (or (h2-connection--peer-settings-p connection) (eq type ':settings))
      (error 'h2-connection-error :code    ':protocol-error
                                  :message "The first frame from the peer is not SETTINGS."))
    (case type
      (:data (h2-connection--receive-data connection frame))
      (:headers (h2-connection--receive-headers connection frame))
      (:priority (h2-connection--receive-priority connection frame))
      (:rst-stream (h2-connection--receive-reset connection frame))
      (:settings (h2-connection--receive-settings connection frame))
      (:push-promise
       (error 'h2-connection-error :code    ':protocol-error
                                   :message "Server push is disabled."))
      (:ping (h2-connection--receive-ping connection frame))
      (:goaway (h2-connection--receive-goaway connection frame))
      (:window-update (h2-connection--receive-window-update connection frame))
      (:continuation (h2-connection--receive-continuation connection frame))
      (t nil)))
  nil)

(-> h2-connection--require-stream-id (h2-frame) null)
(defun h2-connection--require-stream-id (frame)
  "Signal a PROTOCOL_ERROR when FRAME, which belongs to a stream, names stream 0."
  (when (zerop (h2-frame-stream-id frame))
    (error 'h2-connection-error :code    ':protocol-error
                                :message "A stream frame names stream 0."))
  nil)

(-> h2-connection--require-connection-id (h2-frame) null)
(defun h2-connection--require-connection-id (frame)
  "Signal a PROTOCOL_ERROR when FRAME, which belongs to the connection, names a stream."
  (unless (zerop (h2-frame-stream-id frame))
    (error 'h2-connection-error :code    ':protocol-error
                                :message "A connection frame names a stream."))
  nil)

(-> h2-connection--stream-state (h2-connection (unsigned-byte 31)) (values (or null h2-stream) keyword))
(defun h2-connection--stream-state (connection id)
  "Return the live stream ID, if any, and :OPEN, :IDLE, :RESET, or :CLOSED; hold the state lock."
  (let ((stream (gethash id (h2-connection--streams connection))))
    (cond
      (stream
       (values stream ':open))
      ((member id (h2-connection--recent-resets connection))
       (values nil ':reset))
      ((if (h2-connection--local-stream-id-p connection id)
           (>= id (h2-connection--next-stream-id connection))
           (> id (h2-connection--last-peer-stream-id connection)))
       (values nil ':idle))
      (t
       (values nil ':closed)))))

(-> h2-connection--receive-data (h2-connection h2-frame) null)
(defun h2-connection--receive-data (connection frame)
  "Process a DATA frame."
  (h2-connection--require-stream-id frame)
  (multiple-value-bind (start end) (h2-frame-content-bounds frame)
    (let* ((id          (h2-frame-stream-id frame))
           (length      (length (h2-frame-payload frame)))
           (end-p       (h2-frame-flag-p frame *h2-flag-end-stream*))
           (replenish   0)
           (stream-code nil))
      (bt2:with-lock-held ((h2-connection--lock connection))
        (when (> length (h2-connection--receive-window connection))
          (error 'h2-connection-error :code    ':flow-control-error
                                      :message "DATA exceeds the connection window."))
        (decf (h2-connection--receive-window connection) length)
        (when (< (h2-connection--receive-window connection) (floor *h2-connection-receive-window* 2))
          (setf replenish (- *h2-connection-receive-window* (h2-connection--receive-window connection)))
          (incf (h2-connection--receive-window connection) replenish))
        (multiple-value-bind (stream state) (h2-connection--stream-state connection id)
          (case state
            (:idle
             (error 'h2-connection-error :code    ':protocol-error
                                         :message "DATA arrived on an idle stream."))
            (:closed
             (setf stream-code ':stream-closed))
            (:open
             (cond
               ((or (h2-stream--remote-closed-p stream) (not (h2-stream--headers-p stream)))
                (setf stream-code ':stream-closed))
               ((> length (h2-stream--receive-window stream))
                (setf stream-code ':flow-control-error))
               (t
                (decf (h2-stream--receive-window stream) length)
                (incf (h2-stream--consumed stream) (- length (- end start)))
                (incf (h2-stream--received-length stream) (- end start))
                (when (> end start)
                  (setf (h2-stream--incoming stream)
                        (append (h2-stream--incoming stream)
                                (list (subseq (h2-frame-payload frame) start end)))))
                (when end-p
                  (h2-connection--finish-remote connection stream))
                (bt2:condition-broadcast (h2-connection--condition connection))))))))
      (when (plusp replenish)
        (h2-connection--send-window-update connection 0 replenish))
      (when stream-code
        (error 'h2-stream-error :stream-id id
                                :code      stream-code
                                :message   "DATA arrived where the stream accepts none."))))
  nil)

(-> h2-connection--finish-remote (h2-connection h2-stream) null)
(defun h2-connection--finish-remote (connection stream)
  "Record the peer's END_STREAM on STREAM after checking its content length; hold the state lock."
  (let ((expected (h2-stream--expected-length stream)))
    (when (and expected (/= expected (h2-stream--received-length stream)))
      (error 'h2-stream-error :stream-id (h2-stream-id stream)
                              :code      ':protocol-error
                              :message   "The data length differs from content-length.")))
  (setf (h2-stream--remote-closed-p stream) t)
  (h2-connection--note-closed connection stream)
  nil)

(-> h2-connection--receive-headers (h2-connection h2-frame) null)
(defun h2-connection--receive-headers (connection frame)
  "Process a HEADERS frame, completing its header block or awaiting CONTINUATION."
  (h2-connection--require-stream-id frame)
  (multiple-value-bind (start end) (h2-frame-content-bounds frame)
    (when (and (h2-frame-flag-p frame *h2-flag-priority*)
               (= (logand (h2-decode-u32 (h2-frame-payload frame)
                                         (if (h2-frame-flag-p frame *h2-flag-padded*) 1 0))
                          *h2-largest-window*)
                  (h2-frame-stream-id frame)))
      (error 'h2-stream-error :stream-id (h2-frame-stream-id frame)
                              :code      ':protocol-error
                              :message   "A stream depends on itself."))
    (let ((buffer (make-octet-buffer)))
      (octet-buffer-append buffer (h2-frame-payload frame) :start start :end end)
      (setf (h2-connection--header-block connection)
            (list (h2-frame-stream-id frame) (h2-frame-flag-p frame *h2-flag-end-stream*) buffer))
      (when (h2-frame-flag-p frame *h2-flag-end-headers*)
        (h2-connection--complete-header-block connection))))
  nil)

(-> h2-connection--receive-continuation (h2-connection h2-frame) null)
(defun h2-connection--receive-continuation (connection frame)
  "Process a CONTINUATION frame of the header block in progress."
  (destructuring-bind (&optional stream-id end-stream-p buffer) (h2-connection--header-block connection)
    (declare (ignore end-stream-p))
    (unless (and stream-id (= stream-id (h2-frame-stream-id frame)))
      (error 'h2-connection-error :code    ':protocol-error
                                  :message "CONTINUATION arrived without its header block."))
    (when (> (+ (length buffer) (length (h2-frame-payload frame))) *h2-maximum-header-block*)
      (error 'h2-connection-error :code    ':enhance-your-calm
                                  :message "A header block is too large."))
    (octet-buffer-append buffer (h2-frame-payload frame))
    (when (h2-frame-flag-p frame *h2-flag-end-headers*)
      (h2-connection--complete-header-block connection)))
  nil)

(-> h2-connection--complete-header-block (h2-connection) null)
(defun h2-connection--complete-header-block (connection)
  "Decode the finished header block and apply it to its stream."
  (destructuring-bind (stream-id end-stream-p buffer) (h2-connection--header-block connection)
    (setf (h2-connection--header-block connection) nil)
    (let ((fields (hpack-decode (h2-connection--decoder connection) (octet-buffer->vector buffer)))
          (start  nil))
      (bt2:with-lock-held ((h2-connection--lock connection))
        (multiple-value-bind (stream state) (h2-connection--stream-state connection stream-id)
          (case state
            (:reset
             nil)
            (:closed
             (error 'h2-connection-error :code    ':stream-closed
                                         :message "HEADERS arrived on a closed stream."))
            (:idle
             (when (h2-connection--local-stream-id-p connection stream-id)
               (error 'h2-connection-error :code    ':protocol-error
                                           :message "HEADERS opened a stream of the wrong parity."))
             (setf (h2-connection--last-peer-stream-id connection) stream-id)
             (h2-connection--accept-request connection stream-id fields :end-stream-p end-stream-p)
             (setf start (gethash stream-id (h2-connection--streams connection))))
            (:open
             (h2-connection--receive-later-headers connection stream fields
                                                   :end-stream-p end-stream-p))))
        (bt2:condition-broadcast (h2-connection--condition connection)))
      (when start
        (h2-connection--start-handler connection start))))
  nil)

(-> h2-connection--accept-request (h2-connection (unsigned-byte 31) list &key (:end-stream-p boolean)) null)
(defun h2-connection--accept-request (connection stream-id fields &key end-stream-p)
  "Open the peer's new stream STREAM-ID with request FIELDS, or refuse it; hold the state lock."
  (let ((limit  (h2-connection--local-setting connection ':max-concurrent-streams))
        (active (loop for stream being the hash-values of (h2-connection--streams connection)
                      count (not (h2-connection--local-stream-id-p connection (h2-stream-id stream))))))
    (when (or (h2-connection--goaway-sent-p connection) (and limit (>= active limit)))
      (error 'h2-stream-error :stream-id stream-id
                              :code      ':refused-stream
                              :message   "The stream was refused."))
    (let ((problem (h2--request-fields-problem fields))
          (stream  (h2-connection--make-stream connection stream-id)))
      (when problem
        (remhash stream-id (h2-connection--streams connection))
        (error 'h2-stream-error :stream-id stream-id
                                :code      ':protocol-error
                                :message   problem))
      (h2-connection--set-headers stream fields)
      (when end-stream-p
        (h2-connection--finish-remote connection stream))))
  nil)

(-> h2-connection--receive-later-headers (h2-connection h2-stream list &key (:end-stream-p boolean)) null)
(defun h2-connection--receive-later-headers (connection stream fields &key end-stream-p)
  "Apply a header block on an open STREAM: a response, an interim response, or trailers; hold the lock."
  (cond
    ((h2-stream--remote-closed-p stream)
     (error 'h2-stream-error :stream-id (h2-stream-id stream)
                             :code      ':stream-closed
                             :message   "HEADERS arrived after the peer ended the stream."))
    ((not (h2-stream--headers-p stream))
     (let ((status (rest (assoc ":status" fields :test #'string=))))
       (unless (and status (= (length status) 3) (every #'digit-char-p status))
         (error 'h2-stream-error :stream-id (h2-stream-id stream)
                                 :code      ':protocol-error
                                 :message   "A response has no valid :status."))
       (unless (char= (char status 0) #\1)
         (h2-connection--set-headers stream fields))))
    ((not end-stream-p)
     (error 'h2-stream-error :stream-id (h2-stream-id stream)
                             :code      ':protocol-error
                             :message   "Trailers do not end the stream."))
    ((some (lambda (field) (h2--pseudo-field-p (first field))) fields)
     (error 'h2-stream-error :stream-id (h2-stream-id stream)
                             :code      ':protocol-error
                             :message   "Trailers contain a pseudo-header field."))
    (t
     (setf (h2-stream-trailers stream) fields)))
  (when end-stream-p
    (h2-connection--finish-remote connection stream))
  nil)

(-> h2-connection--set-headers (h2-stream list) null)
(defun h2-connection--set-headers (stream fields)
  "Store the peer's first header block FIELDS on STREAM and its content-length; hold the lock."
  (setf (h2-stream-headers stream)    fields
        (h2-stream--headers-p stream) t)
  (let ((length (rest (assoc "content-length" fields :test #'string=))))
    (when (and length (plusp (length length)) (every #'digit-char-p length))
      (setf (h2-stream--expected-length stream) (parse-integer length))))
  nil)

(-> h2--pseudo-field-p (string) boolean)
(defun h2--pseudo-field-p (name)
  "Return whether NAME is a pseudo-header field name."
  (and (plusp (length name)) (char= (char name 0) #\:)))

(-> h2--request-fields-problem (list) (or null string))
(defun h2--request-fields-problem (fields)
  "Return why request FIELDS are malformed under RFC 9113 Section 8.3.1, or NIL."
  (let ((regular-p nil)
        (seen      nil))
    (block nil
      (loop for (name . value) in fields
            do (cond
                 ((some #'upper-case-p name)
                  (return "A field name contains upper case."))
                 ((h2--pseudo-field-p name)
                  (cond
                    (regular-p
                     (return "A pseudo-header field follows a regular field."))
                    ((not (member name '(":method" ":scheme" ":authority" ":path") :test #'string=))
                     (return "The request has an unknown pseudo-header field."))
                    ((member name seen :test #'string=)
                     (return "The request repeats a pseudo-header field.")))
                  (push name seen))
                 ((member name *h2-connection-specific-fields* :test #'string=)
                  (return "The request has a connection-specific field."))
                 ((and (string= name "te") (string/= value "trailers"))
                  (return "The te field has a value other than trailers."))
                 (t
                  (setf regular-p t))))
      (let ((method (rest (assoc ":method" fields :test #'string=)))
            (path   (rest (assoc ":path" fields :test #'string=))))
        (cond
          ((null method)
           "The request has no :method.")
          ((string= method "CONNECT")
           (unless (assoc ":authority" fields :test #'string=)
             "A CONNECT request has no :authority."))
          ((not (member ":scheme" seen :test #'string=))
           "The request has no :scheme.")
          ((or (null path) (zerop (length path)))
           "The request has no :path.")
          (t
           nil))))))

(-> h2-connection--start-handler (h2-connection h2-stream) null)
(defun h2-connection--start-handler (connection stream)
  "Run the server handler on STREAM in a new thread."
  (let ((handler (h2-connection-handler connection)))
    (if handler
        (bt2:make-thread (lambda () (h2-connection--run-handler connection stream))
                         :name (format nil "llh2 stream ~D" (h2-stream-id stream)))
        (h2-stream-reset stream :code ':refused-stream)))
  nil)

(-> h2-connection--run-handler (h2-connection h2-stream) null)
(defun h2-connection--run-handler (connection stream)
  "Call the handler on STREAM and settle whatever the handler left open."
  (handler-case (funcall (h2-connection-handler connection) stream)
    (error (condition)
      (h2-connection--log connection "HTTP/2 handler failed on stream ~D: ~A" (h2-stream-id stream) condition)))
  (handler-case
      (multiple-value-bind (finished-p headers-sent-p remote-open-p)
          (bt2:with-lock-held ((h2-connection--lock connection))
            (values (or (h2-stream-reset-code stream)
                        (h2-connection--closed-p connection)
                        (h2-stream--local-closed-p stream))
                    (h2-stream--headers-sent-p stream)
                    (not (h2-stream--remote-closed-p stream))))
        (cond
          ((and finished-p remote-open-p)
           (h2-stream-reset stream :code ':no-error))
          (finished-p
           nil)
          (headers-sent-p
           (h2-stream-finish stream))
          (t
           (h2-stream-reset stream :code ':internal-error))))
    (h2-error ()
      nil))
  nil)

(-> h2-connection--accept-upgrade (h2-connection list octet-vector) null)
(defun h2-connection--accept-upgrade (connection fields body)
  "Open stream 1 for the request of an HTTP/1.1 Upgrade, with its whole body received."
  (let ((stream nil))
    (bt2:with-lock-held ((h2-connection--lock connection))
      (setf (h2-connection--last-peer-stream-id connection) 1
            stream                                          (h2-connection--make-stream connection 1))
      (h2-connection--set-headers stream fields)
      (setf (h2-stream--expected-length stream) nil)
      (when (plusp (length body))
        (setf (h2-stream--incoming stream) (list body)))
      (setf (h2-stream--remote-closed-p stream) t))
    (h2-connection--start-handler connection stream))
  nil)

(-> h2-connection--receive-priority (h2-connection h2-frame) null)
(defun h2-connection--receive-priority (connection frame)
  "Check a PRIORITY frame; this library does not prioritize streams."
  (declare (ignore connection))
  (h2-connection--require-stream-id frame)
  (unless (= (length (h2-frame-payload frame)) 5)
    (error 'h2-stream-error :stream-id (h2-frame-stream-id frame)
                            :code      ':frame-size-error
                            :message   "A PRIORITY frame is not five octets."))
  (when (= (logand (h2-decode-u32 (h2-frame-payload frame) 0) *h2-largest-window*)
           (h2-frame-stream-id frame))
    (error 'h2-stream-error :stream-id (h2-frame-stream-id frame)
                            :code      ':protocol-error
                            :message   "A stream depends on itself."))
  nil)

(-> h2-connection--receive-reset (h2-connection h2-frame) null)
(defun h2-connection--receive-reset (connection frame)
  "Process a RST_STREAM frame."
  (h2-connection--require-stream-id frame)
  (unless (= (length (h2-frame-payload frame)) 4)
    (error 'h2-connection-error :code    ':frame-size-error
                                :message "A RST_STREAM frame is not four octets."))
  (bt2:with-lock-held ((h2-connection--lock connection))
    (multiple-value-bind (stream state) (h2-connection--stream-state connection (h2-frame-stream-id frame))
      (when (eq state ':idle)
        (error 'h2-connection-error :code    ':protocol-error
                                    :message "RST_STREAM arrived on an idle stream."))
      (when stream
        (h2-connection--mark-reset connection stream
                                   (h2-error-code-keyword (h2-decode-u32 (h2-frame-payload frame) 0))))))
  nil)

(-> h2-connection--receive-settings (h2-connection h2-frame) null)
(defun h2-connection--receive-settings (connection frame)
  "Process a SETTINGS frame or its acknowledgment."
  (h2-connection--require-connection-id frame)
  (if (h2-frame-flag-p frame *h2-flag-ack*)
      (unless (zerop (length (h2-frame-payload frame)))
        (error 'h2-connection-error :code    ':frame-size-error
                                    :message "A SETTINGS acknowledgment has a payload."))
      (h2-connection--apply-settings connection (h2-decode-settings (h2-frame-payload frame))))
  nil)

(-> h2-connection--receive-ping (h2-connection h2-frame) null)
(defun h2-connection--receive-ping (connection frame)
  "Answer a PING frame."
  (h2-connection--require-connection-id frame)
  (unless (= (length (h2-frame-payload frame)) 8)
    (error 'h2-connection-error :code    ':frame-size-error
                                :message "A PING frame is not eight octets."))
  (unless (h2-frame-flag-p frame *h2-flag-ack*)
    (h2-connection--send-frame connection ':ping :flags   *h2-flag-ack*
                                                 :payload (h2-frame-payload frame)))
  nil)

(-> h2-connection--receive-goaway (h2-connection h2-frame) null)
(defun h2-connection--receive-goaway (connection frame)
  "Process a GOAWAY frame: refuse local streams the peer will not process."
  (h2-connection--require-connection-id frame)
  (when (< (length (h2-frame-payload frame)) 8)
    (error 'h2-connection-error :code    ':frame-size-error
                                :message "A GOAWAY frame is shorter than eight octets."))
  (let ((last-stream-id (logand (h2-decode-u32 (h2-frame-payload frame) 0) *h2-largest-window*)))
    (bt2:with-lock-held ((h2-connection--lock connection))
      (setf (h2-connection--goaway-received-p connection) t)
      (loop for stream in (loop for stream being the hash-values of (h2-connection--streams connection)
                                collect stream)
            do (when (and (h2-connection--local-stream-id-p connection (h2-stream-id stream))
                          (> (h2-stream-id stream) last-stream-id))
                 (h2-connection--mark-reset connection stream ':refused-stream)))
      (bt2:condition-broadcast (h2-connection--condition connection))))
  nil)

(-> h2-connection--receive-window-update (h2-connection h2-frame) null)
(defun h2-connection--receive-window-update (connection frame)
  "Process a WINDOW_UPDATE frame for a stream or the connection."
  (unless (= (length (h2-frame-payload frame)) 4)
    (error 'h2-connection-error :code    ':frame-size-error
                                :message "A WINDOW_UPDATE frame is not four octets."))
  (let ((id        (h2-frame-stream-id frame))
        (increment (logand (h2-decode-u32 (h2-frame-payload frame) 0) *h2-largest-window*)))
    (bt2:with-lock-held ((h2-connection--lock connection))
      (if (zerop id)
          (progn
            (when (zerop increment)
              (error 'h2-connection-error :code    ':protocol-error
                                          :message "A connection WINDOW_UPDATE has no increment."))
            (when (> (incf (h2-connection--send-window connection) increment) *h2-largest-window*)
              (error 'h2-connection-error :code    ':flow-control-error
                                          :message "The connection window overflowed.")))
          (multiple-value-bind (stream state) (h2-connection--stream-state connection id)
            (when (eq state ':idle)
              (error 'h2-connection-error :code    ':protocol-error
                                          :message "WINDOW_UPDATE arrived on an idle stream."))
            (when stream
              (when (zerop increment)
                (error 'h2-stream-error :stream-id id
                                        :code      ':protocol-error
                                        :message   "A stream WINDOW_UPDATE has no increment."))
              (when (> (incf (h2-stream--send-window stream) increment) *h2-largest-window*)
                (error 'h2-stream-error :stream-id id
                                        :code      ':flow-control-error
                                        :message   "A stream window overflowed.")))))
      (bt2:condition-broadcast (h2-connection--condition connection))))
  nil)
