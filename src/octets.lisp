(in-package #:llh2)

;;;; -- Octet Types --

(deftype octet ()
  "One eight-bit unit of the wire format."
  '(unsigned-byte 8))

(deftype octet-vector ()
  "A simple vector of octets, the form of every encoded frame and payload."
  '(simple-array (unsigned-byte 8) (*)))


;;;; -- Octet Buffers --

(-> make-octet-vector ((integer 0 *)) octet-vector)
(defun make-octet-vector (length)
  "Return a zero-filled octet vector of LENGTH."
  (make-array length :element-type 'octet :initial-element 0))

(-> make-octet-buffer () (and (vector octet) (not simple-array)))
(defun make-octet-buffer ()
  "Return an empty adjustable octet vector with a fill pointer for building output."
  (make-array 64 :element-type 'octet :adjustable t :fill-pointer 0))

(-> octet-buffer->vector ((vector octet)) octet-vector)
(defun octet-buffer->vector (buffer)
  "Return a simple octet vector holding the contents of BUFFER."
  (coerce buffer 'octet-vector))

(-> octet-buffer-append ((vector octet) (vector octet) &key (:start (integer 0 *)) (:end (or null (integer 0 *))))
    null)
(defun octet-buffer-append (buffer octets &key (start 0) end)
  "Append the octets of OCTETS between START and END to BUFFER."
  (loop for index from start below (or end (length octets))
        do (vector-push-extend (aref octets index) buffer))
  nil)

(-> ensure-octet-vector ((or string (vector octet))) octet-vector)
(defun ensure-octet-vector (data)
  "Return DATA as a simple octet vector; a string must hold only Latin-1 characters."
  (if (stringp data)
      (latin1-string->octets data)
      (coerce data 'octet-vector)))


;;;; -- Latin-1 Text --

;; Header names and values are octet sequences. Mapping each octet to the
;; character with the same code keeps them lossless without an encoding library.

(-> latin1-string->octets (string) octet-vector)
(defun latin1-string->octets (string)
  "Return the octets of STRING, whose characters must all have codes below 256."
  (let ((octets (make-octet-vector (length string))))
    (loop for character across string
          for index from 0
          for code = (char-code character)
          do (unless (< code 256)
               (error 'h2-error
                      :message (format nil "The character ~S does not fit in one octet." character)))
             (setf (aref octets index) code))
    octets))

(-> octets->latin1-string ((vector octet) &key (:start (integer 0 *)) (:end (or null (integer 0 *))))
    simple-string)
(defun octets->latin1-string (octets &key (start 0) end)
  "Return the characters whose codes are the octets of OCTETS between START and END."
  (let* ((end    (or end (length octets)))
         (string (make-string (- end start))))
    (loop for index from start below end
          for position from 0
          do (setf (schar string position) (code-char (aref octets index))))
    string))
