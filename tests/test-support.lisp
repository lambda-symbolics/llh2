(in-package #:llh2)

;;;; -- Test Registry --

(defvar *test-cases* nil
  "The registered test names and functions, newest first.")

(defmacro define-test (name &body body)
  "Define and register one test named NAME."
  `(progn
     (setf *test-cases* (remove ',name *test-cases* :key #'first))
     (push (list ',name (lambda () ,@body)) *test-cases*)
     ',name))

(defmacro test-assert (form &optional description)
  "Signal a test failure unless FORM returns true."
  `(unless ,form
     (error "Assertion failed: ~A~@[ (~A)~]" ',form ,description)))

(defmacro test-equal (expected form &key (test '#'equal))
  "Signal a test failure unless FORM equals EXPECTED under TEST."
  (let ((expected-value (gensym "EXPECTED"))
        (actual-value   (gensym "ACTUAL")))
    `(let ((,expected-value ,expected)
           (,actual-value ,form))
       (unless (funcall ,test ,expected-value ,actual-value)
         (error "Expected ~S, got ~S from ~S." ,expected-value ,actual-value ',form))
       ,actual-value)))

(defmacro test-signals (condition-type &body body)
  "Evaluate BODY and return the signaled CONDITION-TYPE condition."
  (let ((condition (gensym "CONDITION")))
    `(handler-case
         (progn
           ,@body
           (error "Expected condition ~S, but none was signaled." ',condition-type))
       (,condition-type (,condition)
         ,condition))))


;;;; -- Octet Helpers --

(-> test-hex (string) octet-vector)
(defun test-hex (text)
  "Return the octets the hexadecimal digits of TEXT spell, ignoring other characters."
  (let ((digits (remove-if-not (lambda (character) (digit-char-p character 16)) text)))
    (coerce (loop for index from 0 below (length digits) by 2
                  collect (parse-integer digits :start index :end (+ index 2) :radix 16))
            'octet-vector)))

(-> test-text-octets (string) octet-vector)
(defun test-text-octets (text)
  "Return the Latin-1 octets of TEXT."
  (latin1-string->octets text))

(-> test-call-with-octet-stream (function function) t)
(defun test-call-with-octet-stream (writer reader)
  "Call WRITER with an octet output stream, then READER with an input stream over what it wrote."
  (uiop:with-temporary-file (:stream stream :element-type 'octet :direction ':io)
    (funcall writer stream)
    (finish-output stream)
    (file-position stream 0)
    (funcall reader stream)))
