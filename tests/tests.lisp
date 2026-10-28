(in-package #:llh2)

;;;; -- Test Runner --

(-> run-tests () (eql t))
(defun run-tests ()
  "Run every registered test and signal an error when any test fails."
  (let ((failures nil)
        (tests    (reverse *test-cases*)))
    (dolist (test tests)
      (destructuring-bind (name function) test
        (format t "~&~A ... " name)
        (finish-output)
        (handler-case
            (progn
              (funcall function)
              (format t "pass~%"))
          (error (condition)
            (push (cons name condition) failures)
            (format t "FAIL~%  ~A~%" condition)))))
    (when failures
      (error "~D of ~D llh2 tests failed:~%~{  ~A: ~A~%~}"
             (length failures)
             (length tests)
             (loop for (name . condition) in (nreverse failures)
                   append (list name condition))))
    (format t "~&All ~D llh2 tests passed.~%" (length tests))
    t))
