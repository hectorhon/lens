(in-package #:test)

(defvar *skip-tests* nil)

(define-condition test-failure (error)
  ((name :initarg :name
         :accessor name)
   (description :initarg :description
                :reader description)
   (expected :initarg :expected
             :reader expected)
   (actual :initarg :actual
           :reader actual)))

(defmethod print-object :around ((condition test-failure) stream)
  (print-unreadable-object (condition stream)
    (format stream "Test failed: ~a~%~a"
            (name condition)
            (description condition))
    (call-next-method)))

(defmacro define-test (test-name &rest body)
  (let ((function-name (intern
                        (string-uppercase
                         (replace-all
                          (concatenate 'string "test-" test-name)
                          " " "-"))))
        (doc-string (concatenate 'string "TEST: " test-name)))
    `(progn (defun ,function-name ()
              ,doc-string
              (handler-bind
                  ((test-failure (lambda (condition)
                                   (setf (name condition) ,test-name)
                                   (format *error-output* "~%~a~%~%" condition)
                                   ;; (invoke-restart 'continue)
                                   )))
                (progn ,@body)))
            (unless *skip-tests* (,function-name)))))

(defmacro expect-equals (expected actual)
  (let ((expected-value expected)
        (actual-value actual))
    `(if (equal ,expected-value ,actual-value)
         t
         (progn
           (cerror "Ignore this test failure."
                   'test-failure
                   :description (format nil "Expected ~S but got ~S instead.~%"
                                        ,expected-value ,actual-value)
                   :expected ,expected-value
                   :actual ,actual-value)))))
