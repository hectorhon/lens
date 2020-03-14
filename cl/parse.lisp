(in-package :lens-parse)

(defvar *tokens*)

(defclass parse-result () ())

(defclass parse-success (parse-result)
  ((result :initarg :result)
   (remaining :initarg :remaining)))

(defclass parse-failure (parse-result)
  ((reason :initarg :reason)))

(defun parse-success (result remaining)
  (make-instance 'parse-success :result result :remaining remaining))

(defun parse-failure (&optional reason)
  (make-instance 'parse-failure :reason reason))

(defgeneric parse (result-type))

(defmethod mreturn ((result-type (eql 'parse-result)) result)
  (parse-success result *tokens*))

(defmethod bind ((type (eql 'parse-result)) parse-result mapper)
  (etypecase parse-result
    (parse-success
     (with-slots (result remaining) parse-result
       (let ((*tokens* remaining))
         (funcall mapper result))))
    (parse-failure parse-result)))

;;; do-notation looks something like this:
;; (do-notation parse-result
;;   (n (parse 'integer))
;;   (s (parse 'string))
;;   (mreturn 'parse-result (format nil "~a ~a" n s)))

(defun match (token-test)
  (let ((token-test-result (funcall token-test (car *tokens*))))
    (if token-test-result
        (parse-success token-test-result (cdr *tokens*))
        (parse-failure))))

(defun parse-many (result-type)
  "Parse zero or more RESULT-TYPE results. This parse never fails."
  (let ((parse-result (parse result-type)))
    (etypecase parse-result
      (parse-success
       (with-slots (result remaining) parse-result
         (let* ((*tokens* remaining)
                (inner-parse-result (parse-many result-type)))
           (etypecase inner-parse-result
             (parse-success
              (parse-success (cons result (slot-value inner-parse-result 'result))
                             (slot-value inner-parse-result 'remaining)))
             (parse-failure
              (parse-success (list result) remaining))))))
      (parse-failure
       (parse-success nil *tokens*)))))

(defun parse-one-of (&rest result-types)
  (if (null result-types)
      (parse-failure)
      (let ((parse-result (parse (car result-types))))
        (etypecase parse-result
          (parse-success parse-result)
          (parse-failure (apply 'parse-one-of (cdr result-types)))))))
