(in-package :lens-parse)

(defclass parse-result () ())

(defclass parse-success (parse-result)
  ((parsed-elements :initarg :parsed-elements)
   (remaining-tokens :initarg :remaining-tokens)))

(defclass parse-failure (parse-result)
  ((reason :initarg :reason)))

(defgeneric parse (result-type))

(defvar *tokens*)

(defun parse-sequentially (result-type-1 result-type-2)
  (let ((parse-result-1 (parse result-type-1)))
    (if (typep parse-result-1 'parse-success)
        (with-slots (parsed-elements remaining-tokens) parse-result-1
          (let ((*tokens* remaining-tokens))
            (parse result-type-2)))
        (with-slots (reason) parse-result-1
          (make-instance 'parse-failure :reason reason)))))

(defun parse-one-of (&rest result-types)
  "Parse one element that is one of the RESULT-TYPES."
  (loop :for parser :in (mapcar (lambda (result-type)
                                  (lambda () (parse result-type)))
                                result-types)
     :do (let ((parse-result (funcall parser)))
           (if (typep parse-result 'parse-success)
               (return parse-result)))
     :finally (return (make-instance 'parse-failure
                                     :reason (format nil "Expected to parse one of (~{~a~^, ~}), but failed."
                                                     result-types)))))

(defun parse-many (result-type)
  "Parse zero or more elements of RESULT-TYPE. This parse never fails."
  (let ((parse-result (parse result-type)))
    (if (typep parse-result 'parse-success)
        (with-slots (parsed-elements remaining-tokens) parse-result
          (let* ((*tokens* remaining-tokens)
                 (inner-parse-result (parse-many result-type)))
            (make-instance 'parse-success
                           :parsed-elements (cons parsed-elements
                                                  (slot-value inner-parse-result 'parsed-elements))
                           :remaining-tokens (slot-value inner-parse-result 'remaining-tokens))))
        (make-instance 'parse-success
                       :parsed-elements nil
                       :remaining-tokens *tokens*))))
