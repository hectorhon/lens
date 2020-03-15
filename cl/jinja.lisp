(in-package #:lens-jinja)

(defclass context ()
  ((variables :initform (make-hash-table :test 'equal)
              :accessor variables
              :type hash-table)))

(defmethod set-to ((context context) key value)
  (setf (gethash key (variables context)) value))

(defmethod get-from ((context context) key &key throw-if-not-found)
  (multiple-value-bind (value present-p)
      (gethash key (variables context))
    (if (and throw-if-not-found (not present-p))
        (error 'key-not-found :key key)
        value)))

(defmethod copy ((context context))
  (let ((context-copy (make-instance 'context)))
    (setf (slot-value context-copy 'variables)
          (copy (slot-value context 'variables)))
    context-copy))



(defclass token ()
  ((contents :initarg :contents)))

(defclass literal-token (token)
  ())

(defclass statement-token (token) ())

(defclass for-statement-token (statement-token)
  ((loop-variable :initarg :loop-variable)
   (variable-to-loop-over :initarg :variable-to-loop-over)))

(defclass endfor-statement-token (statement-token) ())

(defun make-statement-token (contents)
  (let ((words (remove-if 'empty-string-p (split #\Space contents))))
    (string-case (first words)
      ("for" (progn (assert (string= "in" (third words)))
                    (make-instance 'for-statement-token
                                   :loop-variable (second words)
                                   :variable-to-loop-over (fourth words))))
      ("endfor" (make-instance 'endfor-statement-token)))))

(defclass expression-token (token)
  ())

(defclass comment-token (token)
  ())

(defun tokenize (template-string)
  (flet ((pairwise (list)
           (loop :for tail on list :by #'cddr
              :collect (list (car tail) (cadr tail)))))
    (loop :with tags-regex = "({%(.*?)%}|{{(.*?)}}|{#(.*?)#})"
       :with all-matches = (cl-ppcre:all-matches tags-regex template-string)
       :and pos = 0
       :for (tag-start tag-end) :in (pairwise all-matches)
       :append (let ((literal
                      (make-instance 'literal-token :contents (subseq template-string pos tag-start)))
                     (tag (let* ((tag
                                  (subseq template-string tag-start tag-end))
                                 (tag-contents
                                  (trim-whitespace (subseq tag 2 (- (length tag) 2)))))
                            (ecase (elt tag 1)
                              (#\% (make-statement-token tag-contents))
                              (#\{ (make-instance 'expression-token :contents tag-contents))
                              (#\# (make-instance 'comment-token :contents tag-contents))))))
                 (prog1 (list literal tag)
                   (setf pos tag-end)))
       :into tokens
       :finally (return (append tokens (list (make-instance 'literal-token :contents (subseq template-string pos))))))))



(defclass element () ())

(defclass literal (element)
  ((str :initarg :str :reader str)))

(defclass expression (element)
  ((accessor :initarg :accessor :reader accessor)
   (filters :initarg :filters :reader filters)))

(defclass statement (element) ())

(defclass for-statement (statement)
  ((loop-variable :initarg :loop-variable)
   (variable-to-loop-over :initarg :variable-to-loop-over)
   (loop-body :initarg :loop-body)))

(defmethod parse ((result-type (eql 'literal)))
  (do-notation parse-result
    (contents (match (lambda (token)
                       (typecase token
                         (literal-token (slot-value token 'contents))
                         (t nil)))))
    (mreturn 'parse-result (make-instance 'literal :str contents))))

(defmethod parse ((result-type (eql 'expression)))
  (do-notation parse-result
    (contents (match (lambda (token)
                       (typecase token
                         (expression-token (slot-value token 'contents))))))
    (destructuring-bind (accessor &rest filters) (split #\| contents)
      (mreturn 'parse-result
               (make-instance 'expression
                              :accessor (split #\. (trim-whitespace accessor))
                              :filters (mapcar (lambda (raw-filter-string)
                                                 (make-instance
                                                  (intern (string-upcase
                                                           (concatenate 'string
                                                                        (trim-whitespace raw-filter-string)
                                                                        "-filter")))))
                                               filters))))))

(defmethod parse ((result-type (eql 'for-statement)))
  (do-notation parse-result
    (start-token (match (lambda (token)
                          (typecase token
                            (for-statement-token token)
                            (t nil)))))
    (body (parse-many 'element))
    (_ (match (lambda (token)
                (typecase token
                  (endfor-statement-token token)
                  (t nil)))))
    (with-slots (loop-variable variable-to-loop-over) start-token
      (mreturn 'parse-result
               (make-instance 'for-statement
                              :loop-variable loop-variable
                              :variable-to-loop-over variable-to-loop-over
                              :loop-body body)))))

(defmethod parse ((result-type (eql 'statement)))
  (parse 'for-statement))

(defmethod parse ((result-type (eql 'element)))
  (parse-one-of 'literal 'expression 'statement))

(defun parse-tokens (tokens)
  (let ((*tokens* tokens))
    (let ((parse-result (parse-many 'element)))
      (etypecase parse-result
        (parse-success
         (with-slots (result remaining) parse-result
           (assert (null remaining))
           result))
        (parse-failure
         (with-slots (reason) parse-result
           (error "Failed to parse: ~a" reason)))))))



(defclass filter () ())

(defclass capitalize-filter (filter) ())
(defclass upper-filter (filter) ())

(defgeneric apply-filter (filter input-value))

(defmethod apply-filter ((filter capitalize-filter) input-value)
  "Capitalize a value. The first character will be uppercase, all
others lowercase."
  (let ((str (string-downcase input-value)))
    (unless (empty-string-p str)
      (setf (elt str 0) (char-upcase (elt str 0))))
    str))

(defmethod apply-filter ((filter upper-filter) input-value)
  "Convert a value to uppercase."
  (string-upcase input-value))



(defgeneric render-element (element stream context)
  (:documentation "Render the element in the given context."))

(defun render-elements (elements stream context)
  (loop :for element :in elements
     :do (render-element element stream context)))

(defmethod render-element ((literal literal) stream context)
  (write-string (str literal) stream))

(defmethod render-element ((expression expression) stream context)
  (with-slots (accessor filters) expression
    (let* ((key
            (car accessor))
           (target-context-variable
            (get-from context key))
           (value
            (loop :with value = target-context-variable
               :for accessor-part :in (cdr accessor)
               :do (setf value (slot-value value (intern (string-upcase accessor-part))))
               :finally (return value)))
           (value-after-filter
            (loop :with value-after-filter = value
               :for filter :in filters
               :do (setf value-after-filter (apply-filter filter value-after-filter))
               :finally (return value-after-filter))))
      (if value-after-filter
          (format stream "~a" value-after-filter)
          (write-string "<undefined>" stream)))))

(defmethod render-element ((for-statement for-statement) stream context)
  (with-slots (loop-variable variable-to-loop-over loop-body) for-statement
    (let ((things-to-loop-over (get-from context variable-to-loop-over)))
      (loop :for thing :in things-to-loop-over
         :do (let ((context (copy context)))
               (set-to context loop-variable thing)
               (render-elements loop-body stream context))))))


(defun render (template-string context)
  (let* ((tokens (tokenize template-string))
         (elements (parse-tokens tokens)))
    (with-output-to-string (stream)
      (render-elements elements stream context))))



(define-test "Render template without variables and expressions"
  (expect-equals
   "abcdef ghijkl"
   (render "abcdef ghijkl" (make-instance 'context))))

(define-test "Render template with simple variable having number. Syntax: {{ var }}"
  (let ((context (make-instance 'context)))
    (set-to context "var" 123)
    (expect-equals
     "abcdef 123"
     (render "abcdef {{ var }}" context))))

(define-test "Render template with simple variable having string. Syntax: {{ var }}"
  (let ((context (make-instance 'context)))
    (set-to context "var" "a string")
    (expect-equals
     "abcdef a string"
     (render "abcdef {{ var }}" context))))

(define-test "Render template with variable having CLOS object. Syntax: {{ object.slot-accessor }}"
  (let ((context (make-instance 'context)))
    (set-to context "object"
                          (make-instance 'token :contents "a slot value"))
    (expect-equals
     "abcdef a slot value xyz"
     (render "abcdef {{ object.contents }} xyz" context))))

(define-test "Render template with a filter. Syntax: {{ object.slot-accessor | upper }}"
  (let ((context (make-instance 'context)))
    (set-to context "object"
                          (make-instance 'token :contents "a slot value"))
    (expect-equals
     "abcdef A SLOT VALUE xyz"
     (render "abcdef {{ object.contents |upper }} xyz" context))))

(define-test "Render template with multiple filters. Syntax: {{ object.slot-accessor | upper | capitalize }}"
  (let ((context (make-instance 'context)))
    (set-to context "object"
                          (make-instance 'token :contents "a slot value"))
    (expect-equals
     "abcdef A slot value xyz"
     (render "abcdef {{ object.contents| upper| capitalize}} xyz" context))))

(define-test "Render template with for loop. Syntax: {% for n in numbers %}number-{{ n }},{% endfor %}"
  (let ((context (make-instance 'context)))
    (set-to context "numbers" (list 1 2 3 4))
    (expect-equals
     "number-1,number-2,number-3,number-4,"
     (render "{% for n in numbers %}number-{{ n }},{% endfor %}" context))))
