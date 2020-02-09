(in-package #:jinja)

(defclass context ()
  ((variables :initform (make-hash-table :test 'equal)
              :accessor variables
              :type hash-table)))

(defmethod set-context-variable ((context context) key value)
  (setf (gethash key (variables context)) value))

(defmethod get-context-variable ((context context) key)
  (gethash key (variables context)))



(defclass token ()
  ((contents :initarg :contents :reader contents)))

(defclass literal-token (token)
  ())

(defclass statement-token (token)
  ())

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
                              (#\% (make-instance 'statement-token :contents tag-contents))
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

(define-condition parse-failure (error) ())

(defun parse-literal (tokens)
  (let ((token (car tokens)))
    (if (eq 'literal-token (type-of token))
        (values (make-instance 'literal :str (contents token))
                (cdr tokens))
        (error 'parse-failure))))

(defun parse-expression (tokens)
  (let ((token (car tokens)))
    (if (eq 'expression-token (type-of token))
        (values (with-slots (contents) token
                  (destructuring-bind (accessor &rest filters)
                      (split-string-by #\| contents)
                    (make-instance 'expression
                                   :accessor (split-string-by #\. (trim-whitespace accessor))
                                   :filters (mapcar (lambda (raw-filter-string)
                                                      (make-instance
                                                       (intern (string-uppercase
                                                                (concatenate 'string
                                                                             (trim-whitespace raw-filter-string)
                                                                             "-filter")))))
                                                    filters))))
                (cdr tokens))
        (error 'parse-failure))))

(defun try-parsers (tokens &rest parsers)
  (loop :for parser :in parsers
     :do (handler-case (return (funcall parser tokens))
           (parse-failure ()
             ;; log error etc., then continue the loop
             ))))

(defun parse (tokens)
  (loop :with remaining-tokens = tokens
     :until (endp remaining-tokens)
     :collect (multiple-value-bind (parsed-elements remaining-tokens-after-parsing-step)
                  (try-parsers remaining-tokens
                               'parse-literal
                               'parse-expression)
                (prog1 parsed-elements
                  (setf remaining-tokens remaining-tokens-after-parsing-step)))))



(defclass filter () ())

(defclass capitalize-filter (filter) ())
(defclass upper-filter (filter) ())

(defgeneric apply-filter (filter input-value))

(defmethod apply-filter ((filter capitalize-filter) input-value)
  "Capitalize a value. The first character will be uppercase, all
others lowercase."
  (let ((str (string-lowercase input-value)))
    (unless (empty-string-p str)
      (setf (elt str 0) (char-upcase (elt str 0))))
    str))

(defmethod apply-filter ((filter upper-filter) input-value)
  "Convert a value to uppercase."
  (string-uppercase input-value))



(defgeneric render-element (element stream context)
  (:documentation "Render the element in the given context."))

(defmethod render-element ((literal literal) stream context)
  (write-string (str literal) stream))

(defmethod render-element ((expression expression) stream context)
  (with-slots (accessor filters) expression
    (let* ((key
            (car accessor))
           (target-context-variable
            (get-context-variable context key))
           (value
            (loop :with value = target-context-variable
               :for accessor-part :in (cdr accessor)
               :do (setf value (funcall (intern (string-uppercase accessor-part)) value))
               :finally (return value)))
           (value-after-filter
            (loop :with value-after-filter = value
               :for filter :in filters
               :do (setf value-after-filter (apply-filter filter value-after-filter))
               :finally (return value-after-filter))))
      (if value-after-filter
          (format stream "~a" value-after-filter)
          (write-string "<undefined>" stream)))))

(defun render (template-string context)
  (let* ((tokens (tokenize template-string))
         (elements (parse tokens)))
    (with-output-to-string (stream)
      (loop :for element :in elements
         :do (render-element element stream context)))))



(define-test "Render template without variables and expressions"
    (expect-equals
     "abcdef ghijkl"
     (render "abcdef ghijkl" (make-instance 'context))))

(define-test "Render template with simple variable having number. Syntax: {{ var }}"
    (let ((context (make-instance 'context)))
      (set-context-variable context "var" 123)
      (expect-equals
       "abcdef 123"
       (render "abcdef {{ var }}" context))))

(define-test "Render template with simple variable having string. Syntax: {{ var }}"
    (let ((context (make-instance 'context)))
      (set-context-variable context "var" "a string")
      (expect-equals
       "abcdef a string"
       (render "abcdef {{ var }}" context))))

(define-test "Render template with variable having CLOS object. Syntax: {{ object.slot-accessor }}"
    (let ((context (make-instance 'context)))
      (set-context-variable context "object"
                            (make-instance 'token :contents "a slot value"))
      (expect-equals
       "abcdef a slot value xyz"
       (render "abcdef {{ object.contents }} xyz" context))))

(define-test "Render template with a filter. Syntax: {{ object.slot-accessor | upper }}"
    (let ((context (make-instance 'context)))
      (set-context-variable context "object"
                            (make-instance 'token :contents "a slot value"))
      (expect-equals
       "abcdef A SLOT VALUE xyz"
       (render "abcdef {{ object.contents |upper }} xyz" context))))

(define-test "Render template with multiple filters. Syntax: {{ object.slot-accessor | upper | capitalize }}"
    (let ((context (make-instance 'context)))
      (set-context-variable context "object"
                            (make-instance 'token :contents "a slot value"))
      (expect-equals
       "abcdef A slot value xyz"
       (render "abcdef {{ object.contents| upper| capitalize}} xyz" context))))
