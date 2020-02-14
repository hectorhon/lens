(in-package #:lens-common)

;; http://www.sbcl.org/manual/#Defining-Constants
(defmacro define-constant (name value &optional doc)
  `(defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
     ,@(when doc (list doc))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun get-symbol-home-package (symbol)
    (let ((packages (loop :for package :in (list-all-packages)
                       :if (eq :external (nth-value 1 (find-symbol (symbol-name symbol) package)))
                       :collect package)))
      (assert (eql 1 (length packages)))
      (car packages))))

(defvar *os-dev-urandom* nil)

(defun random-bytes (num-bytes)
  (let ((seq (make-array num-bytes :element-type '(unsigned-byte 8))))
    (unless *os-dev-urandom*
      (setf *os-dev-urandom* (open #P"/dev/urandom" :element-type '(unsigned-byte 8))))
    (assert (>= (read-sequence seq *os-dev-urandom*) num-bytes))
    seq))

(defmacro gt (a b)
  "> with arguments reversed."
  `(> ,b ,a))

(defmacro lt (a b)
  "< with arguments reversed."
  `(< ,b ,a))

(defgeneric get-from (object key &key throw-if-not-found))

(defgeneric set-to (object key value))

(define-condition key-not-found (error)
  ((key :reader not-found-key
        :initarg :key)))

(defgeneric copy (object))

(defgeneric for-each-in (object function))
