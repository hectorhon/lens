(in-package #:lens-standard)



;;; Redefining standard functions

(defmacro store-and-shadow (function &key as)
  "Store the old function from symbol FUNCTION in the symbol named
'OLD-<FUNCTION>', or in the symbol named AS if it is provided. The
DEFUN can't be placed in this same macro because SHADOW takes effect
only in the next top level form."
  (let ((old-function-symbol-name
         (if as
             (symbol-name as)
             (concatenate 'string "OLD-" (symbol-name function)))))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (setf (symbol-function (quote ,(intern old-function-symbol-name)))
             (symbol-function (quote ,function)))
       (unintern (quote ,function))
       (shadow (quote ,function)))))

(defmacro redefine-standard-function (function args &body body)
  "Like DEFUN but also exports the symbol FUNCTION."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (defun ,function (,@args)
       ,@body)
     (export (quote ,function))))

(store-and-shadow write-byte :as original-write-byte)

(redefine-standard-function write-byte (integer &optional (stream *standard-output*))
  (original-write-byte integer stream))

(store-and-shadow read-byte :as original-read-byte)

(redefine-standard-function read-byte (&optional (stream *standard-output*) (sb-impl::eof-error-p t) sb-impl::eof-value)
  (original-read-byte stream sb-impl::eof-error-p sb-impl::eof-value))



;;; Object with members

(defgeneric get-from (object key &key throw-if-not-found))

(defgeneric set-to (object key value))

(define-condition key-not-found (error)
  ((key :reader not-found-key
        :initarg :key)))

(defgeneric for-each-in (object function))



;;; Unsorted

(defmacro define-constant (name value &optional doc)
  "http://www.sbcl.org/manual/#Defining-Constants"
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

(defun gt (a b)
  "> with arguments reversed."
  (> b a))

(defun lt (a b)
  "< with arguments reversed."
  (< b a))

(defgeneric copy (object))
