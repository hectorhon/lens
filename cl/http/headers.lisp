;;;; HTTP headers. Current implementation stores everything in a hash
;;;; table where the keys and values are always strings.

(in-package #:lens-http-headers)

(defclass http-headers ()
  ((headers :initarg :headers
            :type hash-table)))

(defun make-http-headers (&rest name-and-value-pairs)
  (loop :with headers = (make-hash-table :test 'equalp) ; case insensitive
     :for name-and-value-pair :in name-and-value-pairs
     :do (let ((header-name (car name-and-value-pair))
               (header-value (cdr name-and-value-pair)))
           (setf (gethash header-name headers) header-value))
     :finally (return (make-instance 'http-headers :headers headers))))

(defmethod get-from ((headers http-headers) (header-name string) &key (throw-if-not-found t))
  (with-slots (headers) headers
    (multiple-value-bind (header-value found-p) (gethash header-name headers)
      (if found-p
          (values header-value t)
          (if throw-if-not-found
              (error 'key-not-found :key header-name)
              (values nil nil))))))

(defmethod set-to ((headers http-headers) (header-name string) (header-value string))
  (with-slots (headers) headers
    (setf (gethash header-name headers) header-value)))

(defmethod set-to ((headers http-headers) (header-name string) (header-value integer))
  (set-to headers header-name (format nil "~d" header-value)))

(defmethod copy ((headers http-headers))
  (make-instance 'http-headers
                 :headers (with-slots (headers) headers
                            (apply 'make-http-headers (maphash (lambda (k v) (cons k v)) headers)))))

(defmethod for-each-in ((headers http-headers) function)
  (with-slots (headers) headers
    (maphash function headers)))
