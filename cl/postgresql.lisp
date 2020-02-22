(in-package #:lens-postgresql)

(defun log-debug (control-string &rest args)
  (apply 'format *error-output* control-string args))



(defun write-int32-as-bytes (int32)
  (write-byte (ldb (byte 8 24) int32))
  (write-byte (ldb (byte 8 16) int32))
  (write-byte (ldb (byte 8 8) int32))
  (write-byte (ldb (byte 8 0) int32)))

(defun write-null-terminated-string (string)
  (write-string string)
  (write-byte 0))

(defun read-int32 ()
  (let ((integer 0))
    (setf (ldb (byte 8 24) integer) (read-byte)
          (ldb (byte 8 16) integer) (read-byte)
          (ldb (byte 8 8) integer) (read-byte)
          (ldb (byte 8 0) integer) (read-byte))
    integer))

(defun read-string ()
  (with-output-to-string (stream)
    (loop :for char = (read-char)
       :until (eql char #\Nul)
       :do (write-char char stream))))



(defclass message () ())
(defgeneric message-length (message)
  (:method-combination +))
(defmethod message-length + (message)
  (+ 4))

(defclass frontend-message (message) ())
(defgeneric write-message (message))

(defclass backend-message (message) ())
(defgeneric read-message (message-type))

(defun read-a-message ()
  (let* ((first-byte (read-char))
         (message-length (read-int32))
         (message (ecase first-byte
                    (#\R (read-message 'authentication-*))
                    (#\E (read-message 'error-response))
                    (#\S (read-message 'parameter-status))
                    (#\K (read-message 'backend-key-data))
                    (#\Z (read-message 'ready-for-query)))))
    (assert (eq message-length (message-length message)))
    message))



(defclass startup-message (frontend-message)
  ((user :type string :initarg :user)
   (database :type string :initarg :database)))

(defmethod message-length + ((startup-message startup-message))
  (with-slots (user database) startup-message
    (+ (/ 32 8)
       (1+ (length "user"))
       (1+ (length user))
       (1+ (length "database"))
       (1+ (length database))
       1)))

(defmethod write-message ((startup-message startup-message))
  (with-slots (user database) startup-message
    (write-int32-as-bytes (message-length startup-message))
    (write-int32-as-bytes 196608)
    (write-null-terminated-string "user")
    (write-null-terminated-string user)
    (write-null-terminated-string "database")
    (write-null-terminated-string database)
    (write-byte 0))
  (finish-output))



(defclass error-response (backend-message)
  ((fields :type list :initarg :fields)))

(defclass error-response-field ()
  ((type :type character :initarg :type)
   (value :type string :initarg :value)))

(defmethod message-length + ((error-response error-response))
  (with-slots (fields) error-response
    (loop :for field :in fields
       :summing (with-slots (value) field
                  (+ 1                 ; field type
                     (length value)    ; field value
                     1)))              ; string terminator
    1))                                ; pair terminator

(defmethod read-message ((message-type (eql 'error-response)))
  (let ((response
         (loop :for field-type = (read-char)
            :until (eql #\Nul field-type)
            :collect (let ((field-value (read-string)))
                       (make-instance 'error-response-field
                                      :type field-type
                                      :value field-value))
            :into error-response-fields
            :finally (return (make-instance 'error-response
                                            :fields error-response-fields)))))
    response))

(defmethod print-object ((message error-response) stream)
  (print-unreadable-object (message stream :type t)
    (with-slots (fields) message
      (loop :for field :in fields
         :do (with-slots (type value) field
               (let ((field-type (ccase type
                                   (#\S "Severity")
                                   (#\V "Severity")
                                   (#\C "SQLSTATE")
                                   (#\M "Message")
                                   (#\D "Detail")
                                   (#\H "Hint")
                                   (#\P "Position")
                                   (#\p "Internal position")
                                   (#\q "Internal query")
                                   (#\W "Where")
                                   (#\s "Schema")
                                   (#\t "Table")
                                   (#\c "Column")
                                   (#\d "Data type")
                                   (#\n "Constraint")
                                   (#\F "File")
                                   (#\L "Line")
                                   (#\R "Routine"))))
                 (format stream "~a: ~a~%" field-type value)))))))

(defclass authentication-* (backend-message) ())
(defclass authentication-ok (authentication-*) ())

(defmethod message-length + ((authentication-ok authentication-ok))
  (+ 4))

(defmethod read-message ((message-type (eql 'authentication-*)))
  (let ((auth-type (read-int32)))
    (ecase auth-type
      (0 (make-instance 'authentication-ok)))))



(defclass parameter-status (backend-message)
  ((name :type string :initarg :name)
   (value :type string :initarg :value)))

(defmethod message-length + ((parameter-status parameter-status))
  (with-slots (name value) parameter-status
    (+ (1+ (length name))
       (1+ (length value)))))

(defmethod read-message ((message-type (eql 'parameter-status)))
  (let ((name (read-string))
        (value (read-string)))
    (make-instance 'parameter-status :name name :value value)))

(defmethod print-object ((message parameter-status) stream)
  (print-unreadable-object (message stream :type t :identity nil)
    (with-slots (name value) message
      (format stream "~a: ~a" name value))))


(defclass backend-key-data (backend-message)
  ((process-id :type (unsigned-byte 32) :initarg :process-id)
   (secret-key :type (unsigned-byte 32) :initarg :secret0-key)))

(defmethod message-length + ((backend-key-data backend-key-data))
  (+ 4 4))

(defmethod read-message ((message-type (eql 'backend-key-data)))
  (make-instance 'backend-key-data
                 :process-id (read-int32)
                 :secret0-key (read-int32)))



(defclass ready-for-query (message)
  ((backend-transaction-status :initarg :backend-transaction-status)))

(defmethod message-length + ((ready-for-query ready-for-query))
  (+ 1))

(defmethod read-message ((message-type (eql 'ready-for-query)))
  (let ((indicator (read-char)))
    (make-instance 'ready-for-query
                   :backend-transaction-status (ecase indicator
                                                 (#\I :idle)
                                                 (#\T :in-transaction-block)
                                                 (#\E :in-failed-transaction)))))



(defun connect (user &optional database)
  (let ((socket)
        (database (or database user)))
    (unwind-protect
         (progn (setf socket (make-instance 'local-socket :type :stream))
                (socket-connect socket "/var/run/postgresql/.s.PGSQL.5432")
                (let* ((stream (socket-make-stream socket :element-type :default :input t :output t))
                       (*standard-output* stream)
                       (*standard-input* stream))
                  (write-message (make-instance 'startup-message
                                                :user user
                                                :database database))
                  (let ((response (read-a-message)))
                    (ecase (type-of response)
                      (authentication-ok (loop :for message = (read-a-message)
                                            :do (log-debug "~a~%" message)
                                            :until (typep message 'ready-for-query)))
                      (error-response (log-debug "~a" response))))))
      (socket-close socket))))
