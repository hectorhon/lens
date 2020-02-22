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

(defclass postgresql-message () ())

(defclass startup-message (postgresql-message)
  ((user :type string
         :initarg :user)
   (database :type string
             :initarg :database)))

(defgeneric postgresql-message-length (message))

(defmethod postgresql-message-length ((startup-message startup-message))
  (with-slots (user database) startup-message
    (+ (/ 32 8)
       (/ 32 8)
       (1+ (length "user"))
       (1+ (length user))
       (1+ (length "database"))
       (1+ (length database))
       1)))

(defgeneric write-postgresql-message (message))

(defmethod write-postgresql-message ((startup-message startup-message))
  (with-slots (user database) startup-message
    (write-int32-as-bytes (postgresql-message-length startup-message))
    (write-int32-as-bytes 196608)
    (write-null-terminated-string "user")
    (write-null-terminated-string user)
    (write-null-terminated-string "database")
    (write-null-terminated-string database)
    (write-byte 0))
  (finish-output))

(defclass error-response (postgresql-message)
  ((fields :type list
           :initarg :fields)))

(defclass error-response-field ()
  ((type :type character
         :initarg :type)
   (value :type string
          :initarg :value)))

(defmethod print-object ((message error-response) stream)
  (print-unreadable-object (message stream :type t :identity t)
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

(defclass authentication-ok (postgresql-message)
  ())

(defun read-postgresql-message ()
  (let ((first-byte (read-char))
        (message-length (read-int32)))
    (declare (ignorable message-length))
    (ecase first-byte
      (#\R (let ((auth-type (read-int32)))
             (ecase auth-type
               (0 (make-instance 'authentication-ok)))))
      (#\E (loop :for field-type = (read-char)
              :until (eql #\Nul field-type)
              :collect (let ((field-value (read-string)))
                         (make-instance 'error-response-field
                                        :type field-type
                                        :value field-value))
              :into error-response-fields
              :finally (return (make-instance 'error-response :fields error-response-fields)))))))

(defun connect (user &optional database)
  (let ((socket)
        (database (or database user)))
    (unwind-protect
         (progn (setf socket (make-instance 'local-socket :type :stream))
                (socket-connect socket "/var/run/postgresql/.s.PGSQL.5432")
                (let* ((stream (socket-make-stream socket :element-type :default :input t :output t))
                       (*standard-output* stream)
                       (*standard-input* stream))
                  (write-postgresql-message (make-instance 'startup-message
                                                           :user user
                                                           :database database))
                  (read-postgresql-message)))
      (socket-close socket))))
