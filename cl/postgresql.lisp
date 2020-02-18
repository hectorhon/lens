(in-package #:lens-postgresql)

(defun log-debug (control-string &rest args)
  (apply 'format *error-output* control-string args))

(defun write-int32-as-bytes (stream int32)
  (write-byte (ldb (byte 8 24) int32) stream)
  (write-byte (ldb (byte 8 16) int32) stream)
  (write-byte (ldb (byte 8 8) int32) stream)
  (write-byte (ldb (byte 8 0) int32) stream))

(defun write-null-terminated-string (stream string)
  (write-string string stream)
  (write-byte 0 stream))

(defun read-byte1-as-char (stream)
  (code-char (read-byte stream)))

(defun read-int32 (stream)
  (let ((integer 0))
    (setf (ldb (byte 8 24) integer) (read-byte stream)
          (ldb (byte 8 16) integer) (read-byte stream)
          (ldb (byte 8 8) integer) (read-byte stream)
          (ldb (byte 8 0) integer) (read-byte stream))
    integer))

(defclass postgresql-message () ())

(defclass startup-message (postgresql-message)
  ((user :type string
         :initarg :user)
   (database :type string
             :initarg :database)))

(defmethod postgresql-message-length ((startup-message startup-message))
  (with-slots (user database) startup-message
    (+ (/ 32 8)
       (/ 32 8)
       (1+ (length "user"))
       (1+ (length user))
       (1+ (length "database"))
       (1+ (length database))
       1)))

(defmethod write-postgresql-message ((stream stream) (startup-message startup-message))
  (with-slots (user database) startup-message
    (write-int32-as-bytes stream (postgresql-message-length startup-message))
    (write-int32-as-bytes stream 196608)
    (write-null-terminated-string stream "user")
    (write-null-terminated-string stream user)
    (write-null-terminated-string stream "database")
    (write-null-terminated-string stream database)
    (write-byte 0 stream))
  (finish-output stream))

(defun connect (user database)
  (let ((socket))
    (unwind-protect
         (progn (setf socket (make-instance 'local-socket :type :stream))
                (socket-connect socket "/var/run/postgresql/.s.PGSQL.5432")
                (let ((stream (socket-make-stream socket :element-type :default :input t :output t)))
                  (write-postgresql-message stream (make-instance 'startup-message
                                                                  :user user
                                                                  :database database))
                  (log-debug "~a" (read-byte1-as-char stream))
                  (log-debug "~a" (read-int32 stream))
                  (log-debug "~a" (read-int32 stream))))
      (socket-close socket))))
