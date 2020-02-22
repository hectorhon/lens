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

(defun read-signed-int32 ()
  (let ((unsigned-integer (read-int32)))
    (logior unsigned-integer
            (- (mask-field (byte 1 31) unsigned-integer)))))

(defun read-int16 ()
  (let ((integer 0))
    (setf (ldb (byte 8 8) integer) (read-byte)
          (ldb (byte 8 0) integer) (read-byte))
    integer))

(defun read-null-terminated-string ()
  (with-output-to-string (stream)
    (loop :for char = (read-char)
       :until (eql char #\Nul)
       :do (write-char char stream))))

(defun read-bytes (count)
  (loop :with bytes = (make-array count :element-type '(unsigned-byte 8))
     :for i :from 0 :below count
     :do (setf (aref bytes i) (read-byte))
     :finally (return bytes)))



(defclass message () ())
(defgeneric message-length (message)
  (:method-combination +))
(defmethod message-length + (message)
  (+ 4))

(defclass frontend-message (message) ())
(defgeneric write-message (message))
(defgeneric write-message-type-byte (message))
(defmethod write-message :before ((frontend-message frontend-message))
  (unless (typep frontend-message 'startup-message)
    (write-message-type-byte frontend-message))
  (write-int32-as-bytes (message-length frontend-message)))
(defmethod write-message :after (message)
  (finish-output))

(defvar *message-length* nil
  "To enable PEEK-CHAR and ease interactive programming.")
(defclass backend-message (message) ())
(defgeneric read-message (message-type))
(defmethod read-message :before (message-type)
  (read-char)
  (setf *message-length* (read-int32)))

(defun read-a-message ()
  (let* ((first-byte (peek-char))
         (message (ccase first-byte
                    (#\R (read-message 'authentication-*))
                    (#\E (read-message 'error-response))
                    (#\S (read-message 'parameter-status))
                    (#\K (read-message 'backend-key-data))
                    (#\Z (read-message 'ready-for-query))
                    (#\T (read-message 'row-description))
                    (#\D (read-message 'data-row))
                    (#\C (read-message 'command-complete)))))
    (assert (eq *message-length* (message-length message)))
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
    (write-int32-as-bytes 196608)
    (write-null-terminated-string "user")
    (write-null-terminated-string user)
    (write-null-terminated-string "database")
    (write-null-terminated-string database)
    (write-byte 0)))



(defclass query (frontend-message)
  ((query-string :type string :initarg :query-string)))

(defmethod message-length + ((query query))
  (with-slots (query-string) query
    (1+ (length query-string))))

(defmethod write-message-type-byte ((query query))
  (write-char #\Q))

(defmethod write-message ((query query))
  (with-slots (query-string) query
    (write-null-terminated-string query-string)))



(defclass error-response (backend-message)
  ((fields :type list :initarg :fields)))

(defclass error-response-field ()
  ((type :type character :initarg :type)
   (value :type string :initarg :value)))

(defmethod message-length + ((error-response error-response))
  (with-slots (fields) error-response
    (+ (loop :for field :in fields
          :summing (with-slots (value) field
                     (+ 1               ; field type
                        (length value)  ; field value
                        1)))            ; string terminator
       1)))                             ; pair terminator

(defmethod read-message ((message-type (eql 'error-response)))
  (let ((response
         (loop :for field-type = (read-char)
            :until (eql #\Nul field-type)
            :collect (let ((field-value (read-null-terminated-string)))
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
  (let ((name (read-null-terminated-string))
        (value (read-null-terminated-string)))
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



(defclass ready-for-query (backend-message)
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



(defclass row-description (backend-message)
  ((field-count :type (unsigned-byte 16) :initarg :field-count)
   (fields :initarg :fields)))

(defclass row-description-field ()
  ((name :type string :initarg :name)
   (table-oid :type (unsigned-byte 32) :initarg :table-oid)
   (column-attribute-number :type (unsigned-byte 16) :initarg :column-attribute-number)
   (data-type-oid :type (unsigned-byte 32) :initarg :data-type-oid)
   (data-type-size :type (unsigned-byte 16) :initarg :data-type-size)
   (type-modifier :type (unsigned-byte 32) :initarg :type-modifier)
   (format-code :type (unsigned-byte 16) :initarg :format-code)))

(defmethod message-length + ((row-description row-description))
  (+ 2
     (with-slots (fields) row-description
       (loop :for field :in fields
          :summing (with-slots (name) field
                     (+ (1+ (length name)) 4 2 4 2 4 2))))))

(defmethod read-message ((message-type (eql 'row-description)))
  (let ((field-count (read-int16)))
    (make-instance 'row-description
                   :field-count field-count
                   :fields (loop :for n :from 1 :to field-count
                              :collect (make-instance 'row-description-field
                                                      :name (read-null-terminated-string)
                                                      :table-oid (read-int32)
                                                      :column-attribute-number (read-int16)
                                                      :data-type-oid (read-int32)
                                                      :data-type-size (read-int16)
                                                      :type-modifier (read-int32)
                                                      :format-code (read-int16))))))



(defclass data-row (backend-message)
  ((column-count :type (unsigned-byte 16) :initarg :column-count)
   (columns :initarg :columns)))

(defclass data-row-column ()
  ((length :type (unsigned-byte 32) :initarg :length)
   (value :type (array (unsigned-byte 8)) :initarg :value)))

(defmethod message-length + ((data-row data-row))
  (+ 2
     (with-slots (columns) data-row
       (loop :for column :in columns
          :summing (with-slots (length value) column
                     (let ((actual-length (max 0 length))) ; -1 special case
                       (assert (eq actual-length (length value)))
                       (+ 4 actual-length)))))))

(defmethod read-message ((message-type (eql 'data-row)))
  (let ((column-count (read-int16)))
    (make-instance 'data-row
                   :column-count column-count
                   :columns (let ((column-length (read-signed-int32)))
                              (loop :for n :from 1 :to column-count
                                 :collect (make-instance 'data-row-column
                                                         :length column-length
                                                         :value (unless (eq -1 column-length)
                                                                  (read-bytes column-length))))))))



(defclass command-complete (backend-message)
  ((command-tag :type string :initarg :command-tag)))

(defmethod message-length + ((command-complete command-complete))
  (with-slots (command-tag) command-complete
    (1+ (length command-tag))))

(defmethod read-message ((message-type (eql 'command-complete)))
  (make-instance 'command-complete :command-tag (read-null-terminated-string)))



(defclass connection ()
  ((socket :initarg :socket)
   (stream :initarg :stream)))

(defvar *connection* nil)

(defun connect (user &optional database)
  (if *connection* (disconnect))
  (let ((socket (make-instance 'local-socket :type :stream)))
    (socket-connect socket "/var/run/postgresql/.s.PGSQL.5432")
    (let* ((stream (socket-make-stream socket :element-type :default :input t :output t))
           (*standard-output* stream)
           (*standard-input* stream))
      (write-message (make-instance 'startup-message
                                    :user user
                                    :database (or database user)))
      (let ((response (read-a-message)))
        (ecase (type-of response)
          (authentication-ok (loop :for message = (read-a-message)
                                :do (log-debug "~a~%" message)
                                :until (typep message 'ready-for-query)))
          (error-response (progn (log-debug "~a~%" response)
                                 (error "~a" response))))
        (setf *connection* (make-instance 'connection
                                          :socket socket
                                          :stream stream))))))

(defun disconnect ()
  (if *connection*
      (with-slots (socket) *connection*
        (socket-close socket)
        (setf *connection* nil))))

(defun query (query-string)
  (with-slots (stream) *connection*
    (let ((*standard-output* stream)
          (*standard-input* stream))
      (write-message (make-instance 'query :query-string query-string))
      (loop :for message = (read-a-message)
         :do (log-debug "~a~%" message)
         :until (typep message 'ready-for-query)))))
