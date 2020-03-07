(defpackage :lens-postgresql
  (:use :cl)
  (:import-from :sb-bsd-sockets
                :local-socket
                :socket-connect
                :socket-make-stream
                :socket-open-p
                :socket-close))

(in-package :lens-postgresql)

(defun log-debug (control-string &rest args)
  (apply 'format *error-output* (concatenate 'string control-string "~%") args))

(defun log-debug-value (arg-name arg)
  (format *error-output* (concatenate 'string arg-name ": ~a~%") arg)
  arg)



(defclass connection ()
  ((socket :initarg :socket)
   (stream :initarg :stream)))

(defvar *connection* nil)



(defun pg-write-byte (byte)
  ;; (log-debug "write byte ~a (~a)" byte (code-char byte))
  (with-slots (stream) *connection*
    (write-byte byte stream)))

(defun pg-write-bytes (bytes)
  (loop :for byte :across bytes
     :do (pg-write-byte byte)))

(defun pg-write-int32 (int32)
  (pg-write-byte (ldb (byte 8 24) int32))
  (pg-write-byte (ldb (byte 8 16) int32))
  (pg-write-byte (ldb (byte 8 8) int32))
  (pg-write-byte (ldb (byte 8 0) int32)))

(defun pg-write-int16 (int16)
  (pg-write-byte (ldb (byte 8 8) int16))
  (pg-write-byte (ldb (byte 8 0) int16)))

(defun pg-write-char (char)
  (pg-write-byte (char-code char)))

(defun pg-write-string (string)
  (map nil 'pg-write-char string)
  (pg-write-byte 0))

(defun pg-read-byte ()
  (with-slots (stream) *connection*
    (read-byte stream)))

(defun pg-read-int32 ()
  (let ((integer 0))
    (setf (ldb (byte 8 24) integer) (pg-read-byte)
          (ldb (byte 8 16) integer) (pg-read-byte)
          (ldb (byte 8 8) integer) (pg-read-byte)
          (ldb (byte 8 0) integer) (pg-read-byte))
    integer))

(defun pg-read-signed-int32 ()
  (let ((unsigned-integer (pg-read-int32)))
    (logior unsigned-integer
            (- (mask-field (byte 1 31) unsigned-integer)))))

(defun pg-read-int16 ()
  (let ((integer 0))
    (setf (ldb (byte 8 8) integer) (pg-read-byte)
          (ldb (byte 8 0) integer) (pg-read-byte))
    integer))

(defun pg-read-char ()
  (code-char (pg-read-byte)))

(defun pg-read-string ()
  (with-output-to-string (stream)
    (loop :for char = (pg-read-char)
       :until (eql char #\Nul)
       :do (write-char char stream))))

(defun pg-read-bytes (count)
  (loop :with bytes = (make-array count :element-type '(unsigned-byte 8))
     :for i :from 0 :below count
     :do (setf (aref bytes i) (pg-read-byte))
     :finally (return bytes)))



(defclass message () ())

(defgeneric message-type-char (message-type))

(defgeneric message-length (message)
  (:method-combination +))

(defmethod message-length + (message)
  4)                                    ; length of message (int32)

(defclass frontend-message (message) ())

(defgeneric write-message (frontend-message))

(defclass startup-message (frontend-message) ())

(defmethod write-message :before ((frontend-message frontend-message))
  (unless (typep frontend-message 'startup-message)
    (pg-write-char (message-type-char (type-of frontend-message))))
  (pg-write-int32 (message-length frontend-message)))

(defmethod write-message :after ((frontend-message frontend-message))
  (finish-output (slot-value *connection* 'stream)))

(defclass backend-message (message) ())

(defgeneric read-message (message-type))



(defclass startup-message (frontend-message)
  ((user :initarg :user)
   (database :initarg :database)))

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
    (pg-write-int32 196608)
    (pg-write-string "user")
    (pg-write-string user)
    (pg-write-string "database")
    (pg-write-string database)
    (pg-write-byte 0)))



(defclass authentication-* (backend-message) ())

(defclass authentication-ok (authentication-*) ())

(defmethod message-type-char ((message-type (eql 'authentication-*)))
  #\R)

(defmethod message-length + ((authentication-ok authentication-ok))
  4)

(defmethod read-message ((message-type (eql 'authentication-*)))
  (let ((auth-type (pg-read-int32)))
    (ecase auth-type
      (0 (make-instance 'authentication-ok)))))



(defclass error-response (backend-message)
  ((fields :initarg :fields)))

(defclass error-response-field ()
  ((type :initarg :type)
   (value :initarg :value)))

(defmethod message-type-char ((message-type (eql 'error-response)))
  #\E)

(defmethod message-length + ((error-response error-response))
  (with-slots (fields) error-response
    (+ (loop :for field :in fields
          :summing (with-slots (value) field
                     (+ 1               ; field type
                        (length value)  ; field value
                        1)))            ; string terminator
       1)))                             ; pair terminator

(defmethod read-message ((message-type (eql 'error-response)))
  (make-instance 'error-response
                 :fields
                 (loop :for field-type = (pg-read-char)
                    :until (eql #\Nul field-type)
                    :collect (let ((field-value (pg-read-string)))
                               (make-instance 'error-response-field
                                              :type field-type
                                              :value field-value)))))

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



(defclass parameter-status (backend-message)
  ((name :initarg :name)
   (value :initarg :value)))

(defmethod message-type-char ((message-type (eql 'parameter-status)))
  #\S)

(defmethod message-length + ((parameter-status parameter-status))
  (with-slots (name value) parameter-status
    (+ (1+ (length name))
       (1+ (length value)))))

(defmethod read-message ((message-type (eql 'parameter-status)))
  (let ((name (pg-read-string))
        (value (pg-read-string)))
    (make-instance 'parameter-status :name name :value value)))

(defmethod print-object ((message parameter-status) stream)
  (print-unreadable-object (message stream :type t :identity nil)
    (with-slots (name value) message
      (format stream "~a: ~a" name value))))



(defclass backend-key-data (backend-message)
  ((process-id :initarg :process-id)
   (secret-key :initarg :secret0-key)))

(defmethod message-type-char ((message-type (eql 'backend-key-data)))
  #\K)

(defmethod message-length + ((backend-key-data backend-key-data))
  (+ 4 4))

(defmethod read-message ((message-type (eql 'backend-key-data)))
  (make-instance 'backend-key-data
                 :process-id (pg-read-int32)
                 :secret0-key (pg-read-int32)))



(defclass ready-for-query (backend-message)
  ((backend-transaction-status :initarg :backend-transaction-status)))

(defmethod message-type-char ((message-type (eql 'ready-for-query)))
  #\Z)

(defmethod message-length + ((ready-for-query ready-for-query))
  1)

(defmethod read-message ((message-type (eql 'ready-for-query)))
  (let ((indicator (pg-read-char)))
    (make-instance 'ready-for-query
                   :backend-transaction-status (ecase indicator
                                                 (#\I :idle)
                                                 (#\T :in-transaction-block)
                                                 (#\E :in-failed-transaction)))))



(defclass query (frontend-message)
  ((query-string :initarg :query-string)))

(defmethod message-type-char ((message-type (eql 'query)))
  #\Q)

(defmethod message-length + ((query query))
  (with-slots (query-string) query
    (1+ (length query-string))))

(defmethod write-message ((query query))
  (with-slots (query-string) query
    (pg-write-string query-string)))



(defclass row-description (backend-message)
  ((field-count :initarg :field-count)
   (fields :initarg :fields)))

(defclass row-description-field ()
  ((name :initarg :name)
   (table-oid :initarg :table-oid)
   (column-attribute-number :initarg :column-attribute-number)
   (data-type-oid :initarg :data-type-oid)
   (data-type-size :initarg :data-type-size)
   (type-modifier :initarg :type-modifier)
   (format-code :initarg :format-code)))

(defmethod message-type-char ((message-type (eql 'row-description)))
  #\T)

(defmethod message-length + ((row-description row-description))
  (+ 2
     (with-slots (fields) row-description
       (loop :for field :in fields
          :summing (with-slots (name) field
                     (+ (1+ (length name)) 4 2 4 2 4 2))))))

(defmethod read-message ((message-type (eql 'row-description)))
  (let ((field-count (pg-read-int16)))
    (make-instance 'row-description
                   :field-count field-count
                   :fields (loop :for n :from 1 :to field-count
                              :collect (make-instance 'row-description-field
                                                      :name (pg-read-string)
                                                      :table-oid (pg-read-int32)
                                                      :column-attribute-number (pg-read-int16)
                                                      :data-type-oid (pg-read-int32)
                                                      :data-type-size (pg-read-int16)
                                                      :type-modifier (pg-read-int32)
                                                      :format-code (pg-read-int16))))))



(defclass data-row (backend-message)
  ((column-count :initarg :column-count)
   (columns :initarg :columns)))

(defclass data-row-column ()
  ((length :initarg :length)
   (value :initarg :value)))

(defmethod message-type-char ((message-type (eql 'data-row)))
  #\D)

(defmethod message-length + ((data-row data-row))
  (+ 2
     (with-slots (columns) data-row
       (loop :for column :in columns
          :summing (with-slots (length value) column
                     (let ((actual-length (max 0 length))) ; -1 special case
                       (assert (eq actual-length (length value)))
                       (+ 4 actual-length)))))))

(defmethod read-message ((message-type (eql 'data-row)))
  (let ((column-count (pg-read-int16)))
    (make-instance 'data-row
                   :column-count column-count
                   :columns (let ((column-length (pg-read-signed-int32)))
                              (loop :for n :from 1 :to column-count
                                 :collect (make-instance 'data-row-column
                                                         :length column-length
                                                         :value (unless (eq -1 column-length)
                                                                  (pg-read-bytes column-length))))))))



(defclass command-complete (backend-message)
  ((command-tag :initarg :command-tag)))

(defmethod message-type-char ((message-type (eql 'command-complete)))
  #\C)

(defmethod message-length + ((command-complete command-complete))
  (with-slots (command-tag) command-complete
    (1+ (length command-tag))))

(defmethod read-message ((message-type (eql 'command-complete)))
  (make-instance 'command-complete :command-tag (pg-read-string)))



(defclass parse (frontend-message)
  ((destination-prepared-statement-name :initarg :destination-prepared-statement-name :initform "")
   (query-string :initarg :query-string)
   (number-of-parameter-data-types-specified :initarg :number-of-parameter-data-types-specified :initform 0)
   (parameter-data-type-object-ids :initarg :parameter-data-type-object-ids :initform nil)))

(defmethod message-type-char ((message-type (eql 'parse)))
  #\P)

(defmethod message-length + ((parse parse))
  (with-slots (destination-prepared-statement-name
               query-string
               parameter-data-type-object-ids)
      parse
    (+ (1+ (length destination-prepared-statement-name))
       (1+ (length query-string))
       (/ 16 8)
       (* (length parameter-data-type-object-ids)
          (/ 32 8)))))

(defmethod write-message ((parse parse))
  (with-slots (destination-prepared-statement-name
               query-string
               number-of-parameter-data-types-specified
               parameter-data-type-object-ids)
      parse
    (pg-write-string destination-prepared-statement-name)
    (pg-write-string query-string)
    (pg-write-int16 number-of-parameter-data-types-specified)
    (loop :for oid :in parameter-data-type-object-ids
       :do (pg-write-int32 oid))))



(defclass parse-complete (backend-message) ())

(defmethod message-type-char ((message-type (eql 'parse-complete)))
  #\1)

(defmethod message-length + ((parse-complete parse-complete))
  0)

(defmethod read-message ((message-type (eql 'parse-complete)))
  (make-instance 'parse-complete))



(defclass bind (frontend-message)
  ((destination-portal-name :initform "")
   (source-prepared-statement-name :initform "")
   (number-of-parameter-format-codes :initform 0)
   (parameter-format-codes :initform nil)
   (number-of-parameters)
   (parameters :initarg :parameters)
   (number-of-result-column-format-codes :initform 0)
   (result-column-format-codes :initform nil)))

(defmethod initialize-instance :after ((bind bind) &key)
  (with-slots (parameters number-of-parameters) bind
    (setf number-of-parameters (length parameters))))

(defclass bind-parameter ()
  ((length)
   (value)))

(defmethod message-type-char ((message-type (eql 'bind)))
  #\B)

(defmethod message-length + ((bind bind))
  (with-slots (destination-portal-name
               source-prepared-statement-name
               parameter-format-codes
               parameters
               result-column-format-codes)
      bind
    (+ (1+ (length destination-portal-name))
       (1+ (length source-prepared-statement-name))
       (/ 16 8)
       (* (length parameter-format-codes)
          (/ 16 8))
       (/ 16 8)
       (* (length parameters)
          (+ (/ 32 8)
             (loop :for parameter :in parameters
                :summing (length (slot-value parameter 'value)))))
       (/ 16 8)
       (* (length result-column-format-codes)
          (/ 16 8)))))

(defmethod write-message ((bind bind))
  (with-slots (destination-portal-name
               source-prepared-statement-name
               number-of-parameter-format-codes
               parameter-format-codes
               number-of-parameters
               parameters
               number-of-result-column-format-codes
               result-column-format-codes)
      bind
    (pg-write-string destination-portal-name)
    (pg-write-string source-prepared-statement-name)
    (pg-write-int16 number-of-parameter-format-codes)
    (loop :for code :in parameter-format-codes
       :do (pg-write-int16 code))
    (pg-write-int16 number-of-parameters)
    (loop :for parameter :in parameters
       :do (with-slots (length value) parameter
             (pg-write-int32 length)
             (pg-write-bytes value)))
    (pg-write-int16 number-of-result-column-format-codes)
    (loop :for code :in result-column-format-codes
       :do (pg-write-int16 code))))



(defclass bind-complete (backend-message) ())

(defmethod message-type-char ((message-type (eql 'bind-complete)))
  #\2)

(defmethod message-length + ((bind-complete bind-complete))
  0)

(defmethod read-message ((message-type (eql 'bind-complete)))
  (make-instance 'bind-complete))



(defclass execute (frontend-message)
  ((name-of-portal-to-execute :initform "")
   (max-number-of-rows-to-return :initform 0)))

(defmethod message-type-char ((message-type (eql 'execute)))
  #\E)

(defmethod message-length + ((execute execute))
  (with-slots (name-of-portal-to-execute max-number-of-rows-to-return) execute
    (+ (1+ (length name-of-portal-to-execute))
       (/ 32 8))))

(defmethod write-message ((execute execute))
  (with-slots (name-of-portal-to-execute max-number-of-rows-to-return) execute
    (pg-write-string name-of-portal-to-execute)
    (pg-write-int32 max-number-of-rows-to-return)))



(defclass empty-query-response (backend-message) ())

(defmethod message-type-char ((message-type (eql 'empty-query-response)))
  #\I)

(defmethod message-length + ((empty-query-response empty-query-response))
  0)

(defmethod write-message ((empty-query-response empty-query-response))
  )



(defclass portal-suspended (backend-message) ())

(defmethod message-type-char ((message-type (eql 'portal-suspended)))
  #\s)

(defmethod message-length + ((portal-suspended portal-suspended))
  0)

(defmethod write-message ((portal-suspended portal-suspended))
  )



(defclass flush (frontend-message) ())

(defmethod message-type-char ((message-type (eql 'flush)))
  #\H)

(defmethod message-length + ((flush flush))
  0)

(defmethod write-message ((flush flush))
  )



(defclass sync (frontend-message) ())

(defmethod message-type-char ((message-type (eql 'sync)))
  #\S)

(defmethod message-length + ((sync sync))
  0)

(defmethod write-message ((sync sync))
  )



(defun read-a-message ()
  (let* ((first-byte (pg-read-char))
         (declared-message-length (pg-read-int32))
         (message (ccase first-byte
                    (#\R (read-message 'authentication-*))
                    (#\E (read-message 'error-response))
                    (#\S (read-message 'parameter-status))
                    (#\K (read-message 'backend-key-data))
                    (#\Z (read-message 'ready-for-query))
                    (#\T (read-message 'row-description))
                    (#\D (read-message 'data-row))
                    (#\C (read-message 'command-complete))
                    (#\1 (read-message 'parse-complete))
                    (#\2 (read-message 'bind-complete)))))
    (assert (eq declared-message-length (message-length message)))
    message))




(defun connect (user &optional database)
  (if *connection* (disconnect))
  (setf *connection* (make-instance 'connection))
  (with-slots (socket stream) *connection*
    (setf socket (make-instance 'local-socket :type :stream))
    (socket-connect socket "/var/run/postgresql/.s.PGSQL.5432")
    (setf stream (socket-make-stream socket :element-type :default :input t :output t)))
  (write-message (make-instance 'startup-message :user user :database (or database user)))
  (let ((response (read-a-message)))
    (ecase (type-of response)
      (authentication-ok (loop :for message = (read-a-message)
                            :do (log-debug "~a" message)
                            :until (typep message 'ready-for-query)
                            :finally (return t)))
      (error-response (progn (log-debug "~a" response)
                             (error "~a" response))))))

(defun disconnect ()
  (if *connection*
      (with-slots (socket) *connection*
        (socket-close socket :abort t)
        (setf *connection* nil))))

(defun simple-query (query-string)
  (write-message (make-instance 'query :query-string query-string))
  (loop :for message = (read-a-message)
     :do (log-debug "~a" message)
     :until (typep message 'ready-for-query)))

(defun query (query-string parameters)
  (declare (ignore parameters))
  (write-message (make-instance 'parse :query-string query-string))
  (write-message (make-instance 'flush))
  (let ((response (read-a-message)))
    (ecase (type-of response)
      (parse-complete nil)
      (error-response (error "~a" response))))
  (write-message (make-instance 'bind :parameters nil))
  (write-message (make-instance 'flush))
  (let ((response (read-a-message)))
    (ecase (type-of response)
      (bind-complete nil)
      (error-response (error "~a" response))))
  (write-message (make-instance 'execute))
  (write-message (make-instance 'flush))
  (loop :for response = (read-a-message)
     :do (log-debug "~a" response)
     :until (case (type-of response)
              (command-complete t)
              (empty-query-response t)
              (error-response (error "~a" response))
              (portal-suspended t)))
  (write-message (make-instance 'sync))
  (let ((response (read-a-message)))
    (ecase (type-of response)
      (ready-for-query nil))))
