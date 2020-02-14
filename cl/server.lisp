(in-package #:lens-server)

(defun get-timestamp-string ()
  (multiple-value-bind (seconds minutes hours day month year day-of-week dst tz) (get-decoded-time)
    (declare (ignore day-of-week dst tz))
    (format nil "~4,'0d-~2,'0d-~2,'0d ~2,'0d:~2,'0d:~2,'0d"
            year month day hours minutes seconds)))

(defun log-debug (control-string &rest format-arguments)
  (apply 'format *error-output*
         (concatenate 'string "~a: " control-string "~%")
         (cons (get-timestamp-string) format-arguments)))

(defvar *accept-thread*)

(defvar *worker-threads* nil)

(defun start-background ()
  (setf *accept-thread* (make-thread 'start :name "accept-thread")))

(defun stop-background ()
  (terminate-thread *accept-thread*)
  (setf *accept-thread* nil))

(defun start ()
  (sb-sys:without-interrupts
    (let ((accept-socket (make-instance 'inet-socket :type :stream :protocol :tcp)))
      (unwind-protect
           (progn (setf (sockopt-reuse-address accept-socket) t)
                  (socket-bind accept-socket (make-inet-address "127.0.0.1") 8080)
                  (socket-listen accept-socket 5)
                  (sb-sys:with-local-interrupts
                    (loop (multiple-value-bind (client-socket client-address client-port)
                              (socket-accept accept-socket)
                            ;; clean up done worker threads from list
                            (setf *worker-threads* (delete-if-not #'thread-alive-p *worker-threads*))
                            (push (make-thread 'handle-client-socket
                                               :name (format nil "worker-thread-~a" (random-hex-string 3))
                                               :arguments (list client-socket client-address client-port))
                                  *worker-threads*)))))
        (progn (log-debug "Terminating worker threads")
               (loop :for worker-thread :in *worker-threads*
                  :if (thread-alive-p worker-thread)
                  :do (progn (log-debug "Terminating")
                             (terminate-thread worker-thread)))
               (log-debug "Closing accept-socket")
               (socket-close accept-socket))))))

(defun handle-client-socket (client-socket client-address client-port)
  (sb-sys:without-interrupts
    (progn (log-debug "Connection from ~a:~a" client-address client-port)
           (let ((client-socket-stream (socket-make-stream client-socket :element-type :default :input t :output t)))
             (unwind-protect
                  (sb-sys:with-local-interrupts
                    (process-client-stream client-socket-stream client-address client-port))
               (progn (log-debug "Closing client socket and stream")
                      (close client-socket-stream)
                      (socket-close client-socket)))))))

(defclass http-request ()
  ((http-method :reader http-request-method
                :initarg :http-method)
   (uri :reader http-request-uri
        :initarg :uri)
   (version :reader http-request-version
            :initarg :version)
   (headers :reader http-request-headers
            :initarg :headers
            :type http-headers)
   (body :reader http-request-body
         :initarg :body)))

(defmethod print-object ((request http-request) stream)
  (print-unreadable-object (request stream :type t :identity t)
    (with-slots (http-method uri) request
      (format stream "~a ~a" http-method uri))))

(defclass http-response ()
  ((version :reader http-response-version
            :initarg :version)
   (status-code :reader http-response-status-code
                :initarg :status-code)
   (reason-phrase :reader http-reason-phrase
                  :initarg :reason-phrase)
   (headers :reader http-response-headers
            :initarg :headers
            :type http-headers)
   (body :reader http-response-body
         :initarg :body)))

(defun reason-phrase-lookup (status-code)
  (ecase status-code
    (200 "OK")))

(defun create-simple-http-response (status-code headers &optional (body ""))
  (let ((headers (make-http-headers headers)))
    (set-to headers "content-length" (length body))
    (make-instance 'http-response
                   :version "1.1"
                   :status-code status-code
                   :reason-phrase (reason-phrase-lookup status-code)
                   :headers headers
                   :body body)))

(defun process-client-stream (client-stream client-address client-port)
  (declare (ignore client-address client-port))
  (loop
     :do (let* ((request-line
                 (read-until-crlf client-stream))
                (request-line-matches
                 (nth-value 1 (cl-ppcre:scan-to-strings "(GET|POST) (.*?) HTTP/(.\\..)" request-line)))
                (http-method
                 (aref request-line-matches 0))
                (uri
                 (aref request-line-matches 1))
                (version
                 (aref request-line-matches 2))
                (headers
                 (loop :with headers = (make-http-headers)
                    :for header-line = (read-until-crlf client-stream)
                    :unless (empty-string-p header-line)
                    :do (cl-ppcre:register-groups-bind (header-name header-value) ("(.*?) *: *(.*)" header-line)
                          (log-debug "~a: ~a" header-name header-value)
                          (set-to headers header-name header-value))
                    :else :return headers :end))
                (body
                 (multiple-value-bind (content-length-header-value foundp)
                     (get-from headers "content-length" :throw-if-not-found nil)
                   (if foundp
                       (let* ((content-length
                               (parse-integer content-length-header-value))
                              (body
                               (make-array content-length :element-type '(unsigned-byte 8))))
                         (read-sequence body client-stream)))))
                (request
                 (make-instance 'http-request
                                :http-method http-method
                                :uri uri
                                :version version
                                :headers headers
                                :body body))
                (response
                 (process-request request)))
           (write-http-response-to-stream client-stream response))))

(defun process-request (request)
  (log-debug "~a" request)
  (create-simple-http-response 200 (list (cons "content-length" 5))
                               (get-timestamp-string)))

(defun write-http-response-to-stream (stream http-response)
  (macrolet ((output (format-string &rest values)
               `(progn (format stream ,format-string ,@values)
                       (write-string +crlf+ stream))))
    (with-slots (version status-code reason-phrase headers body) http-response
      (output "HTTP/~a ~d ~a" version status-code reason-phrase)
      (for-each-in headers (lambda (name value)
                             (output "~a: ~a" name value)))
      (output "")
      (output body))
    (finish-output stream)))
