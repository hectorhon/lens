(in-package #:lens-server)

(defmacro log-debug (control-string &rest format-arguments)
  `(format *error-output* ,(concatenate 'string control-string "~%") ,@format-arguments))

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
           (let ((client-socket-stream (socket-make-stream client-socket :element-type :default :input t :output nil)))
             (unwind-protect
                  (sb-sys:with-local-interrupts
                    (process-client-stream client-socket-stream client-address client-port))
               (progn (log-debug "Closing client socket and stream")
                      (close client-socket-stream)
                      (socket-close client-socket)))))))

(defclass http-header ()
  ((name :initarg :name :reader name)
   (value :initarg :value :reader value)))

(defclass http-request ()
  ((http-method :initarg :http-method :reader http-method)
   (uri :initarg :uri :reader uri)
   (version :initarg :version :reader version)
   (headers :initarg :headers :reader headers)
   (body :initarg :body :reader body)))


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
                 (loop :for header-line = (read-until-crlf client-stream)
                    :unless (empty-string-p header-line)
                    :collect (cl-ppcre:register-groups-bind (header-name header-value) ("(.*?) *: *(.*)" header-line)
                               (log-debug "~a: ~a" header-name header-value)
                               (make-instance 'http-header :name header-name :value header-value)) :into headers
                    :else :return headers :end))
                (content-length-header
                 (find-if (lambda (header) (string-equal "content-length" (name header))) headers))
                (body
                 (unless (null content-length-header)
                   (let* ((content-length
                           (parse-integer (value content-length-header)))
                          (body
                           (make-array content-length :element-type '(unsigned-byte 8))))
                     (read-sequence body client-stream))))
                (request
                 (make-instance 'http-request
                                :http-method http-method
                                :uri uri
                                :version version
                                :headers headers
                                :body body)))
           (process-request request))))

(defun process-request (request)
  (sleep 1)
  (break)
  (log-debug "~a" request))
