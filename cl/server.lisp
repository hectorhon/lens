(in-package #:server)

(defmacro log-debug (control-string &rest format-arguments)
  `(format t ,(concatenate 'string control-string "~%") ,@format-arguments))

(defvar *accept-thread*)

(defvar *worker-threads* nil)

(defun run-background ()
  (setf *accept-thread* (make-thread 'run :name "accept-thread")))

(defun stop-background ()
  (terminate-thread *accept-thread*)
  (setf *accept-thread* nil))

(defun run ()
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
                                               :name (format nil "worker-thread-~a" (string:random-hex-string 3))
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

(defun process-client-stream (client-stream client-address client-port)
  (declare (ignore client-address client-port))
  (loop :for (line missing-newline-p) = (multiple-value-list (read-line client-stream nil))
     :do (funcall 'process-request line)
     :until missing-newline-p))

(defun process-request (request)
  (sleep 10)
  (log-debug "~a" request))
