(in-package #:lens-stream)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun read-until-string (delimiter stream)
    (with-output-to-string (output-stream)
      (loop :with counter = 0
         :until (eql (length delimiter) counter)
         :for next-char = (read-char stream)
         :do (if (eql next-char (char delimiter counter))
                 (incf counter)
                 (progn (if (gt 0 counter)
                            (write-string (subseq delimiter 0 counter) output-stream))
                        (write-char next-char output-stream)
                        (setf counter 0)))))))

(defmacro read-until-crlf (stream)
  `(read-until-string +crlf+ ,stream))
