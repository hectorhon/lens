(defun replace-all (string part replacement &key (test #'char=))
  "Returns a new string in which all the occurrences of the part is
replaced with replacement."
  (with-output-to-string (out)
    (loop with part-length = (length part)
       for old-pos = 0 then (+ pos part-length)
       for pos = (search part string
                         :start2 old-pos
                         :test test)
       do (write-string string out
                        :start old-pos
                        :end (or pos (length string)))
       when pos do (write-string replacement out)
       while pos)))

(defun string-lowercase (string)
  (format nil "~(~a~)" string))

(defun string-uppercase (string)
  (format nil "~@:(~a~)" string))

(define-constant +whitespace-characters+
    (concatenate 'string '(#\Newline) '(#\Return) '(#\Tab) '(#\Space)))

(defun trim-whitespace (string)
  "Returns a new string with whitespace trimmed from both ends of the string."
  (string-trim +whitespace-characters+ string))

(defun split-string-by (character string)
  "Returns a list of substrings of string divided by 'character. Note:
Two consecutive 'character s will be seen as if there were an empty
string between them."
  (loop :for i = 0 :then (1+ j)
     :as j = (position character string :start i)
     :collect (subseq string i j)
     :while j))
