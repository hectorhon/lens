(defsystem "lens"
  :depends-on ("cl-ppcre")
  :components ((:file "common")
               (:file "string")
               (:file "test")
               (:file "jinja")))
