(asdf:defsystem "lens"
  :depends-on ("cl-ppcre")
  :serial t
  :components ((:file "package")
               (:file "common")
               (:file "test")
               (:file "string")
               (:file "stream")
               (:file "jinja")
               (:file "server")))
