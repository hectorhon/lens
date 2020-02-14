(defsystem "lens"
  :depends-on ("cl-ppcre")
  :serial t
  :components ((:file "package")
               (:file "common")
               (:file "string")
               (:file "test")
               (:file "jinja")
               (:file "server")))
