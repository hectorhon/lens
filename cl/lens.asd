(asdf:defsystem "lens"
  :depends-on ("cl-ppcre")
  :serial t
  :components ((:file "package")
               (:file "standard")
               (:file "test")
               (:file "string")
               (:file "stream")
               (:file "jinja")
               (:file "http/headers")
               (:file "server")
               (:file "postgresql")))
