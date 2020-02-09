(defpackage #:common
  (:use #:common-lisp)
  (:export define-constant
           random-bytes))

(defpackage #:string
  (:use #:common-lisp
        #:common)
  (:export replace-all
           string-lowercase
           string-uppercase
           +whitespace-characters+
           trim-whitespace
           split-string-by
           empty-string-p
           random-hex-string))

(defpackage #:test
  (:use #:common-lisp
        #:string)
  (:export define-test
           expect-equals))

(defpackage #:jinja
  (:use #:common-lisp
        #:string
        #:test))

(defpackage #:server
  (:use #:common-lisp
        #:sb-bsd-sockets
        #:sb-thread
        #:sb-ext)
  (:export run
           run-background
           stop-background))
