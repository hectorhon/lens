(uiop:define-package #:lens-standard
    (:mix #:common-lisp)
  (:reexport #:common-lisp)
  (:export #:get-from
           #:set-to
           #:key-not-found
           #:for-each-in
           :bind
           :mreturn
           :do-notation
           #:define-constant
           #:get-symbol-home-package
           #:random-bytes
           #:gt
           #:lt
           #:copy))

(defpackage #:lens-test
  (:use #:lens-standard)
  (:export #:define-test
           #:expect-equals))

(defpackage #:lens-string
  (:use #:lens-standard)
  (:export #:replace-all
           #:+whitespace-characters+
           #:trim-whitespace
           #:split
           #:empty-string-p
           #:random-hex-string
           #:string-ends-with-p
           #:+crlf+
           #:string-case))

(defpackage #:lens-stream
  (:use #:lens-standard)
  (:import-from #:lens-string
                #:+crlf+)
  (:export #:read-until-string
           #:read-until-crlf))

(defpackage :lens-parse
  (:use :lens-standard)
  (:export :*tokens*
           :parse-result
           :parse-success :result :remaining
           :parse-failure :reason
           :parse
           :match
           :parse-many
           :parse-one-of))

(defpackage #:lens-jinja
  (:use #:lens-standard)
  (:import-from #:lens-test
                #:define-test
                #:expect-equals)
  (:import-from #:lens-string
                #:trim-whitespace
                #:split
                #:empty-string-p
                #:string-case)
  (:import-from :lens-parse
                :*tokens*
                :parse-result
                :parse-success :result :remaining
                :parse-failure :reason
                :parse
                :match
                :parse-many
                :parse-one-of))

(defpackage #:lens-http-headers
  (:use #:lens-standard)
  (:export #:http-headers               ; for type checking
           #:make-http-headers))

(defpackage #:lens-server
  (:use #:lens-standard)
  (:import-from #:sb-thread
                #:make-thread
                #:terminate-thread
                #:thread-alive-p)
  (:import-from #:sb-bsd-sockets
                #:inet-socket
                #:sockopt-reuse-address
                #:socket-bind
                #:make-inet-address
                #:socket-listen
                #:socket-accept
                #:socket-close
                #:socket-make-stream)
  (:import-from #:lens-string
                #:random-hex-string
                #:empty-string-p
                #:+crlf+)
  (:import-from #:lens-stream
                #:read-until-crlf)
  (:import-from #:lens-http-headers
                #:http-headers
                #:make-http-headers)
  (:export #:start
           #:start-background
           #:stop-background))

(defpackage :lens-postgresql
  (:use :lens-standard)
  (:import-from :sb-bsd-sockets
                :local-socket
                :socket-connect
                :socket-make-stream
                :socket-open-p
                :socket-close))
