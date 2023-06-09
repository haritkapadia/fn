;;;; fn.asd

(asdf:defsystem #:fn
  :description "Execute Common Lisp in command-line, usually as part of a shell script."
  :author "Harit Kapadia <haritkapadia@outlook.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:unix-opts #:cl-plumbing #:str #:cl-fad)
  :components ((:file "package")
               (:file "fn"))
  :build-operation "program-op" ;; leave as is
  :build-pathname "fn-lisp"
  :entry-point "fn::main")
