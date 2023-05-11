;;;; package.lisp

(defpackage #:fn
  (:use #:cl)
  (:export
   :*stack*
   :s-pop
   :s-delete
   :s-get))
