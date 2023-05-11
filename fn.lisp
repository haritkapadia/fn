;;;; fn.lisp

(in-package #:fn)

(defvar *stack* (list))

(defun s-pop (&optional (index 0))
  "Remove element at `index' from `*stack*', then returns removed element."
  (prog1 (elt *stack* index)
    (setq *stack* (delete-if (constantly t) *stack* :start index :count 1))))

(defun s-delete (&optional (index 0))
  "Remove element at `index' of `*stack*', returning nothing. Useful for shrinking the stack."
  (s-pop index)
  (values))

(defun s-get (&optional (index 0))
  "Returns element at `index' of `*stack*'. Useful for growing the stack."
  (elt *stack* index))

(defun stack-reader (func)
  "Returns a function that is used in a reader macro to execute `func-name' with the provided following number, or 0 if no number is specified."
  (lambda (stream char)
    (declare (ignore char))
    (let ((i (if (digit-char-p (peek-char nil stream nil #\Space))
                 (read stream)
                 0)))
      `(funcall ,func ,i))))

(defun stream-to-string (stream)
  (let ((string (make-array '(0) :element-type 'base-char
                               :fill-pointer 0 :adjustable t)))
    (with-output-to-string (out-stream string)
      (cl-plumbing:connect-streams stream out-stream :background nil))
    string))

(defun main ()
  (opts:define-opts
    (:name :in
     :description "Read STDIN as string."
     :short #\i
     :long "in")
    (:name :out
     :description "Print top of stack to STDOUT."
     :short #\o
     :long "out")
    (:name :read
     :description "Read stack from STDIN."
     :short #\r
     :long "read")
    (:name :write
     :description "Write stack to STDOUT."
     :short #\w
     :long "write")
    (:name :no-reader-macro
     :description "Disable reader macros (. -> s-delete, % -> s-pop, $ -> s-get)."
     :short #\n
     :long "no-reader-macro"))

  (multiple-value-bind (options free-args)
      (opts:get-opts)

    (unless (getf options :no-reader-macro)
      (set-macro-character #\. (stack-reader #'s-delete))
      (set-macro-character #\% (stack-reader #'s-pop))
      (set-macro-character #\$ (stack-reader #'s-get)))

    (cond ((getf options :in)
           (push (stream-to-string *standard-input*) *stack*))
          ((getf options :read)
           (handler-case (loop (push (read) *stack*))
             (end-of-file nil))))

    (loop :for arg :in free-args :do
      (with-input-from-string (stream arg)
        (handler-case
            (loop (setq *stack* (concatenate 'list (multiple-value-list (eval (read stream))) *stack*)))
          (end-of-file nil))))

    (cond ((getf options :out)
           (when *stack* (princ (car *stack*)))
           (terpri))
          ((getf options :write)
           (loop :for el :in (reverse *stack*) :do
             (write el)
             (terpri))))))
