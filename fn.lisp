;;;; fn.lisp

(in-package #:fn)

(defun get-config-path ()
  (flet ((non-empty-stringp (str)
           (and (stringp str) (> (length str) 0))))
    (let (; Linux support
          (env-home (uiop:getenv "HOME"))
          (env-xdg-config-home (uiop:getenv "XDG_CONFIG_HOME"))
                                        ; Windows support
          (env-localappdata (uiop:getenv "LOCALAPPDATA")))
      (cond ((non-empty-stringp env-xdg-config-home)
             (cl-fad:pathname-as-directory env-xdg-config-home))
            ((non-empty-stringp env-home)
             (cl-fad:merge-pathnames-as-directory (cl-fad:pathname-as-directory env-home) #P".config/"))
            ((non-empty-stringp env-localappdata)
             (cl-fad:pathname-as-directory env-localappdata))))))

(defun make-in-config (dir-name &optional file-name)
  (let* ((dir (cl-fad:merge-pathnames-as-directory (get-config-path) (cl-fad:pathname-as-directory dir-name)))
         (file (when file-name (cl-fad:merge-pathnames-as-file dir (cl-fad:pathname-as-file file-name)))))
    (if file-name file dir)))

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
  "Returns a string of the contents of an output `stream'. The `stream' is consumed."
  (let ((string (make-array '(0) :element-type 'base-char
                                 :fill-pointer 0 :adjustable t)))
    (with-output-to-string (out-stream string)
      (cl-plumbing:connect-streams stream out-stream :background nil))
    string))

(defun set-macros ()
  "Sets the ., %, and $ macro characters."
  (set-macro-character #\. (stack-reader #'s-delete))
  (set-macro-character #\% (stack-reader #'s-pop))
  (set-macro-character #\$ (stack-reader #'s-get)))

(defun validate-opts (options)
  "Checks if combination of `options' from command line opts is valid."
  (destructuring-bind (&key help in out read write no-reader-macro no-config) options
    (when (and in read)
      (error "Cannot provide both --in and --read."))
    (when (and out write)
      (error "Cannot provide both --out and --write."))))

(defun repl (free-args)
  "Read and evaluate each string in `free-args'."
  (flet ((eval-to-stack (stream)
           (setq *stack* (concatenate 'list (multiple-value-list (eval (read stream))) *stack*))))
    (loop :for arg :in free-args :do
      (with-input-from-string (stream arg)
        (handler-case
            (loop (eval-to-stack stream))
          (end-of-file nil))))))

(defun configure ()
  "Load configuration file."
  (let ((config-path (make-in-config "fn" "config.lisp")))
    (ensure-directories-exist config-path)
    (when (probe-file config-path)
      (load config-path))))

(defun main ()
  (opts:define-opts
    (:name :help
     :description "Print this help text."
     :short #\h
     :long "help")
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
     :short #\m
     :long "no-reader-macro")
    (:name :no-config
     :description "Disable configuration file."
     :short #\n
     :long "no-config"))

  (multiple-value-bind (options free-args) (opts:get-opts)
    (validate-opts options)

    (cond ((getf options :help) (opts:describe))
          (t
           (unless (getf options :no-config) (configure))

           (unless (getf options :no-reader-macro) (set-macros))

           (cond ((getf options :in)
                  (push (stream-to-string *standard-input*) *stack*))
                 ((getf options :read)
                  (handler-case (loop (push (read) *stack*)) (end-of-file nil))))

           (repl free-args)

           (cond ((getf options :out)
                  (when *stack* (princ (car *stack*)))
                  (terpri))
                 ((getf options :write)
                  (loop :for el :in (reverse *stack*) :do
                    (write el)
                    (terpri))))))))
