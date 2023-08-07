;; Allow quicklisp usage
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

;; Ensure random is not seeded
(setf *random-state* (make-random-state t))

;; Import alexandria for common utilities, lparallel for parallel map
(ql:quickload '(:alexandria :lparallel) :silent t)

;; Make alexandria part of current namespace so its symbols don't have to be prefixed by the package name
(use-package :alexandria)
