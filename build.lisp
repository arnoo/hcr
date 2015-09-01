(require 'asdf)
#-quicklisp
(let ((quicklisp-init (merge-pathnames "dev/quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(ql:quickload 'keepcl)
(clutch:rm "keepcl")

(asdf:make-build :keepcl :type :program :prologue-code '(setf *compile-verbose* nil) :epilogue-code '(keepcl::main) :move-here "./")

(quit)
