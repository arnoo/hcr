keep: build.lisp keep.lisp keepcl.lisp
	ecl -load build.lisp
	mv keepcl keep

tests: keep
	sbcl --load testscl.lisp
