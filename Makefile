keep:
	ecl -load build.lisp
	mv keepcl keep

tests: keep
	sbcl --load testscl.lisp
