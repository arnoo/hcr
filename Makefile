keep: build.lisp keep.lisp keepcl.lisp keep.asd keepcl.asd
	ecl -load build.lisp
	mv keepcl keep

tests:
	ecl -load testscl.lisp
