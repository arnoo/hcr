hcr: build.lisp hcr.lisp hcr-cli.lisp hcr.asd hcr-cli.asd
	which ecl || { echo "Please install ECL to build hcr."; exit 1; }
#[ ! -e quicklisp.lisp ] && wget https://beta.quicklisp.org/quicklisp.lisp
#ecl -load quicklisp.lisp -eval '(quicklisp-quickstart:install :path "./quicklisp")'
	ecl -load build.lisp
	mv hcr-cli hcr

tests: hcr
	ecl -load testsrun.cl
	A=$$(./hcr); echo "$$A" | grep -q "Usage" || echo "Test failed: hcr without command (1)"; echo "$$A" | grep -qv "Unknown command" || echo "Test failed: hcr without command (2)"

sbtests:
	sbcl --load testsrun.cl

install: hcr
	/usr/bin/install -c -m 0755 hcr /usr/bin/hcr

