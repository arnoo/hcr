;
;   Copyright 2014-2016 Arnaud Betremieux <arnaud@btmx.fr>
;
;   This file is a part of Hcr.
;
;   This program is free software: you can redistribute it and/or modify
;   it under the terms of the GNU General Public License as published by
;   the Free Software Foundation, either version 3 of the License, or
;   (at your option) any later version.
;
;   This program is distributed in the hope that it will be useful,
;   but WITHOUT ANY WARRANTY; without even the implied warranty of
;   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;   GNU General Public License for more details.
;
;   You should have received a copy of the GNU General Public License
;   along with this program.  If not, see <http://www.gnu.org/licenses/>.
;

(require 'asdf)
#-quicklisp
(let ((quicklisp-init (merge-pathnames "dev/quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(ql:quickload 'hcr-cli)
(clutch:rm "hcr-cli")

(asdf:make-build :hcr-cli
  :type :program
  :prologue-code '(progn (setf *compile-verbose* nil)
                         (setf *debugger-hook*
                               (lambda (condition hook)
                                  (declare (ignore hook))
                                  (if (typep condition 'ext:interactive-interrupt)
                                      (format t "~%~A" condition)
                                      (format t "~%ERROR: ~A~1%" condition))
                                  (si:quit 100))))
  :epilogue-code '(hcr-cli::main)
  :move-here "./")

(clutch:exit)
