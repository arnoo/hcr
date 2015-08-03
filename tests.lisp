(require 'keep)

(defpackage :keep-tests
    (:use     #:cl #:keep #:clutch))

(in-package :keep-tests)

(defvar ref-file "/tmp/keep_test.txt")
(defvar broken-file "/tmp/keep_test_broken.txt")

(with-open-file (f ref-file
                   :element-type 'character
		               :direction :output
			             :if-exists :supersede
			             :if-does-not-exist :creat)
  (loop for i from 0 to (random 50000)
        do (write-sequence "sadfas;ldkjas;dfklj" f)))

(defvar ref-hash (ironclad:byte-array-to-hex-string (ironclad:digest-file :sha1 ref-file)))

(sh (str "cp " ref-file " " broken-file))

(with-open-file (f broken-file :if-exists :overwrite
		               :direction :output
		               :element-type 'character)
  (loop for i from 0 below (file-length f) by 9500
	do (file-position f i)
     (write-char #\a f)))

(defvar broken-hash (ironclad:byte-array-to-hex-string (ironclad:digest-file :sha1 broken-file)))

(assert (string/= broken-hash ref-hash))

(defvar original-metadata (compute-metadata ref-file))

(assert (not (file-errors ref-file original-metadata)))
(assert (file-errors broken-file original-metadata))

(repair-file broken-file original-metadata (list broken-file))

(assert (file-errors broken-file original-metadata))

(repair-file broken-file original-metadata (list ref-file))

(assert (not (file-errors broken-file original-metadata)))
(assert (not (file-errors ref-file original-metadata)))
