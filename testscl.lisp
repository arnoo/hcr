(ql:quickload 'clutch)
(ql:quickload 'anaphora)
(ql:quickload 'keep)
(defpackage :keep-testscl
    (:use     #:cl #:clutch))

(in-package :keep-testscl)

(defvar *testdir* "/tmp/keeptest")
(trace sh)

(defun create-file (name size)
  (sh (str "dd if=/dev/urandom iflag=count_bytes count=" size " of='" *testdir* "/" name "'")))

(defun init-files ()
  (princ (str "Setting up" #\Newline))
  (rm *testdir* :recursive t)
  (mkdir *testdir*)
  (create-file "empty"  "0"   )
  (create-file "tiny"   "10"  )
  (create-file "small"  "10K" )
  (create-file "medium" "10M" )
  (create-file "2chunks" "8192" )
  (mkdir (str *testdir* "/subdir/subsubdir"))
  (create-file "subdir/file1"     "10K" )
  (create-file "subdir/file2"     "10K" )
  (create-file "subdir/subsubdir/file3"  "10K" ))

(defun test-hash-single (file)
  (let ((path (str *testdir* "/" file)))
    (assert (~ "/Hash for .* written/" (sh (str "./keep hash '" path "'"))))
    (assert (probe-file (str path ".kmd")))
    (assert (~ "/Found up-to-date hash/" (sh (str "./keep hash '" path "'"))))
    ))

(defun alter (file mode)
  (let* ((path (str *testdir* "/" file)))
    (case mode
      ((1 2) (with-open-file (f path
                       :if-exists :overwrite
                       :direction :output
                       :element-type 'character)
               (case mode
                 (1 (file-position f (file-length f))
                    (write-char #\x f))
                 (2 (file-position f (floor (/ (file-length f) 10)))
                    (write-char #\a f)))))
      (3 (keep::file-truncate path (- (with-open-file (f path) (file-length f)) 1))))))

(defun test-check (file mode)
  (let* ((path (str *testdir* "/" file))
         (sum-before (sh (str "md5sum '" path "'"))))
    (assert (~ "/Hash for .* written/" (sh (str "./keep hash '" path "'"))))
    (assert (~ "/File .* looks good/" (sh (str "./keep check '" path "'"))))
    (alter file mode)
    (assert (string/= sum-before
                      (sh (str "md5sum '" path"'"))))
    (sh (str "./keep check '" path "'"))))

(defun test-repair (file mode)
  (let* ((path (str *testdir* "/" file))
         (sum-before (sh (str "md5sum '" path"'"))))
    (sh (str "./keep hash '" path "'"))
    (sh (str "cp '" path "' '" path ".bak'"))
    (sh (str "./keep hash '" path ".bak'"))
    (alter file mode)
    (assert (string/= sum-before
                      (sh (str "md5sum '" path"'"))))
    (sh (str "./keep repair '" path "' '" path ".bak'"))
    (assert (string= sum-before
                     (sh (str "md5sum '" path"'"))))
    (assert (string= sum-before
                     (sh (str "md5sum '" path".bak'"))))))

(defvar *all-root* (list "empty" "tiny" "small" "medium" "2chunks"))
(defvar *all* (append *all-root* (list "subdir/file1" "subdir/file2" "subdir/subsubdir/file3")))

(init-files)
(mapcar 'test-hash-single *all-root*)

(init-files)
(mapcar [test-check _ 1] *all-root*)

(init-files)
(mapcar [test-check _ 2] *all-root*)

(init-files)
(mapcar [test-check _ 3] (remove "empty" *all-root* :test 'string=))

(init-files)
(mapcar [test-repair _ 1] (remove "empty" *all-root* :test 'string=))

(init-files)
(mapcar [test-repair _ 2] (remove "empty" *all-root* :test 'string=))

(init-files)
(mapcar [test-repair _ 3] (remove "empty" *all-root* :test 'string=))

(init-files)
(let ((hash (sh (str "./keep hash '" *testdir* "'"))))
  (assert (= (length (~ "/Hash for .* written/" hash))
             (length *all*))))
(let ((hash (sh (str "./keep hash '" *testdir* "'"))))
  (assert (= (length (~ "/Found up-to-date hash/" hash))
             (length *all*))))
(let ((hash (sh (str "./keep check '" *testdir* "'"))))
  (assert (= (length (~ "/File .* looks good/" hash))
             (length *all*))))
(mapcar [alter _ 1] *all*)
(let ((hash (sh (str "./keep check '" *testdir* "'"))))
  (assert (= (length (~ "/File .* looks good/" hash))
             (length *all*))))

;TODO: test repair non broken file
;TODO: test wrong number of arguments to each command
;TODO: test all commands with invalid paths
;TODO: test help
