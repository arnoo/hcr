(ql:quickload 'clutch)
(ql:quickload 'anaphora)
(ql:quickload 'keepcl)
(defpackage :keep-testscl
    (:use     #:cl #:clutch))

(in-package :keep-testscl)

(defvar *testdir* "/tmp/keeptest")
(trace sh)
(trace keepcl:main)

(defun create-file (name size)
  (sh (str "dd if=/dev/urandom iflag=count_bytes count=" size " of='" *testdir* "/" name "'")))

(defun init-files ()
  (princ (str "Setting up" #\Newline))
  (rm *testdir* :recursive t)
  (mkdir *testdir*)
  (create-file "empty"  "0"    )
  (create-file "tiny"   "10"   )
  (create-file "small"  "10K"  )
  (create-file "medium" "10M"  )
  (create-file "2chunks" "8192")
  (mkdir (str *testdir* "/subdir/subsubdir"))
  (create-file "subdir/file1"     "10K" )
  (create-file "subdir/file2"     "10K" )
  (create-file "subdir/subsubdir/file3"  "10K" ))

(defun test-hash-single (file)
  (let ((path (str *testdir* "/" file)))
    (m-v-b (output exit-code)
           (keepcl:main "hash" path)
      (assert (~ "/Hash for .* written/" output))
      ;TODO(assert (= exit-code)))
    )
    (assert (probe-file (str path ".kmd")))
    (m-v-b (output exit-code)
           (keepcl:main "hash" path)
      (assert (~ "/Found up-to-date hash/" output)))))

(defun alter (file mode)
  (let* ((path (str *testdir* "/" file))
         (wdate (file-write-date path)))
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
      (3 (keep::file-truncate path (- (with-open-file (f path) (file-length f)) 1))))
      (keep::file-set-write-date path wdate)))

(defun test-check (file mode)
  (let* ((path (str *testdir* "/" file))
         (sum-before (sh (str "md5sum '" path "'"))))
    (assert (~ "/Hash for .* written/" (keepcl:main "hash" path)))
    (assert (~ "/File .* looks good/" (keepcl:main "check" path)))
    (alter file mode)
    (assert (string/= sum-before
                      (sh (str "md5sum '" path "'"))))
    (assert (~ "/File .* has errors/" (keepcl:main "check" path)))))

(defun test-repair (file mode)
  (let* ((path (str *testdir* "/" file))
         (sum-before (sh (str "md5sum '" path "' | cut -d\\  -f1"))))
    (keepcl:main "hash" path)
    (sh (str "cp '" path "' '" path ".bak'"))
    (keepcl:main "hash" (str path ".bak"))
    (alter file mode)
    (assert (string/= sum-before
                      (sh (str "md5sum '" path "' | cut -d\\  -f1"))))
    (keepcl:main "repair" path (str path ".bak"))
    (assert (string= sum-before
                     (sh (str "md5sum '" path "' | cut -d\\  -f1"))))
    (assert (string= sum-before
                     (sh (str "md5sum '" path ".bak' | cut -d\\  -f1"))))))

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

;=== test repair on various kinds of brokenness for a single file  ===
(init-files)
(mapcar [test-repair _ 1] (remove "empty" *all-root* :test 'string=))

(init-files)
(mapcar [test-repair _ 2] (remove "empty" *all-root* :test 'string=))

(init-files)
(mapcar [test-repair _ 3] (remove "empty" *all-root* :test 'string=))

;=== test repair on non broken file ===
(init-files)
(let* ((path (str *testdir* "/small"))
       (sum-before (sh (str "md5sum '" path "' | cut -d\\  -f1"))))
  (keepcl:main "hash" path)
  (sh (str "cp '" path "' '" path ".bak'"))
  (keepcl:main "hash" (str path ".bak"))
  (keepcl:main "repair" path (str path ".bak"))
  (assert (string= sum-before
                   (sh (str "md5sum '" path "' | cut -d\\  -f1"))))
  (assert (string= sum-before
                   (sh (str "md5sum '" path ".bak' | cut -d\\  -f1")))))

;TODO: test repair broken file with itself
;(init-files)
;(let* ((path (str *testdir* "/small"))
;       (sum-before (sh (str "md5sum '" path "' | cut -d\\  -f1"))))
;  (keepcl:main "hash" path)
;  (alter "small" 2)
;  (assert (string/= sum-before
;                    (sh (str "md5sum '" path "' | cut -d\\  -f1"))))
;  (m-v-b (output exit-code)
;         (keepcl:main "repair" path path)
;
;         )
;  (assert (string/= sum-before
;                    (sh (str "md5sum '" path "' | cut -d\\  -f1")))))

;TODO: test repair file that is valid except for data appended to it with itself
;TODO: test repair non broken file with broken file
;TODO: test repair broken file with other broken file (elsewhere)
;TODO: test repair broken file with other broken file in same chunk
;TODO: test wrong number of arguments to each command


(init-files)
(m-v-b (output exit-code)
       (keepcl:main "hash" *testdir*)
  (assert (= (length (~ "/Hash for .* written/g" output))
             (length *all*))))
(m-v-b (output exit-code)
       (keepcl:main "hash" *testdir*)
  (assert (= (length (~ "/Found up-to-date hash/g" output))
             (length *all*))))
(m-v-b (output exit-code)
       (keepcl:main "check" *testdir*)
  (assert (= (length (~ "/File .* looks good/g" output))
             (length *all*))))
(mapcar [alter _ 1] *all*)
(m-v-b (output exit-code)
       (keepcl:main "check" *testdir*)
  (assert (= (length (~ "/File .* looks good/g" output))
             (length *all*))))

(m-v-b (output exit-code)
       (keepcl:main "asdf")
  (assert (~ "/Unknown command/" ) output)
  (assert (= 9 exit-code)))
(m-v-b (output exit-code)
       (sh "./keep")
  (assert (= 9 exit-code)))
(m-v-b (output exit-code)
       (keepcl:main "check" "adfsdfasgjh")
  (assert (~ "/File not found/" ) output)
  (assert (= 99 exit-code)))
(m-v-b (output exit-code)
       (keepcl:main "repair" "asdfasdf" "/tmp/keeptest/tiny")
  (assert (~ "/File not found/" ) output)
  (assert (= 99 exit-code)))
(m-v-b (output exit-code)
       (keepcl:main "repair" "/tmp/keeptest/tiny asdfasdfasd")
  (assert (~ "/File not found/" ) output)
  (assert (= 99 exit-code)))
(m-v-b (output exit-code)
       (keepcl:main "hasn" "asdfasdfasd")
  (assert (~ "/File not found/" ) output)
  (assert (= 99 exit-code)))
(m-v-b (output exit-code)
       (keepcl:main "hasn" "asdfasdfasd")
  (assert (~ "/File not found/" ) output)
  (assert (= 99 exit-code)))

;TODO: test all commands with invalid paths
;TODO: test help
;TODO: test replicate
;TODO: test repair folder
