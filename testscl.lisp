(ql:quickload 'clutch)
(ql:quickload 'anaphora)
(ql:quickload 'keepcl)
(ql:quickload 'fiveam)

(defpackage :keep-testscl
    (:use     #:cl #:clutch #:fiveam))
(in-package :keep-testscl)

(setf *debug-on-error* t)
(setf *debug-on-failure* t)

(defvar *testdir* "/tmp/keeptest")
(trace sh)
(trace keepcl:main)
(trace keep:logmsg)
(trace ls)

(defun keepcl (cmd &rest args)
  (apply 'keepcl:main `(,cmd "-vvv" ,@args)))

(defun create-file (name size)
  (sh (str "dd if=/dev/urandom iflag=count_bytes count=" size " of='" *testdir* "/" name "'")))

(defun md5sum (path)
  (sh (str "md5sum '" path "' | cut -d\\  -f1")))

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
           (keepcl "hash" path)
      (is (~ "/Hash for .* written/" output))
      ;TODO(is (= exit-code)))
    )
    (is (probe-file (str path ".kmd")))
    (m-v-b (output exit-code)
           (keepcl "hash" path)
      (is (~ "/Found up-to-date hash/" output)))))

(defun alter (file mode &optional (overwrite-chunk 1))
  (let* ((path (str *testdir* "/" file))
         (wdate (file-write-date path)))
    (case mode
      ((:append :overwrite) (with-open-file (f path
                       :if-exists :overwrite
                       :direction :output
                       :element-type 'character)
               (case mode
                 (:append (file-position f (file-length f))
                          (write-char #\x f))
                 (:overwrite (file-position f (* (floor (/ (file-length f) 10))
                                                 overwrite-chunk))
                             (write-char #\a f)))))
      (:shorten (keep::file-truncate path (- (with-open-file (f path) (file-length f)) 1))))
      (keep::file-set-write-date path wdate)))

(defun test-check (file mode)
  (let* ((path (str *testdir* "/" file))
         (sum-before (md5sum path)))
    (is (~ "/Hash for .* written/" (keepcl "hash" path)))
    (m-v-b (output exit-code)
           (keepcl "check" path)
      (is (~ "/File .* looks good/" output))
      (is (= 0 exit-code)))
    (alter file mode)
    (is (string/= sum-before
                      (md5sum path)))
    (is (~ "/File .* has errors/" (keepcl "check" path)))))

(defun test-repair (file mode)
  (let* ((path (str *testdir* "/" file))
         (sum-before (md5sum path)))
    (keepcl "hash" path)
    (sh (str "cp '" path "' '" path ".bak'"))
    (keepcl "hash" (str path ".bak"))
    (alter file mode)
    (is (string/= sum-before
                      (md5sum path)))
    (keepcl "repair" path (str path ".bak"))
    (is (string= sum-before
                     (md5sum path)))
    (is (string= sum-before
                     (md5sum path)))))

(defvar *all-root* (list "empty" "tiny" "small" "medium" "2chunks"))
(defvar *all* (append *all-root* (list "subdir/file1" "subdir/file2" "subdir/subsubdir/file3")))

(def-suite keep)
(in-suite keep)

(test hash-file
  (init-files)
  (mapcar 'test-hash-single *all-root*))

(test check-appended-to-files
  (init-files)
  (mapcar [test-check _ :append] *all-root*))

(test check-overwritten-files
  (init-files)
  (mapcar [test-check _ :overwrite] *all-root*))

(test check-shortened-files
  (init-files)
  (mapcar [test-check _ :shorten] (remove "empty" *all-root* :test 'string=)))

(test repair-appended-to-files
  (init-files)
  (mapcar [test-repair _ :append] (remove "empty" *all-root* :test 'string=)))

(test repair-overwritten-files
  (init-files)
  (mapcar [test-repair _ :overwrite] (remove "empty" *all-root* :test 'string=)))

(test repair-shortened-files
  (init-files)
  (mapcar [test-repair _ :shorten] (remove "empty" *all-root* :test 'string=)))

(test repair-non-broken-file
  (init-files)
  (let* ((path (str *testdir* "/small"))
         (sum-before (md5sum path)))
    (keepcl "hash" path)
    (sh (str "cp '" path "' '" path ".bak'"))
    (keepcl "hash" (str path ".bak"))
    (keepcl "repair" path (str path ".bak"))
    (is (string= sum-before
                     (md5sum path)))
    (is (string= sum-before
                     (md5sum path)))))

(test repair-broken-file-with-itself
  (init-files)
  (let* ((path (str *testdir* "/small"))
         (sum-before (md5sum path)))
    (keepcl "hash" path)
    (alter "small" :overwrite)
    (is (string/= sum-before
                  (md5sum path)))
    (m-v-b (output exit-code)
           (keepcl "repair" path path)
      (is (= exit-code 0))) ;TODO: should not be zero since the file could not be repaired + check text output
    (is (string/= sum-before
                      (md5sum path)))))

(test repair-file-with-data-appended-with-itself
  (init-files)
  (let* ((path (str *testdir* "/small"))
         (sum-before (md5sum path)))
    (keepcl "hash" path)
    (alter "small" :append)
    (is (string/= sum-before
                      (md5sum path)))
    (m-v-b (output exit-code)
           (keepcl "repair" path path) ;TODO: check text output, should not need arg twice
      (is (= exit-code 0)))
    (is (string= sum-before
                     (md5sum path)))))

(test repair-non-broken-file-with-broken-file
  (init-files)
  (let* ((path (str *testdir* "/small"))
         (sum-before (md5sum path)))
    (keepcl "hash" path)
    (let ((bakpath (str path ".bak")))
      (sh (str "cp '" path "' '" bakpath "'"))
      (keepcl "hash" bakpath)
      (alter "small.bak" :overwrite)
      (keepcl "repair" path bakpath)) ;TODO check output
    (is (string= sum-before
                     (md5sum path)))))

(test repair-very-broken-file
  (init-files)
  (let* ((path (str *testdir* "/small"))
         (sum-before (md5sum path)))
    (keepcl "hash" path)
    (let ((bakpath (str path ".bak")))
      (sh (str "cp '" path "' '" bakpath "'"))
      (keepcl "hash" bakpath)
      (alter "small" :overwrite)
      (alter "small" :overwrite 9)
      (alter "small" :append)
      (is (string/= sum-before
                   (md5sum path)))
      (keepcl "repair" path bakpath)) ;TODO check output
    (is (string= sum-before
                     (md5sum path)))))

(test repair-broken-file-with-file-broken-elsewhere
  (init-files)
  (let* ((path (str *testdir* "/small"))
         (sum-before (md5sum path)))
    (keepcl "hash" path)
    (let ((bakpath (str path ".bak")))
      (sh (str "cp '" path "' '" bakpath "'"))
      (keepcl "hash" bakpath)
      (alter "small" :overwrite)
      (alter "small.bak" :overwrite 9)
      (keepcl "repair" path bakpath)) ;TODO check output
    (is (string= sum-before
                     (md5sum path)))))

(test hash-with-corrupt-metadata
  (init-files)
  (let* ((path (str *testdir* "/small"))
         (kmd-path (str path ".kmd")))
    (keepcl "hash" path)
    (let ((sum-before (md5sum kmd-path)))
      (alter "small.kmd" :overwrite)
      (is (string/= sum-before
                        (md5sum path)))
      (m-v-b (output exit-code)
             (keepcl "hash" path)
         (is (= (length (~ "/Updated hash .* original corrupted/" output))))
         (is (= exit-code 0))
         (is (string= sum-before
                      (md5sum kmd-path)))))))

(test hash-and-check-dir
  (init-files)
  (mapcar [keepcl "hash" (str *testdir* "/" _)] *all*)
  (m-v-b (output exit-code)
         (keepcl "check" *testdir*)
    (is (= (length (~ "/File .* looks good/g" output))
           (length *all*))))
  (mapcar [alter _ :overwrite] *all*)
  (m-v-b (output exit-code)
         (keepcl "check" *testdir*)
    (is (= (length (~ "/File .* has errors/g" output))
           (length *all*)))))

(test unknown-command
  (m-v-b (output exit-code)
         (keepcl "asdf")
    (is (~ "/Unknown command/" output))
    (is (= 9 exit-code))))

(test missing-files
  (m-v-b (output exit-code)
         (keepcl "check" "adfsdfasgjh")
    (is (~ "/File not found/" output))
    (is (= 99 exit-code)))
  (m-v-b (output exit-code)
         (keepcl "repair" "asdfasdf" "/tmp/keeptest/tiny")
    (is (~ "/File not found/" output))
    (is (= 99 exit-code)))
  (m-v-b (output exit-code)
         (keepcl "repair" "/tmp/keeptest/tiny asdfasdfasd")
    (is (~ "/File not found/" output))
    (is (= 99 exit-code)))
  (m-v-b (output exit-code)
         (keepcl "hash" "asdfasdfasd")
    (is (~ "/File not found/" output))
    (is (= 99 exit-code)))
  (m-v-b (output exit-code)
         (keepcl "hash" "asdfasdfasd")
    (is (~ "/File not found/" output))
    (is (= 99 exit-code))))

;TODO: test repair folder
;TODO: test with corrupt metadata in all commands
;TODO: test wrong number of arguments to each command
;TODO: test replicate

(test hash-dir ()
  (init-files)
  (m-v-b (output exit-code)
         (keepcl "hash" *testdir*)
    (is (~ "/is a directory/" output))))

(test hash-dir-and-file ()
  (init-files)
  (m-v-b (output exit-code)
         (keepcl "hash" *testdir* (str *testdir* "/small"))
    (is (= 1 (length (~ "/is a directory/" output))))
    (is (= 1 (length (~ "/Hash for .* written/" output))))
    (is (probe-file (str *testdir* "/small.kmd")))))

(run! 'keep)
(quit)
