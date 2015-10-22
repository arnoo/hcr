;
;   Copyright 2014-2015 Arnaud Bétrémieux <arnaud@btmx.fr>
;
;   This file is a part of Hcr.
;
;   The program in this file is free software: you can redistribute it
;   and/or modify it under the terms of the GNU General Public License
;   as published by the Free Software Foundation, either version 3 of
;   the License, or (at your option) any later version.
;
;   This program is distributed in the hope that it will be useful,
;   but WITHOUT ANY WARRANTY; without even the implied warranty of
;   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;   GNU General Public License for more details.
;
;   You should have received a copy of the GNU General Public License
;   along with this program.  If not, see <http://www.gnu.org/licenses/>.
;

(ql:quickload 'clutch)
(ql:quickload 'anaphora)
(ql:quickload 'hcr-cli)
(ql:quickload 'fiveam)

(defpackage :hcr-tests-cli
    (:use     #:cl #:clutch #:fiveam #:named-readtables))
(in-package :hcr-tests-cli)
(in-readtable clutch)

(setf *debug-on-error* t)
(setf *debug-on-failure* t)

(defvar *testdir* "/tmp/hcrtest")
(trace sh)
(trace hcr-cli:main)
(trace hcr:logmsg)
(trace ls)

(defun hcr-cli (cmd &rest args)
  (apply 'hcr-cli:main `(,cmd "-vvv" ,@args)))

(defun create-file (name size)
  (sh (str "dd if=/dev/urandom iflag=count_bytes count=" size " of='" *testdir* "/" name "'")))

(defun md5sum (path)
  (sh (str "md5sum '" path "' | cut -d\\  -f1")))

(defvar +test-files+
  (list  (cons "empty"                   "0")
         (cons "tiny"                    "10")
         (cons "small"                   "10K")
         (cons "medium"                  "10M")
         (cons "2chunks"                 "8192")
         (cons "subdir/file1"            "10K")
         (cons "subdir/file2"            "10K")
         (cons "subdir/subsubdir/file3"  "10K")))

(defun init-files (&rest which)
  (princ (str "Setting up" #\Newline))
  (rm *testdir* :recursive t)
  (mkdir *testdir*)
  (mkdir (str *testdir* "/subdir/subsubdir"))
  (loop for file in (or which (mapcar 'car +test-files+))
        do (create-file file (cdr (assoc file +test-files+ :test 'string=)))))

(defun test-hash-single (file)
  (let ((path (str *testdir* "/" file)))
    (m-v-b (output exit-code)
           (hcr-cli "hash" path)
      (is (~ "/Hash for .* written/" output))
      (is (= 0 exit-code)))
    (is (probe-file (str path ".hmd")))
    (m-v-b (output exit-code)
           (hcr-cli "hash" path)
      (is (~ "/Found up-to-date hash/" output))
      (is (= 0 exit-code)))))

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
      (:shorten (hcr::file-truncate path (- (with-open-file (f path) (file-length f)) 1))))
      (hcr::file-set-write-date path wdate)))

(defun test-check (file mode)
  (let* ((path (str *testdir* "/" file))
         (sum-before (md5sum path)))
    (is (~ "/Hash for .* written/" (hcr-cli "hash" path)))
    (m-v-b (output exit-code)
           (hcr-cli "check" path)
      (is (~ "/File .* looks good/" output))
      (is (= 0 exit-code)))
    (alter file mode)
    (is (string/= sum-before
                      (md5sum path)))
    (is (~ "/File .* has errors/" (hcr-cli "check" path)))))

(defun test-repair (file mode)
  (let* ((path (str *testdir* "/" file))
         (sum-before (md5sum path)))
    (hcr-cli "hash" path)
    (sh (str "cp '" path "' '" path ".bak'"))
    (hcr-cli "hash" (str path ".bak"))
    (alter file mode)
    (is (string/= sum-before
                      (md5sum path)))
    (hcr-cli "repair" path (str path ".bak"))
    (is (string= sum-before
                     (md5sum path)))
    (is (string= sum-before
                     (md5sum path)))))

(defvar *all-root* (list "empty" "tiny" "small" "medium" "2chunks"))
(defvar *all* (append *all-root* (list "subdir/file1" "subdir/file2" "subdir/subsubdir/file3")))

(def-suite hcr)
(in-suite hcr)

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
  (init-files "small")
  (let* ((path (str *testdir* "/small"))
         (sum-before (md5sum path)))
    (hcr-cli "hash" path)
    (sh (str "cp '" path "' '" path ".bak'"))
    (hcr-cli "hash" (str path ".bak"))
    (hcr-cli "repair" path (str path ".bak"))
    (is (string= sum-before
                     (md5sum path)))
    (is (string= sum-before
                     (md5sum path)))))

(test repair-broken-file-with-itself
  (init-files "small")
  (let* ((path (str *testdir* "/small"))
         (sum-before (md5sum path)))
    (hcr-cli "hash" path)
    (alter "small" :overwrite)
    (is (string/= sum-before
                  (md5sum path)))
    (m-v-b (output exit-code)
           (hcr-cli "repair" path path)
      (is (= exit-code 1)))  ;TODO: check text output
    (is (string/= sum-before
                      (md5sum path)))))

(test repair-file-with-data-appended-with-itself
  (init-files "small")
  (let* ((path (str *testdir* "/small"))
         (sum-before (md5sum path)))
    (hcr-cli "hash" path)
    (alter "small" :append)
    (is (string/= sum-before
                      (md5sum path)))
    (m-v-b (output exit-code)
           (hcr-cli "repair" path path) ;TODO: check text output, should not need arg twice
      (is (= exit-code 0)))
    (is (string= sum-before
                     (md5sum path)))))

(test repair-non-broken-file-with-broken-file
  (init-files "small")
  (let* ((path (str *testdir* "/small"))
         (sum-before (md5sum path)))
    (hcr-cli "hash" path)
    (let ((bakpath (str path ".bak")))
      (sh (str "cp '" path "' '" bakpath "'"))
      (hcr-cli "hash" bakpath)
      (alter "small.bak" :overwrite)
      (hcr-cli "repair" path bakpath)) ;TODO check output
    (is (string= sum-before
                     (md5sum path)))))

(test repair-very-broken-file
  (init-files "small")
  (let* ((path (str *testdir* "/small"))
         (sum-before (md5sum path)))
    (hcr-cli "hash" path)
    (let ((bakpath (str path ".bak")))
      (sh (str "cp '" path "' '" bakpath "'"))
      (hcr-cli "hash" bakpath)
      (alter "small" :overwrite)
      (alter "small" :overwrite 9)
      (alter "small" :append)
      (is (string/= sum-before
                   (md5sum path)))
      (hcr-cli "repair" path bakpath)) ;TODO check output
    (is (string= sum-before
                     (md5sum path)))))

(test repair-broken-file-with-file-broken-elsewhere
  (init-files "small")
  (let* ((path (str *testdir* "/small"))
         (sum-before (md5sum path)))
    (hcr-cli "hash" path)
    (let ((bakpath (str path ".bak")))
      (sh (str "cp '" path "' '" bakpath "'"))
      (hcr-cli "hash" bakpath)
      (alter "small" :overwrite)
      (alter "small.bak" :overwrite 9)
      (hcr-cli "repair" path bakpath)) ;TODO check output
    (is (string= sum-before
                     (md5sum path)))))

(test hash-with-corrupt-metadata
  (init-files "small")
  (let* ((path (str *testdir* "/small"))
         (hmd-path (str path ".hmd")))
    (hcr-cli "hash" path)
    (let ((sum-before (md5sum hmd-path)))
      (alter "small.hmd" :overwrite)
      (is (string/= sum-before
                        (md5sum path)))
      (m-v-b (output exit-code)
             (hcr-cli "hash" path)
         (is (= (length (~ "/Updated hash .* original corrupted/" output))))
         (is (= exit-code 0))
         (is (string= sum-before
                      (md5sum hmd-path)))))))

(test hash-and-check-dir
  (init-files)
  (mapcar [hcr-cli "hash" (str *testdir* "/" _)] *all*)
  (m-v-b (output exit-code)
         (hcr-cli "check" *testdir*)
    (is (= (length (~ "/File .* looks good/g" output))
           (length *all*))))
  (mapcar [alter _ :overwrite] *all*)
  (m-v-b (output exit-code)
         (hcr-cli "check" *testdir*)
    (is (= (length (~ "/File .* has errors/g" output))
           (length *all*)))))

(test repair-dir
  (init-files)
  (mapcar [hcr-cli "hash" (str *testdir* "/" _)] *all*)
  (m-v-b (output exit-code)
         (hcr-cli "check" *testdir*)
    (is (= (length (~ "/File .* looks good/g" output))
           (length *all*))))
  (sh (str "cp -r '" *testdir* "' '" *testdir* ".bak'"))
  (unwind-protect
    (let ((md5sums (mapcar [md5sum (str *testdir* "/" _)] *all*)))
      (mapcar [alter _ :overwrite] *all*)
      (is (not (equal (mapcar [md5sum (str *testdir* "/" _)] *all*)
                      md5sums)))
      (m-v-b (output exit-code)
             (hcr-cli "check" *testdir*)
        (is (= (length (~ "/File .* has errors/g" output))
               (length *all*))))
      (m-v-b (output exit-code)
             (hcr-cli "repair" *testdir* (str *testdir* ".bak"))
            (is (~ (str "/" (length *all*) " file\\(s\\) repaired/") output)))
      (is (equal (mapcar [md5sum (str *testdir* "/" _)] *all*)
                  md5sums))
      (is (equal (mapcar [md5sum (str *testdir* ".bak/" _)] *all*)
                  md5sums)))
    (rm (str *testdir* ".bak") :recursive t)))

(test unknown-command
  (m-v-b (output exit-code)
         (hcr-cli "asdf")
    (is (~ "/Unknown command/" output))
    (is (= 9 exit-code))))

(test missing-files
  (m-v-b (output exit-code)
         (hcr-cli "check" "adfsdfasgjh")
    (is (~ "/File not found/" output))
    (is (= 99 exit-code)))
  (m-v-b (output exit-code)
         (hcr-cli "repair" "asdfasdf" "/tmp/hcrtest/tiny")
    (is (~ "/File not found/" output))
    (is (= 99 exit-code)))
  (m-v-b (output exit-code)
         (hcr-cli "repair" "/tmp/hcrtest/tiny asdfasdfasd")
    (is (~ "/File not found/" output))
    (is (= 99 exit-code)))
  (m-v-b (output exit-code)
         (hcr-cli "hash" "asdfasdfasd")
    (is (~ "/File not found/" output))
    (is (= 99 exit-code)))
  (m-v-b (output exit-code)
         (hcr-cli "hash" "asdfasdfasd")
    (is (~ "/File not found/" output))
    (is (= 99 exit-code))))

;TODO: test repair unwritable file, test repair with unreadable file, with unredable hmd

(test hash-over-non-readable-hmd ()
  (init-files "small")
  (let* ((path (str *testdir* "/small")))
    (sh (str "touch '" *testdir* "/small'"))
    (sh (str "chmod -r '" *testdir* "/small'"))
    (m-v-b (output exit-code)
           (hcr-cli "hash" path)
      (is (~ "/Error accessing/" output))
      (is (= 2 exit-code)))))

(test hash-non-readable-file ()
  (init-files "small")
  (let* ((path (str *testdir* "/small")))
    (is (~ "/Hash for .* written/" (hcr-cli "hash" path)))
    (sh (str "chmod -r '" *testdir* "/small'"))
    (m-v-b (output exit-code)
           (hcr-cli "check" path)
      (is (~ "/Error accessing/" output))
      (is (= 2 exit-code)))))

(test check-with-non-readable-hmd ()
  (init-files "small")
  (let* ((path (str *testdir* "/small")))
    (is (~ "/Hash for .* written/" (hcr-cli "hash" path)))
    (sh (str "chmod -r '" *testdir* "/small.hmd'"))
    (m-v-b (output exit-code)
           (hcr-cli "check" path)
      (is (~ "/Error opening metadata file/" output))
      (is (= 1 exit-code)))))

(test check-non-readable-file ()
  (init-files "small")
  (let* ((path (str *testdir* "/small")))
    (is (~ "/Hash for .* written/" (hcr-cli "hash" path)))
    (sh (str "chmod -r '" *testdir* "/small'"))
    (m-v-b (output exit-code)
           (hcr-cli "check" path)
      (is (~ "/Error accessing file/" output))
      (is (= 2 exit-code)))))

(test check-with-broken-hmd ()
  (init-files "small")
  (let* ((path (str *testdir* "/small")))
    (is (~ "/Hash for .* written/" (hcr-cli "hash" path)))
    (alter "small.hmd" :overwrite)
    (m-v-b (output exit-code)
           (hcr-cli "check" path)
      (is (~ "/Metadata corrupted/" output))
      (is (= 3 exit-code)))))

(test repair-with-broken-hmd ()
  (init-files "small")
  (let* ((path (str *testdir* "/small")))
    (is (~ "/Hash for .* written/" (hcr-cli "hash" path)))
    (alter "small.hmd" :overwrite)
    (sh (str "cp '" path "' '" path ".bak'"))
    (alter "small" :overwrite)
    (m-v-b (output exit-code)
           (hcr-cli "repair" path (str path ".bak"))
      (is (~ "/Metadata corrupted/" output))
      (is (= 1 exit-code)))))

(test repair-with-broken-hmd-and-ok-hmd ()
  (init-files "small")
  (let* ((path (str *testdir* "/small"))
         (sum-before (md5sum path)))
    (is (~ "/Hash for .* written/" (hcr-cli "hash" path)))
    (sh (str "cp '" path ".hmd' '" path ".bak.hmd'"))
    (alter "small.hmd" :overwrite)
    (sh (str "cp '" path "' '" path ".bak'"))
    (alter "small" :overwrite)
    (m-v-b (output exit-code)
           (hcr-cli "repair" path (str path ".bak"))
      (is (~ "/File repaired/" output))
      (is (= 0 exit-code))
      (is (string= sum-before
                   (md5sum path))))))

(test check-with-bad-hmd-option ()
  (init-files "small")
  (let* ((path (str *testdir* "/small")))
    (is (~ "/Hash for .* written/" (hcr-cli "hash" path)))
    (sh (str "mv '" path ".hmd' '" path "2.hmd'"))
    (m-v-b (output exit-code)
           (hcr-cli "check" (str "--hmd=" *testdir* "/dfsdf.hmd") path)
      (is (~ "/Can't open meta file/" output))
      (is (= 1 exit-code)))))

(test check-with-hmd-option ()
  (init-files "small")
  (let* ((path (str *testdir* "/small")))
    (is (~ "/Hash for .* written/" (hcr-cli "hash" path)))
    (sh (str "mv '" path ".hmd' '" path "2.hmd'"))
    (m-v-b (output exit-code)
           (hcr-cli "check" (str "--hmd=" *testdir* "/small2.hmd") path)
      (is (~ "/File .* looks good/" output))
      (is (= 0 exit-code)))))

(test check-with-ignore-date-option ()
  (init-files "small")
  (let* ((path (str *testdir* "/small")))
    (is (~ "/Hash for .* written/" (hcr-cli "hash" path)))
    (sleep 1.1)
    (alter "small" :overwrite) ;if the date is wrong but the file ok the date would be fixed automatically, so we have to break the file...
    (hcr::file-set-write-date path (ut))
    (m-v-b (output exit-code)
           (hcr-cli "check" path)
      (is (~ "/Write date in metadata does not match file date/" output))
      (is (= 2 exit-code)))
    (m-v-b (output exit-code)
           (hcr-cli "check" "--ignore-date" path)
      (is (~ "/File .* has errors/" output))
      (is (= 4 exit-code)))))

(test repair-with-ignore-date-option ()
  (init-files "small")
  (let* ((path (str *testdir* "/small"))
         (sum-before (md5sum path)))
    (is (~ "/Hash for .* written/" (hcr-cli "hash" path)))
    (sleep 1.1)
    (alter "small" :overwrite) ;if the date is wrong but the file ok the date would be fixed automatically, so we have to break the file...
    (hcr::file-set-write-date path (ut))
    (sh (str "cp '" path "' '" path ".bak'"))
    (is (string/= sum-before
                  (md5sum path)))
    (m-v-b (output exit-code)
           (hcr-cli "repair" path (str path ".bak"))
      (is (~ "/Write date in metadata does not match file date/" output))
      (is (= 1 exit-code)))
    (m-v-b (output exit-code)
           (hcr-cli "repair" "--ignore-date" path (str path ".bak"))
      (is (~ "/File repaired/" output))
      (is (= 0 exit-code)))))

(test check-with-wrong-date-but-ok-file-fixes-date ()
  (init-files "small")
  (let* ((path (str *testdir* "/small")))
    (is (~ "/Hash for .* written/" (hcr-cli "hash" path)))
    (sleep 1.1)
    (hcr::file-set-write-date path (ut))
    (m-v-b (output exit-code)
           (hcr-cli "check" path)
      (is (~ "/Write date for file was incorrect in metadata. Fixed/" output))
      (is (~ "/File .* looks good/" output))
      (is (= 0 exit-code)))))

(test hash-dir ()
  (init-files)
  (m-v-b (output exit-code)
         (hcr-cli "hash" *testdir*)
    (is (~ "/is a directory/" output))))

(test hash-dir-and-file ()
  (init-files)
  (m-v-b (output exit-code)
         (hcr-cli "hash" *testdir* (str *testdir* "/small"))
    (is (= 1 (length (~ "/is a directory/" output))))
    (is (= 1 (length (~ "/Hash for .* written/" output))))
    (is (probe-file (str *testdir* "/small.hmd")))))

(run! 'hcr)
(exit)
