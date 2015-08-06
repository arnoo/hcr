(defpackage :keepcl
   (:use     #:cl #:clutch #:keep))

(in-package :keepcl)

(define-condition meta-condition  (error) ())
(define-condition meta-open-error (meta-condition) ())
(define-condition meta-outdated   (meta-condition) ())
(define-condition meta-corrupted  (meta-condition) ())

(defun command-help (&optional message)
  (awhen message
    (logmsg 0 it))
  (logmsg 0 "keep <command> <option> <files>
  available commands :
   hash
   check
   repair <file> <mirrors>
   replicate <src> <dest>"))

(defun meta-file-path (file)
  (str file ".kmd"))

(defun get-meta (file)
  (handler-bind ((file-error [error 'meta-open-error]))
    (awith (read-meta-from-file (meta-file-path file))
      (when (meta-outdated it file)
        (error 'meta-outdated))
      (when (meta-error it)
        (error 'meta-corrupted))
      it)))

(defun command-check ()
  (mapcar #'check-file {(argv) 2 -1}))

(defun command-hash ()
    (loop for file in {(argv) 2 -1}
          for meta-path = (meta-file-path file)
          do (block hash-file
                (handler-bind ((meta-condition (lambda (c) (write-meta-to-file (compute-meta file) meta-path)
                                                      (case (type-of c)
                                                         ('meta-open-error (logmsg 0 "Hash for " file " written to " meta-path))
                                                         ('meta-outdated   (logmsg 0 "Updated hash for " file " written to " meta-path))
                                                         ('meta-corrupted  (logmsg 0 "Updated hash /!\\ original corrupted /!\\ for " file " written to " meta-path))
                                                         (otherwise (error c)))
                                                      (return-from hash-file nil))))
                 (get-meta file)
                 (logmsg 0 "Found up-to-date hash for " file " in " meta-path)))))

(defun meta-outdated (meta file)
  (/= (file-write-date file)
      (meta-file-date meta)))

(defun check-file (file)
  (handler-bind ((meta-open-error (lambda (c) (logmsg 0 "Can't open meta file for " file)   (return-from check-file 1)))
                 (meta-outdated   (lambda (c) (logmsg 0 "Meta file is outdated for " file)  (return-from check-file 2)))
                 (meta-corrupted  (lambda (c) (logmsg 0 "Meta file is corrupted for " file) (return-from check-file 3))))
    (let* ((meta (get-meta file))
           (errors (file-errors file meta)))
      (cond (errors
              (logmsg 0 "File " file " has errors in " (meta-chunk-size meta) "b chunks " (join "," (mapcar 'str errors)))
              4)
            (t
              (logmsg 0 "File " file " looks good")
              0)))))

(defun list-hashed-files (&rest paths)
  (remove-if-not [probe-file (meta-file-path _)]
                 (flatten (mapcar [ls _ :recursive t :files-only t] paths))))

(defun replicate (src dest &key repair-src)
  (let ((orig-meta)
        (dest-meta))
    (handler-bind ((meta-open-error (lambda (c) (logmsg 0 "Can't open meta file for " src)   (return-from replicate 1)))
                   (meta-outdated   (lambda (c) (write-meta-to-file (compute-meta src) meta-path)
                                           (continue)))
                   (meta-corrupted  (lambda (c) (logmsg 0 "Meta file is corrupted for " src)
                                           (when repair-src
                                              ;TODO)
                                           (return-from replicate 2)))))
      (setf src-meta (get-meta src)))
    (when (file-errors src src-meta)
      (logmsg 0 "Src file is corrupted: " src)
      (when repair-src
          ;TODO
          )
      (return-from replicate 3))
    (when (and (probe-file dest)
               (not (file-errors dest src-meta)))
       (logmsg 0 "Replica file up to date: " dest)
       (return-from replicate 0))
    (let ((temp-dest (str dest ".kpart")))
      (handler-bind ((meta-open-error (lambda (c) (muffle-warning)))
                     (meta-outdated   (lambda (c) (muffle-warning)))
                     (meta-corrupted  (lambda (c) (logmsg 0 "/!\\ Replica meta file was corrupted: " (meta-file-path dest)) (muffle-warning))))
          (when (and (probe-file dest) (file-errors dest (get-meta dest)))
            (logmsg 0 "/!\\ Replica file was corrupted: " dest)))
       (copy-file src temp-dest :keep-date t)
       (if (file-errors temp-dest src-meta)
           (progn (logmsg 0 "Copy failed:" dest)
                  (return-from replicate 4))
           (rename-file temp-dest dest))
      (awith (meta-file-path dest)
        (copy-file (meta-file-path src) it :keep-date t))
      (handler-bind ((meta-condition (lambda (c) (logmsg 0 "Meta file copy failed for" src) (return-from replicate 5))))
        (get-meta dest))))
  0)

(defun copy-file (file dest &key keep-date)
  (let ((seq (make-array 4096 :element-type '(unsigned-byte 8))))
    (keep::with-open-binfile (f file)
      (keep::with-open-binfile (fdest dest :direction :output)
        (loop for pos = (read-sequence seq f)
              until (= pos 0)
              do (write-sequence seq fdest :end pos)))))
  ; TODO: set file write date to same as original if keep-date (how ?)
  ; TODO: set file permissions to same as original (how ?)
)

(defun command-replicate ()
  "Copy hashed srcs to destination. Updates destination if files already exist."
  (let* ((srcs {(argv) 2 -2})
         (dest {(argv) -1})
         (single-file (and (= (length srcs) 1)
                           (not (probe-dir {srcs 0})))))
    (loop for path in srcs
          unless (probe-file path)
          do (logmsg 0 "File or directory not found:" path)
             (return-from command-replicate 2))
    (when (and (not single-file)
               (not (probe-dir dest)))
       (logmsg 0 "When copying multiple files, destination must be a directory")
       (return-from command-replicate 1))
    (loop with unhashed = 0
          with hashed = 0
          with errors = 0
          for src in (mapcar 'probe-file srcs)
          do (loop for file in (ls src :files-only t :recursive t)
                   when (not (probe-file (meta-file-path file)))
                   do (incf unhashed)
                   else
                   do (awith (if single-file
                                dest
                                (merge-path {file (length src) -1} (probe-file dest)))
                             (incf hashed)
                             (incf errors (replicate file it))))
          finally (if (zerop errors)
                      (logmsg 0 "Done, no errors occured")
                      (logmsg 0 "/!\\ Done, but errors occcured /!\\"))
                  (logmsg 0 hashed " file(s) copied")
                  (logmsg 0 unhashed " unhashed file(s) not copied"))))

(defun main ()
  (if (argv 1)
      (aif (find-symbol (str "COMMAND-" (uc (argv 1))) :keepcl)
           (funcall it)
           (command-help (str "Unkown command : " (argv 1))))
      (command-help)))
