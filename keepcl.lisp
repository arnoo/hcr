(defpackage :keepcl
   (:use     #:cl #:clutch #:keep))

(in-package :keepcl)

(defun command-help (&optional message)
  (awhen message
    (logmsg 0 it))
  (logmsg 0 "keep <command> <option> <files>
  available commands :
   hash
   check
   repair <file> <mirrors>
   replicate <source> <dest>"))

(defun meta-file-path (file)
  (str file ".kmd"))

(defun command-hash ()
  (loop for file in {(argv) 2 -1}
        for meta-path = (meta-file-path file)
        do (write-metadata-to-file (compute-metadata file) meta-path)
           (logmsg 0 "Hash for " file " written to " meta-path #\Newline)))

(defun check-file (file)
  (let ((meta-path (meta-file-path file)))
    (acond ((not (probe-file meta-path))
              (logmsg 0 "No metadata file for file " file)
              1)
           ((metadata-error (read-metadata-from-file meta-path))
              (logmsg 0 "Metadata file corrupted for " file)
              2)
           ((file-errors file (read-metadata-from-file meta-path))
              (logmsg 0 "File " file " has errors in " (metadata-chunk-size metadata) "b chunks " (join "," (mapcar 'str it)))
              3)
           (t
              (logmsg 0 "File " file " looking good")
              0))))

(defun command-check ()
  (mapcar 'check-file {(argv) 2 -1})) ;TODO: exit code = reduce '+ ? Non, plutot le nombre de fichiers en erreur (quelconque)

(defun command-repair ()
  (unless (> (length (argv)) 3)
    (command-help "Repair needs a file and at least one mirror")
    (return-from command-repair))
  (let* ((file (argv 2))
         (mirrors {(argv) 3 -1})
         (valid-metadata nil))
    (loop for mfile in (mapcar 'meta-file-path (cons file mirrors))
          for md  = (aand (probe-file mfile) (read-metadata-from-file it)) ; TODO: handler-bind / unwind-protect...
          when (and md (not (metadata-error md)))
          do (setf valid-metadata md))
    (unless valid-metadata
      (logmsg 0 "No valid metadata found for file or mirrors ... do you have any other copy ?")
      (return-from command-repair))
  (repair-file file valid-metadata mirrors)))

(defun list-hashed-files (&rest paths)
  (remove-if-not [probe-file (meta-file-path _)]
                 (flatten (mapcar [ls _ :recursive t :files-only t] paths))))

(defun replicate (file dest &key auto-repair)
  (let* ((meta-path (meta-file-path file))
         (metadata (read-metadata-from-file meta-path)))
    (cond
      ((metadata-error metadata)
         (logmsg 0 "Metadata is corrupted: " meta-path)
         (when auto-repair
            ;TODO
            )
         (return-from replicate 1))
      ((file-errors file metadata)
         (logmsg 0 "Source file is corrupted: " file)
         (when auto-repair
            ;TODO
            )
         (return-from replicate 2))
      ((probe-file dest)
         (when (file-errors dest metadata)
           (let ((temp-dest (str dest ".kpart")))
              (copy-file file temp-dest :keep-date t)
              (cond ((file-errors temp-dest metadata)
                      (logmsg 0 "Copy failed:" dest)
                      (return-from replicate 3))
                    (t
                      (rename-file temp-dest dest))))))
      ((not (probe-file dest))
         (copy-file file dest :keep-date t)))
    (let ((dest-meta-path (meta-file-path dest)))
      (copy-file (meta-file-path file) dest-meta-path :keep-date t)
      (when (metadata-error (read-metadata-from-file dest-meta-path))
        (logmsg 0 "Metadata copy failed:" dest-meta-path)
        (return-from replicate 3))))
  (return-from replicate 0))

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
  "Copy hashed sources to destination. Updates destination if files already exist."
  (let* ((sources {(argv) 2 -2})
         (dest {(argv) -1})
         (single-file (and (= (length sources) 1)
                           (not (probe-dir {sources 0})))))
    (loop for path in sources
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
          for source in (mapcar 'probe-file sources)
          do (loop for file in (ls source :files-only t :recursive t)
                   when (not (probe-file (meta-file-path file)))
                  do (incf unhashed)
                  else
                  do (awith (if single-file
                               dest
                               (merge-path {file (length source) -1} (probe-file dest)))
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
