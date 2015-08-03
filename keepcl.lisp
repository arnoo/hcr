(defpackage :keepcl
   (:use     #:cl #:clutch #:keep))

(in-package :keepcl)

(defun command-help (&optional message)
  (awhen message
    (print it))
  (print "keep <command> <option> <files>
  available commands :
   hash
   check
   repair <file> <mirrors>
   replicate <source> <dest>"))

(defun meta-file-path (file)
   (~s "/\\/([^\\/]*)$/\\/.\\1.kmd/" file))

(defun command-hash ()
  (loop for file in {(argv) 2 -1}
        for meta-path = (meta-file-path file)
        do (write-metadata-to-file (compute-metadata file) meta-file)
           (print (str "Hash for " file " written to " meta-file #\Newline))))

(defun command-check ()
  (loop for file in {(argv) 2 -1}
        for meta-path = (meta-file-path file)
        for metadata = (read-metadata-from-file meta-path)
        do (if (metadata-error metadata)
	       (print (str "Metadata file corrupted for " file))
	       (aif (file-errors file metadata)
  	            (print (str "File " file " has errors in " (metadata-chunk-size metadata) "b chunks " (join "," it)))
		    (str "File " file " looking good")))))

(defun command-repair ()
  (unless (> (length (argv)) 3)
    (command-help "Repair needs a file and at least one mirror")
    (return-from command-repair))
  (let* ((file {argv 2})
         (mirrors {argv 3 -1})
         (valid-metadata nil))
    (loop for mfile in (mapcar 'meta-file-path (cons file mirrors))
          for md  = (aand (probe-file mfile) (read-metadata-from-file it)) ; TODO: handler-bind / unwind-protect...
          when (and md (not (metadata-error md)))
          do (setf valid-metadata md))
    (unless valid-metadata
      (printf "No valid metadata found for file or mirrors ... do you have any other copy ?")
      (return-from command-repair))
  (repair-file file md mirrors)))

(defun list-hashed-files (&rest paths)
  (remove-if-not [probe-file (meta-file-path _)]
                 (flatten (mapcar [ls _ :recursive t :files-only t] paths))))

(defun replicate (file dest &key auto-repair)
  (let* ((meta-path (meta-file-path file))
         (metadata (read-metadata-from-file meta-path)))
    (cond
      ((metadata-error metadata)
         (print (str "Metadata is corrupted: " meta-path))
         (when auto-repair
            ;TODO
            )
         (return-from replicate 1))
      ((file-errors file metadata)
         (print (str "Source file is corrupted: " file))
         (when auto-repair
            ;TODO
            )
         (return-from replicate 2))
      ((probe-file dest)
         (when (file-errors dest metadata)
           (let ((temp-dest (str dest ".kpart")))
              (copy-file file temp-dest :keep-date t)
              (cond ((file-errors temp-dest metadata)
                      (print (str "Copy failed:" dest))
                      (return-from replicate 3))
                    (t
                      (rename-file temp-dest dest))))))
      ((not (probe-file dest))
         (copy-file file dest :keep-date t)))
    (let ((dest-meta-path (meta-file-path dest)))
      (copy-file (meta-file-path file) dest-meta-path :keep-date t)
      (when (metadata-error (read-metadata-from-file dest-meta-path))
        (print (str "Metadata copy failed:" dest-meta-path))
        (return-from replicate 3))))
  (return-from replicate 0))

(defun copy-file (file dest &key keep-date)
  (let ((seq (make-array 4096 :element-type '(unsigned-byte 8))))
    (with-open-binfile (f file)
      (with-open-binfile (fdest dest :direction :output)
        (loop for pos = (read-sequence seq f)
              until (= pos 0)
              do (write-sequence seq fdest)))))
  ; TODO: set file write date to same as original if keep-date (how ?)
  ; TODO: set file permissions to same as original (how ?)
)

(defun command-replicate ()
  "Copy hashed sources to destination. Updates destination if files already exist."
  (let ((sources {argv 2 -2})
        (dest {argv -1}))
    (when (and (= (length sources) 1)
               (not (probe-dir {sources 0}))
               (not (probe-dir dest)))
       (print "When copying multiple files, destination must be a directory")
       (return-from command-replicate 1))
    (if (zerop (reduce '+
                       (mapcar 'replicate
                               (remove-if-not [probe-file (meta-file-path _)]
                                              (flatten (mapcar 'ls sources))))))
        (print "Done, no errors occured")
        (print "/!\\ Done, but errors occcured /!\\"))))

(defun command-help (&optional message)
  "Not yet implemented"
  (print "This is a help message"))

(defun main ()
  (if (argv 1)
      (aif (find-symbol (str "COMMAND-" (uc (argv 1))) :keepcl)
           (funcall it)
           (command-help (str "Unkown command : " (argv 1))))
      (command-help)))
