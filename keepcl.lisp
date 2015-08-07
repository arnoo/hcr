(defpackage :keepcl
   (:use     #:cl #:clutch #:keep))

(in-package :keepcl)

(define-condition meta-condition  (error) ())
(define-condition meta-open-error (meta-condition) ())
(define-condition meta-outdated   (meta-condition) ())
(define-condition meta-corrupted  (meta-condition) ())
(defvar *log-level* 0)

(defmacro defcommand (name shortopts longopts help &rest body)
  `(progn
      (defun ,(symb "COMMAND-" (uc name)) ,args
         ,help
         (multiple-value-bind (args cmd-options free-args
                              (getopt (argv)
                                      ,shortopts
                                      ,longopts))
            ,@body))))

(defun main ()
  (if (argv 1)
      (if-bind (cmd-fn (find-symbol (str "COMMAND-" (uc (argv 1))) :keepcl))
         (progn
           (multiple-value-bind (args cmd-options free-args)
                                (getopt (argv) "v" '("help"))
              (setf *log-level* (count "v" cmd-options))
              (if (in cmd-options "help")
                  (command-help) ; TODO: display command help !
                  (funcall cmd-fn))))
         (progn
          (logmsg 0 "Unkown command : " (argv 1))
          (command-help)))
      (command-help)))

(defun free-args ()
  (multiple-value-bind (args cmd-options free-args)
                       (getopt (argv) "" nil)
    free-args))

(defcommand help ()
  (awhen message
    (logmsg 0 it))
  (logmsg 0 "keep <command> <option> <files>
  available commands :
   hash <file>+
   check <file>+
   repair <file> <mirror>+
   replicate <src>+ <dest>
   
Use keep <command> --help for detailed help on a command"))

(defun meta-file-path (file)
  (str file ".kmd"))

(defun get-meta-from-opts (file)
  (multiple-value-bind (args opts free-args)
                       (getopt (argv) "" '("kmd=" "ignore-date"))
    (awhen (find "kmd" opts)
      (get-meta file :kmd {opts (+ it 1)} :allow-outdated (in opts "ignore-date")))))

(defun get-meta (file &key kmd allow-outdated)
  (handler-bind ((file-error (lambda (c) (logmsg 1 "Error opening metadata file: " meta-path)
                                    (error 'meta-open-error))))
    (let ((meta-path (or kmd (meta-file-path file))))
      (awith (read-meta-from-file meta-path)
        (when (and (not allow-outdated)
                   (meta-outdated it file))
          (logmsg 1 "Write date in metadata does not match file date: " file)
          (error 'meta-outdated))
        (when (meta-error it)
          (logmsg 1 "Metadata corrupted: " meta-path)
          (error 'meta-corrupted))
        it))))

(defun mirror-path (src-dir src-path mirror-dir)
  (merge-pathnames {src-path (length src-dir) -1} mirror-dir))

(defcommand repair ()
  "keep repair [options] <file> <copy>+
   
  repairs <file> based on data from copies (<copy>)

  options:
    --mkd=<file.kmd>: use the following metadata file
    --ignore-date: ignore the file write date in the specified kmd file (use only if you know what you are doing)"
    (d-b (target &rest copies)
         (free-args)
      (mapcar (lambda (f)
                 (let ((mirrors (if (probe-dir target)
                                    (mapcar [mirror-path target f _] copies)
                                    copies))
                       (valid-meta))
                   (aif (get-meta-from-opts target)
                     (setf valid-meta it)
                     (loop for copy in (cons f mirrors)
                           do (describe copy)
                              (ignore-errors
                                (awith (get-meta copy :allow-outdated t)
                                  (if (and (meta-outdated it f)
                                           (not (in cmd-options "ignore-date")))
                                    (logmsg 1 "Write date in metadata does not match file date: " copy)
                                    (setf valid-meta it))))))
                   (if valid-meta
                       (progn
                         (apply #'repair-file
                               (append (list f valid-meta mirrors)))
                         (logmsg 0 "File repaired: " f))
                      (logmsg 0 "/!\\ can't repair " f ": no valid and up-to-date metadata found"))))
              (ls target :recursive t :files-only t))))
    
(defcommand check ()
  (mapcar #'check-file {(argv) 2 -1}))

(defcommand hash ()
    (loop for file in (free-args)
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
                 (meta-corrupted  (lambda (c) (logmsg 0 "Meta file is corrupted for " file) (return-from check-file 3))))
    (let* ((meta (get-meta file :allow-outdated t))
           (errors (file-errors file meta)))
      (cond (errors
              (if (meta-outdated meta file)
                (progn (logmsg 0 "/!\\ Write date in metadata does not match file date:" file)
                       2)
                (progn (logmsg 0 "/!\\ File " file " has errors in " (meta-chunk-size meta) "B chunks " (join "," (mapcar 'str errors)))
                       4)))
            (t
              (when (meta-outdated meta file)
                (logmsg 1 "Write date for file was incorrect in metadata. Fixed: " file)
                (setf (meta-file-date meta) (ut-to-unix (file-write-date file)))
                (setf (meta-meta-date meta) (ut-to-unix (ut)))
                (write-meta-to-file meta (meta-file-path file)))
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

(defcommand replicate ()
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
                                (merge-pathnames {file (length src) -1} (probe-file dest)))
                             (incf hashed)
                             (incf errors (replicate file it))))
          finally (if (zerop errors)
                      (logmsg 0 "Done, no errors occured")
                      (logmsg 0 "/!\\ Done, but errors occcured /!\\"))
                  (logmsg 0 hashed " file(s) copied")
                  (logmsg 0 unhashed " unhashed file(s) not copied"))))
