(defpackage :keepcl
   (:use     #:cl #:clutch #:keep #:unix-options))

(in-package :keepcl)

(define-condition meta-condition  (error) ())
(define-condition meta-open-error (meta-condition) ())
(define-condition meta-outdated   (meta-condition) ())
(define-condition meta-corrupted  (meta-condition) ())
(defvar *log-level* 0)
(defvar *commands* (mkhash))
(defstruct cmd opts doc fn)

(defmacro defcmd (name (&rest opts) doc &rest body)
  `(setf (gethash ,(lc name) *commands*)
         (make-cmd
           :opts (list ,@opts)
           :doc ,doc
           :fn (lambda (opts free-args) ,@body))))

(defun main ()
  (unless (argv 1)
    (exit-with-help))
  (let* ((cmd-name (lc (argv 1)))
         (cmd {*commands* cmd-name})
         (cmd-opts (when cmd (group [= (length _) 1] (cmd-opts cmd)))))
    (unless cmd
      (logmsg 0 "Unkown command : " cmd-name)
      (exit-with-help))
    (m-v-b (args opts free-args)
           (getopt (argv 2 -1)
                   (str {cmd-opts t})
                   {cmd-opts nil})
       (setf *log-level* (count "v" opts))
       (when (in opts "help")
         (exit-with-help cmd-name))
       ;TODO: exit with help if invalid opts
       (funcall (cmd-fn cmd) opts free-args))))

(defun exit-with-help (&optional cmd-name)
  (if cmd-name
      (logmsg 0 (cmd-doc {*commands* cmd-name}))
      (logmsg 0 "Usage :keep <command> <option> <files>

Available commands :
   hash <path>+
   check <path>+
   repair <file> <mirror>+
   replicate <src>+ <dest>
   ls <path>+

Use keep <command> --help for detailed help on a command"))
  (exit 9))

(defun meta-file-path (file)
  (str file ".kmd"))

(defun opt-param (opts name)
  (awhen (find name opts :test 'string=)
    {opts (+ it 1)}))

(defun get-meta (file &key kmd ignore-date ignore-checksum explicit-errors)
  (let ((meta-path (or kmd (meta-file-path file))))
    (handler-bind ((file-error (lambda (c) (logmsg (if explicit-errors 0 1) "Error opening metadata file: " meta-path)
                                      (error 'meta-open-error))))
      (awith (read-meta-from-file meta-path)
        (when (and (not ignore-date)
                   (meta-outdated it file))
          (logmsg (if explicit-errors 0 1) "Write date in metadata does not match file date: " file)
          (error 'meta-outdated))
        (when (and (not ignore-checksum)
                        (meta-error it))
          (logmsg (if explicit-errors 0 1) "Metadata corrupted: " meta-path)
          (error 'meta-corrupted))
        it))))

(defun mirror-path (src-dir src-path mirror-dir)
  (merge-pathnames {src-path (length src-dir) -1} mirror-dir))

(defun repair-single-file (target mirrors &key target-dir kmd ignore-date)
  (let ((valid-meta))
    (if kmd
      (ignore-errors (setf valid-meta (get-meta target :kmd kmd :ignore-date ignore-date :explicit-errors t)))
      (loop for copy in (cons target mirrors)
            do (ignore-errors
                 (awith (get-meta copy :ignore-date t)
                   (if (and (meta-outdated it target)
                            (not ignore-date))
                     (logmsg 1 "Write date in metadata does not match file date: " copy)
                     (setf valid-meta it))))))
    (if valid-meta
        (progn
          (apply #'repair-file
                (append (list target valid-meta mirrors)))
          (logmsg 0 "File repaired: " target))
       (logmsg 0 "/!\\ can't repair " target ": no valid and up-to-date metadata found"))))

(defun exit-unless-paths-exist (&rest paths)
  (loop for path in (flatten paths)
        when (not (probe-file path))
        do (logmsg 0 "File not found: " path)
           (exit 99)))

(defun meta-outdated (meta file)
  (/= (file-write-date file)
      (meta-file-date meta)))

(defun check-single-file (file &key kmd)
  (handler-bind ((meta-open-error (lambda (c) (logmsg 0 "Can't open meta file for " file)   (return-from check-single-file 1)))
                 (meta-corrupted  (lambda (c) (logmsg 0 "Meta file is corrupted for " file) (return-from check-single-file 3))))
    (let* ((meta (get-meta file :kmd kmd :ignore-date t))
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
  (let ((src-meta)
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

(defcmd repair ("kmd=" "ignore-date")
  "keep repair [options] <file> <copy>+
   
  repairs <file> based on data from copies (<copy>)

  options:
    --kmd=<file.kmd>: use a specific metadata file
    --ignore-date: ignore the file write date in the kmd file (use only if you know what you are doing)"
  (unless free-args
    (exit-with-help "repair"))
  (d-b (target &rest copies)
       free-args
    (exit-unless-paths-exist target copies)
    (if (probe-dir target)
        (mapcar (lambda (f) (repair-single-file f
                                           (mapcar [mirror-path target f _] copies)
                                           :target-dir target))
                (ls target :recursive t :files-only t))
        (repair-single-file target copies :kmd (opt-param opts "kmd")
                                          :ignore-date (in opts "ignore-date")))))
    
(defcmd check ("kmd=")
  "TODO"
  (unless free-args
    (exit-with-help "check"))
  (exit-unless-paths-exist free-args)
  (mapcar [check-single-file _ :kmd (opt-param opts "kmd")]
          (apply #'list-hashed-files free-args)))

(defcmd hash ("kmd=")
  "Computes metadata for the files passed in arguments."
  (unless free-args
    (exit-with-help "hash"))
  (exit-unless-paths-exist free-args)
  (when (and (in opts "kmd")
             (or (> (length free-args) 1)
                 (probe-dir {free-args 0})))
    (logmsg 0 "Can't hash multiple files into a single kmd."))
  (loop for file in (flatten (mapcar [ls _ :recursive t :files-only t] free-args))
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

(defcmd ls ()
  "List hashed files in the specified paths"
  (unless (>= (length free-args) 1)
    (exit-with-help "ls"))
  (exit-unless-paths-exist free-args)
  (mapcar [format nil
                  (list-hashed-files _)]
          free-args))

(defcmd replicate ()
  "Copy hashed srcs to destination. Updates destination if files already exist."
  (unless (>= (length free-args) 2)
    (exit-with-help "replicate"))
  (let* ((srcs {free-args 0 -2})
         (dest {free-args -1})
         (single-file (and (= (length srcs) 1)
                           (not (probe-dir {srcs 0})))))
    (exit-unless-paths-exist srcs)
    (when (and (not single-file)
               (not (probe-dir dest)))
       (logmsg 0 "When copying multiple files, destination must be a directory")
       (exit 1))
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
