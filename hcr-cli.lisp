;TODO: Handle C-c gracefuly
;TODO: Handle unforeseen errors gracefuly
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

(defpackage :hcr-cli
   (:use     #:cl #:clutch #:hcr #:unix-options #:named-readtables)
   (:export  #:main))

(in-package :hcr-cli)
(declaim (optimize debug))
(in-readtable clutch)

(define-condition meta-condition  (error) ())
(define-condition meta-open-error (meta-condition) ())
(define-condition meta-outdated   (meta-condition) ())
(define-condition meta-corrupted  (meta-condition) ())
(defvar *commands* (mkhash))
(defstruct cmd opts doc fn)
(defvar *output* *standard-output*)
(declaim (special *output*))

(defmacro defcmd (name (&rest opts) doc &rest body)
  `(setf (gethash ,(lc name) *commands*)
         (make-cmd
           :opts (list ,@opts)
           :doc ,doc
           :fn (lambda (opts free-args) ,@body))))

(defun main (&rest args)
  (let* ((from-shell (not args))
         (*output* (if from-shell *standard-output* (make-string-output-stream)))
         (exit-code
           (catch 'exit 
             (setf args (if args
                            (cons "hcr" args)
                            (argv)))
             (unless {args 1}
               (exit-with-help))
             (let* ((cmd-name (lc {args 1}))
                    (cmd {*commands* cmd-name})
                    (cmd-opts (when cmd (group [= (length _) 1] (cmd-opts cmd)))))
               (unless cmd
                 (logmsg 0 "Unknown command : " cmd-name)
                 (exit-with-help))
               (m-v-b (args opts free-args)
                      (getopt {args 2 -1}
                              (str {cmd-opts t} "v")
                              {cmd-opts nil})
                  (setf *log-level* (count "v" opts :test #'string=))
                  (when (in opts "help")
                    (exit-with-help cmd-name))
                  ;TODO: exit with help if invalid opts
                  (funcall (cmd-fn cmd) opts free-args))))))
      (if from-shell
          (exit exit-code)
          (values (get-output-stream-string *output*) exit-code))))

(defun exit-with-help (&optional cmd-name)
  (if cmd-name
      (logmsg 0 (cmd-doc {*commands* cmd-name}))
      (logmsg 0 "Usage : hcr <command> <option> <files>

Available commands :
   hash <path>+
   check <path>+
   repair <file> <mirror>+

Use hcr <command> --help for detailed help on a command"))
  (throw 'exit 9))

(defun meta-file-path (file)
  (str file ".hmd"))

(defun opt-param (opts name)
  (awhen (position name opts :test 'string=)
    {opts (+ it 1)}))

(defun is-meta-file (path)
  (and (probe-file path)
       (~ "/\\.hmd$/" path)
       (string= "hmd" (gulp path :limit 3))))

(defun get-meta (file &key hmd ignore-date ignore-checksum explicit-errors)
  (let ((meta-path (or hmd (meta-file-path file))))
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
  (merge-pathnames (~s "/^\\///" {src-path (length src-dir) -1})
                   (probe-dir mirror-dir)))

(defun repair-single-file (target mirrors &key target-dir hmd ignore-date)
  (let ((valid-meta))
    (if hmd
      (ignore-errors (setf valid-meta (get-meta target :hmd hmd :ignore-date ignore-date :explicit-errors t)))
      (loop for copy in (cons target mirrors)
            do (ignore-errors
                 (awith (get-meta copy :ignore-date t)
                   (if (and (meta-outdated it target)
                            (not ignore-date))
                     (logmsg 1 "Write date in metadata does not match file date: " copy)
                     (setf valid-meta it))))))
    (if valid-meta
        (progn
          (handler-bind ((file-error (lambda (c) (logmsg 0 "Error accessing file: " (file-error-pathname c))
                                            (return-from repair-single-file 2)
                                            (error 'meta-open-error))))
            (apply #'repair-file
                  (append (list target valid-meta mirrors))))
          (logmsg 0 "File repaired: " target)
          (return-from repair-single-file 0))
        (progn
          (logmsg 0 "/!\\ can't repair " target ": no valid and up-to-date metadata found")
          (return-from repair-single-file 1)))))

(defun exit-unless-paths-exist (&rest paths)
  (loop for path in (flatten paths)
        when (not (probe-file path))
        do (logmsg 0 "File not found: " path)
           (throw 'exit 99)))

(defun meta-outdated (meta file)
  (/= (file-write-date file)
      (meta-file-date meta)))

(defun check-single-file (file &key hmd ignore-date)
  (handler-bind ((meta-open-error (lambda (c) (logmsg 0 "Can't open meta file for " file)   (return-from check-single-file 1)))
                 (meta-corrupted  (lambda (c) (logmsg 0 "Meta file is corrupted for " file) (return-from check-single-file 3)))
                 (file-error      (lambda (c) (logmsg 0 "Error accessing file " (file-error-pathname c)) (return-from check-single-file 2))))
    (let* ((meta (get-meta file :hmd hmd :ignore-date t))
           (errors (file-errors file meta)))
      (cond (errors
              (if (and (not ignore-date) (meta-outdated meta file))
                (progn (logmsg 0 "/!\\ Write date in metadata does not match file date:" file)
                       2)
                (progn (logmsg 0 "/!\\ File " file " has errors in " (meta-chunk-size meta) "B chunks " (join "," (mapcar 'str errors)))
                       4)))
            (t
              (when (and (not ignore-date) (meta-outdated meta file))
                (logmsg 1 "Write date for file was incorrect in metadata. Fixed: " file)
                (setf (meta-file-date meta) (ut-to-unix (file-write-date file)))
                (setf (meta-meta-date meta) (ut-to-unix (ut)))
                (write-meta-to-file meta (meta-file-path file)))
              (logmsg 0 "File " file " looks good")
              0)))))

(defun list-hashed-files (&rest paths)
  (remove-if-not [probe-file (meta-file-path _)]
                 (flatten (mapcar [ls _ :recursive t :files-only t] (flatten paths)))))

(defcmd repair ("hmd=" "ignore-date")
  "hcr repair [options] <file> <copy>*
   
  repairs <file> based on data from copies (<copy>)

  options:
    --hmd=<file.hmd>: use a specific metadata file
    --ignore-date: ignore the file write date in the hmd file (use only if you know what you are doing)"
  (unless free-args
    (exit-with-help "repair"))
  (d-b (target &rest copies)
       free-args
    (exit-unless-paths-exist target copies)
    (awith (if (probe-dir target)
               (mapcar (lambda (f) (repair-single-file f
                                                  (mapcar [mirror-path target f _] copies)
                                                  :target-dir target))
                       (if (in opts "hmd")
                           (ls target :recursive t :files-only t)
                           (list-hashed-files target)))
               (list (repair-single-file target copies :hmd (opt-param opts "hmd")
                                                       :ignore-date (in opts "ignore-date"))))
        (logmsg 0 (count 0 it) " file(s) repaired")
        (logmsg 0 (length (remove-if #'zerop it)) " file(s) not repaired")
        (reduce #'+ it))))
    
(defcmd check ("hmd=" "ignore-date")
  "TODO: doc for check"
  (unless free-args
    (exit-with-help "check"))
  (exit-unless-paths-exist free-args)
  (awith (mapcar [check-single-file _ :hmd (opt-param opts "hmd") :ignore-date (in opts "ignore-date")]
                 (apply (if (in opts "hmd")
                            [ls _ :recursive t :files-only t]
                            #'list-hashed-files)
                        free-args))
      (logmsg 0 (count 0 it) " file(s) OK")
      (logmsg 0 (length (remove-if #'zerop it)) " file(s) with errors")
      (reduce #'+ it)))

(defcmd hash ("hmd=")
  "Computes metadata for the files passed in arguments."
  (unless free-args
    (exit-with-help "hash"))
  (exit-unless-paths-exist free-args)
  (when (and (in opts "hmd")
             (> (length free-args) 1))
    (logmsg 0 "Can't hash multiple files into a single hmd."))
  (reduce #'+
    (mapcar (lambda (file) 
               (block hash-file
                 (when (probe-dir file)
                    (logmsg 0 file " is a directory. Skipping.")
                    (return-from hash-file 3))
                 (logmsg 1 "Hashing file " file)
                 (when (is-meta-file file)
                    (logmsg 0 file " is a meta file. Skipping.")
                    (return-from hash-file 3))
                 (let ((meta-path (meta-file-path file)))
                   (handler-bind ((meta-condition (lambda (c) (handler-bind ((file-error (lambda (c) (logmsg 0 "Error accessing " (file-error-pathname c))
                                                                                           (return-from hash-file 2))))
                                                             (write-meta-to-file (compute-meta file) meta-path))
                                                         (case (type-of c)
                                                            ('meta-open-error (logmsg 0 "Hash for " file " written to " meta-path))
                                                            ('meta-outdated   (logmsg 0 "Updated hash for " file " written to " meta-path))
                                                            ('meta-corrupted  (logmsg 0 "Updated hash /!\\ original corrupted /!\\ for " file " written to " meta-path))
                                                            ('file-error      (logmsg 0 "Error accessing " (file-error-pathname c))
                                                                              (return-from hash-file 1))
                                                            (otherwise        (logmsg 0 "Error hashing " file ":" )
                                                                              (return-from hash-file 1)))
                                                         (return-from hash-file 0))))
                     (get-meta file)
                     (logmsg 0 "Found up-to-date hash for " file " in " meta-path)))
                  0))
            free-args)))