(defpackage :keep
    (:use     #:cl #:clutch #:cl-store)
    (:export  #:compute-meta #:meta-error #:file-errors #:repair-file
      	      #:read-meta-from-file #:write-meta-to-file #:logmsg))

(in-package :keep)

(defvar *log-level* 1)

(defstruct-and-export meta 
  meta-date
  file-date
  file-size
  chunk-size
  hash-tree
  meta-hash)

(defun logmsg (level &rest msg)
  (when (<= level *log-level*)
    (princ (str (x "*" level) msg #\Newline))))

(defun digest-seq (seq &optional end)
  (ironclad:byte-array-to-hex-string (ironclad:digest-sequence :sha256 seq :end end)))

(defmacro with-open-binfile (params &body body)
   `(with-open-file (,@params :element-type '(unsigned-byte 8)) ,@body))

(defun hash-hashes (hashes)
  (loop for tail on hashes
        by [nthcdr 10 _]
        collect (sha256 (join "" (trim tail 10)))))

(defun compute-hash-tree (file &optional chunk-size)
  (loop with hashes = (list (compute-hashes file chunk-size))
        while (> (length {hashes -1}) 1)
        do (pushend (hash-hashes {hashes -1}) hashes)
        finally (return hashes)))

(defun compute-hashes (file &optional (chunk-size 4096))
    (mapcar [car (split " " _)]
            (remove ""
              (split (str #\Newline)
                     (sh (str "split -b" chunk-size " --filter=sha256sum '" file "'")))
              :test 'string=))
  ;(with-open-binfile (f file)
  ;  (loop with seq = (make-array chunk-size :element-type '(unsigned-byte 8))
  ;        for pos = (read-sequence seq f)
  ;        until (= pos 0)
  ;        collect (digest-seq seq pos)))
)

(defun compute-meta-hash (meta)
   (sha256 (str (meta-file-date meta) "#" 
                (meta-file-size meta) "#" 
                (meta-meta-date meta) "#" 
                (meta-chunk-size meta) "#" 
                {(meta-hash-tree meta) -1})))

(defun compute-meta (file &optional (chunk-size 4096))
  (awith
    (make-meta
        :meta-date (ut)
       	:file-date (file-write-date file)
       	:file-size (filesize file)
       	:chunk-size chunk-size
       	:hash-tree (compute-hash-tree file chunk-size))
    (setf (meta-meta-hash it)
          (compute-meta-hash it))
    it))

(defun write-meta-to-file (meta file)
  (ungulp file
          (str (meta-meta-date meta)
               #\Newline
               (meta-file-date meta)
               #\Newline
               (meta-file-size meta)
               #\Newline
               (meta-chunk-size meta)
               #\Newline
              (join #\Newline (mapcar [join #\; _] (meta-hash-tree meta)))
               #\Newline
               (meta-meta-hash meta))
          :if-exists :overwrite))

(defun read-meta-from-file (file)
  (let ((data (remove ""
                      (split (str #\Newline) (gulp file))
                      :test 'string=)))
    (make-meta
      :meta-date (read-from-string {data 0})
      :file-date (read-from-string {data 1})
      :file-size (read-from-string {data 2})
      :chunk-size (read-from-string {data 3})
      :hash-tree (mapcar [split (str #\;) _] {data 4 -2})
      :meta-hash {data -1})))

(defun meta-error (meta)
  (string/= (meta-meta-hash meta)
            (compute-meta-hash meta)))

(defun file-errors (file meta)
  (let ((file-hashes (compute-hashes file (meta-chunk-size meta)))
        (correct-hashes (car (meta-hash-tree meta))))
    (loop for i from 0
                below (length correct-hashes)
	        when (or (> i (- (length file-hashes) 1))
                   (string/= {file-hashes i}
                             {correct-hashes i}))
          collect i)))

(defun write-chunk-from-copies (dest-handle chunk-index meta copies)
  (logmsg 1 "Trying to repair chunk " chunk-index)
  (file-position dest-handle (* (meta-chunk-size meta)
                      chunk-index))
  (let ((seq (make-array (meta-chunk-size meta)
                         :element-type '(unsigned-byte 8))))
    (loop for copy in copies
          do (logmsg 1 "Trying copy " copy)
             (with-open-binfile (c copy)
                  (file-position c (* (meta-chunk-size meta)
                                      chunk-index))
                  (let ((pos (read-sequence seq c)))
                      (if (string= {{(meta-hash-tree meta) 0} chunk-index}
                                   (digest-seq seq pos))
                          (progn
                              (write-sequence seq dest-handle :end pos)
                              (logmsg 1 "Chunk repair successful")
                              (loop-finish))
                          (logmsg 1 "Copy chunk is also broken")))))))

(defun repair-file (file meta copies)
  (let ((errors (file-errors file meta)))
    (logmsg 1 "Errors in " file " : chunks " (join ", " (mapcar #'str errors)))
    (with-open-binfile (f file :if-exists :overwrite
                               :direction :output
                               :if-does-not-exist :error)
        (loop for error in errors
              do (write-chunk-from-copies f error meta copies)))
    (when (/= (file-length file)
              (meta-file-size meta))
      (file-truncate file (meta-file-size meta)))))

(defun create-new-file (new-filename meta &rest copies)
  (with-open-binfile (f new-filename :direction :output)
    (loop for chunk-index from 0
          do (write-chunk-from-copies f chunk-index meta copies))))

(defun file-truncate (file length) ;TODO: use cffi + make work on windows -> see truncate in osicat ?
  (sh (str "truncate -s" length " '" file "'")))
