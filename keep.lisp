(defpackage :keep
    (:use     #:cl #:clutch #:cl-store)
    (:export  #:compute-metadata #:metadata-error #:file-errors #:repair-file
      	      #:read-metadata-from-file #:write-metadata-to-file))

(in-package :keep)

(defstruct metadata 
  metadata-date
  file-date
  file-size
  chunk-size
  hash-tree
  metadata-hash)

(defun logmsg (level &rest msg)
  (print (str "[" level "] " msg)))

(defun digest-seq (seq &optional end)
  (ironclad:byte-array-to-hex-string (ironclad:digest-sequence :sha256 seq :end end)))

(defmacro with-open-binfile (params &body body)
   `(with-open-file (,@params :element-type '(unsigned-byte 8)) ,@body))

(defun hash-hashes (&rest hashes)
  (loop for tail on hashes
        by [nthcdr 10 _]
        collect (sha256 (join "" trim tail 10))))

(defun compute-hashtree (file &optional chunk-size)
  (loop with hashes = (compute-hashes file chunk-size)
        while (> (length {hashes -1}) 1)
        do (pushend (hash-hashes {hashes -1}) hashes))
  hashes)

(defun compute-hashes (file &optional (chunk-size 4000))
  (with-open-binfile (f file)
    (loop with seq = (make-array chunk-size
                                 :element-type '(unsigned-byte 8))
          for pos = (read-sequence seq f)
          until (= pos 0)
          collect (digest-seq seq pos))))

(defun compute-metadata-hash (metadata)
   (sha256 (str (metadata-file-date metadata) "#" 
                (metadata-file-size metadata) "#" 
                (metadata-metadata-date metadata) "#" 
                (metadata-chunk-size metadata) "#" 
                {(metadata-hash-tree metadata) -1})))

(defun compute-metadata (file &optional (chunk-size 4000))
  (awith
    (make-metadata
        :metadata-date (ut)
       	:file-date (file-write-date file)
       	:file-size (filesize file)
       	:chunk-size chunk-size
       	:hash-tree (compute-hash-tree file chunk-size))
    (setf (metadata-metadata-hash it)
    (compute-metadata-hash it))
    it))

(defun write-metadata-to-file (metadata file)
  (cl-store:store metadata file))

(defun read-metadata-from-file (file)
  (cl-store:restore file))

(defun metadata-error (metadata)
  (string= (metadata-metadata-hash metadata)
	   (compute-metadata-hash metadata)))

(defun file-errors (file metadata)
  (let ((file-hashes (car (compute-hashes file (metadata-chunk-size metadata)))))
    (loop for i from 0
                below (length (metadata-hashes metadata))
	        when (string/= {file-hashes i}
		      	             {{(metadata-hashes metadata) 0} i})
          collect i)))

(defun write-chunk-from-copies (dest-handle chunk-index metadata copies)
  (logmsg "DEBUG" "Trying to repair chunk " chunk-index)
  (file-position dest-handle (* (metadata-chunk-size metadata)
                      chunk-index))
  (let ((seq (make-array (metadata-chunk-size metadata)
                         :element-type '(unsigned-byte 8))))
    (loop for copy in copies
          do (logmsg "DEBUG" "Trying copy " copy)
             (with-open-binfile (c copy)
                  (file-position c (* (metadata-chunk-size metadata)
                                      chunk-index))
                  (let ((pos (read-sequence seq c)))
                      (if (string= {(metadata-hashes metadata) chunk-index}
                                    (digest-seq seq pos))
                          (progn
                              (write-sequence seq dest-handle :end pos)
                              (logmsg "DEBUG" "Chunk repair successful")
                              (loop-finish))
                          (logmsg "DEBUG" "Copy chunk is also broken")))))))

(defun repair-file (file metadata copies)
  (let ((errors (file-errors file metadata)))
    (logmsg "DEBUG" "Errors in " file " : chunks " (join ", " (mapcar #'str errors)))
    (with-open-binfile (f file :if-exists :overwrite
                               :direction :output
                               :if-does-not-exist :error)
        (loop for error in errors
              do (write-chunk-from-copies f error metadata copies)))))

(defun create-new-file (new-filename metadata &rest copies)
  (with-open-binfile (f new-filename :direction :output)
    (loop for chunk-index from 0
          do (write-chunk-from-copies f chunk-index metadata copies))))
