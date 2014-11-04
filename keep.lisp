(defpackage :keep
    (:use     #:cl #:anaphora #:clutch #:cl-store)
    (:export  #:compute-metadata #:metadata-error #:file-errors #:repair-file
	      #:read-metadata-from-file #:write-metadata-to-file))

(in-package :keep)

(defstruct metadata 
  metadata-date
  file-date
  file-size
  chunk-size
  hashes
  metadata-hash)

(defun logmsg (level &rest msg)
  (print (str "[" level "] " msg)))

(defmacro with-open-binfile (params &body body)
   `(with-open-file (,@params :element-type '(unsigned-byte 8)) ,@body))

(defun compute-hashes (file &optional (chunk-size 4000))
    (with-open-binfile (f file)
	(loop with seq = (make-array chunk-size
		  		     :element-type '(unsigned-byte 8))
	      for pos = (read-sequence seq f)
	      until (= pos 0)
	      collect (ironclad:byte-array-to-hex-string (ironclad:digest-sequence :sha256 seq :end pos)))))

(defun compute-metadata-hash (metadata)
   (clutch:sha256 (join "" (metadata-hashes metadata))))

(defun compute-metadata (file &optional (chunk-size 4000))
  (awith
    (make-metadata
        :metadata-date (ut)
	:file-date (file-write-date file)
	:file-size (filesize file)
	:chunk-size chunk-size
	:hashes (compute-hashes file chunk-size))
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
  (let ((file-hashes (compute-hashes file (metadata-chunk-size metadata))))
    (loop for i from 0
                below (length (metadata-hashes metadata))
	  when (string/= {file-hashes i}
			 {(metadata-hashes metadata) i})
	  collect i)))

(defun repair-file (file metadata &rest copies)
  (let ((errors (file-errors file metadata)))
    (logmsg "DEBUG" "Errors in " file " : chunks " (join ", " (mapcar #'str errors)))
    (let ((seq (make-array (metadata-chunk-size metadata)
   			   :element-type '(unsigned-byte 8))))
	(with-open-binfile (f file :if-exists :overwrite
			           :direction :output
		 	           :if-does-not-exist :error)
	    (loop for error in errors
		  do (logmsg "DEBUG" "Trying to repair chunk " error)
		     (file-position f (* (metadata-chunk-size metadata)
	  		                 error))
		     (loop for copy in copies
			   do (logmsg "DEBUG" "Trying copy " copy)
			      (with-open-binfile (c copy)
				(file-position c (* (metadata-chunk-size metadata)
						    error))
				(let ((pos (read-sequence seq c)))
				    (if (string= {(metadata-hashes metadata) error}
					          (ironclad:byte-array-to-hex-string (ironclad:digest-sequence :sha256 seq :end pos)))
					(progn
					    (write-sequence seq f :end pos)
					    (logmsg "DEBUG" "Chunk repair successful")
					    (loop-finish))
					(logmsg "DEBUG" "Copy chunk is also broken"))))))))))
