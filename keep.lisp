(defpackage :keep
    (:use     #:cl #:ironclad #:anaphora #:clutch #:cl-store))

(in-package :keep)

(defstruct metadata 
  metadata-date
  file-date
  file-size
  chunk-size
  hashes
  metadata-hash)

(defmacro with-open-binfile ((handle path)&body body)
   `(with-open-file (,handle ,path :element-type '(unsigned-byte 8)) ,@body))

(defun compute-hashes (file &optional (chunk-size 4000))
    (with-open-binfile (f file)
	(loop with seq = (make-array chunk-size
		  		     :element-type '(unsigned-byte 8))
	      for pos = (read-sequence seq f)
	      until (= pos 0)
	      collect (byte-array-to-hex-string (digest-sequence :sha256 seq :end pos)))))

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
    (let ((seq (make-array (metadata-chunk-size metadata)
   			   :element-type '(unsigned-byte 8))))
	(with-open-binfile (f file)
	    (loop for error in errors
		  do (loop for copy in copies
			   do (file-position f (* (metadata-chunk-size metadata)
	  				          error))
			      (with-open-binfile (c copy)
				(let ((pos (read-sequence seq c)))
				    (when (string= {(metadata-hashes metadata) error}
						   (byte-array-to-hex-string (digest-sequence :sha256 seq :end pos)))
					(write-sequence seq f)
					(loop-finish))))))))))


;--help
;
;  hash
;  check
;  repair
;  sync
