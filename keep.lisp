(require 'ironclad)
(require 'anaphora)
(require 'clutch)
(require 'cl-store)

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

(defun compute-hashes (file &optional (chunk-size 4000))
    (with-open-file (f file :element-type '(unsigned-byte 8))
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
    (setf (metadata-metadata-hash it) (compute-metadata-hash it))
    it))

(defun write-metadata-to-file (metadata file)
  (cl-store:store metadata file))

(defun read-metadata-from-file (file)
  (cl-store:restore file))

(defun metadata-error (metadata)
  (/= (metadata-metadata-hash metadata) (compute-metadata-hash metadata)))

(defun file-errors (file metadata)
  (let ((errors)
	(n-chunks (length file-hashes))
	(file-hashes (compute-hashes file (metadata-chunk-size metadata))))
    (loop for i from 0 below n-chunks
	  do (unless (= {file-hashes i} {(metadata-hashes metadata) i})
		(push (str "Chunk " (+ i 1) "/" n-chunks " is invalid") errors)))
    errors))

(defun repair (file metadata &rest copies)
  )
