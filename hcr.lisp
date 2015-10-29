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

(defpackage :hcr
    (:use     #:cl #:clutch #:cl-store #:named-readtables)
    (:export  #:compute-meta #:meta-error #:file-errors #:repair-file
      	      #:read-meta-from-file #:write-meta-to-file #:logmsg #:*log-level* #:*output*))

(in-package :hcr)
(declaim (optimize debug))
(in-readtable clutch)

(defvar *log-level* 1)
(defvar *output* *standard-output*)

(defstruct-and-export meta 
  meta-date
  file-date
  file-size
  chunk-size
  hash-tree
  version
  meta-hash)

(defun logmsg (level &rest msg)
  (when (<= level *log-level*)
    (princ (str (x "*" level) msg #\Newline) *output*)))

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
  (with-open-binfile (f file)
    (loop with seq = (make-array chunk-size :element-type '(unsigned-byte 8))
          for pos = (read-sequence seq f)
          until (= pos 0)
          collect (digest-seq seq pos))))

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
          (str "hmd1"
               #\Newline
               (ut-to-unix (meta-meta-date meta))
               #\Newline
               (ut-to-unix (meta-file-date meta))
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
      :version (read-from-string {{data 0} 3 -1})
      :meta-date (unix-to-ut (read-from-string {data 1}))
      :file-date (unix-to-ut (read-from-string {data 2}))
      :file-size (read-from-string {data 3})
      :chunk-size (read-from-string {data 4})
      :hash-tree (mapcar [split (str #\;) _] {data 5 -2})
      :meta-hash {data -1})))

(defun meta-error (meta)
  (string/= (meta-meta-hash meta)
            (compute-meta-hash meta)))

(defun file-errors (file meta)
  (let ((file-hashes (compute-hashes file (meta-chunk-size meta)))
        (correct-hashes (car (meta-hash-tree meta))))
    (append (loop for i from 0
                  below (max (length correct-hashes) (length file-hashes))
	                when (or (> i (- (length file-hashes) 1))
                           (> i (- (length correct-hashes) 1))
                           (string/= {file-hashes i}
                                     {correct-hashes i}))
                  collect i))))

(defun write-chunk-from-copies (dest chunk-index meta copies)
  (logmsg 1 "Trying to repair chunk " chunk-index)
  (let ((seq (make-array (meta-chunk-size meta)
                         :element-type '(unsigned-byte 8))))
    (loop for copy in copies
          do (logmsg 1 "Trying copy " copy)
             (let* ((max (when (= chunk-index (- (length {(meta-hash-tree meta) 0}) 1))
                            (awith (rem (meta-file-size meta) (meta-chunk-size meta))
                              (if (zerop it) (meta-chunk-size meta) it))))
                    (pos (with-open-binfile (c copy)
                           (file-position c (* (meta-chunk-size meta)
                                               chunk-index))
                           (read-sequence seq c :end max))))
                 (if (string= {{(meta-hash-tree meta) 0} chunk-index}
                              (digest-seq seq pos))
                    (progn
                      (with-open-binfile (dest-handle dest :if-exists :overwrite
                                                           :direction :output
                                                           :if-does-not-exist :error)
                        (file-position dest-handle (* (meta-chunk-size meta)
                                                      chunk-index))
                        (write-sequence seq dest-handle :end pos))
                      (logmsg 1 "Chunk repair successful")
                      (return-from write-chunk-from-copies 0))
                    (logmsg 1 "Copy chunk is also broken")))))
  1)

(defun fix-file-length (file meta)
  (when
    (with-open-binfile (f file :if-exists :overwrite
                                 :direction :output
                                 :if-does-not-exist :error)
        (> (file-length f) (meta-file-size meta)))
    (logmsg 1 "Fixing file length")
    (file-truncate file (meta-file-size meta))))

(defun repair-file (file meta copies)
  (let ((errors (file-errors file meta)))
    (unless errors (return-from repair-file nil))
    (logmsg 1 "Error(s) in " file " : chunk(s) " (join ", " (mapcar #'str errors)))
    (prog1
      (reduce #'+
              (mapcar (lambda (err) (write-chunk-from-copies file err meta copies))
                      (remove-if [>= _ (length (car (meta-hash-tree meta)))] errors)))
      (fix-file-length file meta)
      (file-set-write-date file (meta-file-date meta)))))

(defun create-new-file (new-filename meta &rest copies)
  (loop for chunk-index from 0 below (length (car (meta-hash-tree meta)))
        do (write-chunk-from-copies new-filename chunk-index meta copies))
  (file-set-write-date file (meta-file-date meta)))

(defun file-truncate (file length) ;TODO: use cffi + make work on windows -> see truncate in osicat ?
  (sh (str "truncate -s" length " '" file "'")))

(defun file-set-write-date (file universal-time)   
  (sh (str "touch -d @" (ut-to-unix universal-time) " '" file "'")))  ;TODO: use cffi + make work on windows -> something in osicat ?
