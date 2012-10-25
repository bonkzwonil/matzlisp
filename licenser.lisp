; Matzes License Updater

(in-package :matzlisp)


(defparameter *license* nil)

(defun read-commented-block (s)
  (loop for line = (read-line s nil 'eof)
	while (and (not (eq line 'eof))
		   (search ";" line))
	collect line))

(defun read-license (file)
  (with-open-file (s file)
    (read-commented-block s) ;strip first
    (read-commented-block s))) 
	  
	  
(defun check-license (file)
  (car (remove-if #'null 
		  (mapcar #'string= (read-license file) *license*)))) ;;LISP-POWER



(defun inject-license (file)
  (with-open-file (s file)
    ;Save header
    (let ((header (read-commented-block s)))
      ;strip current license
      (read-commented-block s)
      ;slurp in rest
      (let ((rest
	     (loop for line = (read-line s nil 'eof)
		   while (not (eq line 'eof))
		   collect line)))
	(append header '("") *license* '("") rest)))))
	  


(defun overwrite-license (file)
  (with-open-file (s file :direction :output :if-exists :overwrite)
    (mapcar #'(lambda (line) (format s "~a~%" line)) 
	    (inject-license file)))
  'done)

(defun batch-process (files &key dry-run)
  (loop for file in files do
	(if (check-license file)
	    (format t "~a is ok!~%" (namestring file))
	    (if dry-run
		(format t "~a has NO up2date License! Ignoring 'cause of dry-run~%" (namestring file))
		(progn 
		  (overwrite-license file)
		  (format t "~a was updated!~%" (namestring file)))))))
	      
  
(defun read-license-file (file)
  "Slurps in a LICENSE file and adds \;'s  useful to set *license*"
  (mapcar #'(lambda (line) (format nil "; ~a" line))
	  (with-open-file (s file)
	    (loop for line = (read-line s nil 'eof)
		  while (not (eq line 'eof))
		  collect line))))
