; Matzes Common Lisp extensions
; $Id: matzlisp.lisp,v 1.10 2007/10/29 21:20:06 matze Exp $

(in-package :cl)

(defpackage :matzlisp
  (:use :cl
	:cl-ppcre
	:flexi-streams)
  (:export 
           :split-list
           :open-tcp-stream
	   :send
	   :logg
	   :close-socket
	   :open-telnet-connection
	   :run-telnet
	   :telnet-send
	   :create-string-buffer
	   :failsafe-read-line))

	      

(in-package :matzlisp) 

(defconstant +VERSION+ "$Revision: 1.11 $")



(defun split-list (lst max &optional (res nil) (pos 0))
  "Splits a list in segments of max size"
  (if (>= max (length lst))
    (reverse (cons lst res))
    (split-list (subseq lst max) 
	     max 
	     (cons (subseq lst 0 max) res) 
	     (+ max pos))))



;; FIXME: write CLISP code

; Network

; returns a tcp stream
(defun open-tcp-stream (host port &optional (encoding :utf-8))
      #+clozure
      (ccl:make-socket :remote-host host :remote-port port)
      #+lispworks
      (comm:open-tcp-stream host port :direction :io)
      #+sbcl
      (let ((s
	(make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp)
	      ))
	(sb-bsd-sockets:socket-connect s 
				       (sb-bsd-sockets:host-ent-address 
					(sb-bsd-sockets:get-host-by-name host))
				       port)
	(if (not (null encoding))
	    (sb-bsd-sockets:socket-make-stream s :input t :output t :element-type 'character :auto-close t :external-format encoding)
	    (sb-bsd-sockets:socket-make-stream s :input t :output t :element-type 'character :auto-close t )
	    )
	)
      #+clisp
      (flexi-streams:make-flexi-stream 
       (socket:socket-connect port host :element-type '(unsigned-byte 8) :timeout 10)
       :external-format encoding)
      #+allegro
      (socket:make-socket :remote-host host :remote-port port)
      )
;;FIX_ME- utf

(defun open-tcp-server-socket (port &optional (address #(127 0 0 1)) (backlog 10))
  #+sbcl
  (let ((s (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp)))
    (sb-bsd-sockets:socket-bind s address port)
    (sb-bsd-sockets:socket-listen s backlog)
    s)
  #-sbcl
  (error "Only SBCL supported at the moment"))
	
(defun close-socket (socket)
  #+sbcl
  (sb-bsd-sockets:socket-close socket)
  #+clozure
  (close socket)
  #-sbcl
  (error "Only SBCL supported at the moment"))

(defun accept-tcp-stream (socket)
  #+sbcl
  (let* ((s (sb-bsd-sockets:socket-accept socket))
	 (str (sb-bsd-sockets:socket-make-stream s :input t :output t :element-type 'character :auto-close t)))
    (values 
     str
     s))
  #-sbcl
  (error "Only SBCL supported at the moment"))
    


(defun net-newline ()
  (concatenate 'string (string #\Return) (string #\Newline))
)

(defun send (stream line)
  (format stream "~a~a" line (net-newline))
  (force-output stream)
)

(defun failsafe-read-line (stream &optional (eof-symbol 'eof))
  (handler-bind (
		 #+sbcl
		 (sb-int:stream-decoding-error  ;; Auto restarts at decoding errors (utf-8)
		  #'(lambda (e)
		      (invoke-restart 'sb-int:attempt-resync)))
		 #+sbcl
		 (SB-INT:SIMPLE-STREAM-ERROR ;; e.g. Connection reset by Peer
		  #'(lambda (e)
		      (return-from failsafe-read-line eof-symbol)))
;;		 #+clisp(flexi-streams::external-format-encoding-error 
;;		  #'(lambda (e)
;;		      (invoke-restart 'flexi-streams::use-value #\?)))
		 (simple-error
		  #'(lambda (e)
		      (format T "Simple ERROR in readline (returning empty string): ~a~%" e)
		      (return-from failsafe-read-line "")))
		 (error ;;Unexpected Error
		  #'(lambda (e)
		      (format T "ERROR in readline: ~a~%" e)
		      (return-from failsafe-read-line eof-symbol)))
		 )
		(read-line stream nil eof-symbol)))


;; String Buffer Closure
;; XXX: Would be more performat with a vector push instead of concatenate
(defmacro create-string-buffer (print-symb get-symb)
  "creates a stringbuffer with a printer and a reader method with the specified names"
  `(let ((str "")
	)
       (defun ,print-symb (line &optional (retnil nil))
	 (setq str (concatenate 'string str line))
	 (if retnil nil str))
       (defun ,get-symb ()
	 str)
       ))

;; Loggin
(setq *logfile* nil)
(setq *logstream* nil)

(defun set-logfile (filename)
  (setq *logfile* filename)
  (setq *logstream*
        (open filename :direction :output))
  )

(defun logg (line)
  (if *logstream*
      (format *logstream* "~a~%" line)
    (format t "~a~%" line)))

(defun safe-parseint (str)
  (handler-case (parse-integer str) 
    #+sbcl (sb-int:simple-parse-error (e) 0)
    #-sbcl (error (e) 0)))



(defun iso-date (&optional (time (get-universal-time)))
  "Return iso format date string given universal time"
  (multiple-value-bind (sec min hour day month year)
      (decode-universal-time time)
    (declare (ignore sec))
    (format nil "~d-~2,'0d-~2,'0d ~2,'0d:~2,'0d"
            year month day hour min))) 


(defun get-response (command)
  "Return output from command as list of strings"
  #+clisp (with-open-stream (stream (ext:make-pipe-input-stream command))
			    (loop for line = (read-line stream nil)
				  while line
				  collect line))
  #-clisp (error "Only clisp implemented"))

