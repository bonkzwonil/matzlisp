; telnet abstraction
; $Id: telnet-connection.lisp,v 1.7 2007/10/06 20:23:48 matze Exp $

;; Struct version
;; could be rewritten in CLOS

(in-package :matzlisp) 

(defparameter *debug* nil)

(defstruct telnet-connection stream handlerfun)

(defun open-telnet-connection (host port &optional handler initialsendlines)
  (let ((tcpstream (matzlisp::open-tcp-stream host port)))
    (let ((conn (make-telnet-connection :stream tcpstream :handlerfun handler)))
      (loop for line in initialsendlines do
	   (telnet-send conn line))
      conn)))

(defun telnet-send (conn line)
  (if *debug* (format t "TELNET: ---> ~a~%" line))
  (matzlisp::send (telnet-connection-stream conn) line))

(defun read-telnet (conn)
  (failsafe-read-line (telnet-connection-stream conn)))
    
(defun handle-telnet-line (conn)
  (let ((line (read-telnet conn))
	(handler (telnet-connection-handlerfun conn)))
    (if *debug* (format t "TELNET: <--- ~a~%" line))
    (if (not (eq 'eof line))
	(if (not (null handler))
	    (funcall handler line))
      line)))

(defun handle-telnet-line-non-blocking (conn)
  "useful for polling in single threaded implementations"
  (when (listen (telnet-connection-stream conn))
    (handle-telnet-line conn)))

(defun run-telnet (conn)
  (loop while (not (eq
		    'eof
		    (handle-telnet-line conn)))))
;;example
  
;(run-telnet (open-telnet-connection "www.google.de" 80 #'princ '("GET / HTTP/1.0" "")))
