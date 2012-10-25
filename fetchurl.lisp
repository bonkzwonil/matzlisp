;(defun open-tcp-socket (host port)
;  (let (
;	(s (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp))
;	(addr (sb-bsd-sockets:host-ent-address (sb-bsd-sockets:get-host-by-name host)))
;	)
;    (sb-bsd-sockets:socket-connect s addr port)
;    s))


;(defun close-socket (s)
;  (sb-bsd-sockets:socket-close s))
(in-package :matzlisp)

(defun rem-double-whitespaces (text)
  (cl-ppcre:regex-replace-all "\ +" text " "))

(defun strip-entities (text)
  (cl-ppcre:regex-replace-all "\&nbsp;" text ""))

(defun strip-tag (tagname text)
  "Strips <tagname>blabla</tagname>"
  (cl-ppcre:regex-replace-all (format nil "(?s)<~a[^>]*>.*</~a>" tagname tagname) text ""))

(defun strip-tags (text)
  (rem-double-whitespaces (cl-ppcre:regex-replace-all "(?m)<[^>]+>" (string-trim '((code-char 0)) text) " ")))

(defun prettify (text)
  (cl-ppcre:regex-replace-all "<p" (cl-ppcre:regex-replace-all "<br>" text (format nil "~%")) (format nil "~%<p")))  

(defun decodeurl (url)
  (let* ((toks (split-sequence:split-sequence #\/ url))
	(pr (+ (length (first toks)) (length (third toks)) 2))
	(suf (if (= (length (subseq url pr)) 0) "/" (subseq url pr)))
	)
    (if (not (string= (first toks) "http:"))
	nil
	(list :host (third toks) :query suf))))




(defun read-string (stream)
  (let ((ret "")
	(afterheader nil))
    (loop
     (let ((c (read-line stream nil 'eof)))
       ;(princ afterheader)
       (if (eq c 'eof) (return ret))
       (if (string= c (string #\Return)) (setq afterheader T))
       (if afterheader
	   (setq ret (concatenate 'string ret (string c) (string #\newline))))
       ))))


(defun net-newline ()
  (concatenate 'string (string #\Return) (string #\Newline))
)

(defvar *fetch-result* NIL)

(defun fetch-url (url &optional encoding (timeout 10))
  (let ((result nil))
    #+sb-thread (let ((thread (sb-thread:make-thread #'(lambda ()
							 (setf result
							       (fetch-url-blocking url encoding ))))))
		  (format t "started thread~%")
		  (loop for sl from 1 to timeout do
		       (sleep (/ timeout 10))
		       (format t "sleep")
		       (when (not (sb-thread:thread-alive-p thread))
			 (return-from fetch-url result)))
		  (when (sb-thread:thread-alive-p thread)
		    (format t "killing thread~%")
		    (sb-thread:terminate-thread thread)
		    "Abfrage dauerte zu lang... geKILLT!"))
    #-sb-thread (fetch-url-blocking url encoding)))

#+sbcl(defun fetch-url-blocking (url &optional encoding)
  (handler-bind #+sbcl((sb-int:stream-decoding-error #'(lambda (e) (invoke-restart 'sb-int:attempt-resync)))) 
		#-sbcl()
    
    (let* ((host (getf (decodeurl url) :host))
	 (query (getf (decodeurl url) :query))
	 (stream (if (null encoding) (open-tcp-stream host 80) (open-tcp-stream host 80 encoding)))
	 (ret nil)
	 )
    (format stream "GET ~a HTTP/1.1~aHost: ~a~aConnection: close~aUser-Agent: Lisp/~a (~a;)~aAccept: *~a~a" 
	    query (net-newline) 
	    host (net-newline) 
	    (net-newline) 
	    (net-newline)
	    (lisp-implementation-version)
	    (lisp-implementation-type)
	    (net-newline) (net-newline))
    (force-output stream)
    (setq ret (read-string stream))
    (close stream)
    ;(close-socket s)
    ret
    )))

  
    
    
    
  
    
    
    
