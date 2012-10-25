;; A reliable Connector for telnet-connections
;; reconnects automatically etc

(in-package :matzlisp)

(defclass connector ()
  ((current-telnet-connection :reader get-telnet-connection :initform nil)
   (hosts :initform nil :initarg :hosts :accessor hosts
	  :documentation "host port pairs")
   (pos :initform -1)
   (handler-function :initarg :handler :initform nil :accessor handler-function)
   (reconnect-delay :initarg :reconnect-delay :initform 60)
   (running :initform nil :accessor running?)
   (connection-change-handler :initarg :connection-change-handler :initform nil)
   (initlines :initform nil :initarg :initlines)))

(defmethod run ((connector connector))
  (when (not (running? connector))
      (setf (slot-value connector 'running) T)
      (loop while (running? connector) do
	   (handler-case 
	       (let ((hostport (next-host connector)))
		 (when *debug* (format t "Connecting to ~a port ~a" (first hostport) (second hostport)))
		 (setf (slot-value connector 'current-telnet-connection) 
		       (open-telnet-connection (first hostport) (second hostport) (handler-function connector) (slot-value connector 'initlines)))
		 (with-slots (connection-change-handler) connector
		   (when connection-change-handler
		     (funcall connection-change-handler (get-telnet-connection connector)))) 
		 (run-telnet (get-telnet-connection connector)))
	     (condition (e) 
	       (format t "~%******~%~a .. will reconnect~%" e)
	       (format t "~%close old~%")
	       (handler-case 
		   (close 
		    (matzlisp::telnet-connection-stream
		     (slot-value connector 'current-telnet-connection)))
		 (condition (e)
		   (format t "~%cannot close: ~a~%" e)))
			
	       (sleep (slot-value connector 'reconnect-delay)))))))
					       
(defmethod next-host ((connector connector))
  (with-slots (pos hosts) connector
     (incf pos)
     (when (>= pos (length hosts))
       (setf pos 0))
     (nth pos hosts)))

(defmethod terminate ((connector connector))
  (setf (slot-value connector 'running) nil))
       
