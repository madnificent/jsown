(in-package :jsown)

(declaim (optimize (speed 3) (debug 3) (safety 0)))

;;;;;;;;;;;;;;;;;;
;; generic writing
(defgeneric to-json (object)
  (:documentation "Writes the given object to json in a generic way."))

(defmethod to-json ((string string))
  (with-output-to-string (stream)
    (write string :stream stream :pretty nil)))
(defmethod to-json ((number number))
  (with-output-to-string (stream)
    (write number :stream stream :pretty nil)))
(defmethod to-json ((ratio ratio))
  (to-json (coerce ratio 'float)))

(defmethod to-json ((list list))
  (let ((*print-pretty* nil)) ;; *pretty-print* makes printing very slow, internal json objects needn't have this
    (if (eq (car list) :obj)
	(object-to-json (cdr list))
	(list-to-json list))))

(defun object-to-json (list)
  (format nil "{~{~{~A:~A~}~^,~}}"
	  (loop for item in list collect
	       (list (to-json (car item))
		     (to-json (cdr item))))))

(defun list-to-json (list)
  (format nil "[~{~A~^,~}]"
	  (mapcar #'to-json list)))

;;;;;;;;;;;;;;;;;
;; speedy writing
(defun list-is-object-p (list)
  (declare (type (or cons nil) list))
  (and (consp list)
       (eq (car list) :obj)))

(defun to-json* (object)
  "Converts an object in internal jsown representation to a string containing the json representation"
  (let ((*print-pretty* nil))
   (with-output-to-string (output)
     (write-object-to-stream object output))))

(defun write-number* (object output)
  (declare (type number object)
	   (type stream output))
  (write object :stream output))

(defun write-string* (object output)
  (declare (type string object)
	   (type stream output))
  (write object :stream output :pretty nil))

(declaim (inline write-number* write-string*))

(defun write-object-to-stream (object output)
  (declare (type stream output))
  (typecase object
    (ratio (write (coerce object 'float) :stream output))
    (number (write-number* object output))
    (string (write-string* object output))
    (T (if (list-is-object-p object)
	   (if (null (cdr object))
	       "{}"
	       (progn
		 (format output "{")
		 (write-string* (caadr object) output)
		 (format output ":")
		 (write-object-to-stream (cdadr object) output)
		 (loop
		    for curr-obj in (cddr object)
		    do (progn
			 (let ((k (car curr-obj))
			       (v (cdr curr-obj)))
			   (declare (type string k))
			   (format output ",")
			   (write-string* k output)
			   (format output ":")
			   (write-object-to-stream v output))))
		 (format output "}")))
	   (progn
	     (if (null (cdr object))
		 (format output "[]")
		 (progn
		   (format output "[")
		   (write-object-to-stream (first object) output)
		   (loop for item in (rest object)
		      do (progn
			   (format output ",")
			   (write-object-to-stream item output)))
		   (format output "]"))))))))