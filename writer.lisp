(in-package :jsown)

(declaim (optimize (speed 3) (debug 3) (safety 0)))

;;;;;;;;;;;;;;;;;;
;; generic writing
(defgeneric to-json (object)
  (:documentation "Writes the given object to json in a generic way."))

(defmethod to-json ((string string))
  (with-output-to-string (stream)
    (flet ((write-characters (string)
	     (loop for c across string
		do (write-char c stream))))
      (write-char #\" stream)
      (loop for char across string
	 do (case char
	      (#\newline (write-characters "\\n"))
	      (#\return (write-characters "\\r"))
	      (#\tab (write-characters "\\t"))
	      (#\" (write-characters "\\\""))
	      (#\\ (write-characters "\\\\"))
	      (t (write-char char stream))))
      (write-char #\" stream))))

(defmethod to-json ((number number))
  (with-output-to-string (stream)
    (write number :stream stream :pretty nil)))
(defmethod to-json ((ratio ratio))
  (to-json (coerce ratio *read-default-float-format*)))

(defmethod to-json ((list list))
  (let ((*print-pretty* nil)) ;; *pretty-print* makes printing very slow, internal json objects needn't have this
    (if (eq (car list) :obj)
	(object-to-json (cdr list))
	(list-to-json list))))

(defmethod to-json ((true (eql t)))
  "true")
(defmethod to-json ((true (eql :t)))
  "true")
(defmethod to-json ((true (eql :true)))
  "true")
(defmethod to-json ((false (eql :false)))
  "false")
(defmethod to-json ((false (eql :f)))
  "false")
(defmethod to-json ((false (eql :null)))
  "null")
(defmethod to-json ((false (eql :n)))
  "null")

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
    (null (format output "[]"))
    (t (if (list-is-object-p object)
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
	   (progn ; we know the object isn't nil
	     (format output "[")
	     (write-object-to-stream (first object) output)
	     (loop for item in (rest object)
		do (progn
		     (format output ",")
		     (write-object-to-stream item output)))
	     (format output "]"))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; translating lispy content to json

(defun as-js-null (value)
  "returns <value> with nil being javascript's null (in jsown format)."
  (if value value :null))

(defun as-js-bool (value)
  "returns <value> as a boolean value (in jsown format)"
  (if value :true :false))
