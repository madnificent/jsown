(in-package :jsown)

(declaim (optimize (speed 0) (debug 3) (safety 3)))

(defgeneric to-json (object)
  (:documentation "Writes the given object to json"))

(defmethod to-json ((string string))
  (with-output-to-string (stream)
    (write string :stream stream)))
(defmethod to-json ((number number))
  (with-output-to-string (stream)
    (write number :stream stream)))
(defmethod to-json ((ratio ratio))
  (to-json (coerce ratio 'float)))

(defmethod to-json ((list list))
  (format nil "{~{~{\"~A\":~A~}~^,~}}"
	  (loop for item in list collect
	       (list (first item)
		     (if (listp (cdr item))
			 (format nil "[~{~A~^,~}]" (mapcar #'to-json (cdr item)))
			 (to-json (cdr item)))))))