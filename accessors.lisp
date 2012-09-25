(in-package :jsown)

;;;; This file contains the helpers which make it easier to edit the parsed json objects

(defun keywords (object)
  "Returns a list of all the keywords contained in the object"
  (mapcar #'car (cdr object)))

(defun key-val (object key)
  "Returns the list which represents the key-val pair in the json object"
  (loop for k-v in (cdr object)
     when (string= (car k-v) key)
     do (return-from key-val k-v))
  (error "Key ~A is not available in the given object" key))

(defun val (object key)
  "Returns the value of the given key in object"
  (cdr (key-val object key)))

(defun push-key (object key value)
  "Adds the given key to the object at front"
  (setf (cdr object)
	(cons (cons key value) (cdr object)))
  object)

(defun append-key (object key value)
  "Appends the given key to the object"
  (setf (cdr (last object))
	(list (cons key value)))
  object)

(defun overwrite-val (object key value)
  "Overwrites the given key's value with value.  Errors out if the key didn't exist"
  (setf (cdr (key-val object key)) value)
  object)

(defun (setf val) (value object key)
  "Sets the value of the keyword key of the object object to value.  If the key didn't have any value yet, the keyword is added to the object"
  (handler-case
      (overwrite-val object key value)
    (error ()
      (push-key object key value))))

(defmacro do-json-keys ((key val) object &body body)
  "Iterates over the json key-value pairs"
  (let ((k-v (gensym)))
    `(loop for ,k-v in (rest ,object)
	for ,key = (car ,k-v)
	for ,val = (cdr ,k-v)
	do (progn ,@body))))

(defun empty-object ()
  "Returns an empty object which can be used to build new objects upon"
  (list :obj))

(defmacro extend-js (obj &body specs)
  "fills in a bunch of jsown values for obj.  each spec should contain a list with the first element being the string which represents the key and the second being the form which evaluates to the value to which the key should be set.

it is heavily related to jsown-object, which fills in an empty object.

eg: (jsown-values (empty-object)
      (\"kind\" \"onyx.Groupbox\")
      (\"components\" (list (jsown-object
                               (\"content\" \"Hello World\")
                               (\"tag\" \"h1\"))
                            (jsown-object (\"tag\" \"p\") (\"content\" \"This is jsown talkin!\")))))"
  (let ((obj-gensym (gensym "obj")))
    `(let ((,obj-gensym ,obj))
       ,@(loop for spec in specs
            collect `(setf (val ,obj-gensym ,(first spec))
                           (progn ,@(rest spec))))
       ,obj-gensym)))

(defmacro new-js (&body specs)
  "creates a new empty object and fills it is per jsown-values"
  `(extend-js (empty-object)
     ,@specs))
