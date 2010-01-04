(defpackage :hashson
  (:use :common-lisp))

(in-package :hashson)

;(declaim (optimize (speed 0) (safety 3) (debug 3)))
(declaim (optimize (speed 3) (safety 0) (debug 3)))

;;;;;;;;;;;;;;;;;
;;;; introduction
;; this library doesn't take the escaping of spaces etc into account

;;;;;;;;;
;;;; code

(defconstant +space-characters+ '(#\Space #\Newline #\Tab #\Linefeed)
  "List of characters which may denote a space in the JSON format (these have not been verified")
(defparameter *key-string* (make-string 100)
  "A string which is used to read the characters of the key in.  This must be big enough to store the key, so no extra allocation is necessary")
(declaim (type string *key-string*))

(defun skip-to (stream last-char)
  "Skips characters until <char> has been found.  <char> is the last char which is skipped"
  (declare (type stream stream)
	   (type character last-char))
  (loop for char = (read-char stream)
     until (char= char last-char))
  (values))

(defun skip-spaces (stream)
  "Skips spaces, tabs and newlines until a non-space character has been found"
  (let ((char))
    (progn (loop while (progn (setf char (read-char stream))
			      (find char +space-characters+))))
    (unread-char char stream)))

(let ((nr 0)
      (char #\ ))
  (declare (type fixnum nr)
	   (type character char))
  (defun subseq-until (stream &rest chars)
    "Returns a subsequence of stream, reading everything before a character belonging to chars is found.  The character which was found in chars is unread from the stream"
    (declare (type stream stream))
    (setf nr 0)
    (loop for char = (read-char stream)
       until (find char chars :test #'char=)
       do (progn (incf nr)
		 (setf (elt *key-string* nr) char)))
    (unread-char char stream)
    (subseq *key-string* 0 nr)))

(defparameter *hash-table* (make-hash-table :test 'equal))

(defun read-hash (stream &optional (hash-table (make-hash-table :test 'equal)))
  "reads a key-value pair into the hash"
  (loop do (setf (gethash (read-key stream) hash-table) (read-value stream))
     until (progn (skip-spaces stream) (string= (read-char stream) #\}))) ; we may read-char here, as the character is a , to be skipped if it is not a }
  hash-table)

(let ((nr 0))
  (declare (type fixnum nr))
  (defun read-key (stream)
    "reads a key from the key-value list"
    (skip-to stream #\")
    (setf nr 0)
    (loop for char = (read-char stream)
       until (char= char #\")
       do (progn (setf (elt *key-string* nr) char)
		 (incf nr)))
    (subseq *key-string* 0 nr)))

(defun read-value (stream)
  "Reads a value from the stream.
 This searches for the first meaningful character, and delegates to the right function for that character"
  (skip-to stream #\:)
  (skip-spaces stream)
  (let ((meaning (read-char stream)))
    (cond ((string= meaning #\")
	   (parse-string stream))
	  ((string= meaning #\{)
	   (read-hash stream))
	  ((string= meaning #\[)
	   (read-array stream))
	  ((char= (peek-char nil stream) #\t)
	   T)
	  ((char= (peek-char nil stream) #\f)
	   nil)
	  ((char= (peek-char nil stream) #\n)
	   nil)
	  (T
	   (read-number stream)))))

(let ((nr 0))
  (declare (type fixnum nr))
  (defun parse-string (stream)
    "Reads a JSON string from the stream (assumes the first \" is missing and NO escaped characters are in there"
    (setf nr 0)
    (loop for char = (read-char stream)
       until (char= char #\")
       do (progn (setf (elt *key-string* nr) char)
		 (incf nr)))
    (subseq *key-string* 0 nr)))

(defun read-array (stream)
  "Reads a JSON array from the stream (assumes the first [ is missing"
  (loop for value = (read-value stream)
     collect value
     until (progn (skip-spaces stream) (string= (read-char stream) #\]))) ; we may read-char here, as the character is a , to be skipped if it is not a ]
  )

(defun read-number (stream)
  (skip-spaces stream)
  (read-from-string (subseq-until stream #\] #\} #\,))) ;; only these characters are allowed to actually end a number

(defun read-json (stream)
  (skip-to stream #\{)
  (read-hash stream))