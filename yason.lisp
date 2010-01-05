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

(defstruct buffer
  "A string-buffer which is used to operate on the strings
 The use of a string-buffer allows us to read the data in bulk, and to operate on it, by using simple index manipulations.
 Reading the string up front removes the hassle of having a fixed-size maximal input"
  (string ""
	  :type simple-string) ; This contains the content of the buffer
  (index 0 :type fixnum) ; This is the current index of the buffer
  (mark 0 :type fixnum)) ; This contains a single number to indicate the start of a region.  The user must ensure that this does not get overwritten himself

(defun build-buffer (string)
  "Makes a new buffer and ensures the string is of the correct type"
  (make-buffer :string (if (typep string 'simple-string)
			   string
			   (coerce string 'simple-string))))

(declaim (inline next-char decr-char current-char fetch-char subseq-buffer-mark mark-buffer))
(defun next-char (buffer)
  (declare (type buffer buffer))
  "Sets the pointer to the next char in the buffer"
  (incf (buffer-index buffer)))
(defun decr-char (buffer)
  (declare (type buffer buffer))
  "Sets the pointer to the previous char in the buffer"
  (decf (buffer-index buffer)))
(defun current-char (buffer)
  (declare (type buffer buffer))
  "Returns the current character the buffer is pointing to"
  (elt (buffer-string buffer) (buffer-index buffer)))
(defun fetch-char (buffer)
  (declare (type buffer buffer))
  "Reads a character from the buffer and increases the index"
  (next-char buffer)
  (elt (buffer-string buffer) (1- (buffer-index buffer))))
(defun subseq-buffer-mark (buffer)
  (declare (type buffer buffer))
  "Returns the content between index and mark for the current buffer
 result: (subseq buffer-string mark index))"
  (subseq (buffer-string buffer) (buffer-mark buffer) (buffer-index buffer)))
(defun mark-buffer (buffer)
  "Sets the mark of the buffer to the current character"
  (setf (buffer-mark buffer) (buffer-index buffer)))

(defun skip-to (buffer last-char)
  "Skips characters until <char> has been found.  <char> is the last char which is skipped"
  (declare (type buffer buffer)
	   (type character last-char))
  (loop until (char= (current-char buffer) last-char)
     do (next-char buffer))
  (values))

(defun skip-spaces (buffer)
  "Skips spaces, tabs and newlines until a non-space character has been found"
  (loop while (find (current-char buffer) +space-characters+ :test #'char=)
     do (next-char buffer)))

;; (defmacro skip-spaces (buffer)
;;   (declare (ignore buffer))
;;   nil)

(defun subseq-until (buffer &rest chars)
  "Returns a subsequence of stream, reading everything before a character belonging to chars is found.  The character which was found is not read from the buffer"
  (declare (type buffer buffer))
  (mark-buffer buffer)
  (loop until (find (current-char buffer) chars :test #'char=)
     do (next-char buffer))
  (subseq-buffer-mark buffer))

(defun read-object (buffer)
  "reads a key-value pair into the hash"
  (declare (type buffer buffer))
  (let ((obj nil))
    (loop until (progn (skip-spaces buffer)
		       (char= (fetch-char buffer) #\})) ; we may read-char here, as the character is a , to be skipped if it is not a }
       collect (cons (read-key buffer) (read-value buffer))) 
    obj))

(defun read-key (buffer)
  "reads a key from the key-value list"
  (declare (type buffer buffer))
  (skip-to buffer #\")
  (parse-string buffer))

(defun read-value (buffer)
  "Reads a value from the stream.
 This searches for the first meaningful character, and delegates to the right function for that character"
  (declare (type buffer buffer))
  (skip-to buffer #\:)
  (skip-spaces buffer)
  (let ((meaning (fetch-char buffer)))
    (cond ((string= meaning #\")
	   (parse-string buffer))
	  ((string= meaning #\{)
	   (read-object buffer))
	  ((string= meaning #\[)
	   (read-array buffer))
	  ((char= meaning #\t)
	   T)
	  ((char= meaning #\f)
	   nil)
	  ((char= meaning #\n)
	   nil)
	  (T
	   (read-number buffer)))))

(defun parse-string (buffer)
  "Reads a JSON string from the stream (assumes the first \" is missing and NO escaped characters are in there"
  (declare (type buffer buffer))
  (subseq-until buffer #\"))

(defun read-array (buffer)
  "Reads a JSON array from the stream (assumes the first [ is missing"
  (declare (type buffer buffer))
  (loop for value = (read-value buffer)
     collect value
     until (progn (skip-spaces buffer) (string= (fetch-char buffer) #\]))) ; we may fetch-char here, as the character is a , to be skipped if it is not a ]
  )

(defun read-number (buffer)
  (declare (type buffer buffer))
  (skip-spaces buffer)
  (read-from-string (subseq-until buffer #\] #\} #\,))) ;; only these characters are allowed to actually end a number

(defun read-json (string)
  (let ((buffer (build-buffer string)))
    (skip-to buffer #\{)
    (read-object buffer)))
