(in-package :jsown)

(declaim (optimize (speed 3) (safety 0) (debug 3)))

;;;;;;;;;;;;;;;;;;;
;;;; character-tree
(defun build-character-tree (&rest strings)
  "Builds a character tree from a set of strings"
  (build-tree (loop for string in strings collect
		   (loop for char across (the simple-string string) collect char))))

(define-compiler-macro build-character-tree (&whole form &rest strings)
  (if (loop for string in strings unless (stringp string) return T)
      form
      `(quote ,(apply #'build-character-tree strings))))

(defun find-first-elts (lists)
  (remove-duplicates (loop for list in lists
			when (first list)
			collect (first list))
		     :test #'eql))

(defun build-tree (lists)
  "Builds a tree from a range of lists and a function to compare its elements by"
  (when lists
    (loop for first-elt in (find-first-elts lists)
	collect (let ((matching-lists (loop for list in lists when (and (first list) (eql (the character first-elt) (the character (first list))))
					 collect (rest list))))
		  (list first-elt
			(loop for list in matching-lists unless list return T) ;; T shows that this is an end-result
			(build-tree matching-lists))))))

(defun iterate-tree (tree char)
  "Iterates a character-tree with the given character
 Returns two values, being the new tree and whether or not this is an end-point."
  (declare (type (or cons nil) tree)
	   (type character char))
  (let ((solution (rest (find char tree :key #'first :test #'eql))))
    (when solution
      (values (second solution) (first solution)))))

;;;;;;;;;;;;;;;;;
;;;; parsing code
(defconstant +space-characters+ '(#\Space #\Newline #\Tab #\Linefeed)
  "List of characters which may denote a space in the JSON format (these have not been verified")
(defconstant +do-skip-spaces+ nil
  "If this constant is T the library will try to skip spaces.  If it is nil at compile-time the code assumes that spaces may not occur outside of strings")

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

(declaim (inline next-char next-char/ decr-char current-char peek-behind-char fetch-char subseq-buffer-mark mark-buffer mark-length skip-to skip-to/ skip-until skip-until/ skip-until* skip-spaces subseq-until subseq-until/ subseq-tree))
(defun next-char (buffer)
  (declare (type buffer buffer))
  "Sets the pointer to the next char in the buffer"
  (incf (buffer-index buffer)))
(defun next-char/ (buffer)
  (declare (type buffer buffer))
  "Sets the pointer to the next char in the buffer, ignores escaped characters (they start with a \\) through"
  (incf (buffer-index buffer))
  (loop until (char/= (current-char buffer) #\\)
     do (incf (buffer-index buffer) 2)))
(defun decr-char (buffer)
  (declare (type buffer buffer))
  "Sets the pointer to the previous char in the buffer"
  (decf (buffer-index buffer)))
(defun current-char (buffer)
  (declare (type buffer buffer))
  "Returns the current character the buffer is pointing to"
  (elt (buffer-string buffer) (buffer-index buffer)))
(defun peek-behind-char (buffer)
  (declare (type buffer buffer))
  (elt (buffer-string buffer) (1- (buffer-index buffer))))
(defun fetch-char (buffer)
  (declare (type buffer buffer))
  "Reads a character from the buffer and increases the index"
  (next-char buffer)
  (peek-behind-char buffer))
(defun subseq-buffer-mark (buffer)
  (declare (type buffer buffer))
  "Returns the content between index and mark for the current buffer
 result: (subseq buffer-string mark index))"
  (subseq (buffer-string buffer) (buffer-mark buffer) (buffer-index buffer)))
(defun mark-buffer (buffer)
  "Sets the mark of the buffer to the current character"
  (setf (buffer-mark buffer) (buffer-index buffer)))
(defun mark-length (buffer)
  (declare (type buffer buffer))
  "Returns the current amount of characters in the marked piece of the buffer"
  (the fixnum (- (buffer-index buffer) (buffer-mark buffer))))

(defun skip-to (buffer last-char)
  "Skips characters until <char> has been found.  <char> is the last char which is skipped
 see: skip-until"
  (declare (type buffer buffer)
	   (type character last-char))
  (skip-until buffer last-char)
  (next-char buffer))
(defun skip-to/ (buffer last-char)
  "What skip-to does, but with the ignoring of \\"
  (declare (type buffer buffer)
	   (type character last-char))
  (skip-until/ buffer last-char)
  (next-char/ buffer))
(defun skip-until (buffer last-char)
  "Skips characters until <char> has been found.  <char> is NOT skipped
 See: skip-to"
  (declare (type buffer buffer)
	   (type character last-char))
  (loop until (eql (current-char buffer) last-char)
     do (next-char buffer))
  (values))
(defun skip-until/ (buffer last-char)
  "What skip-until does, but with \\ escaping"
  (declare (type buffer buffer)
	   (type character last-char))
  (decr-char buffer)
  (loop do (next-char/ buffer)
     until (eql (current-char buffer) last-char)))

(defun skip-until* (buffer &rest chars)
  "Skips characters until one of the characters in <chars> has been found.  The character which was found is not read from the buffer"
  (declare (type buffer buffer))
  (loop until (find (current-char buffer) chars :test #'eql)
     do (next-char buffer)))

(defun skip-spaces (buffer)
  "Skips spaces, tabs and newlines until a non-space character has been found"
  (when +do-skip-spaces+
    (loop while (find (current-char buffer) +space-characters+ :test #'eql)
       do (next-char buffer))))
(define-compiler-macro skip-spaces (&whole whole buffer)
  (declare (ignore buffer))
  (when +do-skip-spaces+
    whole))

;; (defmacro skip-spaces (buffer)
;;   (declare (ignore buffer))
;;   nil)

(defun subseq-until (buffer &rest chars)
  "Returns a subsequence of stream, reading everything before a character belonging to chars is found.  The character which was found is not read from the buffer"
  (declare (type buffer buffer))
  (mark-buffer buffer)
  (loop until (find (current-char buffer) chars :test #'eql)
     do (next-char buffer))
  (subseq-buffer-mark buffer))

(defun subseq-until/ (buffer last-char)
  "Does what subseq-until does, but does escaping too"
  (declare (type buffer buffer)
	   (type character last-char))
  (mark-buffer buffer)
  (decr-char buffer)
  (loop do (next-char/ buffer)
     until (eql (current-char buffer) last-char))
  (subseq-buffer-mark buffer))

(defun subseq-tree (buffer end-char tree)
  "Returns a sequence of the buffer, reading everything that matches with the given tree before end-char is found.  end-char is not read from the buffer
 Returns nil if no sequence matching the tree could be found.  It then stops iterating at the failed position
 Skips #\\"
  (declare (type buffer buffer)
	   (type character end-char))
  (mark-buffer buffer)
  (decr-char buffer)
  (let ((accepted-p nil))
    (loop
       while (progn (next-char/ buffer)
		    (and tree (char/= (current-char buffer) end-char)))
       do (multiple-value-setq (tree accepted-p) (iterate-tree tree (current-char buffer))))
    (values accepted-p
	    (if accepted-p (subseq-buffer-mark buffer) ""))))

(defun read-object (buffer)
  "reads a key-value pair into the hash"
  (declare (type buffer buffer))
  (cons :obj
	(loop until (progn (skip-spaces buffer)
			   (eql (fetch-char buffer) #\})) ; we may fetch-char here, as the character is a #\, to be skipped if it is not a #\}
	   collect (cons (read-key buffer)
			 (progn (skip-to buffer #\:)
				(read-value buffer))))))

(defun read-partial-object (buffer tree)
  "Reads an object from the buffer, but only when the key matches a key in the tree"
  (declare (type buffer buffer)
	   (type (or cons nil) tree))
  (cons :obj
	(loop until (progn (skip-spaces buffer)
			   (eql (fetch-char buffer) #\})) ; we may fetch-char here, as the character is a #\, to be skipped if it is not a #\}
	   append (multiple-value-bind (found-p key)
		      (read-partial-key buffer tree)
		    (progn (skip-to buffer #\:)		
			   (if found-p
			       (list (cons key (read-value buffer)))
			       (progn (skip-value buffer) nil)))))))

(defun skip-object (buffer)
  "Skips an object from the buffer"
  (declare (type buffer buffer))
  (loop until (progn (skip-spaces buffer)
		     (eql (fetch-char buffer) #\})) ; we may read-char here, as the character is a , to be skipped if it is not a }
     do (skip-key buffer) (skip-value buffer)))

(defun read-partial-key (buffer tree)
  "reads a key from the buffer.  Returns (values key T) if the key was found as a valid key in the tree, or (values nil nil) if it was not"
  (declare (type buffer buffer)
	   (type (or cons nil) tree))
  (skip-to buffer #\")
  (multiple-value-bind (accepted-p solution)
      (subseq-tree buffer #\" tree)
    (declare (type (or nil T) accepted-p)
	     (type simple-string solution))
    (skip-to buffer #\") ;; clean up mess
    (values accepted-p solution)))

(defun read-key (buffer)
  "reads a key from the key-value list"
  (declare (type buffer buffer))
  (skip-to buffer #\")
  (parse-string buffer))

(defun skip-key (buffer)
  "reads a key from the key-value list"
  (declare (type buffer buffer))
  (skip-to buffer #\")
  (skip-string buffer))

(defun read-value (buffer)
  "Reads a value from the stream.
 This searches for the first meaningful character, and delegates to the right function for that character"
  (declare (type buffer buffer))
  (skip-spaces buffer)
  (case (fetch-char buffer)
    (#\" (parse-string buffer))
    (#\{ (decr-char buffer) (read-object buffer))
    (#\[ (read-array buffer))
    (#\t (incf (buffer-index buffer) 3)
	 T)
    (#\f (incf (buffer-index buffer) 4)
	 nil)
    (#\n (incf (buffer-index buffer) 3)
	 nil)
    (T (read-number buffer))))

(defun skip-value (buffer)
  "Skips a value from the stream.
 This searches for the first meaningful character, and delegates to the right function for skipping that"
  (declare (type buffer buffer))
  (skip-spaces buffer)
  (case (fetch-char buffer)
    (#\" (skip-string buffer))
    (#\{ (skip-object buffer))
    (#\[ (skip-array buffer))
    (#\t (incf (buffer-index buffer) 3))
    (#\f (incf (buffer-index buffer) 4))
    (#\n (incf (buffer-index buffer) 3))
    (T (skip-number buffer)))
  (values))

(defun skip-string (buffer)
  (declare (type buffer buffer))
  "Skips the contents of an input string from the buffer.  Assumes the first #\" has been read"
  (skip-to/ buffer #\"))

(defun parse-string (buffer)
  "Reads a JSON string from the stream (assumes the first \" is missing and NO escaped characters are in there"
  (declare (type buffer buffer))
  (let ((result (subseq-until/ buffer #\")))
    (next-char buffer)
    result))

(defun skip-array (buffer)
  (declare (type buffer buffer))
  "Skips the contents of an array from the buffer.  Assumes the first #\[ is already read from the buffer"
  (decr-char buffer)
  (loop 
     until (progn (skip-spaces buffer)
		  (next-char buffer)
		  (eql (peek-behind-char buffer) #\]))
     do (skip-value buffer))
  )

(defun read-array (buffer)
  "Reads a JSON array from the stream (assumes the first [ is missing"
  (declare (type buffer buffer))
  (decr-char buffer)
  (loop 
     until (progn (skip-spaces buffer)
		  (next-char buffer)
		  (eql (peek-behind-char buffer) #\]))
     collect (read-value buffer)))

(defun read-number (buffer)
  (declare (type buffer buffer))
  (decr-char buffer)
  (let ((whole-part (parse-integer (subseq-until buffer #\] #\} #\, #\.)))) ;; only these chars can delimit the whole part of a number 
    (if (eql (current-char buffer) #\.)
	(progn 
	  (next-char buffer)
	  (let ((float-part (parse-integer (subseq-until buffer #\] #\} #\,)))) ;; only these characters are allowed to actually end a number
	    (+ whole-part (/ float-part (the integer (expt 10 (mark-length buffer)))))))
	whole-part)))

(defun skip-number (buffer)
  (declare (type buffer buffer))
  (decr-char buffer)
  (skip-until* buffer #\] #\} #\,))

;;;;;;;;;;;;;;;;;;;
;;;; User interface
(defun build-key-container (&rest keywords-to-read)
  "Builds an internal structure to speed up the keywords which you can read.  This should be used when the keywords needed are not known at compiletime, but you still want to parse those keywords of a lot of documents.
 If the keywords you are interested in are known at compiletime, the use of #'parse will automatically expand the kewords at compiletime.
 parse-with-container takes the result of this function and will return the keywords which have been inserted here."
  (apply #'build-character-tree keywords-to-read))
(define-compiler-macro build-key-container (&rest keywords-to-read)
  `(build-character-tree ,@keywords-to-read))

(defun parse-with-container (json-string container)
  "Parses the keywords which have been specified in the container from the  json string json-string.
 For most cases you can just use the parse function without a special key container.  This is only here to support some cases where the building of the key container takes too much time.  
 See #'parse for the normal variant.
 See #'build-key-container for a way to build new keyword containers."
  (let ((buffer (build-buffer json-string)))
    (skip-spaces buffer)
    (read-partial-object buffer container)))

(defun parse (string &rest keywords-to-read)
  "Reads a json object from the given string, with the given keywords being the keywords which are fetched from the object.
 All parse functions assume <string> is not an empty json object.  (string/= string \"{}\")"
  (let ((buffer (build-buffer string)))
    (skip-spaces buffer)
    (if keywords-to-read
	(read-partial-object buffer (apply #'build-character-tree keywords-to-read))
	(read-object buffer))))
(define-compiler-macro parse (&whole whole string &rest keywords-to-read) ; this allows the character tree to be precompiled
  (if keywords-to-read
      `(let ((buffer (build-buffer ,string)))
	 (skip-spaces buffer)
	 (read-partial-object buffer (build-character-tree ,@keywords-to-read)))
      whole))
