
(defvar *php-pprint-dispatch* (copy-pprint-dispatch))

(proclaim '(special *B*))

(defun phpize (x)
  (let ((*print-pprint-dispatch* *php-pprint-dispatch*))
    (write x :pretty t)
    (values)))

(defmacro defprinter ((typespec obj &optional (priority 0)) &body body)
  (let ((stream (gensym)))
    `(set-pprint-dispatch ',typespec
			  (lambda (,stream ,obj)
			    (declare (ignorable ,obj))
			    (macrolet ((fmt (&rest args) `(format ,',stream ,@args))
				       (write-str (arg) `(cl:write-string ,arg ,',stream)))
			      ,@body))
			  ,priority
			  *php-pprint-dispatch*)))

(defun undefprinter (typespec)
  (set-pprint-dispatch typespec nil))

(defun eq-t-p (x)
  (eq t x))

(defprinter (symbol x)
  (fmt "$~A" (string-downcase (symbol-name x))))

(defprinter (null x 1)
  (write-str "FALSE"))

(defun php-escape-string (x)
  (with-output-to-string (s)
    (dotimes (i (length x))
      (let ((c (aref x i)))
	(write-string
	 (case c 
	   (#\Newline "\\n")
	   (#\Tab "\\t")
	   (#\Return "\\r")
	   (#\Page "\\f")
	   (#\\ "\\\\")
	   (#\$ "\\$")
	   (#\" "\\\"")
	   ;; TODO: improve the logic of what char codes must be escaped
	   (t (if (graphic-char-p c)
		  (format nil "~C" c)
		  (format nil "\\0~O" (char-code c)))))
	 s)))))

(defprinter (string x)
  (write-str "\"")
  (write-str (php-escape-string x))
  (write-str "\""))

(defprinter ((satisfies eq-t-p) x 2)
  (write-str "TRUE"))

(defmacro defspecialform (form &body body)
  (let ((stream (gensym))
	(arg (gensym)))
    `(set-pprint-dispatch '(cons (member ,(first form)))
			  (lambda (,stream ,arg)
			    (destructuring-bind ,form ,arg
			      (declare (ignorable ,(first form)))
			      (macrolet ((fmt (&rest args) `(format ,',stream ,@args)))
				,@body)))
			    10
			    *php-pprint-dispatch*)))
  
(defun pprint-block (stream block &optional colon at-sign)
  (declare (ignore colon at-sign))
  (format stream  "~@<{~;~8I~:@_~{~W~^~:@_~}~-8I~:@_~;}~:>" block))

;; defspecialform should allow destructuring, and make patterns
;; from destructuring
(defspecialform (if cond true false)
  (fmt "if (~W) ~/pprint-block/ else ~/pprint-block/" cond (list true) (list false)))

;; how do we map lisp expressions to PHP ones
(defvar *ops*
  (loop
     for ops in (reverse
		 '(((not "!" right))
		   ((/ "/" left)
		    (* "*" left)
		    (mod "%" left))
		   ((+ "+" left)
		    (- "-" left))
		   ((concat "." left))
		   ((<< "<<" left)
		    (>> ">>" left))
		   ((< "<<" left)
		    (<= "<=" none)
		    (>  ">" none)
		    (>= ">=" none))
		   ((/= "!=" none)
		    (= "=" none))
		   ((ref "&" left))
		   ((logand "&" left))
		   ((logxor "^" left))
		   ((logior "|" left))
		   ((and "&&" left))
		   ((or "||" left))
		   ((setq "=" right))))
     for k from 0
     do (loop for (op php-op assoc) in ops
	   do
	   (format t "op: ~a~%" op)
	   (setf (get op :php-op) php-op)
	   (setf (get op :associativity) assoc)
	   (setf (get op :precedence) k))))


;; TODO rest of assingment ops
;; should be like optimized stuff from others
