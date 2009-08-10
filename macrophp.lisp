;; Copyright (c) 2009 Valeriy Zamarayev

(defpackage :php (:use :cl))

(in-package :php)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *php-pprint-dispatch* (copy-pprint-dispatch))
  (proclaim '(special *B*)))

(defun phpize (x)
  (let ((*print-pprint-dispatch* *php-pprint-dispatch*)
	(*B* 0))
    (write x :pretty t :escape nil)
    (values)))

(defmacro defprinter ((typespec obj &optional (priority 1)) &body body)
  `(set-pprint-dispatch ',typespec
			(lambda (stream ,obj)
			  (declare (ignorable ,obj))
			  (macrolet ((fmt (&rest args) `(format stream ,@args))
				     (write-str (arg) `(cl:write-string ,arg stream)))
			    ,@body))
			,priority
			*php-pprint-dispatch*))

(defun undefprinter (typespec)
  (set-pprint-dispatch typespec nil))

(defun eq-t-p (x)
  (eq t x))

(defprinter (symbol x)
  (write-str (string-downcase (symbol-name x))))

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
  (let ((arg (gensym)))
    `(set-pprint-dispatch '(cons (member ,(first form)))
			  (lambda (stream ,arg)
			    (destructuring-bind ,form ,arg
			      (declare (ignorable ,(first form)))
			      (macrolet ((fmt (&rest args) `(format stream ,@args))
					 (write-str (arg) `(cl:write-string ,arg stream)))
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
(loop
   for ops in (reverse
	       ;; op php-op associativity arity
	       '(((not ! :right 1))
		 ((/ / :left 2)
		  (* * :left 2)
		  (mod % :left 2))
		 ((+ + :left 2)
		  (- - :left 2))
		 ((concat \. :left 2))
		 ((<< << :left 2)
		  (>> >> :left 2))
		 ((< < :left 2)
		  (<= <= :none 2)
		  (> > :none 2)
		  (>= >= :none 2))
		 ((/= != :none 2)
		  (= = :none 2))
		 ((ref & :left 1))
		 ((logand & :left 2))
		 ((logxor ^ :left 2))
		 ((logior \| :left 2))
		 ((and && :left 2))
		 ((setq = :right 2))
		 ((or \|\| :left 2))))
   for k from 0
   do (loop for (op php-op assoc arity) in ops
	 do
	   (setf (get op :php-op) php-op)
	   (setf (get op :associativity) assoc)
	   (setf (get op :arity) arity)
	   (setf (get op :precedence) k)))

(defun php-op (exp)
  (and (consp exp)
       (symbolp (car exp))
       (get (car exp) :php-op)))

(defun associativity (exp)
  (get (car exp) :associativity))

(defun precedence (exp)
  (get (car exp) :precedence))

(defun arity (exp)
  (get (car exp) :arity))

(defun unary-p (exp)
  (= 1 (arity exp)))

(defun binary-p (exp)
  (= 2 (arity exp)))

(defun pprint-php-exp (s op)
  (let ((nest (<= (precedence op) *B*)))
    (flet ((write-op ()
	     (write-string (string-downcase (symbol-name (php-op op))) s)))
      (pprint-logical-block (s (cdr op) :prefix (if nest "(" "") :suffix (if nest ")" ""))
	(cond ((unary-p op)
	       ;; TODO: convert those asserts into throwing of
	       ;; compilation errors
	       (assert (= 2 (length op)))
	       (let ((*B* (precedence op)))
		 (write-op)
		 (write (pprint-pop) :stream s)))
	      ((binary-p op)
	       (assert (= 3 (length op)))
	       (let ((*B*
		      (if (eq :left (associativity op))
			  (1- (precedence op))
			  (precedence op))))
		 (write (pprint-pop) :stream s))
	       (write-char #\Space s)
	       (write-op)
	       (write-char #\Space s)
	       (pprint-indent :block 4 s)
	       (pprint-newline :fill s)
	       (let ((*B*
		      (if (eq :right (associativity op))
			  (1- (precedence op))
			  (precedence op))))
		 (write (pprint-pop) :stream s))))))))

(defprinter ((satisfies php-op) exp)
  (pprint-php-exp stream exp))

;; TODO rest of assingment ops
;; should be like optimized stuff from others
