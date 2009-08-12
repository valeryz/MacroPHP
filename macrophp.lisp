;;
;; Macro Preprocessor for High Productivity (MacroPHP)
;; Copyright (c) 2009 Valeriy Zamarayev
;;

(defpackage :php (:use :cl))

(in-package :php)

(defvar *php-pprint-dispatch* (copy-pprint-dispatch))

(declaim (special *B*))

(defvar *special-forms* nil
  "PHP special forms")

(defun phpize (x)
  (let ((*print-pprint-dispatch* *php-pprint-dispatch*)
	(*B* 0))
    (write x :pretty t :escape nil :level nil)
    (values)))

(defun set-php-pprint-dispatch (typespec function &optional (priority 5))
  (set-pprint-dispatch typespec function priority *php-pprint-dispatch*))

(defmacro defprinter ((typespec obj &optional (priority 0)) &body body)
  `(set-php-pprint-dispatch ',typespec
			    (lambda (stream ,obj)
			      (declare (ignorable ,obj))
			      (macrolet ((fmt (&rest args) `(format stream ,@args))
					 (write-str (arg) `(cl:write-string ,arg stream)))
				,@body))
			    ,priority))

(defun undefprinter (typespec)
  (set-php-pprint-dispatch typespec nil))

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
    `(progn (pushnew ',(first form) *special-forms*)
	    (set-php-pprint-dispatch '(cons (member ,(first form)))
				     (lambda (stream ,arg)
				       (destructuring-bind ,form ,arg
					 (declare (ignorable ,(first form)))
					 (labels ((fmt (&rest args) (apply #'format stream args))
						  (write-str (arg) (cl:write-string arg stream)))
					   ,@body)))))))

;; descriptions of varios operations
(defvar *ops* (loop
   for ops in (reverse
	       ;; op php-op associativity arity
	       ;; each sublist contains ops of the same priority
	       '(((funcall "" :left 1 :noauto t))
		 ((clone "clone" :none 1 :makespace t)
		  (new "new" :none 1 :makespace t))
		 ((aref "" :left 2 :noauto t))
		 ((~ "~" :none 1)
		  (- "-" :none 1 :noauto t)
		  (@ "@" :none 1)
		  (cast "" :none 2 :noauto t))
		 ((instanceof "instanceof" :none 2))
		 ((not "!" :right 1))
		 ((/ "/" :left 2)
		  (* "*" :left 2)
		  (mod "%" :left 2))
		 ((+ "+" :left 2)
		  (- "-" :left 2))
		 ((concat "." :left 2))
		 ((<< "<<" :left 2)
		  (>> ">>" :left 2))
		 ((< "<" :left 2)
		  (<= "<=" :none 2)
		  (> ">" :none 2)
		  (>= ">=" :none 2))
		 ((/= "!=" :none 2)
		  (= "==" :none 2))
		 ((ref "&" :left 1))
		 ((logand "&" :left 2))
		 ((logxor "^" :left 2))
		 ((logior "|" :left 2))
		 ((and "&&" :left 2))
		 ((or "||" :left 2))
		 ((|?:| "" :left 3 :noauto t))
		 ((setq "=" :right 2)
		  (+= "+=" :right 2)
		  (-= "-=" :right 2)
		  (*= "*=" :right 2)
		  (/= "/=" :right 2)
		  (.= ".=" :right 2)
		  (%= "%=" :right 2)
		  (&= "&=" :right 2)
		  (\|= "|=" :right 2)
		  (^= "^=" :right 2)
		  (<<= "<<=" :right 2)
		  (>>= ">>=" :right 2))
		 ((xor "xor" :left 2))
		 ((\, "," :left 2 :skip-first-space t))))
   for k from 1
   nconc (loop for op-details in ops
	    collect (cons k op-details))))

(defmacro in-op-pprint-block (&body body)
  `(let ((nest (<= precedence *B*)))
     (pprint-logical-block (s (cdr op) :prefix (if nest "(" "") :suffix (if nest ")" ""))
     ,@body)))

(defun make-unary-op (php-op assoc precedence &rest keys &key makespace &allow-other-keys)
  (declare (ignore assoc keys))
  (lambda (s op)
    (in-op-pprint-block
      (assert (= 2 (length op)))
      (let ((*B* precedence))
	(write-string php-op s)
	(when makespace (write-string " " s))
	(write (pprint-pop) :stream s)))))

(defun make-binary-op (php-op assoc precedence &rest keys &key skip-first-space &allow-other-keys)
  (declare (ignore keys))
  (lambda (s op)
    (in-op-pprint-block
      (assert (= 3 (length op)))
      (let ((*B*
	     (if (eq :left assoc)
		 (1- precedence)
		 precedence)))
	(write (pprint-pop) :stream s))
      ;; first space is skipped for , (comma)
      (unless skip-first-space (write-char #\Space s))
      (write-string php-op s)
      (write-char #\Space s)
      (pprint-indent :block 4 s)
      (pprint-newline :fill s)
      (let ((*B*
	     (if (eq :right assoc)
		 (1- precedence)
		 precedence)))
	(write (pprint-pop) :stream s)))))

(loop for op-details in *ops*
     do (destructuring-bind (priority op php-op assoc arity &rest rest &key noauto &allow-other-keys)
	    op-details
	  (unless noauto
	    (set-pprint-dispatch `(cons (member ,op))
				 (ecase arity
				   (1 (apply #'make-unary-op php-op assoc priority rest))
				   (2 (apply #'make-binary-op php-op assoc priority rest)))
				 10
				 *php-pprint-dispatch*))))

(defun cons-op-p (exp op)
  (and (consp exp)
       (eq (car exp) op)))

(defun unary-minus-p (exp)
  (cons-op-p exp '-))

(defun aref-p (exp)
  (cons-op-p exp 'aref))

(defun cast-p (exp)
  (cons-op-p exp 'cast))

(defun ternary-p (exp)
  (cons-op-p exp '|?:|))

(defun progn-p (exp)
  (cons-op-p exp 'progn))

(defun find-op (op arity)
  (find-if (lambda (op-details) (and (eq op (second op-details))
				     (eql arity (fifth op-details))))
	   *ops*))

(defun special-form-p (form)
  (and (consp form)
       (member (first form) *special-forms*)))

(set-php-pprint-dispatch '(satisfies unary-minus-p)
			 (make-unary-op "-" :none (car (find-op '- 1))))

(set-php-pprint-dispatch '(satisfies aref-p)
			 (let ((precedence (car (find-op 'aref 2))))
			   (lambda (s op)
			     (in-op-pprint-block
			       (assert (= 3 (length op)))
			       (let ((*B* (1- precedence)))
				 (write (pprint-pop) :stream s))
			       (write-string "[" s)
			       (let ((*B* 0))
				 (write (pprint-pop) :stream s))
			       (write-string "]" s)))))

(set-php-pprint-dispatch '(satisfies cast-p)
			 (let ((precedence (car (find-op 'cast 2))))
			   (lambda (s op)
			     (in-op-pprint-block
			       (assert (= 3 (length op)))
			       (let ((type (pprint-pop)))
				 (unless (find type '(int float string array object bool))
				   (error "cast to wrong type ~a" type))
				 (write-string "(" s)
				 (write type :stream s)
				 (write-string ")" s))
			       (let ((*B* precedence))
				 (write (pprint-pop) :stream s))))))

(set-php-pprint-dispatch '(satisfies ternary-p)
			 (let ((precedence (car (find-op '|?:| 3))))
			   (lambda (s op)
			     (in-op-pprint-block
			       (assert (= 4 (length op)))
			       (let ((*B* precedence))
				 (write (pprint-pop) :stream s))
			       (write-string " ? " s)
			       (pprint-indent :block 4 s)
			       (pprint-newline :fill s)
			       (let ((*B* precedence))
				 (write (pprint-pop) :stream s))
			       (write-string " : " s)
			       (pprint-newline :fill s)
			       (let ((*B* precedence))
				 (write (pprint-pop) :stream s))))))

(set-php-pprint-dispatch '(satisfies progn-p)
			 (lambda (s form)
			   (format s "~{~W~:[;~;~]~^~@:_~}"
				   (mapcan (lambda (form)
					     (list form (special-form-p form)))
					   (cdr form)))))
			 
(set-php-pprint-dispatch 'cons
			 (let ((precedence (car (find-op 'funcall 1))))
			   (lambda (s op)
			     (pprint-logical-block (s nil)
			       (when (<= precedence *B*)
				 (write-string "(" s))
			       (let ((*B* (1- precedence)))
				 (write (car op) :stream s))
			       (write-string "(" s)
			       (pprint-indent :block 4 s)
			       (let ((*B* 0))
				 (format s "~<~@{~W~^, ~:_~}~:>" (cdr op)))
			       (write-string ")" s)
			       (when (<= precedence *B*)
				 (write-string ")" s)))))
			 0)

(defun pprint-block-format (block &optional check-if)
  (if (or (progn-p block)
	  (and check-if (cons-op-p block 'if)))
      (values " {~:@_~W~0I~:@_} " (list block))
      (values "~:@_~W~:[;~;~]~0I~:@_" (list block (special-form-p block)))))
