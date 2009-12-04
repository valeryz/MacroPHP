;;
;; Macro Preprocessor for High Productivity (MacroPHP)
;; Copyright (c) 2009 Valeriy Zamarayev
;;

(in-package :php)

(defvar *php-pprint-dispatch* (copy-pprint-dispatch)
  "Pretty printer dispatch table for PHP code")
;; inhibit all the standard Common Lisp keywords
;; (not very nice), but otherwise we can't use CL pretty printer
;; reliably
(loop for cons-op in '(
		       defstruct
		       block
		       case
		       catch
		       ccase
		       compiler-let
		       cond
		       ctypecase
		       defclass
		       ctypecase
		       defconstant
		       define-setf-expander
		       defmacro
		       define-modify-macro
		       defparameter
		       defsetf
		       define-setf-expander
		       deftype
		       defun
		       defmethod
		       defvar
		       do
		       do*
		       do-all-symbols
		       do-external-symbols
		       do-symbols
		       dolist
		       dotimes
		       ecase
		       etypecase
		       eval-when
		       flet
		       function
		       generic-function
		       labels
		       lambda
		       let
		       let*
		       locally
		       loop
		       macrolet
		       multiple-value-bind
		       multiple-value-setq
		       prog
		       prog*
		       progv
		       psetf
		       psetq
		       quote
		       return-from
		       setf
		       setq
		       tagbody
		       throw
		       typecase
		       unless
		       unwind-protect
		       when
		       with-input-from-string
		       with-open-file
		       with-open-stream
		       with-output-to-string)
     do (set-pprint-dispatch `(cons (member ,cons-op)) nil 0 *php-pprint-dispatch*))

(declaim (special *B*))

(defvar *special-forms* nil
  "PHP special forms")

(defvar *expect-statement* t "Whether the statement or expression is expected")

(defvar *no-semicolon-constructs* '(if cond for foreach while switch progn tagbody
				    function class try)
  "All control constructs, that need not be terminated by a semicolon in a progn")

(defvar *symbol-print-names* '((true . "TRUE") (false . "FALSE")
			       ($_POST . "$_POST") ($_GET . "$_GET") ($_REQUEST . "$_REQUEST")
			       ($_SESSION . "$_SESSION")
			       ($_COOKIE . "$_COOKIE")
			       ($_SERVER . "$_SERVER")
			       ($_ENV . "$_ENV")
			       ($_FILES . "$_FILES")
			       ($_HTTP_RAW_POST_DATA . "$_HTTP_RAW_POST_DATA")
			       ($GLOBALS . "$_GLOBALS"))
  "print names for some special symbols, e.g. TRUE, FALSE and superglobals")

(defvar *toplevel-progn* t "Only bound to t at the toplevel of phpizing")

(defun no-semicolon-p (form)
  "check if the form is a control construct"
  (and (consp form)
       (find (first form) *no-semicolon-constructs*)))

#+CCL(declaim (special ccl::*print-catch-errors*))

(defun phpize (stream form)
  "Print PHP code"
  (let ((*print-pprint-dispatch* *php-pprint-dispatch*)
	(*print-length* nil)
	(*B* 0)
	(*toplevel-progn* t))
    (pprint-logical-block (stream nil)
      (write (remove-progn (remove-nested-progn (php/macroexpand-all form))) :pretty t :stream stream))
    (values)))

(defun set-php-pprint-dispatch (typespec function &optional (priority 5))
  (set-pprint-dispatch typespec function priority *php-pprint-dispatch*))

(defmacro defprinter ((typespec obj &optional (priority 0)) &body body)
  `(set-php-pprint-dispatch ',typespec
			    (lambda (stream ,obj)
			      (declare (ignorable stream ,obj))
			      (macrolet ((fmt (&rest args) `(format stream ,@args))
					 (write-str (arg) `(cl:write-string ,arg stream)))
				,@body))
			    ,priority))

(defun undefprinter (typespec)
  (set-php-pprint-dispatch typespec nil))

(defun symbol-name-to-php (name)
  "Convert LISP symbol name to PHP name

The rules:
foo-bar => fooBar
*foo => FOO
*foo**bar => FOO::bar
*foo***bar => FOO::BAR
"
  (let ((new-name (copy-seq name)))
    (do ((i 0 (1+ i)) (j 0) (state :INIT) (all-upper) (upper) (star))
	((>= i (length name)) (subseq new-name 0 j))
      (ecase state
	(:INIT
	 (if (eql (aref name i) #\*)
	     (setf all-upper t)
	     (if (eql (aref name i) #\-)
		 (setf upper t)
		 (progn (setf (aref new-name j) (char-downcase (aref name i)))
			(incf j))))
	 (setf state :INSIDE))
	(:INSIDE
	 (if (eql (aref name i) #\*)
	     (if star
		 (progn (setf (aref new-name j) #\:)
			(incf j)
			(setf (aref new-name j) #\:)
			(incf j)
			(setf state :INIT)
			(setf all-upper nil)
			(setf star nil))
		 (setf star t))
	     (progn (when star
		      (setf (aref new-name j) #\*)
		      (incf j)
		      (setf star nil))
		    (if (eql (aref name i) #\-)
			(setf upper t)
			(progn
			  (if (and upper (eql (aref name i) #\>))
			      (progn(setf (aref new-name j) #\-)
				    (incf j)
				    (setf (aref new-name j) #\>)
				    (incf j))
			      (progn
				(setf (aref new-name j)
				      (if (or upper all-upper)
					  (char-upcase (aref name i))
					  (char-downcase (aref name i))))
				(incf j)))
			  (setf upper nil))))))))))

(defprinter (symbol x)
  ;; TODO: use aif
  (let ((print-syntax (cdr (assoc x *symbol-print-names*))))
    (if print-syntax
	(write-str print-syntax)
	(write-str (symbol-name-to-php (symbol-name x))))))
    
(defun php-escape-string-double (x)
  (with-output-to-string (s)
    (dotimes (i (length x))
      (let ((c (aref x i)))
	(write-string
	 (case c 
	   (#\Newline "\\n")
	   (#\Tab "\\t")
	   (#\Return "\\r")
	   (#\PageUp "\\v")
	   (#\Page "\\f")
	   (#\\ "\\\\")
	   (#\$ "\\$")
	   (#\" "\\\"")
	   ;; TODO: improve the logic of what char codes must be escaped
	   (t (if (graphic-char-p c)
		  (format nil "~C" c)
		  (format nil "\\0~O" (char-code c)))))
	 s)))))

(defun php-escape-string-single (x)
  (with-output-to-string (s)
    (dotimes (i (length x))
      (let ((c (aref x i)))
	(write-string
	 (case c
	   (#\' "\\'")
	   (#\\ "\\\\")
	   (t (format nil "~C" c)))
	 s)))))

(defun representable-as-signle-quote (x)
  (not (find-if-not #'graphic-char-p x)))

(defprinter (string x)
  (if (representable-as-signle-quote x)
      (progn
	(write-str "'")
	(write-str (php-escape-string-single x))
	(write-str "'"))
      (progn
	(write-str "\"")
	(write-str (php-escape-string-double x))
	(write-str "\""))))

(defprinter (null x 1)
  ;; do nothing
  )

(defmacro defspecialform (form &body body)
  (let ((arg (gensym)))
    `(progn (pushnew ',(first form) *special-forms*)
	    (set-php-pprint-dispatch '(cons (member ,(first form)))
				     (lambda (stream ,arg)
				       (handler-bind ((error (lambda (c)
							       (declare (ignore c))
							       (error "Invalid number of arguments for special form ~a" ',(first form)))))
					 (destructuring-bind ,form ,arg
					   #+SBCL(declare (sb-ext:muffle-conditions cl:style-warning))))
				       (destructuring-bind ,form ,arg
					 (declare (ignorable ,(first form)))
					 (labels ((fmt (&rest args) (apply #'format stream args))
						  (write-str (arg) (cl:write-string arg stream)))
					   ,@body)))))))

(defmacro undefspecialform (form)
  `(progn
     (delete ',form *special-forms*)
     (set-php-pprint-dispatch '(cons (member ,form)) nil)))

;; descriptions of varios operations
(defvar *ops* (loop
		 for ops in (reverse
			     ;; op php-op associativity arity
			     ;; each sublist contains ops of the same priority
			     '(((ref "->" :left 2 :skip-first-space t :skip-second-space t))
			       ((funcall "" :left 1 :noauto t))
			       ((clone "clone" :none 1 :makespace t)
				(new "new" :none 1 :makespace t))
			       ((aref "" :left 2 :noauto t))
			       ((inc "++" :none 1)
				(dec "--" :none 1)
				(postinc "" :none 1 :noauto t)
				(postdec "" :none 1 :noauto t))
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
			       ((%concat "." :left 2))
			       ((<< "<<" :left 2)
				(>> ">>" :left 2))
			       ((< "<" :left 2)
				(<= "<=" :none 2)
				(> ">" :none 2)
				(>= ">=" :none 2))
			       ((/= "!=" :none 2)
				(= "==" :none 2)
				(eq "===" :none 2)
				(neq "!==" :none 2))
			       ((getref "&" :left 1))
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
				(\&= "&=" :right 2)
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

(defun make-binary-op (php-op assoc precedence &rest keys &key skip-first-space skip-second-space &allow-other-keys)
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
      (unless skip-second-space (write-char #\Space s))
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

(defun tagbody-p (exp)
  (cons-op-p exp 'tagbody))

(defun find-op (op arity)
  (find-if (lambda (op-details) (and (eq op (second op-details))
				     (eql arity (fifth op-details))))
	   *ops*))

(defun eq-t-p (x)
  (eq t x))

(defun postinc-p (exp)
  (cons-op-p exp 'postinc))

(defun postdec-p (exp)
  (cons-op-p exp 'postdec))

(defun special-form-p (form)
  (and (consp form)
       (member (first form) *special-forms*)))

(set-php-pprint-dispatch '(satisfies unary-minus-p)
			 (make-unary-op "-" :none (car (find-op '- 1))))

(defmacro postfix-op (typespec op php-op)
  `(set-php-pprint-dispatch ',typespec
			    (let ((precedence (car (find-op ',op 1))))
			      (lambda (s op)
				(in-op-pprint-block
				  (assert (= 2 (length op)))
				  (let ((*B* precedence))
				    (write (pprint-pop) :stream s))
				  (write-string ,php-op s))))))

(postfix-op (satisfies postinc-p) postinc "++")
(postfix-op (satisfies postdec-p) postdec "--")

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

(set-php-pprint-dispatch '(satisfies tagbody-p)
			 (lambda (s form)
			   (format s "~{~W~:[~:[;~;~]~;:~*~]~^ ~@:_~}"
				   (mapcan (lambda (form)
					     (list form (atom form) (special-form-p form)))
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
