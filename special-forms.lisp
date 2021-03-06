(in-package :php)

(defun must-enclose-in-braces (body check-if)
  "check if the body must be enclosed in braces, i.e. the body has more
than one member or the body is an 'if' or 'cond' and it is a part of an enclosing
if or cond"
  (assert (listp body))
  (or (cdr body) ;; more than one
      (and check-if
	   (listp (car body))
	   (find (caar body) '(if cond)))))

(defun remove-progn (body)
  (assert (listp body))
  (if (or (cdr body)
	  (not (eq 'progn (caar body))))
      body
      (remove-progn (cdar body))))

(defun remove-nested-progn (body)
  (assert (listp body))
  (loop for form in body
     append (if (progn-p form)
		(remove-nested-progn (cdr form))
		(list form))))
	 
(defun print-body (stream body &optional no-indent empty-semicolon)
  (let ((body (if (listp body) body (list body)))
	(*expect-statement* t))
      (unless no-indent (format stream "~8I~:@_"))
      (when (and empty-semicolon (null body))
	(write-string ";" stream))
      (format stream "~{~W~:[;~;~]~^ ~@:_~}"
	      (mapcan (lambda (form)
			(list form (no-semicolon-p form)))
		      body))
      (unless no-indent (format stream "~0I~:@_"))))

(defun ctl-body (stream body &optional check-if trailing-space enclose)
  (let ((body (remove-progn (remove-nested-progn body))))
    (if (or enclose (must-enclose-in-braces body check-if))
	(progn
	  (write-string " {" stream)
	  (print-body stream body)
	  (write-string "}" stream)
	  (if trailing-space
	      (write-string " " stream)))
	(print-body stream body nil t))))

(defun def-body (stream body &optional colon-p at-sign-p)
  (declare (ignore colon-p at-sign-p))
  (write-string "{" stream)
  (print-body stream body)
  (write-string "}" stream))

(defspecialform (while cond &rest body)
  (pprint-logical-block (stream nil)
    (fmt "while (~W)" cond)
    (ctl-body stream body)))

(defspecialform (for (&optional init cond step) &rest body)
  (let ((*expect-statement* nil))
    (pprint-logical-block (stream nil)
      (fmt "<for (~<~W; ~:_~W; ~:_~W~:>)~/php::ctl-body/"
	   (list init cond step) body))))

(defspecialform (cond &rest conditions)
  (flet ((clause (c &key initial else)
	   (destructuring-bind (cond &rest body)
	       c
	     (fmt "~:[~:[elseif~;if~] (~W)~;else~*~*~]" else initial cond)
	     (ctl-body stream body t t))))
    (pprint-logical-block (stream nil)
      (clause (first conditions) :initial t)
      (loop for c in (rest conditions)
	 do (clause c :else (eq-t-p (first c)))))))

(defspecialform (do-while while &rest body)
  (pprint-logical-block (stream nil)
    (fmt "do {")
    (print-body stream body)
    (fmt "} while (~W)" while)))

(defspecialform (foreach (array-expr binding) &rest body)
  (pprint-logical-block (stream nil)
    (if (consp binding)
	(fmt "foreach (~W as ~W => ~W)~/php::ctl-body/"
	     array-expr (first binding) (second binding) body)
	(fmt "foreach (~W as ~W)~/php::ctl-body/"
	     array-expr binding body))))

(defspecialform (progn &rest body)
  (let ((body (remove-nested-progn body)))
    (if *expect-statement*
	(pprint-logical-block (stream nil)
	  (if *toplevel-progn*
	      (let ((*toplevel-progn*))
		(print-body stream body t))
	      (progn
		(write-string "{" stream)
		(print-body stream body)
		(write-string "}" stream))))
	(format stream "~{~W~^, ~}" body))))
					   
(defmacro break/continue (stmt)
  `(defspecialform (,stmt &optional (level 1 level-provided-p))
     (assert (plusp level))
     (write-str ,(string-downcase (symbol-name stmt)))
     (if level-provided-p
	 (fmt " ~W;" level))))
	 
(break/continue break)
(break/continue continue)

(defspecialform (switch exp &rest cases)
  (pprint-logical-block (stream nil)
    (fmt "switch (~W) {~0I~:@_" exp)
    (loop for case in cases
       do
       (if (or (eq (first case) 'otherwise)
	       (eq-t-p (first case)))
	   (write-str "default:")
	   (fmt "case ~W:" (first case)))
	 (print-body stream (rest case)))
    (fmt "}~0I~:@_")))

(defspecialform (tagbody &rest body)
  (loop for form in body
     do (if (atom form)
	    (fmt "~W:~:@_" form)
	    (fmt "~W~:@_" form))))

(defmacro oneargspecial (name &optional str-name)				  
  `(defspecialform (,name exp)
     (write-str (or ,str-name ,(string-downcase (symbol-name name))))
     (fmt " ~@_~W" exp)))

(oneargspecial return)
(oneargspecial include)
(oneargspecial require)
(oneargspecial include-once "include_once")
(oneargspecial require-once "require_once")
(oneargspecial echo)
(oneargspecial go "goto")
(oneargspecial throw)

(defspecialform (function name (&rest vars) &rest body)
  (pprint-logical-block (stream nil)
    (fmt "function ~:[~*~;~W~](~{~W~^, ~})~:@_~/php::def-body/"
	 name name vars body)))

(defspecialform (class name (&optional base) &rest body)
  (pprint-logical-block (stream nil)
    (fmt "class ~:[~*~;~W~]~:[~; extends ~W~]~:@_~/php::def-body/" name name base body)))

;; TODO: implement comment wrapping
(defspecialform (comment text &rest body)
  (fmt "/* ~S */~:@_~:/php::print-body/" text body))

(defspecialform (var variable &optional init)
  (fmt "var ~W~:[~; = ~:*~W~]" variable init))

(defspecialform (array &rest init)
  (fmt "array(~{~:[~W~;~:*~W => ~W~]~^, ~})"
       (mapcan (lambda (arg) (if (and (listp arg)
				      (eq (first arg) '=>))
				 (list (second arg) (third arg))
				 (list nil arg)))
	       init)))

(defspecialform (try (&rest handlers) &body body)
  (pprint-logical-block (stream nil)
    (fmt "try~@/php::ctl-body/" body)
    (loop for (exc-class exc . body) in handlers
       do
	 (fmt "catch (~W ~W)" exc-class exc)
	 (ctl-body stream body nil t t))))
