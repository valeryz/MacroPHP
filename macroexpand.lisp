(in-package :php)

(defvar *symbol-macro-expanders* nil
  "macro expanders produced as a result of symbol-macrolet")

(defvar *macro-expanders* nil "macro expanders")

(defun find-symbol-expander (symbol)
  (cdr (assoc symbol *symbol-macro-expanders*)))

(defun find-macro-expander (symbol)
  (cdr (assoc symbol *macro-expanders*)))

(defmacro macro-php-special (template &body body)
  `(lambda (form)
     (destructuring-bind ,template form
       (declare (ignorable ,(first template)))
       (let ((expanded-p))
	 (flet ((expand (form)
		  (multiple-value-bind (expanded-form exp-p)
		      (php/macroexpand form)
		    (when exp-p
		      (setf expanded-p t))
		    expanded-form)))
	   (let ((expansion ,@body))
	     (values expansion expanded-p)))))))

(defvar *generic-application-expander*
  (macro-php-special (function-position &rest body)
    `(,function-position ,@(mapcar #'expand body)))
  "expansion function for the generic application: (function arg1 arg2 ...)")

(defmacro defmacro-php-special (template &body body)
  `(progn (setf *macro-expanders* (delete ',(first template) *macro-expanders*))
	  (push (cons ',(first template)
		      (macro-php-special ,template ,@body))
		*macro-expanders*)))

;; we define expanstion only for those special forms which are expanded
;; differently from the simple function application
;; i.e. we don't provide a special case for while, do-while, progn etc.

(defmacro-php-special (for (&optional init cond step) &rest body)
  `(for (,(expand init) ,(expand cond) ,(expand step))
	,@(mapcar #'expand body)))

(defmacro-php-special (cond &rest clauses)
  `(cond ,@(mapcar (lambda (clause) (mapcar #'expand clause)) clauses)))

(defmacro-php-special (foreach (array-expr binding) &rest body)
  `(foreach (,(expand array-expr) ,binding)
	    ,@(mapcar #'expand body)))

;; switch
(defmacro-php-special (switch exp &rest cases)
  `(switch ,(expand exp)
     ,@(mapcar (lambda (case) (mapcar #'expand case)) cases)))

;; function
(defmacro-php-special (function name (&rest vars) &rest body)
  `(function ,name ,vars ,@(mapcar #'expand body)))

;; class
(defmacro-php-special (class name (&optional base) &rest body)
  `(class ,name (,base) ,@(mapcar #'expand body)))

(defmacro-php-special (var variable &optional init)
  (list* 'var variable (when init (list (expand init)))))

(defmacro defmacro-php (name lambda-list &body body)
  `(progn (setf *macro-expanders* (delete ',name *macro-expanders*))
	  (push (cons ',name
		      (lambda (form)
			(destructuring-bind ,(cons name lambda-list) form
			  (declare (ignorable ,name))
			  (values (progn ,@body) t))))
		*macro-expanders*)))

(defun php/macroexpand (form)
  "recursively macroexpand form once"
  ;; a real evaluator / code-walker is needed here
  (cond ((null form) (values nil niL))
	((symbolp form)
	 ;; check if symbol is a macrolet
	 (aif (find-symbol-expander form)
	      (values (funcall it form) t)
	      (values form nil)))
	 ((atom form)
	  (values form nil))
	 (t ;; list
	  (if (symbolp (car form))
	      (aif (find-macro-expander (car form))
		   (funcall it form)
		   ;; generic application
		   (funcall *generic-application-expander* form))
	      (values form nil)))))
	       
(defun php/macroexpand-all (form)
  "macroexpand repeatedly until everything is expanded"
  (do ((expanded-p t))
      ((not expanded-p) form)
    (multiple-value-setq (form expanded-p) (php/macroexpand form))))


;; (defmacro-php defmacro ((&rest lambda-list) &rest body)
  

(defmacro-php if (cond then &rest else)
  `(cond (,cond ,then) (t ,@else)))

;; just to test it
(defmacro-php inc (x)
  `(setq ,x (+ 1 ,x)))

