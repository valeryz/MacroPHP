
(in-package :php)

(defmacro-php if (cond then &rest else)
  `(cond (,cond ,then) ,@(if else (list (list* t else)))))

(defmacro-php when (cond &rest then)
  `(cond (,cond ,@then)))

(defmacro-php unless (cond &rest stmt)
  `(cond ((not ,cond) ,@stmt)))

;; just to test it
(defmacro-php inc (x)
  `(setq ,x (+ 1 ,x)))

(defmacro-php concat (&rest exprs)
  (cond ((null exprs) nil)
	((null (cdr exprs)) (car exprs))
	(t `(concat (%concat ,(car exprs) ,(cadr exprs))
		    ,@(cddr exprs)))))