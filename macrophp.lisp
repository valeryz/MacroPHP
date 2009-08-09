
(defvar *php-pprint-dispatch* (copy-pprint-dispatch))

(defun phpize (x)
  (let ((*print-pprint-dispatch* *php-pprint-dispatch*))
    (write x :pretty t)
    (values)))

(defmacro defprinter (typespec obj &body body)
  (let ((stream (gensym)))
    `(set-pprint-dispatch ',typespec
			  (lambda (,stream ,obj)
			    (macrolet ((fmt (&rest args) `(format ,',stream ,@args)))
			      ,@body))
			  0
			  *php-pprint-dispatch*)))

(defprinter symbol x
  (fmt "$~A" (string-downcase (symbol-name x))))

(defmacro defspecialform (form &body body)
  (let ((stream (gensym))
	(arg (gensym)))
    `(set-pprint-dispatch '(cons (member ,(first form)))
			  (lambda (,stream ,arg)
			    (destructuring-bind ,form ,arg
			      (declare (ignorable ,(first form)))
			      (macrolet ((fmt (&rest args) `(format ,',stream ,@args)))
				,@body))
			    0
			    *php-pprint-dispatch*))))
  
(defun pprint-block (stream block &optional colon at-sign)
  (declare (ignore colon at-sign))
  (format stream  "~@<{~;~8I~:@_~{~W~^~:@_~}~-8I~:@_~;}~:>" block))

;; defspecialform should allow destructuring, and make patterns
;; from destructuring

(defspecialform (if cond true false)
    (fmt "~<~<if (~;~W~;)~:>~/pprint-block/else~/pprint-block/~:>" cond true false))
     
(defspecialform form
  (fmt "~<~W = ~W;~:>"
	  (list (second obj)
		(third obj))))

