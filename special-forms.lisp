(in-package :php)

;; defspecialform should allow destructuring, and make patterns
;; from destructuring
(defspecialform (if cond true &optional (false nil have-false))
  (if have-false
      (apply #'fmt `("~@<if (~W)~8I~?else~8I~?~:>"
		   ,cond
		   ,@(multiple-value-list (pprint-block-format true :check-if t))
		   ,@(multiple-value-list (pprint-block-format false :check-if t))))
      (apply #'fmt `("~@<if (~W)~8I~?~:>"
		   ,cond
		   ,@(multiple-value-list (pprint-block-format true :check-if t))))))
  
(defspecialform (while cond stmt)
  (apply #'fmt (list*
		"~@<while (~W)~8I~?~:>"
		cond
		(multiple-value-list (pprint-block-format stmt)))))
  
(defspecialform (for (&optional
		      (init nil have-init)
		      (cond nil have-cond)
		      (step nil have-step)) stmt)
  (apply #'fmt (list*
		"~@<for (~:[~*~;~W~]; ~:[~*~;~W~]; ~:[~*~;~W~])~8I~?~:>"
		have-init init
		have-cond cond
		have-step step
		(multiple-value-list (pprint-block-format stmt)))))

(defspecialform (cond &rest conditions)
  (flet ((clause (c &key initial else)
	   (destructuring-bind (cond stmt)
	       c
	     (apply #'fmt
		    (list* "~:[~:[elseif~;if~] (~W)~;else~*~*~]~8I~?"
			   else initial cond (multiple-value-list (pprint-block-format stmt)))))))
    (pprint-logical-block (stream nil)
      (clause (first conditions) :initial t)
      (loop for c in (rest conditions)
	 do (clause c :else (eq-t-p (first c)))))))

(defspecialform (do stmt while)
  (apply #'fmt (append (list* "~@<do~8I~?while (~W)~:>"
			      (multiple-value-list (pprint-block-format (if (or (progn-p stmt))
									    stmt
									    (list 'progn stmt)))))
		       (list while))))

(defspecialform (foreach (array-expr binding) stmt)
  (apply #'fmt (append (if (consp binding)
			   (list "~@<foreach (~W as ~W => ~W)~8I~?~:>"
				 array-expr (first binding) (second binding))
			   (list "~@<foreach (~W as ~W)~8I~?~:>"
				 array-expr binding))
		       (multiple-value-list (pprint-block-format stmt)))))
