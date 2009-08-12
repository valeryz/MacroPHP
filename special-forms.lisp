(in-package :php)

;; defspecialform should allow destructuring, and make patterns
;; from destructuring
(defspecialform (if cond true &optional (false nil have-false))
  (if have-false
      (apply #'fmt `("~@<if (~W)~8I~?else~8I~?~:>"
		   ,cond
		   ,@(multiple-value-list (pprint-block-format true t))
		   ,@(multiple-value-list (pprint-block-format false t))))
      (apply #'fmt `("~@<if (~W)~8I~?~:>"
		   ,cond
		   ,@(multiple-value-list (pprint-block-format true t))))))
  
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
