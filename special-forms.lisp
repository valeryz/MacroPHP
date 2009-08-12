(in-package :php)

;; defspecialform should allow destructuring, and make patterns
;; from destructuring
(defspecialform (if cond true &optional (false nil have-false))
  (if have-false
      (apply #'fmt `("~@<if (~W)~8I~?else~8I~?~:>"
		   ,cond
		   ,@(multiple-value-list (pprint-block-format true))
		   ,@(multiple-value-list (pprint-block-format false))))
      (apply #'fmt `("~@<if (~W)~8I~?~:>"
		   ,cond
		   ,@(multiple-value-list (pprint-block-format true))))))
  

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
