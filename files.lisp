(in-package :php)

(defparameter *document-root*
  #+SBCL(merge-pathnames (make-pathname :directory '(:relative "Sites"))
			 (parse-namestring (concatenate 'string (sb-ext:posix-getenv "HOME") "/")))
  #+CCL(make-pathname :host "home" :directory '(:absolute "Sites")))

(defun rootpath (path)
  (merge-pathnames (parse-namestring path) *document-root*))

(defun write-php-file (pathname &rest code)
  (with-open-file (out pathname
		       :direction :output :if-exists :supersede)
    (write-string "<?php" out)
    (terpri out)
    (phpize out (cons 'progn code))
    (fresh-line out)
    (write-string "?>" out)
    (terpri out)))

(defun write-js-file (pathname &rest code)
  (with-open-file (out pathname
		       :direction :output :if-exists :supersede)
	 (parenscript:compile-script (list 'progn code) :output-stream out)
	 (fresh-line out)))

(defmacro php (path &body code)
  `(apply #'write-php-file ,(rootpath path) ',code))

(defmacro js (path &body code)
  `(apply #'write-js-file ,(rootpath path) ',code))
