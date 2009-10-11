(in-package :php)

(defparameter *document-root*
  #+SBCL(merge-pathnames (make-pathname :directory '(:relative "Sites"))
			 (parse-namestring (concatenate 'string (sb-ext:posix-getenv "HOME") "/")))
  #+CCL(make-pathname :host "home" :directory '(:absolute "Sites")))

(defun rootpath (path)
  (merge-pathnames (parse-namestring path) *document-root*))

(defun write-php-file (pathname code)
  (with-open-file (out pathname
		       :direction :output :if-exists :supersede)
    (format out "<?php
/*
 * This file is generated with the MacroPHP tool
 * http://github.com/valeryz/MacroPHP
 * DO NOT EDIT!
 */
")
    (phpize out code)
    (fresh-line out)
    (write-string "?>" out)
    (terpri out)))

(defun write-js-file (pathname code)
  (with-open-file (out pathname
		       :direction :output :if-exists :supersede)
	 (parenscript:compile-script code :output-stream out)
	 (fresh-line out)))

(defmacro php-file (path &body code)
  `(write-php-file ,(rootpath path) ',(cons 'progn code)))

(defmacro js-file (path &body code)
  `(write-js-file ,(rootpath path) ',(cons 'progn code)))
