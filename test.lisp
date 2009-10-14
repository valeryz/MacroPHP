
(in-package :php)

(php-file "db_conn.php"
  (setq $db (new (*PDO "mysql:host=localhost;dbname=mamulya" "mamulya" "h0czg1VM"))))

(php-file "header.php"
  (function standard-header ($title)
	    (echo (concat (concat "<html><head><title>"
				  $title)
			  "</title></head><body>")))
  (function standard-footer ()
	    (echo "</body></html>")))

(defmacro-php show-column (row name)
  `(progn
     (echo ,(cl:concatenate 'string "<div class='" name "'>"))
     (echo (aref ,row ,name))
     (echo "</div>")))

(defmacro-php ul (name &body body)
  `(progn
     (macrolet ((li (row &rest cols)
		  `(progn (echo (concat ,',(concatenate 'string "<li id='" name "-") (aref $row "id") "'>"))
			  ,@(loop for c in cols collect `(show-column ,row ,c))
			  (echo "</li>"))))
       (echo ,(concatenate 'string "<ul id='" name "'>"))
       ,@body
       (echo "</ul>"))))

(php-file "goods.php"
  (require-once "db_conn.php")
  (require "header.php")
  (when (and (isset (aref *$_POST* "action"))
	     (= (aref *$_POST* "action") "add"))
    ;; server side validation
    (try ((-p-d-o-exception $e
			    (echo (concat "error" $e))
			    (echo "failed"))
	  (-exception $e
		      (echo "general error")))
	 ($db->begin-transaction)
	 (let (($stmt ($db->prepare "insert into goods (name, description, qty, price) values (?, ?, ?, ?)")))
	   ($stmt->bindParam 1 (aref $_POST "name"))
	   ($stmt->bindParam 2 (aref $_POST "description"))
	   ($stmt->bindParam 3 (aref $_POST "qty"))
	   ($stmt->bindParam 4 (aref $_POST "price"))
	   ($stmt->execute))
	 ($db->commit)))
  (standard-header "The goods!!!")
  (ul "goodslist"
      (foreach (($db->query "select id, name, description, qty, price from goods")
		$row)
	(li $row "name" "description" "price" "qty")))
  (standard-footer))

(defvar *php-form-generators* nil "PHP code for generating forms")
(defvar *php-form-handlers* nil "PHP code for handling forms")

(cl-interpol:enable-interpol-syntax)

(defun make-form (name fields model &rest kargs &key &allow-other-keys)
  "make PHP and Javascript code parts that display, validate and process a form"
  ;; TODO: add JS validation
  (setf *php-form-generators* (delete name *php-form-generators* :key #'car))
  (push (apply #'make-php-form-generator name fields model kargs) *php-form-generators*)
  (setf *php-form-handlers* (delete name *php-form-handlers* :key #'car))
  (push (apply #'make-php-form-handler name fields kargs) *php-form-handlers*))

(defun make-php-form-generator (form model &key href)
  "example of the fields format

  (:fieldset (:legend \"Enter the items details\") (:text :name :name :label \"Name of the goods item\" :size 10 :maxlength 20))
  "
  (let ((have-submit))
    (labels ((make-form (form)
	       (cond ((null form) nil)
		     ((atom form)
		      (error "Invalid form description"))
		     (t (ecase (first form)
			(:fieldset
			 `(progn (echo "<fieldset>")
				 ,@(mapcar (lambda (x) (make-form x)) (rest form))
				 (echo "</fieldset>")))
			(:legend
			 (list #?"<legend><span>${(second form)}</span></legend>"))
			((:text :password)
			 (destructuring-bind (&key name label size maxlength)
			     (rest form)
			   (let* ((slot-def (model-slot model name)))
			     (let ((default-size (slot-size slot-def))
				   (default-maxlength (slot-maxlength slot-def))
				   (type (first form)))
			       (list 'echo (format nil "<label for='~a'>~a</label>
<input type='~a' class='~:*~a' id='~a' name='~a' ~@[size='~a' ~]~@[maxlength='~a' ~]value='${~a}' />"
						   name label
						   (string-downcase (string type)) name name
						   (or size default-size)
						   (or maxlength default-maxlength) name))))))
			(:submit
			 (destructuring-bind (&key name title)
			     (rest form)
			   (setf have-submit t)
			   (list (format nil "<input type='submit' name='~a' value='~a' />"
				 name title))))
			(:textarea
			 (destructuring-bind (&key name rows cols value)
			     (rest form)
			   (list 'echo (format nil
					      "<textarea class='textarea' name='~a' ~@[rows='~a' ~]~@[cols='~a' ~]>~a</textarea>"
					      name rows cols value)))))))))
      ;; why did I need the have-sibmit?
      (make-form form))))
#|
(defpackage :mamulya
  (:use :cl))

(in-package :mamulya)

(deftable categories
    "a table of categories"
  (name (:varchar 100))
  (slug (:varchar 20)))

(deftable goods
    "what we sell"
  (name (:varchar 100))
  (category :intrger (:foreign-key categories))
  (description :text)
  (price :decimal)
  (qty :integer))


(defwidget categories-list (table-list :table categories :text name :link (lambda () (ps (show-goods slug))))

(defwidget goods-list (paginated-table-list :table goods :text name :link slug))

(defwidget goods-selection (composite categories-list goods-list))

|#

#|

The code that should be produced:

Display of the goods, browseable by category (paginated list plus categories)
PHP code that produces the following:

<div id="goods-selection">
<div id="categories-list">
<ul>
<li><a href="javascript:show_goods('cat1')">Category 1</a></li>
<li><a href="javascript:show_goods('cat2')">Category 2</a></li>
</ul>
</div>
<div id="goods-list">
<ul>
<li><a href="product-details/prod1">Product 1</a></li>
</ul>
</div>
</div>

|#
