;;
;; This model stuff follows the Django design very closely
;;

(in-package :php)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun gen-slots (slots)
    (mapcar (lambda (slot)
	      (let ((name (string (if (listp slot) (first slot) slot)))
		    (initform (if (listp slot) (second slot))))
		(list (find-symbol name) :initarg (intern name :keyword)
		      :initform initform :reader
		      (find-symbol name))))
	      slots))

  (defmacro defclass* (class-name bases slots &rest options)
    (list* 'defclass class-name bases (gen-slots slots) options)))

(defclass* field ()
  (allow-null blank choices db-column db-index default non-editable
	      help-text primary-key unique verbose-name)
   (:documentation "Base field class"))

(defclass* boolean-field (field)
  ()
  (:documentation "Boolean field"))

(defclass* char-field (field)
  (max-length)
  (:documentation "Char field"))

(defclass* email-field (char-field)
  ((max-length 75))
  (:documentation "Email field"))

(defclass* file-field (char-field)
  (upload-to (max-length 100))
  (:documentation "File-upload field"))

(defclass* image-field (file-field)
  (height-field width-field)
  (:documentation "Image Field"))

(defclass* url-field (char-field)
  (verify-exists)
  (:documentation "URL field"))

(defclass* ip-address-field (field)
  ()
  (:documentation "IP Address Field"))

(defclass* text-field (field)
  ()
  (:documentation "Large text are field"))

(defclass* xml-field (field)
  (schema-path)
  (:documentation "XML field with validation"))

(defclass* time-field (field)
  (auto-now auto-now-add)
  (:documentation "Time field"))

(defclass* date-field (field)
  (auto-now auto-now-add)
  (:documentation "Date field"))

(defclass* date-time-field (date-field)
  ()
  (:documentation "Date/Time field"))

(defclass* integer-field (field)
  ()
  (:documentation "Integer field"))

(defclass* positive-integer-field (integer-field)
  ()
  (:documentation "Positive Integer field"))

(defclass* small-integer-field (integer-field)
  ()
  (:documentation "Small Integer field"))

(defclass* positive-small-integer-field (small-integer-field positive-integer-field)
  ()
  (:documentation "Positive small integer field"))

(defclass* decimal-field (field)
  (max-digits decimal-places)
  (:documentation "Decimal Field"))

(defclass* float-field (field)
  ()
  (:documentation "Float field"))

(defclass* null-boolean-field (field)
  ()
  (:documentation "Null Boolean Field"))

(defclass* slug-field (field)
  ((db-index t)
   (maxlength 50))
  (:documentation "Slug field"))

(defclass* foreign-key ()
  (other-model)
  (:documentation "Foreign key to other model"))

;; TODO many-to-many, one-to-one etc. see Django docs

(defclass model ()
  ((name :initarg :name :reader model-name)
   (fields :initarg :fields :reader model-fields))
  (:documentation "Model class, defines a relation"))

(defgeneric model-field (model name)
  (:documentation "Retrive a field from the model"))

(defmethod model-field ((model model) name)
  (getf (model-fields model) name))
