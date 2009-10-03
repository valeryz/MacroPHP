
(defsystem "macrophp"
  :description "Macro Processor for High Productivity"
  :version "0.1"
  :author "Valeriy Zamarayev"
  :components ((:file "package")
	       (:file "utils" :depends-on ("package"))
	       (:file "macroexpand" :depends-on ("package" "utils"))
	       (:file "macrophp" :depends-on ("macroexpand"))
	       (:file "macros"  :depends-on ("macroexpand"))
	       (:file "special-forms" :depends-on ("macrophp")))
  :depends-on ("arnesi" "parenscript"))
