
(defpackage :php 
  (:use :cl :parenscript)
  (:import-from :arnesi #:aif #:it)
  (:export 
   #:phpize
   #:defmacro-php
   #:php/macroexpand
   #:php/macroexpand-all
   #:funcall
   #:clone
   #:new
   #:aref
   #:inc
   #:dec
   #:postinc
   #:postdec
   #:~
   #:-
   #:@
   #:cast
   #:instanceof
   #:not
   #:*
   #:/
   #:mod
   #:+
   #:-
   #:%concat
   #:concat
   #:<<
   #:>>
   #:<
   #:<=
   #:>
   #:>=
   #:/=
   #:=
   #:ref
   #:logand
   #:logxor
   #:logior
   #:and
   #:or
   #:|?:|
   #:setq
   #:+=
   #:-=
   #:*=
   #:/=
   #:.=
   #:%=
   #:\&=
   #:\|=
   #:^=
   #:<<=
   #:>>=
   #:xor
   #:\,
   #:progn
   #:tagbody
   #:cond
   #:for
   #:foreach
   #:switch
   #:function
   #:class
   #:while
   #:do-while
   #:break
   #:continue
   #:return
   #:include
   #:require
   #:require-once
   #:include-once
   #:echo
   #:go
   #:comment
   #:var
   #:if
   #:when
   #:unless
   ))
