;;; Dump of interface repository generated by dump-ifr.lisp

(provide 'loadidl-clorb_ex-hello)

(require 'corba)

(corba-load-repository '
(repository nil
 (module "::CLORB_EX"
  (interface "::CLORB_EX::HelloWorld" nil
   (:tk_objref "IDL:CLORB_EX/HelloWorld:1.0" "HelloWorld")
   (operation "greet" :op_normal (:tk_string 0) nil nil)))))

