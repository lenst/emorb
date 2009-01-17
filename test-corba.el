;;;; test-corba.el --- Test cases for corba.el

(require 'corba)



;;;; Mutable TC

(let ((tc (make-corba-typecode :tk_sequence '(:tk_null 0))))
  (corba-put tc :content_type corba-tc-string)
  (assert (equal tc `(:tk_sequence ,corba-tc-string 0))))



;;;; noir-resolve

(defun noir-resolve (ns name)
  (corba-funcall corba-tc-object "resolve_str" ns :in corba-tc-string name))



;;;; Services

(defvar corba-services-running t)

(when corba-services-running
  (corba-init)
  (assert (corba-object-p
           (car (corba-funcall corba-tc-object "resolve_str" (corba-get-ns)  
                               :in corba-tc-string "hello-lapps"))))
  ;;(setq id "IDL:omg.org/GIOP/MsgType_1_1:1.0")
  ;;(corba-get-typecode "IDL:omg.org/GIOP/MsgType_1_1:1.0")
  ;;(corba-get-interface "IDL:omg.org/CosNaming/NamingContext:1.0")
  (require 'corba-load-naming)
  (setq id "IDL:omg.org/CosNaming/NamingContext:1.0")
  ;;(setq def (corba-ir-lookup-id id))
  ;;(setq opseq (corba-ir-contents def 7 t))
  ;;(setq irdef (car opseq))
  ;;(corba-opdef-from-ir irdef)
  (require 'corba-load-ifr)
  (setq parseq (corba-typecode "IDL:omg.org/CORBA/ParDescriptionSeq:1.0"))
  ;;(corba-get-attribute irdef "_get_params" parseq)
  (require 'loadidl-clorb_ex-hello)
  (assert (corba-typep (corba-resolve "hello-lapps") "::CLORB_EX::HelloWorld")))
