
(setq tc-e1
      (corba-intern-type '(tk_enum "IDL:my-enum:1.0" "my-enum"
                           ("foo" "fie" "fumm"))))

(assert (equal (corba-enum-symbols tc-e1) '(:foo :fie :fumm)))

(corba-in-work-buffer
  (corba-marshal :fie tc-e1)
  (corba-marshal :fumm tc-e1)
  (goto-char (point-min))
  (assert (= (corba-read-ulong) 1))
  (assert (eq (corba-unmarshal tc-e1) :fumm)))


(defvar corba-services-running nil)

(when corba-services-running
  (corba-init)
;;(setq id "IDL:omg.org/GIOP/MsgType_1_1:1.0")
  (corba-get-typecode "IDL:omg.org/GIOP/MsgType_1_1:1.0")
  (corba-get-interface "IDL:omg.org/CosNaming/NamingContext:1.0")
  (setq id "IDL:omg.org/CosNaming/NamingContext:1.0")
  (setq def (corba-ir-lookup-id id))
  (setq opseq (corba-ir-contents def 7 t))
  (setq irdef (car opseq))
  (corba-opdef-from-ir irdef)
  (setq parseq (corba-get-typecode "IDL:omg.org/CORBA/ParDescriptionSeq:1.0"))
  (corba-get-attribute irdef "_get_params" parseq)
  (corba-resolve "hello")
  )


