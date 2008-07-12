
(setq tc-e1
      (corba-typecode '(:tk_enum "IDL:my-enum:1.0" "my-enum"
                        ("foo" "fie" "fumm"))))

(assert (equal (corba-enum-symbols tc-e1) '(:foo :fie :fumm)))

(corba-in-work-buffer
  (corba-marshal :fie tc-e1)
  (corba-marshal :fumm tc-e1)
  (goto-char (point-min))
  (assert (= (corba-read-ulong) 1))
  (assert (eq (corba-unmarshal tc-e1) :fumm)))

(corba-in-work-buffer
  (corba-write-typecode tc-e1)
  (goto-char (point-min))
  (corba-read-typecode))


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
  (let ()
    (corba-funcall :noir "resolve_str" (corba-get-ns) corba-tc-object 
                   :in corba-tc-string "hello-lapps")))


(setq any (corba-any corba-tc-string "hello"))
(assert (equal (corba-any-value any) "hello"))
(assert (equal (corba-get any :any-value) "hello"))
(corba-put any :any-value "fisk")
(assert (equal (corba-get any :any-value) "fisk"))
(assert (equal (corba-get corba-tc-string :length) 0))
(assert (equal (corba-get corba-tc-object :name) "Object"))

(setq tc (make-corba-typecode :tk_sequence '(:tk_null 0)))
(corba-put tc :content_type corba-tc-string)
(assert (equal tc `(:tk_sequence ,corba-tc-string 0)))


(defun noir-resolve (ns name)
  (corba-funcall corba-tc-object "resolve_str" ns :in corba-tc-string name))
