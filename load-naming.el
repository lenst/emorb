
(corba-load-repository
 '(repository nil
   (module "::CosNaming"
    (type "::CosNaming::Istring"
     (:tk_alias "IDL:omg.org/CosNaming/Istring:1.0" "Istring" (:tk_string 0)))
    (struct "::CosNaming::NameComponent"
     (:tk_struct "IDL:omg.org/CosNaming/NameComponent:1.0" "NameComponent"
                 (("id" "IDL:omg.org/CosNaming/Istring:1.0")
                  ("kind" "IDL:omg.org/CosNaming/Istring:1.0"))))
    (type "::CosNaming::Name"
     (:tk_alias "IDL:omg.org/CosNaming/Name:1.0" "Name"
                (:tk_sequence "IDL:omg.org/CosNaming/NameComponent:1.0" 0)))
    (type "::CosNaming::BindingType"
     (:tk_enum "IDL:omg.org/CosNaming/BindingType:1.0" "BindingType"
               ("nobject" "ncontext")))
    nil nil
    (struct "::CosNaming::Binding"
     (:tk_struct "IDL:omg.org/CosNaming/Binding:1.0" "Binding"
                 (("binding_name" "IDL:omg.org/CosNaming/Name:1.0")
                  ("binding_type" "IDL:omg.org/CosNaming/BindingType:1.0"))))
    (type "::CosNaming::BindingList"
     (:tk_alias "IDL:omg.org/CosNaming/BindingList:1.0" "BindingList"
                (:tk_sequence "IDL:omg.org/CosNaming/Binding:1.0" 0)))
    (interface "::CosNaming::BindingIterator" nil
     (:tk_objref "IDL:omg.org/CosNaming/BindingIterator:1.0" "BindingIterator")
     (operation "next_one" :op_normal (:tk_boolean)
                (("b" :param_out "IDL:omg.org/CosNaming/Binding:1.0")) nil)
     (operation "next_n" :op_normal (:tk_boolean)
                (("how_many" :param_in (:tk_ulong))
                 ("bl" :param_out "IDL:omg.org/CosNaming/BindingList:1.0"))
                nil)
     (operation "destroy" :op_normal (:tk_void) nil nil))
    (interface "::CosNaming::NamingContext" nil
     (:tk_objref "IDL:omg.org/CosNaming/NamingContext:1.0" "NamingContext")
     (type "::CosNaming::NamingContext::NotFoundReason"
           (:tk_enum "IDL:omg.org/CosNaming/NamingContext/NotFoundReason:1.0"
                     "NotFoundReason" ("missing_node" "not_context" "not_object")))
     nil nil nil
     (exception "::CosNaming::NamingContext::NotFound"
                (:tk_except "IDL:omg.org/CosNaming/NamingContext/NotFound:1.0" "NotFound"))
     (exception "::CosNaming::NamingContext::CannotProceed"
                (:tk_except "IDL:omg.org/CosNaming/NamingContext/CannotProceed:1.0"
                            "CannotProceed"))
     (exception "::CosNaming::NamingContext::InvalidName"
                (:tk_except "IDL:omg.org/CosNaming/NamingContext/InvalidName:1.0"
                            "InvalidName"))
     (exception "::CosNaming::NamingContext::AlreadyBound"
                (:tk_except "IDL:omg.org/CosNaming/NamingContext/AlreadyBound:1.0"
                            "AlreadyBound"))
     (exception "::CosNaming::NamingContext::NotEmpty"
                (:tk_except "IDL:omg.org/CosNaming/NamingContext/NotEmpty:1.0" "NotEmpty"))
     (operation "bind" :op_normal (:tk_void)
                (("n" :param_in "IDL:omg.org/CosNaming/Name:1.0")
                 ("obj" :param_in (:tk_objref "IDL:omg.org/CORBA/Object:1.0" "Object")))
                ("IDL:omg.org/CosNaming/NamingContext/NotFound:1.0"
                 "IDL:omg.org/CosNaming/NamingContext/CannotProceed:1.0"
                 "IDL:omg.org/CosNaming/NamingContext/InvalidName:1.0"
                 "IDL:omg.org/CosNaming/NamingContext/AlreadyBound:1.0"))
     (operation "rebind" :op_normal (:tk_void)
                (("n" :param_in "IDL:omg.org/CosNaming/Name:1.0")
                 ("obj" :param_in "IDL:omg.org/CORBA/Object:1.0"))
                ("IDL:omg.org/CosNaming/NamingContext/NotFound:1.0"
                 "IDL:omg.org/CosNaming/NamingContext/CannotProceed:1.0"
                 "IDL:omg.org/CosNaming/NamingContext/InvalidName:1.0"))
     (operation "bind_context" :op_normal (:tk_void)
                (("n" :param_in "IDL:omg.org/CosNaming/Name:1.0")
                 ("nc" :param_in "IDL:omg.org/CosNaming/NamingContext:1.0"))
                ("IDL:omg.org/CosNaming/NamingContext/NotFound:1.0"
                 "IDL:omg.org/CosNaming/NamingContext/CannotProceed:1.0"
                 "IDL:omg.org/CosNaming/NamingContext/InvalidName:1.0"
                 "IDL:omg.org/CosNaming/NamingContext/AlreadyBound:1.0"))
     (operation "rebind_context" :op_normal (:tk_void)
                (("n" :param_in "IDL:omg.org/CosNaming/Name:1.0")
                 ("nc" :param_in "IDL:omg.org/CosNaming/NamingContext:1.0"))
                ("IDL:omg.org/CosNaming/NamingContext/NotFound:1.0"
                 "IDL:omg.org/CosNaming/NamingContext/CannotProceed:1.0"
                 "IDL:omg.org/CosNaming/NamingContext/InvalidName:1.0"))
     (operation "resolve" :op_normal "IDL:omg.org/CORBA/Object:1.0"
                (("n" :param_in "IDL:omg.org/CosNaming/Name:1.0"))
                ("IDL:omg.org/CosNaming/NamingContext/NotFound:1.0"
                 "IDL:omg.org/CosNaming/NamingContext/CannotProceed:1.0"
                 "IDL:omg.org/CosNaming/NamingContext/InvalidName:1.0"))
     (operation "unbind" :op_normal (:tk_void)
                (("n" :param_in "IDL:omg.org/CosNaming/Name:1.0"))
                ("IDL:omg.org/CosNaming/NamingContext/NotFound:1.0"
                 "IDL:omg.org/CosNaming/NamingContext/CannotProceed:1.0"
                 "IDL:omg.org/CosNaming/NamingContext/InvalidName:1.0"))
     (operation "new_context" :op_normal
                "IDL:omg.org/CosNaming/NamingContext:1.0" nil nil)
     (operation "bind_new_context" :op_normal
                "IDL:omg.org/CosNaming/NamingContext:1.0"
                (("n" :param_in "IDL:omg.org/CosNaming/Name:1.0"))
                ("IDL:omg.org/CosNaming/NamingContext/NotFound:1.0"
                 "IDL:omg.org/CosNaming/NamingContext/AlreadyBound:1.0"
                 "IDL:omg.org/CosNaming/NamingContext/CannotProceed:1.0"
                 "IDL:omg.org/CosNaming/NamingContext/InvalidName:1.0"))
     (operation "destroy" :op_normal (:tk_void) nil
                ("IDL:omg.org/CosNaming/NamingContext/NotEmpty:1.0"))
     (operation "list" :op_normal (:tk_void)
                (("how_many" :param_in (:tk_ulong))
                 ("bl" :param_out "IDL:omg.org/CosNaming/BindingList:1.0")
                 ("bi" :param_out "IDL:omg.org/CosNaming/BindingIterator:1.0"))
                nil))
    (interface "::CosNaming::NamingContextExt"
     ("IDL:omg.org/CosNaming/NamingContext:1.0")
     (:tk_objref "IDL:omg.org/CosNaming/NamingContextExt:1.0" "NamingContextExt")
     (type "::CosNaming::NamingContextExt::StringName"
           (:tk_alias "IDL:omg.org/CosNaming/NamingContextExt/StringName:1.0"
                      "StringName" (:tk_string 0)))
     (type "::CosNaming::NamingContextExt::Address"
           (:tk_alias "IDL:omg.org/CosNaming/NamingContextExt/Address:1.0" "Address"
                      (:tk_string 0)))
     (type "::CosNaming::NamingContextExt::URLString"
           (:tk_alias "IDL:omg.org/CosNaming/NamingContextExt/URLString:1.0"
                      "URLString" (:tk_string 0)))
     (operation "to_string" :op_normal
                "IDL:omg.org/CosNaming/NamingContextExt/StringName:1.0"
                (("n" :param_in "IDL:omg.org/CosNaming/Name:1.0"))
                ("IDL:omg.org/CosNaming/NamingContext/InvalidName:1.0"))
     (operation "to_name" :op_normal "IDL:omg.org/CosNaming/Name:1.0"
                (("sn" :param_in "IDL:omg.org/CosNaming/NamingContextExt/StringName:1.0"))
                ("IDL:omg.org/CosNaming/NamingContext/InvalidName:1.0"))
     (exception "::CosNaming::NamingContextExt::InvalidAddress"
                (:tk_except "IDL:omg.org/CosNaming/NamingContextExt/InvalidAddress:1.0"
                            "InvalidAddress"))
     (operation "to_url" :op_normal
                "IDL:omg.org/CosNaming/NamingContextExt/URLString:1.0"
                (("addr" :param_in "IDL:omg.org/CosNaming/NamingContextExt/Address:1.0")
                 ("sn" :param_in "IDL:omg.org/CosNaming/NamingContextExt/StringName:1.0"))
                ("IDL:omg.org/CosNaming/NamingContextExt/InvalidAddress:1.0"
                 "IDL:omg.org/CosNaming/NamingContext/InvalidName:1.0"))
     (operation "resolve_str" :op_normal "IDL:omg.org/CORBA/Object:1.0"
                (("n" :param_in "IDL:omg.org/CosNaming/NamingContextExt/StringName:1.0"))
                ("IDL:omg.org/CosNaming/NamingContext/NotFound:1.0"
                 "IDL:omg.org/CosNaming/NamingContext/CannotProceed:1.0"
                 "IDL:omg.org/CosNaming/NamingContext/InvalidName:1.0"
                 "IDL:omg.org/CosNaming/NamingContext/AlreadyBound:1.0"))))))

