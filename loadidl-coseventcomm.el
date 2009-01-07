;;; loadidl-coseventcomm.el -- generated by corba.el's corba-make-loadidl

(provide 'loadidl-coseventcomm)

(corba-load-repository
  '(module "::CosEventComm"
        (exception "::CosEventComm::Disconnected"
                   (:tk_except "IDL:omg.org/CosEventComm/Disconnected:1.0" "Disconnected" nil))
        (interface "::CosEventComm::PushConsumer" nil
                   (:tk_objref "IDL:omg.org/CosEventComm/PushConsumer:1.0" "PushConsumer")
                   (operation "push" :OP_NORMAL
                              (:tk_void)
                              (("data" :PARAM_IN
                                (:tk_any)))
                              ("IDL:omg.org/CosEventComm/Disconnected:1.0"))
                   (operation "disconnect_push_consumer" :OP_NORMAL
                              (:tk_void)
                              nil nil))
        (interface "::CosEventComm::PushSupplier" nil
                   (:tk_objref "IDL:omg.org/CosEventComm/PushSupplier:1.0" "PushSupplier")
                   (operation "disconnect_push_supplier" :OP_NORMAL
                              (:tk_void)
                              nil nil))
        (interface "::CosEventComm::PullSupplier" nil
                   (:tk_objref "IDL:omg.org/CosEventComm/PullSupplier:1.0" "PullSupplier")
                   (operation "pull" :OP_NORMAL
                              (:tk_any)
                              nil
                              ("IDL:omg.org/CosEventComm/Disconnected:1.0"))
                   (operation "try_pull" :OP_NORMAL
                              (:tk_any)
                              (("has_event" :PARAM_OUT
                                (:tk_boolean)))
                              ("IDL:omg.org/CosEventComm/Disconnected:1.0"))
                   (operation "disconnect_pull_supplier" :OP_NORMAL
                              (:tk_void)
                              nil nil))
        (interface "::CosEventComm::PullConsumer" nil
                   (:tk_objref "IDL:omg.org/CosEventComm/PullConsumer:1.0" "PullConsumer")
                   (operation "disconnect_pull_consumer" :OP_NORMAL
                              (:tk_void)
                              nil nil)))
)


;;; loadidl-coseventcomm.el ends here
