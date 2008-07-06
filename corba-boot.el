(defconst corba-object-interface
  (make-corba-interface
   :id "IDL:omg.org/CORBA/Object:1.0"
   :operations (list
                (make-corba-opdef :name "_is_a"
                            :inparams (list corba-tc-string)
                            :outparams '((:tk_boolean))
                            :raises '())
                (make-corba-opdef :name "_interface"
                            :outparams (list corba-tc-object))
                (make-corba-opdef :name "_get_interface"
                            :outparams (list corba-tc-object))
                (make-corba-opdef :name "_non_existent"
                            :outparams '((:tk_boolean))))))


;; Interface:
(defun corba-add-interface (interface)
  (setf (gethash (corba-interface-id interface)
		 corba-local-repository)
	interface))


(defun corba-get-interface (id)
  (or (gethash id corba-local-repository)
      (puthash id (corba-interface-from-id id) corba-local-repository)))


(defun corba-get-typecode (id)
  (or (gethash id corba-local-typecode-repository)
      (setf (gethash id corba-local-typecode-repository)
	    (corba-typecode-from-def id))))


(defun corba-get-objects-interface (object)
  (condition-case exc
      (let ((idef
             (car (corba-invoke object "_get_interface"))))
        (if idef
            (corba-interface-from-def-cached idef)))
    (corba-system-exception
     (message "_get_interface: %s" exc)
     nil)
    (end-of-buffer
     ;; Work around ORBit bug in exception marshaling
     (message "_get_interface: %s" exc)
     nil)))


(defun corba-intern-type (typecode &optional force)
  ;; Make typecodes with repository ID be stored in the internal
  ;; typecode repository in a single instance. Usually used for
  ;; typecodes gotten from the Interface Repository.
  (let ((params (corba-typecode-params typecode)))
    (macrolet ((mush (&optional def)
                 `(or (and (not force) (corba-has-typecode-p (first params)))
                      (progn (corba-add-typecode-with-id (first params)
                                                         typecode)
                             ,(or def 'typecode))))
               (simplifyf (var)
                 `(progn (setf ,var (corba-intern-type ,var force))
                         typecode))
               (simplifyv (vec fun)
                 `(progn (loop for el in ,vec do (simplifyf (,fun el)))
                         typecode)))
      (case (corba-typecode-kind typecode)
        ((:tk_objref :tk_enum) (mush))
        ((:tk_alias) (mush (simplifyf (third params))))
        ((:tk_sequence) (simplifyf (first params)))
        ((:tk_struct :tk_except) (mush (simplifyv (third params) second)))
        (t typecode)))))



;;;; IR -- initial repository contents

(corba-add-interface corba-object-interface)
(corba-intern-type corba-tc-object t)



;;;; Using real IR

(defun corba-get-ir ()
  "Get an object reference to the Interface Repository"
  (assert corba-orb nil "ORB initialized")
  (corba-resolve-initial-references corba-orb "InterfaceRepository"))


(defun corba-ir-lookup-id (id)
  (let ((req (corba-create-request
              (corba-get-ir) "lookup_id"
              '(:tk_string) '(:tk_objref) nil
              (list id))))
    (or (car (corba-request-invoke req))
        (error "InterfaceRepository does not know about %s" id))))


(defun corba-get-attribute (object getter result-tc)
  (car (corba-request-invoke
        (corba-create-request object getter () (list result-tc) () ()))))


(defun corba-opdef-from-attrdef (irdef)
  (let* ((name (corba-get-attribute irdef "_get_name" :tk_string))
         (attr-mode-tc (corba-get-typecode "IDL:omg.org/CORBA/AttributeMode:1.0"))
         (mode (corba-get-attribute irdef "_get_mode" attr-mode-tc))
         (type (corba-intern-type
                (corba-get-attribute irdef "_get_type" :tk_TypeCode))))
    (cons (make-corba-opdef
           :name (format "_get_%s" name)
           :outparams (list type))
          (if (eq mode 0)               ; :attr_normal
              (list (make-corba-opdef
                     :inparams (list type)
                     :name (format "_set_%s" name)))))))


(defun corba-opdef-from-ir (irdef)
  "Obtain a Operation Definition (corba-opdef) from the IRDEF.
IRDEF can be an Operation Definition object or the interface repository
id for the operation."
  (when (stringp irdef)
    (setq irdef (corba-ir-lookup-id irdef)))
  (let ((name   (corba-get-attribute irdef "_get_name" :tk_string))
        (result (corba-get-attribute irdef "_get_result" :tk_TypeCode))
        (parseq (corba-get-typecode "IDL:omg.org/CORBA/ParDescriptionSeq:1.0"))
	(inpars nil)
	(outpars nil))
    (unless (eq :tk_void (corba-typecode-kind result))
      (push  (corba-intern-type result) outpars))
    (loop for pardesc in (corba-get-attribute irdef "_get_params" parseq)
	  for tc   = (corba-intern-type (corba-struct-get pardesc 'type))
	  for mode = (corba-struct-get pardesc 'mode)
	  do (unless (eq mode :PARAM_OUT)
               (push tc inpars))
	  do (unless (eq mode :PARAM_IN)
               (push tc outpars)))
    (make-corba-opdef
     :name name
     :inparams (nreverse inpars)
     :outparams (nreverse outpars))))


(defun corba-interface-from-id (id)
  (let ((def (corba-ir-lookup-id id)))
    (when (corba-object-is-nil def)
      (error "InterfaceRepository do not know about %s" id))
    (corba-interface-from-def def id)))


(defun corba-interface-from-def-cached (def)
  (let ((id (corba-get-attribute def "_get_id" :tk_string)))
    (or (corba-has-interface-p id)
        (corba-add-interface (corba-interface-from-def def id)))))


(defun corba-ir-contents (container limit-type exclude-inherit)
  (let* ((cseq (corba-get-typecode "IDL:omg.org/CORBA/ContainedSeq:1.0"))
         (req (corba-create-request
               container "contents"
               '(:tk_long :tk_boolean) (list cseq) ()
               (list limit-type exclude-inherit))))
    (car (corba-request-invoke req))))


(defun corba-interface-from-def (def id)
  (let ((mess "Getting interface %s %s")
        (progress ""))
    (message mess id progress)
    (let ((idseq (corba-get-typecode "IDL:omg.org/CORBA/InterfaceDefSeq:1.0")))
      (make-corba-interface
       :id id
       :inherit
       (or (mapcar #'(lambda (idef)
                       (corba-interface-from-def-cached idef))
                   (corba-get-attribute def "_get_base_interfaces" idseq))
           (list corba-object-interface))
       :operations
       (nconc (apply 'append
                     (mapcar 'corba-opdef-from-attrdef
                             (corba-ir-contents def 2 t)))
              (mapcar 'corba-opdef-from-ir
                      (corba-ir-contents def 7 t)))))))


(defun corba-typecode-from-def (def)
  (when (stringp def)
    (let ((id def))
      (message "Getting type %s" id)
      (setq def (corba-ir-lookup-id id))
      (when (corba-object-is-nil def)
	(error "InterfaceRepository do not know about %s" id))))
  (corba-intern-type
   (corba-get-attribute def "_get_type" :tk_TypeCode) ))
