(defun corba-make-struct-typecode (name id fields)
  (make-corba-type-code
   :kind 'tk_struct
   :params (list id (format "%s" name) fields)))

(defmacro corba-def-struct (name id fields)
  ;; name = symbol
  ;; id = string repository id
  ;; fields = ( (name type)* )
  ;; type  = tk_* or symbol with marshal/typecode attribute
  (let* (temp)
    `(progn
       (defun ,(intern (format "make-%s" name))
	 ,(mapcar 'car fields)
	 (list ,id
	       ,@(mapcar (lambda (descr)
			   `(cons ',(car descr) ,(car descr)))
			 fields)))

       (put ',name 'corba-typecode ,(corba-make-struct-typecode
				      name id fields)))))


;;;; Testing NameService

(corba-def-struct name-component
		  "IDL:omg.org/CosNaming/NameComponent:1.0"
		  ((id string)
		   (kind string)))

(corba-def-struct binding
		  "IDL:omg.org/CosNaming/Binding:1.0"
		  ((binding-name (sequence name-component))
		   (binding-type tk_ulong)))


(defun namingcontext.list (obj how-many)
  (let ((req
	 (make-corba-request
	  :object obj
	  :operation  '("list"		;operation name
			;; Arguments
			(("how_many" tk_ulong))
			;; Results
			(("bl" (sequence binding))
			 ("bi" (object BindingIterator))))
	  :arguments (list how-many))))
    (corba-request-invoke req)))


(defun makereq-namingcontext.resolve (obj name)
  (make-corba-request
   :object obj
   :operation '("resolve"
		(("name" (sequence name-component)))
		(("" object)))
   :arguments (list name)))

(defun namingcontext.resolve (obj name)
  (let ((req
	 (make-corba-request
	  :object obj
	  :operation '("resolve"
		       (("name" (sequence name-component)))
		       (("" object)))
	  :arguments (list name))))
    (car (corba-request-invoke req)) ))


(defvar default-name-service nil)

(defun corba-struct-get (struct key)
  (cdr (assq key struct)))

(defun* ls (&key start avoid (recursive t))
  (unless start
    (unless default-name-service
      (setq default-name-service
	    (corba-orb-resolve-initial-references "NameService")))
    (setq start default-name-service))
  (unless (member (corba-object-object-key start) avoid)
    (push (corba-object-object-key start) avoid)
    (let ((result (namingcontext.list start 1000)))
      (loop for binding in (first result)
	    collect
	    (cons (mapconcat (lambda (nc) (corba-struct-get nc 'id))
			     (corba-struct-get binding 'binding-name)
			     "/")
		  (if recursive
		      (let ((child
			     (namingcontext.resolve
			      start (corba-struct-get binding 'binding-name))))
			(if (eq (corba-struct-get binding 'binding-type) 1)
			    (cons 'context (ls :start child :avoid avoid))
			  (corba-object-id child)))))))))


;;;; IR-test

(defun corba-check-type (obj id)
  (unless (object.is-a obj id)
    (error "Not a %s" id)))

(defun repository.lookup-id (obj search-id)
  (corba-check-type obj "IDL:omg.org/CORBA/Repository:1.0")
  (let ((req
	 (make-corba-request
	  :object obj
	  :operation '("lookup_id"
		       (("search_id" string))
		       (("" object)))
	  :arguments (list search-id))))
    (car (corba-request-invoke req)) ))

(defun container.lookup (obj name)
 (corba-check-type obj "IDL:omg.org/CORBA/Container:1.0")
  (let ((req
	 (make-corba-request
	  :object obj
	  :operation '("lookup"
		       (("scoped_name" string))
		       (("" object)))
	  :arguments (list name))))
    (car (corba-request-invoke req)) ))


(defun container.contents (obj limit-type exclude-inherited)
  (corba-check-type obj "IDL:omg.org/CORBA/Container:1.0")
  (let ((req
	 (make-corba-request
	  :object obj
	  :operation '("contents"
		       (("limit_type" tk_ulong) ; enum realy
			("exclute_inherited" tk_boolean))
		       (("" (sequence object))))
	  :arguments (list limit-type exclude-inherited))))
    (car (corba-request-invoke req)) ))

(corba-def-struct contained.description
		  "IDL:omg.org/CORBA/Contained/Description:1.0"
		  ((kind tk_ulong)
		   (value tk_any)))

(defun contained.describe (obj)
  (corba-check-type obj "IDL:omg.org/CORBA/Contained:1.0")
  (let ((req
	 (make-corba-request
	  :object obj
	  :operation '("describe"
		       ()
		       (("" contained.description)))
	  :arguments nil)))
    (car (corba-request-invoke req)) ))



(defun test-mass ()
  (loop for r
	in (loop for i to 1000 collect
		 (let ((req
			(make-corba-request
			 :object ir
			 :operation '("lookup"
				      (("scoped_name" string))
				      (("" object)))
			 :arguments (list
				     "IDL:omg.org/CosNaming/NamingContext:1.0"))))
		   (corba-request-send req)
		   (message "Send %d" (corba-request-req-id req))
		   req))
	do (progn
	     (corba-request-get-response r)
	     (message "Recv %d" (corba-request-req-id r)))))

;;;; Test UrlDB

(defvar urldb-database nil)

(defun setup-urldb ()
  (setq urldb-database
	(corba-file-to-object "/triton:/tmp/udb")))

(defun urldb.get-category (db name)
  (let ((req
	 (make-corba-request
	  :object db
	  :operation '("get_category"
		       (("name" tk_string))
		       (("" object)))
	  :arguments (list name))))
    (car (corba-request-invoke req)) ))
