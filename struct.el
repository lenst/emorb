(require 'corba-defs)


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
    (setq start (corba-orb-resolve-initial-references
                 (corba-orb-init)
                 "NameService")))
  (unless (member (corba-object-key start) avoid)
    (push (corba-object-key start) avoid)
    (let ((result (corba-invoke start "list" 1000)))
      (loop
       for binding in (first result) collect
       (cons
        (mapconcat (lambda (nc) (corba-struct-get nc 'id))
                   (corba-struct-get binding 'binding-name)
                   "/")
        (if recursive
            (let ((child
                   (car (corba-invoke start "resolve"
                                      (corba-struct-get binding 'binding-name)))))
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
  (loop with ir = (corba-get-ir)
        for r
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

(defun test-fnorb-mass (&optional obj max)
  (setq obj (or obj ns))
  (setq max (or max 10))
  (loop for r
	in (loop for i to max collect
		 (let ((req
			(make-corba-request
			 :object obj
			 :operation '("_is_a"
				      (("id" string))
				      (("" tk_boolean)))
			 :arguments (list "IDL:omg.org/CORBA/Object:1.0"))))
		   (corba-request-send req)
		   (message "Send %d" (corba-request-req-id req))
		   req))
	for i from 0
	do (progn
	     (message "Recv %d => %d: %s" i
		      (corba-request-req-id r)
		      (corba-request-get-response r)))))


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

;;;; Constructing structs

(defun name (&rest strseq)
  (mapcar (lambda (id)
            (corba-struct "IDL:omg.org/CosNaming/NameComponent:1.0"
                          'id id 'kind ""))
          strseq))

(defun example2 ()
  (let ((ns (corba-orb-resolve-initial-references nil "NameService")))
    (corba-invoke ns "bind"
                  (name "dev" "ir")
                  (corba-get-ir))
    (corba-invoke ns "resolve" (name "dev" "ir"))))


(defun ils (&optional start)
  (unless start
    (setq start (corba-orb-resolve-initial-references
                 (corba-orb-init) "NameService")))
  (destructuring-bind (l i)
      (corba-invoke start "list" 0)
    (unless (corba-object-is-nil i)
      (prog1 (loop for (f b) = (corba-invoke i "next_one")
                   while (not f)
                   collect (corba-struct-get
                            (elt (corba-struct-get b 'binding-name) 0)
                            'id))
        (corba-invoke i "destroy")))))

(defun show-name-service ()
  (interactive)
  (let* ((orb (corba-orb-init))
         (ns  (corba-orb-resolve-initial-references orb "NameService")))
    (multiple-value-bind (binding-list binding-iter)
        (corba-invoke ns "list" 1000)
      (unless (corba-object-is-nil binding-iter)
        (corba-invoke binding-iter "destroy"))
      (with-output-to-temp-buffer "*NameService*"
        (mapcar
         (lambda (binding)
           (let ((name (corba-struct-get binding 'binding-name)))
             (princ
              (format "%-25s %-13s %s\n"
                      (mapconcat (lambda (nc) (corba-struct-get nc 'id))
                                 name "/")
                      (mapconcat (lambda (nc) (corba-struct-get nc 'kind))
                                 name "/")
                      (if (eq (corba-struct-get binding 'binding-type) 1)
                          "CONTEXT"
                        (let ((object (car (corba-invoke ns "resolve" name))))
                          (if (corba-object-is-nil object)
                              "NIL"
                            (corba-object-id object))))))))
         binding-list)))))


(defun corba-resolve (&rest names)
  (let ((n (mapcar (lambda (id)
                     (corba-struct "IDL:omg.org/CosNaming/NameComponent:1.0"
                                   'id id 'kind ""))
                   names))
        (ns (corba-orb-resolve-initial-references (corba-orb-init)
                                                  "NameService")))
    (first (corba-invoke ns "resolve" n))))