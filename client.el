;;; Marshalling functions

(require 'cl)

;;;; Basic CORBA defs

(defstruct corba-object
  (host nil)
  (port nil)
  (object-key nil)
  (id nil)
  (profiles nil)
  (forward nil))

(defconst TCKind
  [
   tk_null tk_void tk_short tk_long tk_ushort tk_ulong
   tk_float tk_double tk_boolean tk_char
   tk_octet tk_any tk_TypeCode tk_Principal tk_objref
   tk_struct tk_union tk_enum tk_string
   tk_sequence tk_array tk_alias tk_except
   tk_longlong tk_ulonglong t_longdouble
   tk_wchar tk_wstring tk_fixed
   ]
  "The symbols for the TCKind enum")

(eval-when (compile load eval)
  (loop for i from 0 below (length TCKind)
	do (setf (symbol-value (elt TCKind i)) i)))

(put 'tk_fixed 'tk-params '(ushort short))
(put 'tk_objref 'tk-params '(complex string string))
(put 'tk_struct 'tk-params '(complex string string
				     (sequence (anon-struct string tk_TypeCode))))
(put 'tk_union 'tk-params t)
(put 'tk_enum 'tk-params t)
(put 'tk_sequence 'tk-params '(complex tk_TypeCode tk_ulong))
(put 'tk_string 'tk-params '(ulong))
(put 'tk_wstring 'tk-params '(ulong))
(put 'tk_array 'tk-params t)
(put 'tk_alias 'tk-params '(complex string string tk_TypeCode))
(put 'tk_except 'tk-params t)

(defstruct corba-type-code
  (kind nil)
  (params nil))

(defvar *tc-cache*
  (loop with res = (make-vector (length TCKind) nil)
	for tk across TCKind
	as i from 0
	if (not (get tk 'tk-params))
	do (setf (elt res i) (make-corba-type-code :kind tk))
	finally (return res)))


;;;; Marshaling

(defun cdr-octet (n)
  (insert n))

(defun cdr-bool (s)
  (insert (if s 1 0)))

(defun cdr-align (n)
  (while (/= 1 (% (point) n))
    (insert 0)))

(defun cdr-ushort (n)
  (cdr-align 2)
  (insert (% n 256) (/ n 256)))

(defun cdr-ulong (n)
  (cdr-align 4)
  (insert (% n 256)
	  (% (/ n 256) 256)
	  (% (/ n 65536) 256)
	  (% (/ n 16777216) 256)))

(defun cdr-string (s)
  (cdr-ulong (1+ (length s)))
  (insert s 0))

(defun cdr-osequence (s)
  (cdr-ulong (length s))
  (insert s))

(defun cdr-sequence (s el-cdr)
  (cdr-ulong (length s))
  (loop for e in s do (funcall el-cdr e)))

(defun cdr-guess (sexp)
  (cond ((stringp sexp)
	 (cdr-string sexp))
	((numberp sexp)
	 (cdr-ulong sexp))
	((consp sexp)
	 (cdr-sequence sexp (function cdr-guess)))
	(t
	 (error "Marshal %s" sexp))))


(defun cdr-marshal (arg type)
  (case (or (car-safe type) type)
    ((octet byte char) (cdr-octet arg))
    ((bool) (cdr-bool arg))
    ((ushort) (cdr-ushort arg))
    ((ulong long) (cdr-ulong arg))
    ((string) (cdr-string arg))
    ((osequence) (cdr-osequence arg))
    ((object) (cdr-ior arg))
    ((sequence)
     (let ((_el_type_ (second type)))
       (cdr-sequence arg (lambda (arg) (cdr-marshal arg _el_type_)))))
    ((anon-struct)
     (loop for type in (cdr type)
	   for arg in arg
	   collect (cdr-marshal arg type)))
    (t
     (let ((proc (get (or (car-safe type) type) 'corba-marshal)))
       (if proc
	   (apply proc arg (cdr-safe type))
	 (error "MARSHAL: %s" type))))))


;;; UnMarshal

(defvar *byte-order* 1)
(make-variable-buffer-local '*byte-order*)

(defun cdr-read-octet ()
  (prog1 (following-char) (forward-char 1)))

(defun cdr-read-bool ()
  (/= (cdr-read-octet) 0))

(defun cdr-read-align (n)
  (while (/= 1 (% (point) n))
    (forward-char 1)))

(defmacro cdr-with-encapsulation (obj &rest body)
  (let ((envar (gensym))
	(byte-order (gensym)))
    `(let ((,envar ,obj))
       (save-excursion
	 (set-buffer (get-buffer-create "*CDR*"))
	 (setq buffer-undo-list t)
	 (goto-char (point-min))
	 (let ((,byte-order *byte-order*))
	   (save-restriction
	     (unwind-protect
		 (progn (insert ,envar)
			(narrow-to-region (point-min) (point))
			(goto-char (point-min))
			(setq *byte-order* (cdr-read-octet))
			,@body)
	       (delete-region (point-min) (point-max))
	       (setq *byte-order* ,byte-order))))))))


(def-edebug-spec cdr-with-encapsulation
  (form body))


(defsubst cdr-read-number (weights)
  (cdr-read-align (length weights))
  (loop with res = 0
	for w in weights
	do
	(incf res (* w (following-char)))
	(forward-char 1)
	finally (return res)))

(defun cdr-read-ushort ()
  (cdr-read-number
   (if *byte-order* '(1 256)
     '(256 1))))

(defun cdr-read-ulong ()
  (cdr-read-number
   (if *byte-order* '(1 256 65536 16777216)
     '(16777216 65536 256 1))))

(defun cdr-read-sequence (el-reader)
  (let ((len (cdr-read-ulong)))
    (loop for i from 1 upto len
	  collect (funcall el-reader))))

(defun cdr-read-string ()
  (let* ((len (cdr-read-ulong))
	 (start (point)))
    (forward-char len)
    (buffer-substring start (1- (point)))))

(defun cdr-read-osequence ()
  (let* ((len (cdr-read-ulong))
	 (start (point)))
    (forward-char len)
    (buffer-substring start (point))))

(defun cdr-read-typecode ()
  (let* ((tki (cdr-read-ulong))
	 (tk (aref TCKind tki))
	 (params (get tk 'tk-params)))
    (cond ((null params)
	   (aref *tc-cache* tki))
	  ((eq t params)
	   (make-corba-type-code :kind tk :params (cdr-read-osequence)))
	  ((eq 'complex (car params))
	   (cdr-with-encapsulation
	    (cdr-read-osequence)
	    (make-corba-type-code :kind tk
				  :params (mapcar 'cdr-unmarshal (cdr params)))))
	  (t
	   (make-corba-type-code :kind tk
				 :params (mapcar 'cdr-unmarshal params))))))


(defun cdr-typecode-unmarsal-value (tc)
  (let ((kind (corba-type-code-kind tc)))
    (case kind
      ((tk_sequence)
       (let ((_ElType_
	      (car (corba-type-code-params tc))))
	 (cdr-read-sequence
	  (lambda () (cdr-typecode-unmarsal-value _ElType_)))))
      ((tk_struct)
       (let* ((params (corba-type-code-params tc)))
	 ;; FIXME: find struct code, and use that. Fallback:
	 (mapcar (lambda (nt-pair)
		   (cons (first nt-pair)
			 (cdr-typecode-unmarsal-value (second nt-pair))))
		 (third params))))
      ((tk_alias)
       (let* ((params (corba-type-code-params tc)))
	 (cdr-typecode-unmarsal-value (third params))))
      (t
       (cdr-unmarshal kind)))))


(defun cdr-read-any ()
  (let ((tc (cdr-read-typecode)))
    ;; FIXME: Should construct a specail ANY type
    (cdr-typecode-unmarsal-value tc)))

(put 'tk_any 'corba-unmarshal 'cdr-read-any)

(defun cdr-unmarshal (type)
  (case (or (car-safe type) type)
    ((char byte octet tk_char tk_octet) (cdr-read-octet))
    ((bool tk_boolean) (cdr-read-bool))
    ((ushort tk_ushort) (cdr-read-ushort))
    ((ulong tk_ulong) (cdr-read-ulong))
    ((string tk_string) (cdr-read-string))
    ((sequence)
     (if (eq (second type) 'octet)
	 (cdr-read-osequence)
       (cdr-read-sequence (lambda ()
			    (cdr-unmarshal (second type))))))
    ((object tk_objref) (cdr-read-ior))
    ((anon-struct)
     (loop for type in (cdr type)
	   collect (cdr-unmarshal type)))
    ((typecode tk_TypeCode) (cdr-read-typecode))
    (t
     (let ((proc (get (or (car-safe type) type) 'corba-unmarshal)))
       (if proc
	   (apply proc (cdr-safe type))
	 (error "UNMARSHAL: %s" type))))))


;;;; GIOP / IIOP stuff

(defvar *message-size* 0)
(defvar *giop-version* )


(defun cdr-giop-header (type msgthunk)
  (insert "GIOP" 1 0 1
	  (cond ((numberp type) type)
		((eq type 'request) 0)
		((eq type 'reply) 1)
		(t (error "Message type %s" type))))
  (cdr-ulong 0)
  (let ((pos (point)))
    (funcall msgthunk)
    (let ((len (- (point) pos)))
      (goto-char (- pos 4))
      (delete-char 4)
      (cdr-ulong len))))


(defun cdr-simple-request (objectkey operation &rest opargs)
  (erase-buffer)
  (cdr-giop-header
   'request
   (lambda ()
     (cdr-ulong 0)			;context
     (cdr-ulong 4711)			;request id
     (cdr-octet 1)			;respons expected
     (cdr-osequence objectkey)
     (cdr-string operation)
     (cdr-osequence "")			;principal
     (loop for arg in opargs
	   do (cdr-guess arg)))))


(defun cdr-read-giop-header ()
  (unless (looking-at "GIOP")
    (error "Not a GIOP message"))
  (forward-char 4)
  (let* ((major (cdr-read-octet))
	 (minor (cdr-read-octet))
	 (byte-order (cdr-read-octet))
	 (msgtype (cdr-read-octet)))
    (setq *giop-version* (+ (* 100 major) minor))
    (setq *byte-order* byte-order)
    (setq *message-size* (cdr-read-ulong))
    msgtype))

(defun cdr-read-service-context ()
  (let ((len (cdr-read-ulong)))
    (if (= len 0)
	'()
      (error "cdr-read-service-context NIY"))))

(defun cdr-read-reply-header ()
  (let* ((service-context (cdr-read-service-context))
	 (request-id (cdr-read-ulong))
	 (reply-status (cdr-read-ulong)))

    reply-status))

(defun cdr-read-tagged-component ()
  (cons (cdr-read-ulong)
	(cdr-read-osequence)))

(defun cdr-read-iiop-profile-body (reference)
   (cdr-read-octet)			;Version (ignored for now)
   (cdr-read-octet)
   (setf (corba-object-host reference) (cdr-read-string))
   (setf (corba-object-port reference) (cdr-read-ushort))
   (setf (corba-object-object-key reference) (cdr-read-osequence)))

(defun cdr-read-ior ()
  (let* ((type-id (cdr-read-string))
	 (profiles (cdr-read-sequence #'cdr-read-tagged-component))
	 (reference (make-corba-object
		     :id type-id
		     :profiles profiles)))
    (loop for (tag . encaps) in profiles
	  if (= tag 0)
	  do (cdr-with-encapsulation
	      encaps
	      (cdr-read-iiop-profile-body reference))
	  (return))
    reference))

(defun cdr-ior (object)
  (cdr-string (corba-object-id object))
  (cdr-sequence (corba-object-profiles object)
		(lambda (profile)
		  (cdr-ulong (car profile))
		  (cdr-osequence (cdr profile)))))

;;;; Connection handling

(defvar *iiop-connections* nil)

(defun get-connection (host port)
  (let* ((hp (assoc host *iiop-connections*))
	 (pp (assq port (cdr hp))))
    (unless pp
      (unless hp
	(push (setq hp (cons host nil)) *iiop-connections*))
      (let ((buffer (generate-new-buffer "*IIOP*")))
	(save-excursion
	  (set-buffer buffer)
	  (setq buffer-undo-list nil)))
      (setq pp (cons port (open-network-stream "iiop" buffer host port)))
      (push pp (cdr hp)))
    (cdr pp)))


;;;; Requests

(defstruct corba-request
  (object nil)
  (operation nil)
  (arguments nil)
  (req-id nil)
  (client nil)
  (result nil))

(defvar *request-id-seq* 0)
(defvar *corba-waiting-requests* nil)
(make-variable-buffer-local '*corba-waiting-requests*)

(defun corba-request-send (req &optional flags)
  (let* ((object (corba-request-object req))
	 (client (get-connection
		  (corba-object-host object)
		  (corba-object-port object)))
	 (object-key (corba-object-object-key object))
	 (operation (corba-request-operation req)))
    (unless (corba-request-req-id req)
      (setf (corba-request-req-id req) (incf *request-id-seq*)))
    (setf (corba-request-client req) client)
    (save-excursion
      (set-buffer (get-buffer-create "*REQ*"))
      (setq buffer-undo-list t)
      (erase-buffer)
      (cdr-giop-header
       'request
       (lambda ()
	 (cdr-ulong 0)			;context
	 (cdr-ulong (corba-request-req-id req))
	 (cdr-octet 1)			;respons expected
	 (cdr-osequence object-key)
	 (cdr-string (first operation))
	 (cdr-osequence "")		;principal
	 (loop for arg in (corba-request-arguments req)
	       for desc in (second operation)
	       do (cdr-marshal arg (second desc)))))
      (process-send-region client (point-min) (point-max))
      (unless (memq 'invoke flags)
	(push req *corba-waiting-requests*)))))


(defun corba-read-reply (req)
  (ecase (cdr-read-ulong)
    ((0)				; No Exception
     (setf (corba-request-result req)
	   (loop for desc in (third (corba-request-operation req))
		 collect (cdr-unmarshal (second desc))))
     t)
    ((1)				; User Exception
     (error "NIY"))
    ((2)				; System Exception
     (let* ((id (cdr-read-string))
	    (minor (cdr-read-ulong))
	    (status (cdr-read-ulong)))
       (signal 'corba-system-exception
	       (list id minor status))))
    ((4)				; Forward
     (setf (corba-object-forward (corba-request-object req))
	   (cdr-read-ior))
     (corba-request-send req)
     nil)))

(defun corba-get-next-respons-1 (client)
  (save-excursion
    (set-buffer (process-buffer client))
    (when *message-size*
      (goto-char (point-min))
      (delete-char 12)			; header size
      (delete-char *message-size*)
      (setq *message-size* nil))
    (cond
     ((>= (point-max) 12)
      (goto-char (point-min))
      (ecase (cdr-read-giop-header)
	((1)				; Reply
	 (cond
	  ((< (point-max) *message-size*)
	   (setq *message-size* nil))
	  (t
	   (let* ((service-context (cdr-read-service-context))
		  (request-id (cdr-read-ulong))
		  (req
		   (assoc* request-id *corba-waiting-requests*
			   :key corba-request-req-id)))
	     (unless req
	       (error "Unexpected respons for id %s" request-id))
	     (setq *corba-waiting-requests*
		   (delq req *corba-waiting-requests*))
	     (and (corba-read-reply req)
		  req)))))
	((5)				;Close Connection
	 (delete-process client)
	 (error "Connection closed")))))))


(defun corba-get-next-respons (&optional flags)
  (let ((req nil))
    (loop
     do (setq req (loop for client in (get-clients)
			thereis (corba-get-next-respons-1 client)))
     until (or req (not (memq 'no-wait flags)))
     do (accept-process-output))
    req))


(defun corba-get-reply (req)
  (let* ((object (corba-request-object req))
	 (client (corba-request-client req))
	 (object-key (corba-object-object-key object))
	 (operation (corba-request-operation req)))
    (save-excursion
      (set-buffer "*IIOP*")		; depends on client?
      (when *message-size*
	(goto-char (point-min))
	(delete-char 12)		; header size
	(delete-char *message-size*)
	(setq *message-size* nil))
      (while (< (point-max) 12)
	(accept-process-output))
      (goto-char (point-min))
      (ecase (cdr-read-giop-header)
	((1)				; Reply
	 (while (< (point-max) *message-size*)
	   (accept-process-output))
	 (ecase (cdr-read-reply-header)
	   ((0)				; No Exception
	    (loop for desc in (third operation)
		  collect (cdr-unmarshal (second desc))))
	   ((1)				; User Exception
	    (error "NIY"))
	   ((2)				; System Exception
	    (let* ((id (cdr-read-string))
		   (minor (cdr-read-ulong))
		   (status (cdr-read-ulong)))
	      (signal 'corba-system-exception
		      (list id minor status))))
	   ((4)				; Forward
	    (setf (corba-object-forward object)
		  (cdr-read-ior)))))
	((5)				;Close Connection
	 (error "Connection closed"))))))


(defun corba-request-get-response (req &optional flags)
  (let* ((client (corba-request-client req)))
    (save-excursion
      (set-buffer (process-buffer client))
      (when *message-size*
	(goto-char (point-min))
	(delete-char 12)		; header size
	(delete-char *message-size*)
	(setq *message-size* nil))
      (while (< (point-max) 12)
	(accept-process-output))
      (goto-char (point-min))
      (ecase (cdr-read-giop-header)
	((1)				; Reply
	 (while (< (point-max) *message-size*)
	   (accept-process-output))
	 (ecase (cdr-read-reply-header)
	   ((0)				; No Exception
	    (setf (corba-request-result req)
		  (loop for desc in (third operation)
			collect (cdr-unmarshal (second desc)))))
	   ((1)				; User Exception
	    (error "NIY"))
	   ((2)				; System Exception
	    (let* ((id (cdr-read-string))
		   (minor (cdr-read-ulong))
		   (status (cdr-read-ulong)))
	      (signal 'corba-system-exception
		      (list id minor status))))
	   ((4)				; Forward
	    (setf (corba-object-forward object)
		  (cdr-read-ior)))))
	((5)				;Close Connection
	 (error "Connection closed"))))))


(defun object.is-a (obj id)
  (let ((req
	 (make-corba-request
	  :object obj
	  :operation '("_is_a"
		       (("id" string))
		       (("" bool)))
	  :arguments (list id))))
    (corba-invoke req)
    (car (corba-get-reply req)) ))


(defsubst corba-hex-to-int (ch)
  (cdr (assq ch '((?0 . 0)
		  (?1 . 1)
		  (?2 . 2)
		  (?3 . 3)
		  (?4 . 4)
		  (?5 . 5)
		  (?6 . 6)
		  (?7 . 7)
		  (?8 . 8)
		  (?9 . 9)
		  (?a . 10) (?A . 10)
		  (?b . 11) (?B . 11)
		  (?c . 12) (?C . 12)
		  (?d . 13) (?D . 13)
		  (?e . 14) (?E . 14)
		  (?f . 15) (?F . 15)))))

(defun corba-string-to-object (str)
  (if (string-match "IOR:" str)
      (cdr-with-encapsulation
       (loop for i from 4 below (length str) by 2
	     concat (char-to-string
		     (+ (* 16 (corba-hex-to-int (aref str i)))
			(corba-hex-to-int (aref str (1+ i))))))
       (cdr-read-ior))
    (error "Illegal string object")))

;;;; Testing NameService

(defstruct cosnaming-name-component
  (id "")
  (kind ""))

(put 'cosnaming-name-component
     'corba-unmarshal (lambda ()
			(make-cosnaming-name-component
			 :id (cdr-read-string)
			 :kind (cdr-read-string))))

(put 'cosnaming-name-component
     'corba-marshal (lambda (nc)
		      (cdr-string (cosnaming-name-component-id nc))
		      (cdr-string (cosnaming-name-component-kind nc))))


(defun namingcontext.list (obj how-many)
  (let ((req
	 (make-corba-request
	  :object obj
	  :operation  '("list"		;operation name
			;; Arguments
			(("how_many" ulong))
			;; Results
			(("bl" (sequence
				(anon-struct (sequence cosnaming-name-component)
					     ulong)))
			 ("bi" (object BindingIterator))))
	  :arguments (list how-many))))
    (corba-invoke req)
    (corba-get-reply req)))

(defun namingcontext.resolve (obj name)
  (let ((req
	 (make-corba-request
	  :object obj
	  :operation '("resolve"
		       (("name" (sequence cosnaming-name-component)))
		       (("" object)))
	  :arguments (list name))))
    (corba-invoke req)
    (car (corba-get-reply req)) ))


(defun ls (&optional start indent avoid)
  (unless start
    (setq start obj.ns))
  (unless indent
    (setq indent 0))
  (unless (member (corba-object-object-key start) avoid)
    (push (corba-object-object-key start) avoid)
    (let ((result (namingcontext.list start 1000)))
      (loop for binding in (first result)
	    do (princ (format "%s%s: %s\n"
			      (make-string indent ? )
			      (mapconcat (lambda (nc)
					   (cosnaming-name-component-id nc))
					 (first binding)
					 "/")
			      (second binding)))
	    (when (= (second binding) 1)
	      (ls (namingcontext.resolve start (first binding))
		  (1+ indent)
		  avoid))))))


(defconst obj.ns
  (make-corba-object
   :id "IDL:omg.org/CosNaming/NamingContext:1.0"
   :object-key "Noname/ns;root"
   :host "t2"
   :port 20515))

;;;; IR-test

(defun repository.lookup-id (obj search-id)
  (unless (object.is-a obj "IDL:omg.org/CORBA/Repository:1.0")
    (error "Not a repository"))
  (let ((req
	 (make-corba-request
	  :object obj
	  :operation '("lookup_id"
		       (("search_id" string))
		       (("" object)))
	  :arguments (list search-id))))
    (corba-invoke req)
    (car (corba-get-reply req)) ))

(defun container.lookup (obj name)
  (unless (object.is-a obj "IDL:omg.org/CORBA/Container:1.0")
    (error "Not a container"))
  (let ((req
	 (make-corba-request
	  :object obj
	  :operation '("lookup"
		       (("scoped_name" string))
		       (("" object)))
	  :arguments (list name))))
    (corba-invoke req)
    (car (corba-get-reply req)) ))


(defun container.contents (obj limit-type exclude-inherited)
  (unless (object.is-a obj "IDL:omg.org/CORBA/Container:1.0")
    (error "Not a container"))
  (let ((req
	 (make-corba-request
	  :object obj
	  :operation '("contents"
		       (("limit_type" ulong) ; enum realy
			("exclute_inherited" bool))
		       (("" (sequence object))))
	  :arguments (list limit-type exclude-inherited))))
    (corba-invoke req)
    (car (corba-get-reply req)) ))


(defun corba-check-type (obj id)
  (unless (object.is-a obj id)
    (error "Not a %s" id)))

(defstruct contained.description
  kind value)

(put 'contained.description
     'corba-unmarshal
     (lambda ()
       (make-contained.description
	:kind (cdr-read-ulong)
	:value (cdr-read-any))))

(defun contained.describe (obj)
  (corba-check-type obj "IDL:omg.org/CORBA/Contained:1.0")
  (let ((req
	 (make-corba-request
	  :object obj
	  :operation '("describe"
		       ()
		       (("" contained.description)))
	  :arguments nil)))
    (corba-invoke req)
    (car (corba-get-reply req)) ))
