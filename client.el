;;; Marshalling functions

(require 'cl)

;;;; Basic CORBA defs

;;; Exceptions

(put 'corba-system-exception 'error-conditions
     '(corba-system-exception corba-exception error))
(put 'corba-system-exception 'error-message "CORBA System Exception")

(put 'corba-user-exception 'error-conditions
     '(corba-user-exception corba-exception error))
(put 'corba-user-exception 'error-message "CORBA User Exception")

(defstruct corba-object
  (id nil)
  (host nil)
  (port nil)
  (object-key nil)
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

(eval-when (load eval)
  (loop for i from 0 below (length TCKind)
	do (setf (symbol-value (elt TCKind i)) i)))

(put 'tk_fixed 'tk-params '(tk_ushort tk_short))
(put 'tk_objref 'tk-params '(complex string string))
(put 'tk_struct 'tk-params '(complex string string
				     (sequence (anon-struct string tk_TypeCode))))
(put 'tk_union 'tk-params t)
(put 'tk_enum 'tk-params t)
(put 'tk_sequence 'tk-params '(complex tk_TypeCode tk_ulong))
(put 'tk_string 'tk-params '(tk_ulong))
(put 'tk_wstring 'tk-params '(tk_ulong))
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
	   (setf (get tk 'corba-typecode) (elt res i))
	finally (return res)))

(put 'string 'corba-typecode
     (make-corba-type-code
      :kind 'tk_string
      :params '(0)))

(put 'object 'corba-typecode
     (make-corba-type-code
      :kind 'tk_objref
      :params '("CORBA::Object" "IDL:omg.org/CORBA/Object:1.0")))

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

(defun cdr-short (n)
  (cdr-align 2)
  (insert n (ash n -8)))

(defun cdr-ulong (n)
  (cdr-align 4)
  (insert n 
	  (/ n 256)	
	  (/ n 65536)	
	  (/ n 16777216)))

(defun cdr-string (s)
  (cdr-ulong (1+ (length s)))
  (insert s 0))

(defun cdr-osequence (s)
  (cdr-ulong (length s))
  (insert s))

(defun cdr-sequence (s el-cdr)
  (cdr-ulong (length s))
  (loop for e in s do (funcall el-cdr e)))


(defun cdr-marshal (arg type)
  (when (symbolp type)
    (setq type (or (get type 'corba-typecode) type)))
  (let (kind params)
    (cond ((vectorp type)
	   (setq kind (corba-type-code-kind type)
		 params (corba-type-code-params type)))
	  ((consp type)
	   (setq kind (car type)
		 params (cdr type)))
	  (t (setq kind type)))
    (ecase kind
      ((tk_octet byte tk_char) (cdr-octet arg))
      ((tk_boolean bool) (cdr-bool arg))
      ((tk_ushort ushort) (cdr-ushort arg))
      ((tk_ulong ulong long) (cdr-ulong arg))
      ((tk_string string) (cdr-string arg))
      ((osequence) (cdr-osequence arg))
      ((tk_object object) (cdr-ior arg))
      ((sequence tk_sequence)
       (let ((_el_type_ (first params)))
	 (if (or (eq _el_type_ 'tk_octet)
		 (and (vectorp _el_type_)
		      (eq (corba-type-code-kind _el_type_) 'tk_octet)))
	     (cdr-osequence arg)
	   (cdr-sequence arg (lambda (arg) (cdr-marshal arg _el_type_))))))
      ((tk_struct)
       (loop for el in (third params)
	     do (cdr-marshal (cdr (assq (first el) arg))
			     (second el))))
      ((anon-struct)
       (loop for type in params
	     for arg in arg
	     collect (cdr-marshal arg type))))))


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

(defun cdr-read-short ()
  (let ((n (cdr-read-ushort)))
    (if (> n (eval-when-compile (expt 2 15)))
	(- n (eval-when-compile (expt 2 16)))
      n)))

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


(defun cdr-read-any ()
  (let ((tc (cdr-read-typecode)))
    ;; FIXME: Should construct a specail ANY type
    (cdr-unmarshal tc)))

(defun cdr-unmarshal (type)
  (when (symbolp type)
    (setq type (or (get type 'corba-typecode) type)))
  (let (kind params)
    (cond ((vectorp type)
	   (setq kind (corba-type-code-kind type)
		 params (corba-type-code-params type)))
	  ((consp type)
	   (setq kind (car type)
		 params (cdr type)))
	  (t (setq kind type)))
    (ecase kind
      ((char byte octet tk_char tk_octet) (cdr-read-octet))
      ((bool tk_boolean) (cdr-read-bool))
      ((ushort tk_ushort) (cdr-read-ushort))
      ((ulong tk_ulong) (cdr-read-ulong))
      ((string tk_string) (cdr-read-string))
      ((tk_any) (cdr-read-any))
      ((tk_sequence sequence)
       (let ((_ElType_ (car params)))
	 (if (or (eq _ElType_ 'tk_octet)
		 (and (vectorp _ElType_)
		      (eq (corba-type-code-kind _ElType_) 'tk_octet)))
	     (cdr-read-osequence)
	   (cdr-read-sequence (lambda () (cdr-unmarshal _ElType_))))))
      ((tk_alias)
       (cdr-typecode-unmarsal-value (third params)))
      ((tk_struct)
       (cons (first params)
	     (mapcar (lambda (nt-pair)
		       (cons (if (stringp (first nt-pair))
				 (intern (first nt-pair))
			       (first nt-pair))
			     (cdr-unmarshal (second nt-pair))))
		     (third params))))
      ((object tk_objref) (cdr-read-ior))
      ((anon-struct)
       (loop for type in params
	     collect (cdr-unmarshal type)))
      ((tk_TypeCode) (cdr-read-typecode)))))




;;;; GIOP / IIOP stuff

(defvar *message-size* 0)
(make-variable-buffer-local '*message-size*)
(defvar *giop-version* )
(make-variable-buffer-local '*giop-version*)

(defun cdr-giop-header (type)
  (insert "GIOP" 1 0 1
	  (cond ((numberp type) type)
		((eq type 'request) 0)
		((eq type 'reply) 1)
		(t (error "Message type %s" type))))
  ;; Place for message length to be patched in later
  (cdr-ulong 0))

(defun cdr-giop-set-message-length ()
  (goto-char 9)
  (cdr-ulong (- (point-max) 13))
  (delete-char 4))


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
    (unless (and pp (eq (process-status (cdr pp)) 'open))
      (unless hp
	(push (setq hp (cons host nil)) *iiop-connections*))
      (let ((buffer (if pp (process-buffer (cdr pp))
		      (generate-new-buffer "*IIOP*"))))
	(save-excursion
	  (set-buffer buffer)
	  (setq buffer-undo-list nil)
	  (setq *message-size* nil)
	  (erase-buffer))
	(let ((proc (open-network-stream "iiop" buffer host port)))
	  ;; FIXME: should I check if open
	  (if pp
	      (setcdr pp proc)
	    (setq pp (cons port proc))
	    (push pp (cdr hp))))))
    (cdr pp)))

(defun get-clients ()
  (loop for hp in *iiop-connections*
	nconc (loop for pp in (cdr hp) collect (cdr pp))))


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

(defun corba-request-send (req &optional flags)
  (let* ((object (corba-request-object req))
	 (client (get-connection
		  (corba-object-host object)
		  (corba-object-port object)))
	 (operation (corba-request-operation req)))
    (setf (corba-request-req-id req) (incf *request-id-seq*))
    (setf (corba-request-client req) client)
    (setf (corba-request-result req) t)
    (save-excursion
      (set-buffer (get-buffer-create "*REQ*"))
      (setq buffer-undo-list t)
      (erase-buffer)
      (cdr-giop-header 'request)
      (cdr-ulong 0)			;context
      (cdr-ulong (corba-request-req-id req))
      (cdr-octet (if (memq 'no-response flags) 0 1)) ;respons expected
      (cdr-osequence (corba-object-object-key object))
      (cdr-string (first operation))
      (cdr-osequence "")		;principal
      (loop for arg in (corba-request-arguments req)
	    for desc in (second operation)
	    do (cdr-marshal arg (second desc)))
      (cdr-giop-set-message-length)
      (process-send-region client (point-min) (point-max))
      ;;(message "Request %d sent" (corba-request-req-id req))
      (accept-process-output)
      (push req *corba-waiting-requests*))))


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
	  ((<= (point-max) (+ 12 *message-size*))
	   (setq *message-size* nil))
	  (t
	   (let* ((service-context (cdr-read-service-context))
		  (request-id (cdr-read-ulong))
		  (req
		   (loop for req in *corba-waiting-requests*
			 if (= request-id (corba-request-req-id req))
			 return req)))
	     (cond (req
		    (setq *corba-waiting-requests*
			  (delq req *corba-waiting-requests*))
		    (and (corba-read-reply req)
			 req))
		   (t
		    (message "Unexpected respons for id %s" request-id)))))))
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

(defun corba-request-get-response (req &optional flags)
  (let* ((client (corba-request-client req)))
    (loop while (eq t (corba-request-result req))
	  do (corba-get-next-respons-1 client)
	  until (memq 'no-wait flags)
	  do (accept-process-output)))
  (not (eq t (corba-request-result req))))


(defun corba-request-invoke (req &optional flags)
  (corba-request-send req)
  (corba-request-get-response req)
  (corba-request-result req))


;;;; The ORB Interface

(defun corba-file-to-object (file)
  (corba-string-to-object
   (save-excursion
     (set-buffer (get-buffer-create "*REQ*"))
     (erase-buffer)
     (insert-file-contents file)
     (goto-char (point-min))
     (end-of-line 1)
     (buffer-substring (point-min) (point)))))

(defun corba-orb-resolve-initial-references (name)
  (cond
   ((string-equal name "NameService")
    (corba-file-to-object "/tmp/NameService"))
   ((string-equal name "InterfaceRepository")
    (corba-file-to-object "/tmp/ir"))))

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

;;; The Object Interface

(defun object.is-a (obj id)
  (let ((req
	 (make-corba-request
	  :object obj
	  :operation '("_is_a"
		       (("id" tk_string))
		       (("" tk_boolean)))
	  :arguments (list id))))
    (car (corba-request-invoke req))))
