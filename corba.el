;;; corba.el --- A Client Side CORBA Implementation for Emacs

;; Copyright (C) 1998 Lennart Staflin

;; Author: Lennart Staflin <lenst@lysator.liu.se>
;; Version: $Id: corba.el,v 1.3 1998/01/26 19:44:20 lenst Exp $
;; Keywords: 
;; Created: 1998-01-25 11:03:10

;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 1, or (at your option)
;;; any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; A copy of the GNU General Public License can be obtained from this
;;; program's author (send electronic mail to lenst@lysator.liu.se) or from
;;; the Free Software Foundation, Inc., 675 Mass Ave, Cambridge, MA
;;; 02139, USA.
;;;

;; LCD Archive Entry:
;; corba|Lennart Staflin|lenst@lysator.liu.se|
;; A Client Side CORBA Implementation for Emacs|
;; 21-May-95|$Revision: 1.3 $||

;;; Commentary:

;; Provides an implementation of CORBA Dynamic Invocation interface
;; using the IIOP protocol.

;;; TODO:

;; Marshaling code for: long, longlong, ulonglong, floats, fixed,
;; union, array, wchar, wstring, better handling of enum

;; Server side:
;; probably need a helper program that handles the sockets and multiplexes
;; messages.

;; Generation of static stubs.

;; Saving the internal interface repository to a Lisp file for later
;; use without a Repository service.

;; Allow nil in a parameter to represent the CORBA NIL object?


;;; Code:

(require 'cl)

(defvar corba-name-service "/tmp/NameService"
  "*Reference to the CORBA NameService.
This should be the name of a file where the name service IOR is stored
or the IOR.")

(defvar corba-interface-repository "/tmp/ir"
  "*Reference to the CORBA InterfaceRepository.
This should be the name of a file where the service IOR is stored
or the IOR.")


;;;; Exceptions

(put 'corba-system-exception 'error-conditions
     '(corba-system-exception corba-exception error))
(put 'corba-system-exception 'error-message "CORBA System Exception")

(put 'corba-user-exception 'error-conditions
     '(corba-user-exception corba-exception error))
(put 'corba-user-exception 'error-message "CORBA User Exception")


;;;; Structures

;; Interface: corba-object-id ?
(defstruct corba-object 
  (id nil)
  (host nil)
  (port nil)
  (key nil)
  (profiles nil)
  (forward nil))

;; Interface:
(defstruct corba-any
  (typecode nil)
  (value nil))

(defstruct (corba-opdef (:type list))
  name
  inparams
  outparams
  raises)

(defstruct corba-interface
  id
  operations
  inherit)


;;;; TypeCodes

(defconst TCKind
  [
   tk_null tk_void tk_short tk_long tk_ushort tk_ulong
   tk_float tk_double tk_boolean tk_char
   tk_octet tk_any tk_TypeCode tk_Principal tk_objref
   tk_struct tk_union tk_enum tk_string
   tk_sequence tk_array tk_alias tk_except
   tk_longlong tk_ulonglong tk_longdouble
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
(put 'tk_except 'tk-params '(complex string string
                             (sequence (anon-struct string tk_TypeCode))))

(declaim (inline make-typecode typecode-kind typecode-params))

(defun make-typecode (kind &optional params)
  (if params (cons kind params) kind))

(defun typecode-kind (tc)
  (if (symbolp tc) tc (car tc)))

(defun typecode-params (tc)
  (if (symbolp tc) nil (cdr tc)))

(defun lispy-name (string)
  (cond ((symbolp string)
	 string)
	(t
	 (intern (substitute ?- ?_ string)))))


;;;; Misc utilities

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

(defun cdr-make-encapsulation (closure &rest args)
  (save-excursion
    (set-buffer (get-buffer-create "*REQ*"))
    (goto-char (point-min))
    (save-restriction
      (narrow-to-region (point-min) (point-min))
      (apply closure args)
      (prog1 (buffer-substring (point-min) (point-max))
        (delete-region (point-min) (point-max))))))

(defun cdr-typecode (tc)
  (let ((kind (typecode-kind tc))
	(params (typecode-params tc)))
    (cdr-ulong (symbol-value kind))
    (let ((pspec (get kind 'tk-params)))
      (cond ((null pspec))
	    ((eq 'complex (car pspec))
             (cdr-make-encapsulation
              (lambda (params spec)
		(map nil #'cdr-marshal params spec))
	      params (cdr pspec)))
	    (t
             (map nil #'cdr-marshal params (cdr pspec)))))))


(defun cdr-ior (objref)
  (cdr-string (corba-object-id objref))
  (cdr-sequence (corba-object-profiles objref)
		(lambda (tagpair)
		  (cdr-ulong (car tagpair))
		  (cdr-osequence (cdr tagpair)))))

(defun cdr-marshal (arg type)
  (let (kind params)
    (cond ((consp type)
	   (setq kind (car type)
		 params (cdr type)))
	  (t (setq kind type)))
    (case kind
      ((tk_any) (cdr-marshal (corba-any-typecode arg) 'tk_typecode)
       (cdr-marshal (corba-any-value arg) (corba-any-typecode arg)))
      ((tk_octet tk_char) (cdr-octet arg))
      ((tk_boolean bool) (cdr-bool arg))
      ((tk_ushort tk_short) (cdr-ushort arg))
      ((tk_ulong tk_long) (cdr-ulong arg))
      ((tk_string string) (cdr-string arg))
      ((osequence) (cdr-osequence arg))
      ((tk_objref object) (cdr-ior arg))
      ((tk_alias) (cdr-marshal arg (third params)))
      ((sequence tk_sequence)
       (let ((_el_type_ (first params)))
	 (if (eq _el_type_ 'tk_octet)
	     (cdr-osequence arg)
	   (cdr-sequence arg (lambda (arg) (cdr-marshal arg _el_type_))))))
      ((tk_struct)
       (map nil (lambda (el)
                  (cdr-marshal (cdr (assq (lispy-name (first el)) arg))
                               (second el)))
            (third params)))
      ((anon-struct)
       (loop for type in params
	     for arg in arg
	     collect (cdr-marshal arg type)))
      (t
       (cdr-marshal arg (or (get type 'corba-typecode)
                            (error "MARSHAL: %s" type)))))))


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

(defun cdr-in-encapsulation (obj closure &rest args)
  (save-excursion
    (set-buffer (get-buffer-create "*CDR*"))
    (setq buffer-undo-list t)
    (goto-char (point-min))
    (let ((old-byte-order *byte-order*))
      (save-restriction
        (unwind-protect
            (progn (insert obj)
                   (narrow-to-region (point-min) (point))
                   (goto-char (point-min))
                   (setq *byte-order* (cdr-read-octet))
                   (apply closure args))
          (delete-region (point-min) (point-max))
          (setq *byte-order* old-byte-order))))))

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
           tk)
	  ((eq t params)
	   (make-typecode tk (cdr-read-osequence)))
	  ((eq 'complex (car params))
	   (cdr-in-encapsulation
            (cdr-read-osequence)
            (lambda (tk types)
              (make-typecode tk (mapcar #'cdr-unmarshal types)))
            tk (cdr params)))
	  (t
	   (make-typecode tk (mapcar #'cdr-unmarshal params))))))


(defun cdr-read-any ()
  (let ((tc (cdr-read-typecode)))
    (make-corba-any
     :typecode tc
     :value (cdr-unmarshal tc))))

(defun cdr-unmarshal (type)
  (let (kind params)
    (cond ((consp type)
	   (setq kind (car type)
		 params (cdr type)))
	  (t (setq kind type)))
    (case kind
      ((char octet tk_char tk_octet) (cdr-read-octet))
      ((bool tk_boolean) (cdr-read-bool))
      ((ushort tk_ushort) (cdr-read-ushort))
      ((ulong tk_ulong tk_enum) (cdr-read-ulong))
      ((string tk_string) (cdr-read-string))
      ((tk_any) (cdr-read-any))
      ((tk_sequence sequence)
       (let ((_ElType_ (car params)))
	 (if (eq _ElType_ 'tk_octet)
	     (cdr-read-osequence)
	   (cdr-read-sequence (lambda () (cdr-unmarshal _ElType_))))))
      ((tk_alias)
       (cdr-unmarshal (third params)))
      ((tk_struct)
       (cons (first params)
	     (map 'list (lambda (nt-pair)
                          (cons (lispy-name (first nt-pair))
                                (cdr-unmarshal (second nt-pair))))
                  (third params))))
      ((tk_except)
       (map 'list
            (lambda (nt-pair) (cdr-unmarshal (second nt-pair)))
            (third params)))
      ((object tk_objref) (cdr-read-ior))
      ((anon-struct)
       (loop for type in params
	     collect (cdr-unmarshal type)))
      ((tk_TypeCode) (cdr-read-typecode))
      (t
       (cdr-unmarshal (or (get kind 'corba-typecode)
                          (error "Can't handle TypeCode of kind %s" kind)))))))


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
    msgtype))

(defun cdr-read-service-context ()
  (let ((len (cdr-read-ulong)))
    (if (= len 0)
	'()
      (error "cdr-read-service-context NIY"))))

(defun cdr-read-tagged-component ()
  (cons (cdr-read-ulong)
	(cdr-read-osequence)))

(defun cdr-read-iiop-profile-body (reference)
   (cdr-read-octet)			;Version (ignored for now)
   (cdr-read-octet)
   (setf (corba-object-host reference) (cdr-read-string))
   (setf (corba-object-port reference) (cdr-read-ushort))
   (setf (corba-object-key reference) (cdr-read-osequence)))

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

;; Interface: make-corba-request corba-request-result
(defstruct corba-request
  (object nil)
  (operation nil)
  (arguments nil)
  (req-id nil)
  (client nil)
  (result nil))

(defvar *request-id-seq* 0)
(defvar *corba-waiting-requests* nil)

;; Interface:
(defun corba-request-send (req &optional flags)
  (let ((object (corba-request-object req)))
    (setq object (or (corba-object-forward object)
		     object))
    (condition-case exc
        (corba-request-send-to req object flags)
     (system-exception
       (setq object (corba-request-object req))
       (cond ((and (corba-object-forward object)
		   (member (car exc)
			   '("IDL:omg.org/CORBA/OBJECT_NOT_EXIST:1.0"
			     "IDL:omg.org/CORBA/COMM_FAILURE:1.0")))
	      (setf (corba-object-forward object) nil)
	      (corba-request-send-to req object flags))
	     (t
	      (signal exc)))))))

(defun corba-request-send-to (req object &optional flags)
  (let* ((client (get-connection
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
      (cdr-osequence (corba-object-key object))
      (cdr-string (corba-opdef-name operation))
      (cdr-osequence "")		;principal
      (loop for arg in (corba-request-arguments req)
	    for desc in (corba-opdef-inparams operation)
	    do (cdr-marshal arg (cdr desc)))
      (cdr-giop-set-message-length)
      (process-send-region client (point-min) (point-max))
      ;;(message "Request %d sent" (corba-request-req-id req))
      (accept-process-output)
      (push req *corba-waiting-requests*))))


(defun corba-read-reply (req)
  (setf (corba-request-result req) nil)
  (ecase (cdr-read-ulong)
    ((0)				; No Exception
     (setf (corba-request-result req)
	   (loop for desc in (corba-opdef-outparams
			      (corba-request-operation req))
		 collect (cdr-unmarshal (cdr desc))))
     t)
    ((1)				; User Exception
     (let* ((id (cdr-read-string)))
       (signal 'corba-user-exception
               (cons id (cdr-unmarshal (corba-get-typecode id))))))
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
      (delete-char *message-size*)
      (setq *message-size* nil))
    (cond
     ((>= (point-max) 12)
      (goto-char (point-min))
      (let ((msgtype (cdr-read-giop-header)))
        (setq *message-size* (+ 12 (cdr-read-ulong)))
        (cond
         ((<= (point-max) *message-size*)
          (setq *message-size* nil))
         ((= msgtype 1)                 ;Reply
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
                   (message "Unexpected respons for id %s" request-id)))))
         ((= msgtype 5)                 ;Close Connection
          (delete-process client)
          (error "Connection closed"))))))))


;; Interface:
(defun corba-get-next-respons (&optional flags)
  (let ((req nil))
    (loop
     do (setq req (loop for client in (get-clients)
			thereis (corba-get-next-respons-1 client)))
     until (or req (not (memq 'no-wait flags)))
     do (accept-process-output))
    req))

;; Interface:
(defun corba-request-get-response (req &optional flags)
  (let* ((client (corba-request-client req)))
    (loop while (eq t (corba-request-result req))
	  do (corba-get-next-respons-1 client)
	  until (memq 'no-wait flags)
	  do (accept-process-output)))
  (not (eq t (corba-request-result req))))


;; Interface:
(defun corba-request-invoke (req &optional flags)
  (corba-request-send req)
  (corba-request-get-response req)
  (corba-request-result req))


;;;; The ORB Interface

;;;###autoload
(defun corba-orb-init (&optional args orbid)
  nil)

;; Interface:
(defun corba-orb-resolve-initial-references (orb name)
  (cond
   ((string-equal name "NameService")
    (corba-file-to-object corba-name-service))
   ((string-equal name "InterfaceRepository")
    (corba-file-to-object corba-interface-repository))))


;; Interface:
(defun corba-orb-string-to-object (orb str)
  (if (string-match "IOR:" str)
      (cdr-with-encapsulation
       (loop for i from 4 below (length str) by 2
	     concat (char-to-string
		     (+ (* 16 (corba-hex-to-int (aref str i)))
			(corba-hex-to-int (aref str (1+ i))))))
       (cdr-read-ior))
    (error "Illegal string object")))

;; Interface:
(defun corba-orb-object-to-string (orb object)
  (let ((str (cdr-make-encapsulation #'cdr-ior object)))
    (concat "IOR:"
	    (upcase (loop for c across str
			  concat (format "%02x" c))))))


(defun corba-file-to-object (file)
  (corba-orb-string-to-object
   nil					; No orb-struct yet
   (if (string-match "IOR:" file)
       file				; Looks like the IOR itself
     (save-excursion
       (set-buffer (get-buffer-create "*REQ*"))
       (erase-buffer)
       (insert-file-contents file)
       (goto-char (point-min))
       (end-of-line 1)
       (buffer-substring (point-min) (point))))))


;;;; The Object Interface

;; Interface:
(defun corba-object-is-a (obj id)
  (car (corba-request-invoke 
        (make-corba-request
         :object obj
         :operation (make-corba-opdef :name "_is_a"
                                      :inparams '(("id" . tk_string))
                                      :outparams '(("" . tk_boolean)))
         :arguments (list id)))))

;; Interface:
(defun corba-object-is-nil (obj)
  (and (null (corba-object-key obj))
       (zerop (length (corba-object-profiles obj)))))



;;;; Interfaces and operations

(defconst corba-object-interface
  (make-corba-interface
   :id "IDL:omg.org/CORBA/Object:1.0"
   :operations (list
                (make-corba-opdef :name "_is_a"
                            :inparams '(("id" . tk_string))
                            :outparams '(("" . tk_boolean))
                            :raises '())
                (make-corba-opdef :name "_interface"
                            :outparams '(("" . tk_objref)))
                (make-corba-opdef :name "_non_existent"
                            :outparams '(("" . tk_boolean))))))

(defun corba-find-opdef (interface operation)
  "Find in INTERFACE the OPERATION and return the opdef struct."
  (or (find operation
	    (corba-interface-operations interface)
	    :test #'equal
	    :key #'corba-opdef-name)
      (loop for pint in (corba-interface-inherit interface)
	    thereis (corba-find-opdef pint operation))))


;;;; Internal Interface Repository

(defvar corba-local-repository
  (make-hash-table :test #'equal))


;; Interface:
(defun corba-add-interface (interface)
  (setf (gethash (corba-interface-id interface)
		 corba-local-repository)
	interface))


(defun corba-get-interface (id)
  (or (gethash id corba-local-repository)
      (setf (gethash id corba-local-repository)
	    (corba-interface-from-def id))))


(defun corba-get-typecode (id)
  (or (gethash id corba-local-repository)
      (setf (gethash id corba-local-repository)
	    (corba-typecode-from-def id))))


(defun corba-object-create-request (object op args)
  (let* ((interface (corba-get-interface
                     (if (consp op)
                         (first op)
                       (corba-object-id object))))
	 (opdef (corba-find-opdef interface
                                  (if (consp op)
                                      (second op)
                                    op))))
    (unless (= (length args)
	       (length (corba-opdef-inparams opdef)))
      (error "Wrong number of arguments to operation"))
    (make-corba-request :object object
                        :arguments args
                        :operation opdef)))


;; Interface:
(defun corba-invoke (obj op &rest args)
  (corba-request-invoke
   (corba-object-create-request obj op args)))


(defun corba-locate (obj)
  (let ((req (make-corba-request :object obj
                                 :operation 'locate)))
    (corba-request-invoke req)))


(defun corba-simplify-type (typecode)
  (macrolet ((mush (id def)
               `(or (gethash ,id corba-local-repository)
                 (setf (gethash ,id corba-local-repository) ,def)))
             (simplifyf (var)
               `(progn (setf ,var (corba-simplify-type ,var))
                 typecode))
             (simplifyv (vec fun)
               `(progn (loop for el in ,vec do (simplifyf (,fun el)))
                 typecode)))
    (let ((params (typecode-params typecode)))
      (case (typecode-kind typecode)
        ((tk_objref tk_string tk_enum) (typecode-kind typecode))
        ((tk_alias) (mush (first params) (corba-simplify-type (third params))))
        ((tk_sequence) (simplifyf (first params)))
        ((tk_struct) (mush (first params) (simplifyv (third params) second)))
        ((tk_except) (mush (first params) (simplifyv (third params) second)))
        (t  typecode)))))



;;;; CORBA Structure support

;; Interface:
(defun corba-struct-typecode (id  &optional name fields)
  (if (null fields)
      (corba-get-typecode id)
    (corba-simplify-type
     (make-typecode 'tk_struct
                    (list id (format "%s" (or name ""))
                          (if (consp fields)
                              (apply #'vector fields)
                              fields))))))

;; Interface:
(defsubst corba-struct-get (struct key)
  "Get field with KEY from the STRUCT."
  (cdr (assq key struct)))

;; Interface:
(defun corba-struct (id &rest nv-pairs)
  "Make a CORBA structure of type ID.
NV-PAIRS is a list field names and field values.
If ID is nil, then all fields must be supplied. Otherwise some types
of fields can be defaulted (numbers and strings)."
  (cond
   ((null id)
    (cons "" (loop for nv on nv-pairs by #'cddr collect
                   (cons (first nv) (second nv)))))
   (t
    (let ((tc (corba-get-typecode id)))
      (destructuring-bind (id name fields)
          (typecode-params tc)
        (cons id
              (mapcar (lambda (nv)
                        (let* ((fname (lispy-name (first nv)))
                               (val (getf nv-pairs fname nv)))
                          (cons fname
                                (if (eq val nv)
                                    (corba-default-from-type (second nv))
                                  val))))
                      fields)))))))

(defun corba-default-from-type (typecode)
  (ecase (typecode-kind typecode)
    ((tk_ushort tk_short tk_ulong tk_long) 0)
    ((tk_string) "")
    ((tk_objref) (make-corba-object))))

;;;; IR -- initial repository contents

(corba-add-interface
 (make-corba-interface
  :id "IDL:omg.org/CORBA/IRObject:1.0"
  :inherit (list corba-object-interface)
  :operations (list
	       (make-corba-opdef :name "_get_def_kind"
                                 :outparams '(("" . tk_ulong))))))


(corba-add-interface
 (make-corba-interface
  :id "IDL:omg.org/CORBA/Contained:1.0"
  :inherit (mapcar #'corba-get-interface '("IDL:omg.org/CORBA/IRObject:1.0"))
  :operations
  (list
   (make-corba-opdef :name "_get_id"
                     :outparams '(("" . tk_string)))
   (make-corba-opdef :name "_get_name"
                     :outparams '(("" . tk_string)))
   (make-corba-opdef :name "_get_defined_in"
                     :outparams '(("" . tk_objref)))
   (make-corba-opdef :name "describe"
                     :outparams `(("" .
                                   ,(corba-struct-typecode
                                     "IDL:omg.org/CORBA/Contained/Description:1.0"
                                     "Description"
                                     [(kind tk_ulong) (value tk_any)])))))))

(corba-add-interface
 (make-corba-interface
  :id "IDL:omg.org/CORBA/Container:1.0"
  :inherit (mapcar #'corba-get-interface '("IDL:omg.org/CORBA/IRObject:1.0"))
  :operations
  (list
   (make-corba-opdef :name "lookup"
                     :inparams '(("search_name" . tk_string))
                     :outparams '(("" . tk_objref)))
   (make-corba-opdef :name "contents"
                     :inparams '(("limit_type" . tk_ulong)
                                 ("exclude_inherit" . tk_boolean))
                     :outparams '(("" sequence tk_objref)))
   (make-corba-opdef :name "lookup_name"
                     :inparams '(("search_name" . tk_string)
                                 ("levels_to_search" . tk_long)
                                 ("limit_type" . tk_ulong)
                                 ("exclude_inherit" . tk_boolean))
                     :outparams '(("" . (sequence tk_objref)))))))

(corba-add-interface
 (make-corba-interface
  :id "IDL:omg.org/CORBA/IDLType:1.0"
  :inherit (mapcar #'corba-get-interface '("IDL:omg.org/CORBA/IRObject:1.0"))
  :operations (list
	       (make-corba-opdef :name "_get_type"
                                 :outparams '(("" . tk_TypeCode))))))

(corba-add-interface
 (make-corba-interface
  :id "IDL:omg.org/CORBA/Repository:1.0"
  :inherit (mapcar #'corba-get-interface '("IDL:omg.org/CORBA/Container:1.0"))
  :operations (list
	       (make-corba-opdef :name "lookup_id"
                                 :inparams '(("search_id" . tk_string))
                                 :outparams '(("" . tk_objref))))))

(corba-add-interface
 (make-corba-interface
  :id "IDL:omg.org/CORBA/OperationDef:1.0"
  :inherit (mapcar #'corba-get-interface '("IDL:omg.org/CORBA/Contained:1.0"))
  :operations
  (list
   (make-corba-opdef :name "_get_result"
                     :outparams '(("" . tk_TypeCode)))
   (make-corba-opdef
    :name "_get_params"
    :outparams `(("" sequence ,(corba-struct-typecode
                                "IDL:omg.org/CORBA/ParameterDescription:1.0"
                                "ParameterDescription"
                                [(name tk_string )
                                 (type tk_TypeCode)
                                 (type-def tk_objref)
                                 (mode tk_ulong)       ])))))))

(corba-add-interface
 (make-corba-interface
  :id "IDL:omg.org/CORBA/InterfaceDef:1.0"
  :inherit (mapcar #'corba-get-interface '("IDL:omg.org/CORBA/Container:1.0"
                                           "IDL:omg.org/CORBA/Contained:1.0"
                                           "IDL:omg.org/CORBA/IDLType:1.0"))
  :operations
  (list
   (make-corba-opdef :name "_get_base_interfaces"
                     :outparams '(("" . (sequence tk_objref))))
   (make-corba-opdef :name "is_a"
                     :inparams '(("interface_id" . tk_string))
                     :outparams '(("" . tk_boolean))))))


(corba-add-interface
 (make-corba-interface
  :id "IDL:omg.org/CORBA/ExceptionDef:1.0"
  :inherit (mapcar #'corba-get-interface '("IDL:omg.org/CORBA/Contained:1.0"
                                           "IDL:omg.org/CORBA/IDLType:1.0"))
  :operations
  (list
   (make-corba-opdef :name "_get_members"
                     :outparams '(("" sequence
                                   (anon-struct string tk_TypeCode tk_objref))))
   (make-corba-opdef :name "_get_type"
                     :outparams '(("" . tk_TypeCode))))))

;;;; Using real IR

(defun corba-get-ir ()
  (corba-orb-resolve-initial-references nil "InterfaceRepository"))

(defun corba-opdef-from-ir (irdef)
  (when (stringp irdef)
    (setq irdef (car (corba-invoke (corba-get-ir) "lookup_id" irdef))))

  (let ((name (car (corba-invoke irdef "_get_name"))) 
	(inpars nil)
	(outpars nil)
	(result (car (corba-invoke irdef "_get_result"))))

    (unless (eq 'tk_void (typecode-kind result))
      (push (cons "" (corba-simplify-type result)) outpars))

    (loop for pardesc in (car (corba-invoke irdef "_get_params"))
	  for mydesc = (cons (corba-struct-get pardesc 'name)
			     (corba-simplify-type (corba-struct-get pardesc 'type)))
	  for mode = (corba-struct-get pardesc 'mode)
	  do (cond ((memq mode '(0 2))
		    (push mydesc inpars)))
	  do (cond ((memq mode '(1 2))
		    (push mydesc outpars))))
    (make-corba-opdef
     :name name
     :inparams (nreverse inpars)
     :outparams (nreverse outpars))))


(defun corba-interface-from-def (def)
  (when (stringp def)
    (let ((id def))
      (setq def (car (corba-invoke (corba-get-ir) "lookup_id" id)))
      (when (corba-object-is-nil def)
	(error "InterfaceRepository do not know about %s" id))))
  (let ((id (car (corba-invoke def "_get_id")))
        (mess ";;; Getting interface %s %s")
        (progr ""))
    (message mess id progr)
    (make-corba-interface
     :id id
     :inherit (or (mapcar #'corba-get-interface
                          (car (corba-invoke def "_get_base_interfaces")))
                  (list corba-object-interface))
     :operations (mapcar (lambda (o)
                           (prog1 (corba-opdef-from-ir o)
                             (setq progr (concat progr "."))
                             (message mess id progr)))
                         (car (corba-invoke def "contents" 7 t))))))


(defun corba-typecode-from-def (def)
  (when (stringp def)
    (let ((id def))
      (message ";;; Getting type %s" id)
      (setq def (car (corba-invoke (corba-get-ir) "lookup_id" id)))
      (when (corba-object-is-nil def)
	(error "InterfaceRepository do not know about %s" id))))
  (let ((typecode (car (corba-invoke def "_get_type"))))
    (corba-simplify-type typecode)))


;;; corba.el ends here
