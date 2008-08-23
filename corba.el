;;; corba.el --- A Client Side CORBA Implementation for Emacs

;; Copyright (C) 1998--2008 Lennart Staflin

;; Author: Lennart Staflin <lenst@lysator.liu.se>
;; Version: $Id: corba.el,v 1.33 2006/04/13 11:07:23 lenst Exp $
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
;; $Date: 2006/04/13 11:07:23 $|$Revision: 1.33 $||

;;; Commentary:

;; Provides an implementation of CORBA Dynamic Invocation interface
;; using the IIOP protocol.

;;; TODO:

;; Marshaling code for: longlong, ulonglong, floats, fixed,
;; union, array, wchar, wstring

;; How should overflow in long/ulong be handled?
;; -> going to float maybe

;; The typeid in an IOR is optional, handle that case by asking the remote
;; object for the interface.

;; Server side:
;; probably need a helper program that handles the
;; sockets and multiplexes messages.
;; With emacs 22 it should be possible to implement the server in Emacs Lisp.

;; Generation of static stubs.

;; Saving the internal interface repository to a Lisp file for later
;; use without a Repository service.



;;; Code:

(provide 'corba)

(require 'cl)
;;(eval-when-compile (require 'cl))
(eval-when-compile (load "cl-extra"))   ; This seems to fix some strange autoloading
                                        ; problem.


(defgroup corba ()
  "Library for calling CORBA servers."
  :group 'development)


(defcustom corba-name-service nil
  "*Reference to the CORBA NameService.
Should be a object reference in string form (see `corba-obj')."
  :group 'corba
  :type '(choice (const :tag "Off" nil) string))

(defcustom corba-interface-repository nil
  "*Reference to the CORBA InterfaceRepository.
Should be a object reference in string form (see `corba-obj')."
  :group 'corba
  :type '(choice (const :tag "Off" nil) string))

(defcustom corba-use-interface-repository nil
  "*If non-nil, use the configured interface repositiory to find unknown interfaces.
When a operation is called on a object with an interface that is not already
in the local repository (cache), a remote call will be made to the interface repository
to download the interface description."
  :group 'corba
  :type 'boolean)

(defcustom corba-use-get-interface nil
  "*If non-nil, ask an object for its interface definition if its interface is not known.
This uses the _get_interface CORBA message. This is porly implemented in many ORBs."
  :group 'corba
  :type 'boolean)

(defvar corba-principal ""
  "*Octet sequence used for the principal field in the GIOP message.
Used by ORBit for its cookie.")

(defvar corba-explicit-any t
  "*If non-nil, an explicit any struct will be returned for any result.
If nil, the actual value will be returned.")



;;;; Structures


(defstruct corba-object
  (id nil)
  (host nil)
  (port nil)
  (key nil)
  (profiles nil)
  (forward nil))


(defstruct (corba-opdef (:type list))
  name
  inparams
  outparams
  raises
  (mode :OP_NORMAL))


(defstruct corba-interface
  id
  operations
  inherit)



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


(defun corba-posq (x list)
  (let ((i 0) result)
    (while list
      (if (eq x (car list))
          (setq list nil result i)
          (setq i (1+ i) list (cdr list))))
    result))

(defun corba-vposq (x vector)
  (let ((i 0) (len (length vector)) (result nil))
    (while (< i len)
      (if (eq x (aref vector i))
          (setq result i i len )
          (setq i (1+ i))))
    result))

(defun corba-make-keyword (string)
  (intern (concat ":" string)))


;;;; Work buffer managing

(defvar corba-work-buffer nil)

(defun corba-get-work-buffer ()
  (unless (and corba-work-buffer (buffer-live-p corba-work-buffer))
    (setq corba-work-buffer
          (generate-new-buffer " *CDR*"))
    (let ((ob (current-buffer)))
      (set-buffer corba-work-buffer)
      (set-buffer-multibyte nil)
      (make-local-variable 'corba-work-buffer)
      (setq corba-work-buffer nil)
      (setq buffer-undo-list t)
      (set-buffer ob)))
  corba-work-buffer)

(defun corba-set-work-buffer ()
  (set-buffer (corba-get-work-buffer))
  (erase-buffer))

(defmacro corba-in-work-buffer (&rest body)
  (let ((cb-var (gensym)))
      `(let ((,cb-var (current-buffer)))
         (unwind-protect
             (progn (corba-set-work-buffer) ,@body)
           (set-buffer ,cb-var)))))

(put 'corba-in-work-buffer 'lisp-indent-function 0)



;;;; Marshal Primitives


(defun corba-write-octet (n)
  (insert n))

(defun corba-write-bool (s)
  (insert (if s 1 0)))

(defun corba-write-align (n)
  (while (/= 1 (% (point) n))
    (insert 0)))

(defun corba-write-short (n)
  (corba-write-align 2)
  (insert n
          (logand (ash n -8)  255)))

(defsubst corba-write-ushort (n) (corba-write-short n))

(defun corba-write-ulong (n)
  (corba-write-align 4)
  (insert (logand n           255)
          (logand (ash n -8)  255)
          (logand (ash n -16) 255)
          (logand (ash n -24) 255)))

(defsubst corba-write-long (n) (corba-write-ulong n))

(defun corba-write-string (s)
  (corba-write-ulong (1+ (length s)))
  (insert s 0))


(defun corba-write-osequence (s)
  (corba-write-ulong (length s))
  (insert s))


(defun corba-write-sequence (el-cdr s &rest args)
  (corba-write-ulong (length s))
  (if args
      (if (consp s)
          (dolist (x s)
            (apply el-cdr x args))
          (dotimes (i (length s))
            (apply el-cdr (aref s i) args)))
      (mapc el-cdr s)))


(defun corba-make-encapsulation (closure &rest args)
  (corba-in-work-buffer
    (insert 1)                        ; Byte order
    (apply closure args)
    (buffer-substring (point-min) (point-max))))


(defun corba-write-encapsulation (func &rest args)
  (corba-write-osequence
   (apply #'corba-make-encapsulation func args)))



;;;; Unmarshal primitives


(defvar corba-byte-order 1)
(make-variable-buffer-local 'corba-byte-order)

(defsubst corba-read-octet ()
  (prog1 (following-char) (forward-char 1)))

(defun corba-read-bool ()
  (/= (corba-read-octet) 0))

(defsubst corba-read-align (n)
  (while (/= 1 (% (point) n))
    (forward-char 1)))


(defmacro corba-read-number (size signed)
  `(progn
     (corba-read-align ,size)
     (if (= corba-byte-order 1)
         (+
          ,@(loop for c below size collect
                  `(* ,(expt 2 (* c 8))
                      ,(if (and signed (= c (- size 1)))
                           '(let ((b (corba-read-octet)))
                              (if (> b 127) (- b 256) b))
                         '(corba-read-octet)))))
       (+
        ,@(loop for c from (1- size) downto 0 collect
                `(* ,(expt 2 (* c 8))
                    ,(if (and signed (= c (1- size)))
                         '(let ((b (corba-read-octet)))
                            (if (> b 127) (- b 256) b))
                       '(corba-read-octet))))))))

(defun corba-read-ushort ()
  (corba-read-number 2 nil))

(defun corba-read-short ()
  (corba-read-number 2 t))

(defsubst corba-read-ulong ()
  (corba-read-number 4 nil))

(defun corba-read-ulong-split ()
  ;; returns: (upper16 . lower16)
  (corba-read-align 4)
  (if (= corba-byte-order 0)
      (cons (corba-read-ushort)
            (corba-read-ushort))
      (let ((lo (corba-read-ushort)))
        (cons (corba-read-ushort) lo))))

(defun corba-read-long ()
  (corba-read-number 4 t))

(defun corba-read-sequence (el-reader &rest args)
  (let ((len (corba-read-ulong)))
    (loop for i from 1 upto len
	  collect (apply el-reader args))))

(defun corba-read-string ()
  (let* ((len (corba-read-ulong))
	 (start (point)))
    (forward-char len)
    (buffer-substring start (1- (point)))))

(defun corba-read-osequence ()
  (let* ((len (corba-read-ulong))
	 (start (point)))
    (forward-char len)
    (buffer-substring start (point))))


(defun corba-in-encapsulation (obj closure &rest args)
  (corba-in-work-buffer
    (insert obj)
    (goto-char (point-min))
    (setq corba-byte-order (corba-read-octet))
    (apply closure args)))



;;;; TypeCodes

(defconst corba-tc-kind
  [
   :tk_null :tk_void :tk_short :tk_long :tk_ushort :tk_ulong
   :tk_float :tk_double :tk_boolean :tk_char
   :tk_octet :tk_any :tk_typecode :tk_principal :tk_objref
   :tk_struct :tk_union :tk_enum :tk_string
   :tk_sequence :tk_array :tk_alias :tk_except
   :tk_longlong :tk_ulonglong :tk_longdouble
   :tk_wchar :tk_wstring :tk_fixed :tk_value :tk_value_box
   :tk_native :tk_abstract_interface :tk_local_interface
   ]
  "The symbols for the TCKind enum")

(eval-when (load eval)
  (loop for i from 0 below (length corba-tc-kind)
	do (put (elt corba-tc-kind i) 'tk-index i)))



;;;; New TypeCode Definitions


(defmacro corba-define-typecode (kind cdr-syntax &optional marshal unmarshal)
  `(progn
    ,(if cdr-syntax
         `(put ,kind 'tk-params ',cdr-syntax))
    ,(if marshal
         `(put ,kind 'corba-marshal ',marshal))
    ,(if unmarshal
         `(put ,kind 'corba-unmarshal ',unmarshal))))


(corba-define-typecode :tk_octet nil corba-write-octet corba-read-octet)
(corba-define-typecode :tk_char nil corba-write-octet corba-read-octet)
(corba-define-typecode :tk_any nil corba-write-any corba-read-any)
(corba-define-typecode :tk_boolean nil corba-write-bool corba-read-bool)
(corba-define-typecode :tk_short nil corba-write-short corba-read-short)
(corba-define-typecode :tk_ushort nil corba-write-short corba-read-ushort)
(corba-define-typecode :tk_long nil corba-write-ulong corba-read-long)
(corba-define-typecode :tk_ulong nil corba-write-ulong corba-read-ulong)
(corba-define-typecode :tk_typecode nil
                       corba-write-typecode corba-read-typecode)
(corba-define-typecode :tk_struct
                       (complex string string
                                (sequence (string typecode)))
                       (corba-marshal-struct) (corba-unmarshal-struct))
(corba-define-typecode :tk_fixed (ushort short))
(corba-define-typecode :tk_objref (complex string string)
                       corba-write-ior corba-read-ior)
(corba-define-typecode :tk_union
                       (complex string string typecode long
                                (sequence (2 string typecode)))
                       (corba-marshal-union) (corba-unmarshal-union))
(corba-define-typecode :tk_enum
                       (complex string string
                                (sequence string))
                       (corba-marshal-enum) (corba-unmarshal-enum))
(corba-define-typecode :tk_sequence (complex typecode ulong)
                       (corba-marshal-sequence) (corba-unmarshal-sequence))
(corba-define-typecode :tk_string (ulong) corba-write-string corba-read-string)
(corba-define-typecode :tk_wstring (ulong))
(corba-define-typecode :tk_array (complex typecode ulong)
                       (corba-marshal-array) (corba-unmarshal-array))
(corba-define-typecode :tk_alias (complex string string typecode)
                       (corba-marshal-alias) (corba-unmarshal-alias))
(corba-define-typecode :tk_except
                       (complex string string
                                (sequence (string typecode)))
                       (corba-marshal-except) (corba-unmarshal-except))
(corba-define-typecode :tk_value
                       (complex string string short typecode
                                (sequence (string typecode short))))
(corba-define-typecode :tk_value_box (complex string string typecode))
(corba-define-typecode :tk_native (complex string string))
(corba-define-typecode :tk_abstract_interface (complex string string))
(corba-define-typecode :tk_local_interface (complex string string))



;;;; TypeCode operations


(defsubst make-corba-typecode (kind &optional params)
  (cons kind params))


(defun corba-typecode-p (x)
  (and (consp x)
       (keywordp (car x))))


(defsubst corba-typecode-kind (tc)
  (car tc))

(defsubst corba-typecode-params (tc)
  (cdr tc))


(defconst corba-tc-void '(:tk_void))
(defconst corba-tc-short '(:tk_short))
(defconst corba-tc-long '(:tk_long))
(defconst corba-tc-ushort '(:tk_ushort))
(defconst corba-tc-ulong '(:tk_ulong))
(defconst corba-tc-double '(:tk_double))
(defconst corba-tc-boolean '(:tk_boolean))
(defconst corba-tc-char '(:tk_char))
(defconst corba-tc-octet '(:tk_octet))
(defconst corba-tc-any '(:tk_any))
(defconst corba-tc-typecode '(:tk_typecode))
(defconst corba-tc-string '(:tk_string 0))
(defconst corba-tc-object
  '(:tk_objref "IDL:omg.org/CORBA/Object:1.0" "Object"))



;;;; TypeCode Repository


(defvar corba-local-typecode-repository
  (make-hash-table :test #'equal)
  "From RepoID to TypeCode")


(defun corba-has-typecode-p (id)
  (gethash id corba-local-typecode-repository))


(defun corba-add-typecode-with-id (id typecode &optional no-force)
  (or (if no-force (gethash id corba-local-typecode-repository))
      (setf (gethash id corba-local-typecode-repository) typecode)))


(defun corba-kind-has-id-p (kind)
  (memq kind '(:tk_struct :tk_enum :tk_objref :tk_alias :tk_union
               :tk_value :tk_value_box :tk_native :tk_local_interface
               :tk_except :tk_abstract_interface)))


(defconst corba-typecode-canonizers
  '((:tk_struct . corba-struct-canonize)
    (:tk_except . corba-struct-canonize)
    (:tk_union . corba-union-canonize)
    (:tk_alias . corba-type-3-canonize)
    (:tk_union . corba-union-canonize)
    (:tk_value_box . corba-type-3-canonize)
    (:tk_sequence . corba-type-1-canonize)
    (:tk_array . corba-type-1-canonize)))


(defun corba-struct-canonize (tc)
  ;; (:tk_struct id name ((member-name type)*))
  (list (car tc) (cadr tc) (nth 2 tc)
        (mapcar (lambda (member)
                  (list (car member)
                        (corba-typecode (cadr member))))
                (nth 3 tc))))


(defun corba-union-canonize (tc)
  ;; (:tk_union id name discriminator-type default-index
  ;;    ((member-label member-name member-type)*))
  ;;  where default-index is index of member that is the default
  ;;           or -1 if no default
  ;;        member-label is of type discriminator-type
  (list (car tc) (cadr tc) (nth 2 tc)   ; :tk_union id name
        (corba-typecode (nth 3 tc))     ; discriminator-type
        (nth 4 tc)                      ; default-index
        (mapcar (lambda (member)
                  (list (car member)    ; member-label
                        (cadr member)   ; member-name
                        (corba-typecode (nth 2 member))))
                (nth 5 tc))))


(defun corba-type-1-canonize (tc)
  (list* (car tc) (corba-typecode (cadr tc))
         (cddr tc)))

(defun corba-type-3-canonize (tc)
  (list* (car tc) (cadr tc) (nth 2 tc)
         (corba-typecode (nth 3 tc))
         (nthcdr 4 tc)))


(defun corba-typecode-canonize (tc)
  (let ((canonizer (cdr (assq (car tc) corba-typecode-canonizers))))
    (if canonizer
        (funcall canonizer tc)
        tc)))


;; Interface: corba-typecode
(defun corba-typecode (typecode-or-id)
  "Returns canonical typecode"
  (cond ((stringp typecode-or-id)       ; id
         (gethash typecode-or-id corba-local-typecode-repository))
        ((corba-kind-has-id-p (car typecode-or-id))
         (let* ((id (cadr typecode-or-id))
                (old-tc (gethash id corba-local-typecode-repository)))
           (if (and old-tc (eq (car old-tc) (car typecode-or-id)))
               old-tc
               (progn
                 (unless old-tc
                   (let ((id (cadr typecode-or-id)))
                     (setq old-tc (cons nil id))
                     (puthash id old-tc corba-local-typecode-repository)))
                 (let ((tc (corba-typecode-canonize typecode-or-id)))
                   (setcar old-tc (car tc))
                   (setcdr old-tc (cdr tc))
                   old-tc)))))
        (t
         (corba-typecode-canonize typecode-or-id))))


;; Prime
(corba-typecode corba-tc-object)



;;;; Marshal / UnMarshal


(defun corba-marshal (arg type)
  (let ((kind (corba-typecode-kind type)))
    (let ((marshal (get kind 'corba-marshal)))
      (cond ((consp marshal)
             (funcall (car marshal) arg type))
            (marshal
             (funcall marshal arg))
            (t
             (error "CORBA MARSHAL: can't marshal type: %s" type))))))


(defun corba-unmarshal (type)
  (let ((kind (corba-typecode-kind type)))
    (let ((unmarshal (get kind 'corba-unmarshal)))
      (assert unmarshal nil "Can't unmarshal Type=%s" type)
      (if (consp unmarshal)
          (funcall (car unmarshal) type)
          (funcall unmarshal)))))



;;;; TypeCode marshalling


(defvar *typecode-params*)

(defvar corba-indirection-record nil)


(defun corba-write-spec (params pspec)
  (cond ((null pspec))
        ((numberp pspec)
         (corba-marshal (elt *typecode-params* pspec) params))
        ((consp pspec)
         (case (car pspec)
           (complex
            (corba-write-encapsulation
             'corba-write-spec params (cdr pspec)))
           (sequence
            (corba-write-sequence
             'corba-write-spec params (cadr pspec)))
           (otherwise
            (dolist (p pspec)
              (corba-write-spec (pop params) p)))))
        (t
         (ecase pspec
           (string (corba-write-string params))
           ((short ushort) (corba-write-short params))
           ((long ulong) (corba-write-ulong params))
           (typecode (corba-write-typecode params))))))


(defun corba-read-spec (params toplevel)
  ;; toplevel is the toplevel sequence being read
  (cond ((null params) nil)
        ((numberp params)
         (corba-unmarshal (elt (cdr toplevel) params)))
        ((symbolp params)
         (ecase params
           (string (corba-read-string))
           (short (corba-read-short))
           (ushort (corba-read-ushort))
           ((long ulong) (corba-read-ulong))
           (typecode (corba-read-typecode))))
        ((eq 'complex (car params))
         (corba-in-encapsulation
          (corba-read-osequence)
          'corba-read-spec (cdr params) toplevel))
        ((eq 'sequence (car params))
         (corba-read-sequence 'corba-read-spec (cadr params) toplevel))
        (t
         (let* ((record (cons nil nil))
                (origin record))
           (dolist (p params)
             (setcdr record (list (corba-read-spec p (or toplevel origin))))
             (setq record (cdr record)))
           (cdr origin)))))


(defun corba-read-indirect-typecode ()
  (debug))



(defun corba-write-typecode (tc)
  (let ((kind (corba-typecode-kind tc))
	(*typecode-params* (corba-typecode-params tc)))
    (corba-write-ulong (get kind 'tk-index))
    (corba-write-spec *typecode-params*
                      (get kind 'tk-params))))


(defvar corba-intern-all-typecodes nil)

(defun corba-read-typecode ()
  (let* ((kind-index (corba-read-ulong))
	 (tk (if (= kind-index #xFFFFFFFF)
                 :indirection
                 (aref corba-tc-kind kind-index))))
    (if (eq tk :indirection)
        (corba-read-indirect-typecode)
        (let* ((params (get tk 'tk-params))
               (typecode (list tk))
               (corba-indirection-record
                (cons (cons (- (point) 4) typecode)
                      corba-indirection-record)))
          (setcdr typecode (corba-read-spec params nil))
          (if corba-intern-all-typecodes
              (corba-typecode typecode)
            typecode)))))



;;;; TypeCode Meta Cache


(defvar corba-typecode-meta-cache
  (make-hash-table))

(defmacro corba-with-cached-meta (typecode &rest body)
  (declare (indent 1))
  `(let ((_tc ,typecode))
     (or (gethash _tc corba-typecode-meta-cache)
         (puthash _tc
                  (progn ,@body)
                  corba-typecode-meta-cache))))



;;;; CORBA::Enum

;;  (:tk_enum id name (member-name*))

(defun corba-enum-symbols (tc)
  (assert (eql (corba-typecode-kind tc) :tk_enum))
  (corba-with-cached-meta tc
    (mapcar 'corba-make-keyword (nth 3 tc))))

(defun corba-marshal-enum (arg type)
  (if (numberp arg)
      (corba-write-ulong arg)
      (let ((pos (corba-posq arg (corba-enum-symbols type))))
        (if pos
            (corba-write-ulong pos)
            (error "CORBA MARSHAL: %S is not correct enum constant"
                   arg)))))

(defun corba-unmarshal-enum (type)
  (elt (corba-enum-symbols type) (corba-read-ulong)))



;;;; CORBA::Sequence

(defun corba-unmarshal-sequence (type)
  (let ((params (corba-typecode-params type)))
    (let ((el-type (car params)))
	 (if (eq (corba-typecode-kind el-type) :tk_octet)
	     (corba-read-osequence)
             (corba-read-sequence 'corba-unmarshal el-type)))))

(defun corba-marshal-sequence (seq type)
  ;; seq = sequence
  ;; type = (:tk_sequence el-type len)
  (let ((el-type (cadr type)))
    (if (eq el-type :tk_octet)
        (corba-write-osequence seq)
        (corba-write-sequence 'corba-marshal seq el-type))))


;;;; CORBA::Array


(defun corba-marshal-array (seq type)
  ;; seq = vector
  ;; type = (:tk_array el-type len)
  (let ((el-type (cadr type))
        (len (car (cddr type))))
    (assert (= (length seq) len))
    (let ((el-marshal (get (corba-typecode-kind el-type) 'corba-marshal)))
      (mapc (if (consp el-marshal)
                (progn (setq el-marshal (car el-marshal))
                       (lambda (el)
                         (funcall el-marshal el el-type)))
                el-marshal)
            seq))))


(defun corba-unmarshal-array (type)
  (let ((el-type (cadr type))
        (len (nth 2 type)))
    (let ((v (make-vector len nil))
          (el-unmarshal (get (corba-typecode-kind el-type) 'corba-unmarshal)))
      (let ((unmarshal
             (if (consp el-unmarshal)
                 (lambda () (funcall (car el-unmarshal) el-type))
                 el-unmarshal)))
        (dotimes (i len)
          (aset v i (funcall unmarshal))))
      v)))



;;;; CORBA::Alias


(defun corba-marshal-alias (obj type)
  ;; (:tk_alias id name type)
  (corba-marshal obj (nth 3 type)))

(defun corba-unmarshal-alias (type)
  ;; (:tk_alias id name type)
  (corba-unmarshal (nth 3 type)))



;;;; CORBA::Struct

;;; Typecode: (:tk_struct id name ((member-name type)*))
;;; Representation: [[name key1 key2..] value1 value2..]
;;;   where keyi keyword form of member-name i


(defun corba-struct-p (sexp)
  (and (vectorp sexp)
       (vectorp (aref sexp 0))
       (= (length sexp) (length (aref sexp 0)))))


(defun corba-struct-symbols (tc)
  ;; => ["name" :field1 :field2 ...]
  (assert (memq (corba-typecode-kind tc) '(:tk_struct :tk_except)))
  (corba-with-cached-meta tc
    (apply 'vector
           (cons (nth 2 tc)
                 (mapcar (lambda (m) (corba-make-keyword (car m)))
                         (nth 3 tc))))))


(defun corba-marshal-struct (struct struct-type)
  ;; struct = [keys v1 v2..]
  ;; struct-type = (:tk_struct id name ((member-name type)*))
  (let ((members (nth 3 struct-type))
        (i 1))
    (dolist (m members)
      (corba-marshal (aref struct i) (cadr m))
      (setq i (1+ i)))))


(defun corba-unmarshal-struct (type)
  ;; type = (:tk_struct id name ((member-name type)*))
  ;; result: [keys v1 v2..]
  (let* ((members (nth 3 type))
         (result (make-vector (1+ (length members)) nil))
         (i 1))
    (aset result 0 (corba-struct-symbols type))
    (dolist (m members)
      (aset result i (corba-unmarshal (cadr m)))
      (setq i (1+ i)))
    result))


(defun corba-repoid-p (id)
  (and (stringp id) (eq ?I (aref id 0))))
(defun corba-aname-p (id)
  (and (stringp id) (eq ?: (aref id 0))))

(defun corba-lookup-type (name)
  (cond ((corba-repoid-p name) (corba-typecode name))
        ((corba-aname-p name) (corba-lookup name))
        (t (error "Invalid type name: %a" name))))

(defun corba-new-struct (tc fields)
  (let ((syms (corba-struct-symbols tc)))
    (let ((result (make-vector (length syms) nil)))
      (aset result 0 syms)
      (while fields
        (let ((key (pop fields))
              (val (pop fields)))
          (let ((pos (corba-vposq key syms)))
            (or pos (error "Invalid struct key: %s, %s" key syms))
            (aset result pos val))))
      result)))



;;;; Exceptions

;;; User Exceptions

;; TypeCode: (:tk_except id name ((member-name type)*))

;; Representation: (corba-user-exception id [[name key1 key2..] value1 value2..])
;; Cdr suitable for signal data
;; A condition-case var would then have the value:
;;  (corba-user-exception ID [[name key1 key2..] value1 value2..])

;;; System Exceptions
;; Repr: (corba-system-exception id minor-upper minor-lower completed)
;; where corba-system-exception = the symbol corba-system-exception
;;       id = repository id
;;       minor-upper = the upper 16bits of the minor code (= corba-omgvmcid-upper
;;                              for standard minor codes)
;;       minor-lower = the lower 16bits of the minor code
;;       completed  = one of [:COMPLETED_YES :COMPLETED_NO :COMPLETED_MAYBE]


(defconst corba-omgvmcid-upper #x4f4d)


(put 'corba-system-exception 'error-conditions
     '(corba-system-exception corba-exception error))
(put 'corba-system-exception 'error-message "CORBA System Exception")

(put 'corba-user-exception 'error-conditions
     '(corba-user-exception corba-exception error))
(put 'corba-user-exception 'error-message "CORBA User Exception")


(defun corba-unmarshal-except (acceptable-typecodes)
  (let ((id (corba-read-string))
        tc)
    ;; Find typecode
    (while acceptable-typecodes
      (let ((candidate (pop acceptable-typecodes)))
        (when (equal id (cadr candidate))
          (setq tc candidate
                acceptable-typecodes nil))))
    (if tc
        (list 'corba-user-exception
              id (corba-unmarshal-struct tc))
      (list 'corba-system-exception
            "IDL:omg.org/CORBA/UNKNOWN:1.0"
            corba-omgvmcid-upper 1 :COMPLETED_YES))))


(defun corba-read-system-exception ()
  (let ((id (corba-read-string))
        (minor (corba-read-ulong-split))
        (completed (elt [:COMPLETED_YES :COMPLETED_NO :COMPLETED_MAYBE]
                        (corba-read-ulong))))
    (list 'corba-system-exception
          id (car minor) (cdr minor) completed)))



;;;; CORBA::Any


;; Interface: corba-any
(defun corba-any (typecode value)
  (cond ((stringp typecode)
         (setq typecode (corba-typecode typecode)))
        ((null typecode)
         (setq typecode (corba-any-typecode value))))
  (assert (keywordp (car-safe typecode)))
  `(any ,typecode ,value))


;; Interface: corba-any-p
(defsubst corba-any-p (x)
  (and (consp x)
       (eql (car x) 'any)))

;; Interface: corba-any-typecode
(defun corba-any-typecode (any)
  (cond ((corba-any-p any) (cadr any))
        ((integerp any) (if (< 0 any) '(:tk_long) '(:tk_ulong)))
        ((stringp any) corba-tc-string)
        ((corba-object-p any) corba-tc-object)
        ;;((corba-struct-p any) (corba-struct-typecode any))
        (t (error "Can't determine typecode for: %S" any))))

;; Interface: corba-any-value
(defun corba-any-value (any)
  (cond ((corba-any-p any) (caddr any))
        (t any)))


(defun corba-write-any (arg)
  (let ((typecode (corba-any-typecode arg)))
    (corba-write-typecode typecode)
    (corba-marshal (corba-any-value arg) typecode)))


(defun corba-read-any ()
  (let ((tc (corba-read-typecode)))
    (if corba-explicit-any
        (corba-any tc (corba-unmarshal tc))
      (corba-unmarshal tc))))



;;;; CORBA::Union

;;; TypeCode
;; (:tk_union id name discriminator-type default-index
;;    ((member-label member-name member-type)*))
;;  where default-index is index of member that is the default
;;           or -1 if no default
;;        member-label is of type discriminator-type

;;; Mapping
;; (any discriminator value)
;; where discriminator is of type discriminator-type

(defun corba-union-symbols (tc)
  ;; => [:field1 :field2 ...]
  (assert (memq (corba-typecode-kind tc) '(:tk_union)))
  (corba-with-cached-meta tc
    (apply 'vector
           (mapcar (lambda (m) (corba-make-keyword (cadr m)))
                   (nth 5 tc)))))

(defun corba-new-union (tc member-name value)
  (let* ((syms (corba-union-symbols tc))
         (members (nth 5 tc))
         (index (corba-vposq member-name syms)))
    (assert index)
    `(union ,(car (elt members index)) ,value)))


(defun corba-union-p (x)
  (and (consp x) (eq (car x) 'union)))

(defun corba-union-discriminator (union)
  (cadr union))

(defun corba-union-value (union)
  (car (cddr union)))


(defun corba-marshal-union (val tc)
  (assert (corba-union-p val))
  (assert (eq (car tc) :tk_union))
  (let ((discriminator-type (nth 3 tc))
        (default-index (nth 4 tc))
        (members (nth 5 tc))
        (disc (corba-union-discriminator val))
        (value (corba-union-value val)))
    (let ((member-type nil) (i 0))
      (dolist (m members)
        (when (= i default-index)
          (setq member-type (caddr m)))
        (when (eql disc (car m))
          (setq member-type (caddr m)))
        (setq i (1+ i)))
      (assert member-type)
      (corba-marshal disc discriminator-type)
      (corba-marshal value member-type))))


(defun corba-unmarshal-union (tc)
  (assert (eq (car tc) :tk_union))
  (let ((discriminator-type (nth 3 tc))
        (default-index (nth 4 tc))
        (members (nth 5 tc)))
    (let ((disc (corba-unmarshal discriminator-type))
          (member-type nil)
          (i 0))
      (dolist (m members)
        (when (= i default-index)
          (setq member-type (caddr m)))
        (when (eql disc (car m))
          (setq member-type (caddr m)))
        (setq i (1+ i)))
      (assert member-type)
      `(union ,disc ,(corba-unmarshal member-type)))))



;;;; GIOP / IIOP stuff

(defvar corba-message-size 0)
(make-variable-buffer-local 'corba-message-size)
(defvar corba-giop-version )
(make-variable-buffer-local 'corba-giop-version)

(defun corba-write-giop-header (type)
  (insert "GIOP" 1 0 1
	  (cond ((numberp type) type)
		((eq type 'request) 0)
		((eq type 'reply) 1)
		(t (error "Message type %s" type))))
  ;; Place for message length to be patched in later
  (corba-write-ulong 0))

(defun corba-write-giop-set-message-length ()
  (goto-char 9)
  (corba-write-ulong (- (point-max) 13))
  (delete-char 4))


(defun corba-read-giop-header ()
  (unless (looking-at "GIOP")
    (error "Not a GIOP message"))
  (forward-char 4)
  (let* ((major (corba-read-octet))
	 (minor (corba-read-octet))
	 (byte-order (corba-read-octet))
	 (msgtype (corba-read-octet)))
    (setq corba-giop-version (+ (* 100 major) minor))
    (setq corba-byte-order byte-order)
    msgtype))

(defun corba-read-tagged-component ()
  (cons (corba-read-ulong)
	(corba-read-osequence)))

(defun corba-read-service-context ()
  (corba-read-sequence #'corba-read-tagged-component))

(defun corba-read-iiop-profile-body (reference)
   (corba-read-octet)			;Version (ignored for now)
   (corba-read-octet)
   (setf (corba-object-host reference) (corba-read-string))
   (setf (corba-object-port reference) (corba-read-ushort))
   (setf (corba-object-key reference) (corba-read-osequence)))

(defun corba-write-iiop-profile-body (version host port key)
  (let ((major 1)
        (minor 0))
    (when (and (stringp version))
      (let ((vl (mapcar #'string-to-number (split-string version "\\."))))
        (setq major (pop vl)
              minor (pop vl))))
    (corba-write-octet major)
    (corba-write-octet minor)
    (corba-write-string host)
    (corba-write-short port)
    (corba-write-osequence key)))


(defun corba-make-iiop-profile (version host port key)
  (cons 0                               ; iiop tag
        (corba-make-encapsulation 'corba-write-iiop-profile-body
                                  version host port key)))

(defun corba-write-ior (objref)
  (corba-write-string (if objref (or (corba-object-id objref) "") ""))
  (corba-write-sequence (lambda (tagpair)
                          (corba-write-ulong (car tagpair))
                          (corba-write-osequence (cdr tagpair)))
                        (if objref (corba-object-profiles objref) nil)))


(defun corba-read-ior ()
  (let* ((type-id (corba-read-string))
	 (reference (make-corba-object :id type-id)))
    (loop repeat (corba-read-ulong)
          for tag = (corba-read-ulong)
          for encaps = (corba-read-osequence)
	  if (= tag 0)
	  do (corba-in-encapsulation encaps
                                     #'corba-read-iiop-profile-body reference)
          do (push (cons tag encaps) (corba-object-profiles reference)))
    (if (or (not (equal type-id ""))
            (corba-object-key reference))
        reference
      nil)))


;;;; Connection handling

(defvar corba-iiop-connections nil)

(defun corba-get-connection (host port)
  (let* ((hp (assoc host corba-iiop-connections))
	 (pp (assq port (cdr hp))))
    (unless (and pp (eq (process-status (cdr pp)) 'open))
      (unless hp
	(push (setq hp (cons host nil)) corba-iiop-connections))
      (when pp
        (let ((proc (cdr pp)))
          (let ((buffer (process-buffer proc)))
            (when buffer (kill-buffer buffer)))
          (delete-process proc)))
      (let ((buffer (generate-new-buffer " *IIOP*")))
        (save-excursion
          (set-buffer buffer)
          (set-buffer-multibyte nil)
          (setq buffer-undo-list t)
          (setq corba-message-size nil)
          (erase-buffer))
        (let ((proc
               (condition-case errinfo
                   (open-network-stream "iiop" buffer host port)
                 (error (kill-buffer buffer)
                        (signal (car errinfo) (cdr errinfo))))))
          (set-process-coding-system proc 'binary 'binary)
          ;; FIXME: should I check if open
          (if pp
              (setcdr pp proc)
            (setq pp (cons port proc))
            (push pp (cdr hp))))))
    (cdr pp)))


(defun corba-get-clients ()
  (loop for hp in corba-iiop-connections
	nconc (loop for pp in (cdr hp) collect (cdr pp))))



;;;; Requests

(defstruct corba-request
  (object nil)
  (operation nil)
  (inparams nil)                        ; typecodes for input params
  (outparams nil)                       ; typecodes for output params,
                                        ; inclusive result
  (raises nil)                          ; allowed exceptions
  (arguments nil)
  (req-id nil)
  (client nil)
  (result nil)
  (exception nil))

(defvar corba-request-id-seq 0)
(defvar corba-waiting-requests nil)


(defun corba-create-request (proxy operation inparams outparams raises
                             arguments)
  (make-corba-request
   :object proxy
   :operation operation
   :outparams outparams
   :inparams inparams
   :raises raises
   :arguments arguments))


(defun corba-request-send (req &optional no-response)
  "Send the request to preform the remote CORBA operation defined by REQ.
To get the response from the server use `corba-result' or
`corba-next'. The result from the operation will be returned from
`corba-result'. Several requests can be sent before the getting the
response.
NO-RESPONSE true indicates to the server that no response
is excpected."
  (let ((object (corba-request-object req)))
    (setq object (or (corba-object-forward object)
		     object))
    (corba-request-send-to req object no-response)))


(defun corba-request-send-to (req object &optional no-response)
  (let* ((client (corba-get-connection
		  (corba-object-host object)
		  (corba-object-port object)))
	 (operation (corba-request-operation req)))
    (setf (corba-request-req-id req) (incf corba-request-id-seq))
    (setf (corba-request-client req) client)
    (setf (corba-request-result req) t)
    (corba-in-work-buffer
      (cond
       ((eq operation 'locate)
        (corba-write-giop-header 3)		;LocateRequest
        (corba-write-ulong (corba-request-req-id req))
        (corba-write-osequence (corba-object-key object)))
       (t
        (corba-write-giop-header 'request)
        (corba-write-ulong 0)			;context
        (corba-write-ulong (corba-request-req-id req))
        (corba-write-octet (if no-response 0 1)) ;respons expected
        (corba-write-osequence (corba-object-key object))
        (corba-write-string operation)
        (corba-write-osequence corba-principal)		;principal
        (loop for arg in (corba-request-arguments req)
              for tc in (corba-request-inparams req)
              do (corba-marshal arg tc))))
      (corba-write-giop-set-message-length)
      (process-send-region client (point-min) (point-max))
      ;;(message "Request %d sent" (corba-request-req-id req))
      ;;(accept-process-output)
      (unless no-response
        (push req corba-waiting-requests)))))


(defun corba-read-reply (req)
  (setf (corba-request-result req) nil)
  (ecase (corba-read-ulong)
    ((0)				; No Exception
     (setf (corba-request-result req)
           (mapcar 'corba-unmarshal (corba-request-outparams req)))
     req)
    ((1)				; User Exception
     (setf (corba-request-exception req)
           (corba-unmarshal-except (corba-request-raises req)))
     req)
    ((2)				; System Exception
     (setf (corba-request-exception req)
           (corba-read-system-exception))
     req)
    ((3)				; Forward
     (setf (corba-object-forward (corba-request-object req))
	   (corba-read-ior))
     (corba-request-send req)
     nil)))


(defun corba-read-locatereply (req)
  (let ((status (corba-read-ulong)))
    (cond ((= status 2)
           (setf (corba-object-forward (corba-request-object req))
                 (corba-read-ior))
           (corba-request-send req)
           nil)
          (t
           (setf (corba-request-result req) status)
           req))))


(defun corba-get-next-respons-1 (client)
  (save-excursion
    (set-buffer (process-buffer client))
    (when corba-message-size
      (goto-char (point-min))
      (delete-char corba-message-size)
      (setq corba-message-size nil))
    (cond
     ((>= (point-max) 12)
      (goto-char (point-min))
      (let ((msgtype (corba-read-giop-header)))
        (setq corba-message-size (+ 12 (corba-read-ulong)))
        (cond
         ((<= (point-max) corba-message-size)
          (setq corba-message-size nil))
         ((memq msgtype '(1 4))         ;Reply / LocateReply
          (when (= msgtype 1)
            ;; Ignore service context
            (corba-read-service-context))
          (let* ((request-id (corba-read-ulong))
                 (req (loop for req in corba-waiting-requests
                            if (= request-id (corba-request-req-id req))
                            return req)))
            (cond (req
                   (setq corba-waiting-requests (delq req corba-waiting-requests))
                   (if (= msgtype 1)
                       (corba-read-reply req)
                     (corba-read-locatereply req)))
                  (t
                   (message "Unexpected respons for id %s" request-id)
                   nil))))
         ((= msgtype 5)                 ;Close Connection
          (delete-process client)
          (error "Connection closed"))))))))


;; Interface: corba-next
(defun corba-next (&optional no-wait)
  "Return next request ready."
  (let ((req nil))
    (loop
     do (setq req (loop for client in (corba-get-clients)
			thereis (corba-get-next-respons-1 client)))
     until (or req no-wait)
     do (accept-process-output))
    req))


(defun corba-request-get-response (request no-wait)
  "Get the response for the REQUEST sent earlier with `corba-request-send'."
  (loop while (eq t (corba-request-result request))
        do (corba-get-next-respons-1 (corba-request-client request))
        until no-wait
        do (accept-process-output))
  (not (eq t (corba-request-result request))))


;; Interface: corba-poll
(defun corba-poll (request)
  (corba-request-get-response request t))


;; Interface: corba-result
(defun corba-result (request)
  (corba-request-get-response request nil)
  (let ((e (corba-request-exception request)))
    (if e
      (signal (car e) (cdr e))
      (corba-request-result request))))


(defun corba-request-invoke (req &optional flags)
  "Invoke the CORBA operation defined by the corba-request REQ.
Result is the list of the values of the out parameters."
  (corba-request-send req)
  (corba-result req))


(defun corba-reset-all ()
  (loop for c in (corba-get-clients)
        do (delete-process c))
  (setq corba-iiop-connections nil)
  (setq corba-waiting-requests nil))



;;;; The ORB Interface

(defvar corba-orb nil
  "The default orb")


;; Interface: corba-init
;;;###autoload
(defun corba-init (&optional args)
  (unless corba-orb
    (setq corba-orb
          `(orb :initial-references
                (,@(if corba-name-service
                       `(("NameService" ,corba-name-service)))
                 ,@(if corba-interface-repository
                       `(("InterfaceRepository" ,corba-interface-repository)))))))
  (corba-process-orb-args corba-orb args)
  corba-orb)

(defun corba-process-orb-args (orb args)
  (while args
    (let ((arg (pop args)))
      (when (string-match "\\`-ORBInitRef *" arg)
        (let ((value (substring arg (match-end 0))))
          (when (equal value "")
            (setq value (pop args)))
          (if (string-match "\\([^= ]*\\) *= *\\(.+\\)" value)
              (let ((name (match-string 1 value))
                    (ref  (match-string 2 value)))
                (corba-set-initial-reference orb name ref))))))))


(defun corba-set-initial-reference (orb name value)
  (setq value (list value))
  (let ((ir-list (plist-get (cdr orb) :initial-references)))
    (let ((pair (assoc name ir-list)))
      (cond (pair
             (setcdr pair value))
            (t
             (setq pair (cons name value))
             (plist-put (cdr orb) :initial-references
                        (cons pair ir-list)))))))


;; Interface: corba-obj
(defun corba-obj (str &optional type)
  "Create a proxy object from STR, optionally narrowing to TYPE.
STR should be something acceptable by corba-string-to-object, e.g,,
\"corbaloc::localhost:4720/NameService\"
TYPE is a repository ID or an absoulute name for an interface type.
E.g, \"::CosNaming::NamingContext\"."
  (let ((obj (corba-string-to-object (corba-init) str)))
    (if (and obj type)
        (corba-narrow obj type)
        obj)))


(defun corba-resolve-initial-references (orb name)
  (let* ((ir-list (plist-get (cdr orb) :initial-references))
         (entry (assoc name ir-list)))
    (when entry
      (let ((object (cddr entry)))
        (unless object
          (setq object (corba-string-to-object orb (cadr entry)))
          (setcdr (cdr entry) object))
        object))))


(defun corba-string-to-object (orb str)
  (if (string-match "\\(\\w+\\):" str)
      (let ((method (match-string 1 str))
            (start (match-end 0)))
        (cond ((string-equal method "IOR")
               (corba-decode-iorstring orb str start))
              ((string-equal method "file")
               (corba-decode-url-ior orb str start))
              ((string-equal method "corbaloc")
               (corba-decode-corbaloc orb str start))
              ((string-equal method "http")
               (corba-decode-url-ior orb str start))
              (t
               (error "Unsupported method in IOR URL: %S" method))))
      (error "Illegal string object")))


(defun corba-decode-url-ior (orb str start)
  (require 'url)
  (let ((url-buffer (url-retrieve-synchronously str))
        (reference nil))
  (with-current-buffer url-buffer
    (search-forward "\n\n" nil t)
    (when (or (looking-at "IOR:\\([0-9A-Za-z]+\\)")
              (looking-at "corba.*:.*"))
      (setq reference (corba-string-to-object orb (match-string 0)))))
  (kill-buffer url-buffer)
  reference))


(defun corba-decode-iorstring (orb str start)
  ;;(string-match "IOR:\\([a-fA-F0-9]+\\)" str)
  (let* ((len (floor (- (length str) start) 2))
         (buf (make-string len 0)))
    (dotimes (i len)
      (aset buf i
            (let ((pos (+ start i i)))
              (+ (* 16 (corba-hex-to-int (aref str pos)))
                 (corba-hex-to-int (aref str (1+ pos)))))))
    (corba-in-encapsulation buf #'corba-read-ior)))

;; (corba-string-to-object nil "IOR:01000000010000000000000000000000")


(defun corba-decode-corbaloc (orb str start)
  (let ((key-pos
         (if (string-match "/" str)
             (match-beginning 0)
             (1- (length str)))))
    (let ((addr-list (substring str start key-pos))
          (key-string (substring str (1+ key-pos))))
      (let ((key (corba-url-decode key-string))
            (addrs (mapcar #'corba-parse-obj-addr
                           (split-string addr-list ","))))
        (cond ((eq (car addrs) :rir)
               (corba-resolve-initial-references orb key))
              (t
               (let ((addr (car addrs)))
                 (let ((version (car addr))
                       (host (cadr addr))
                       (port (caddr addr)))
                   ;;FIXME: what about other profiles? version?
                   (make-corba-object
                    :id ""
                    :host host :port port :key key
                    :profiles (list (corba-make-iiop-profile
                                     version host port key)))))))))))


(defun corba-parse-obj-addr (str)
  (cond ((string-equal "rir:" str) :rir)
        ((string-match
          "\\(iiop\\)?:\\(\\([0-9.]+\\)@\\)?\\([^:]*\\)\\(:\\([0-9]+\\)\\)?\\'"
                       str)
         (let ((version (match-string 3 str))
               (host    (match-string 4 str))
               (port    (match-string 6 str)))
           (unless version (setq version "1.0"))
           (when (equal host "") (setq host "localhost"))
           (if port
               (setq port (string-to-number port))
               (setq port 2809))
           (list version host port)))
        (t
         (error "Illgal object addres: %S" str))))


(defun corba-url-decode (str)
  (if (fboundp 'url-unhex-string)
      (url-unhex-string str)
      str))


;; Interface: corba-string
(defun corba-string (object)
  "Return a stringified object reference."
  (let ((str (corba-make-encapsulation #'corba-write-ior object)))
    (concat "IOR:"
	    (upcase (loop for c across str
			  concat (format "%02x" c))))))


(defun corba-file-to-object (file)
  (corba-string-to-object
   nil					; No orb-struct yet
   (if (string-match "IOR:" file)
       file				; Looks like the IOR itself
     (save-excursion
       (set-buffer (get-buffer-create "*REQ*"))
       (set-buffer-multibyte nil)
       (erase-buffer)
       (insert-file-contents file)
       (goto-char (point-min))
       (end-of-line 1)
       (buffer-substring (point-min) (point))))))


;;;; The Object Interface

(defvar corba-trust-object-irid t)


(defun corba-require-repoid (str)
  (when (corba-aname-p str)
    (let ((o (corba-lookup str)))
      (when (and o (corba-interface-p o))
        (setq str (corba-interface-id o)))))
  (unless (string-match "^IDL:" str)
    (error "Invalid repoid: %s" str))
  str)


;; Interface: corba-typep
(defun corba-typep (obj id)
  "Test if a CORBA object OBJ is of a given type ID.
ID should be a repository identifier or the absolute name of the interface."
  (setq id (corba-require-repoid id))
  (or (if corba-trust-object-irid
          (equal id (corba-object-id obj)))
      (car (corba-funcall "_is_a" obj id))))


;; Interface: corba-narrow
(defun corba-narrow (obj id)
  "Narrow type of corba proxy OBJ to specified ID.
If ID is nil, no narrowing is done.
If ID is non-nil it should be and absoulute name or repository id for
an interface."
  (cond ((null id) obj)
        ((null obj)
         (error "Cannot narrow to '%s', object %s" id obj))
        (t
         (setq id (corba-require-repoid id))
         (unless (corba-typep obj id)
           (error "Cannot narrow to '%s', object %s" id obj))
         (setf (corba-object-id obj) id)
         obj)))



;;;; Interfaces and operations


(defun corba-find-opdef (interface operation)
  "Find in INTERFACE the OPERATION and return the opdef struct."
  (or (find operation
	    (corba-interface-operations interface)
	    :test #'equal
	    :key #'corba-opdef-name)
      (loop for pint in (corba-interface-inherit interface)
	    thereis (corba-find-opdef pint operation))))



;;;; GET/PUT operators

(defun corba-tcref (tc key)
  ;; Return cons cell with car value for key..
  (unless (consp tc)
    (error "Bad typecode slot: %S, %S" key tc))
  (ecase key
    ((:kind) tc)
    ((:id :length :fixed_digits)
     (cdr tc))
    ((:name :fixed_scale)
     (cddr tc))
    ((:content_type)
     (ecase (car tc)
       ((:tk_sequence :tk_array :tk_value_box) (cdr tc))
       ((:tk_alias) (cdddr tc))))))


;; Interface: corba-get
(defun corba-get (object key)
  "Get slot from struct or attribute from object."
  (cond ((consp object)
         (let ((type (car object)))
           (cond ((eq type 'any)
                  (ecase key
                    (:any-value (corba-any-value object))
                    (:any-typecode (corba-any-typecode object))))
                 ((keywordp type)
                  (car (corba-tcref object key)))
                 (t
                  (error "Bad object type: %S" object)))))
        ((corba-object-p object)
         (cond ((stringp key)
                (car (corba-funcall (concat "_get_" key) object)))
               (t
                (error "NIY"))))
        ((vectorp object)
         (let ((pos (corba-vposq key (aref object 0))))
           (if pos (aref object pos)
               (error "Invalid key: %S, %S" key object))))
        (t (error "Bad object type: %S" object))))


;; Interface: corba-put
(defun corba-put (object key value)
  "Set slot in struct or attribute of object."
  (cond ((stringp key)
         (corba-funcall (concat "_set_" key) object value))
        ((consp object)
         (let ((type (car object)))
           (cond ((eq type 'any)
                  (ecase key
                    (:any-typecode (setcar (cdr object) value))
                    (:any-value    (setcar (cddr object) value))))
                 ((keywordp type)
                  (setcar (corba-tcref object key) value))
                 (t
                  (error "Bad object type: %S" object)))))
        ((corba-object-p object)
         (error "NIY"))
        ((vectorp object)
         (let ((pos (corba-vposq key (aref object 0))))
           (if pos
               (aset object pos value)
               (error "Invalid key: %s, %s" key object))))
        (t (error "Bad object type: %S" object))))



;;;; Initial Services Convenience Access

;; Interface: corba-get-ir
(defun corba-get-ir ()
  "Return the configured InterfaceRepository object."
  (require 'corba-load-ifr)
  (corba-narrow
   (corba-resolve-initial-references (corba-init) "InterfaceRepository")
   "::CORBA::Repository"))

;; Interface: corba-get-ns
(defun corba-get-ns ()
  "Return the configured name service object.
The object has been narrowd to the NamingContextExt interface."
  (require 'corba-load-naming)
  (corba-narrow
   (corba-resolve-initial-references (corba-init) "NameService")
   "::CosNaming::NamingContextExt"))


;;;; Easy DII - corba-funcall


(defun corba-object-create-request (&rest args)
  "Create a request object for an operation on a proxy object.
Two version of arglist for use with interface repository info or without:
1. (op obj args..)
2. (result-type op obj { :in type value | :inout type value | :out type }*
           [ :raises exc-list ])
Arglist 2 is for use without interface repository, all type information is
included in the list."
  (cond ((eq 'locate (car args))
         (corba-create-request (cadr args) 'locate nil nil nil nil))
        ((consp (car args))
         (apply 'corba-create-noir-request args))
        (t
         (apply 'corba-create-ir-request args))))


(defun corba-lame-id-p (id)
  (or (equal id "")
      (equal id (cadr corba-tc-object))))


(defun corba-get-remote-interface (interface-id)
  (when corba-use-interface-repository
    (corba-load-interface-id interface-id)))


(defun corba-get-object-interface (object)
  (when corba-use-get-interface
    (condition-case exc
        (let ((idef (car (corba-funcall "_get_interface" object))))
          (and idef (corba-load-interface-def idef)))
      (corba-system-exception
       (message "_get_interface: %s" exc)
       nil)
      (end-of-buffer
       ;; Work around ORBit bug in exception marshaling
       (message "_get_interface: %s" exc)
       nil))))


(defun corba-load-objects-interface (object)
  ;; Make sure the objects interface is in the local cache
  ;; returns interface
  (let ((interface-id (corba-object-id object))
        iface)
    (when (corba-lame-id-p interface-id)
      (setq interface-id (cadr corba-tc-object))
      (setq iface (corba-get-object-interface object))
      (when iface
        (setq interface-id (corba-interface-id iface))
        (setf (corba-object-id object) interface-id)))
    (or iface
        (corba-lookup interface-id)
        (corba-get-object-interface object)
        (corba-get-remote-interface interface-id)
        (error "No definition for interface: %s" interface-id))))


(defun corba-lookup-operation (operation object)
  (or (corba-find-opdef (corba-lookup "::CORBA::Object")
                        operation)
      (let ((interface
             (if (and (corba-aname-p operation) ; full name of op
                      (string-match "\\(::.+\\)::\\([^:]+\\)" operation))
                 (let ((interface-id (matched-string operation 1)))
                   (setq operation (matched-string operation 2))
                   (or (corba-lookup interface-id)
                       (error "No definition for interface: %s" interface-id)))
               (corba-load-objects-interface object))))
        (or (corba-find-opdef interface operation)
            (error "Undefined operation %s for interface %s"
                   operation (corba-interface-id interface))))))


(defun corba-create-ir-request (operation object &rest args)
  (assert (corba-object-p object))
  (assert (stringp operation))
  (let ((opdef (corba-lookup-operation operation object)))
    (unless (= (length args)
	       (length (corba-opdef-inparams opdef)))
      (error "Wrong number of arguments to operation"))
    (corba-create-request object
                          (corba-opdef-name opdef)
                          (corba-opdef-inparams opdef)
                          (corba-opdef-outparams opdef)
                          (mapcar #'corba-typecode (corba-opdef-raises opdef))
                          args)))


(defun corba-create-noir-request (result-type op obj &rest params)
  ;; params = (  { :in type value | :inout type value | :out type }* exc)
  ;; exc = [ :raises exc-list ]
  (assert (corba-object-p obj))
  (assert (stringp op))
  (setq result-type (corba-typecode result-type))
  (let ((inparams nil)
        (outparams (if (eq (car result-type) :tk_void) nil (list result-type)))
        (args nil)
        (raises nil))
    (while params
      (let ((mode (pop params)))
        (assert (memq mode '(:in :inout :out :raises)))
        (cond ((eq mode :raises)
               (setq raises (mapcar #'corba-lookup-type (pop params))))
              (t
               (let ((type (corba-typecode (pop params))))
                 (unless (eq mode :in)
                   (push type outparams))
                 (unless (eq mode :out)
                   (let ((value (pop params)))
                     (push value args)
                     (push type inparams))))))))
    (corba-create-request obj op (nreverse inparams) (nreverse outparams)
                          raises (nreverse args))))


;; Interface: corba-funcall
(defun corba-funcall (op obj &rest args)
  "Invoke operation OP on object OBJ with arguments ARGS.
Returns the list of result and out parameters.
Use without interface repository information:
\(:noir op obj result-type { :in type value | :inout type value | :out type }*
           [ :raises exc-list ])
alt:
\(result-type op obj { :in type value | :inout type value | :out type }*
           [ :raises exc-list ]) "
  (corba-request-invoke
   (apply #'corba-object-create-request op obj args)))



;;;; Name Service Shortcuts


;; Interface: corba-resolve
(defun corba-resolve (name &optional type)
  (require 'corba-load-naming)
  (corba-narrow (car (corba-funcall "::CosNaming::NamingContextExt::resolve_str"
                                    (corba-get-ns) name))
                type))



;;;; ORBit hacks

(defun corba-setup-orbit-cookie ()
  (setq corba-principal
        (let ((cookie-file
               (format "/tmp/orbit-%s/cookie" user-login-name)))
          (unless (file-exists-p cookie-file)
            (error "No ORBit cookie file"))
          (save-excursion
            (set-buffer (find-file-noselect cookie-file))
            (concat (buffer-string) "\0")))))



;;;; Calling remote Interface Repository


(defun corba-ir-lookup-id (id)
  (or (car (corba-funcall corba-tc-object
                          "lookup_id" (corba-get-ir)
                          :in corba-tc-string id))
      (error "InterfaceRepository does not know about %s" id)))


(defun corba-ir-get-typecode (id)
  (message "Getting type %s" id)
  (let ((corba-intern-all-typecodes t))
    (car (corba-funcall corba-tc-typecode
                        "_get_type" (corba-ir-lookup-id id)))))


(defun corba-get-typecode (id)
  (or (gethash id corba-local-typecode-repository)
      (corba-ir-get-typecode id)))


(defun corba-ir-contents (container limit-type exclude-inherit)
  (car (corba-funcall (corba-get-typecode "IDL:omg.org/CORBA/ContainedSeq:1.0")
                      "contents" container
                      :in corba-tc-long limit-type
                      :in corba-tc-boolean exclude-inherit)))


(defun corba-opdef-from-attr-desc (desc)
  (let ((name (corba-get desc :name))
        (mode (corba-get desc :mode))
        (type (corba-get desc :type)))
    (cons (make-corba-opdef
           :name (format "_get_%s" name)
           :outparams (list type))
          (if (eq mode :ATTR_NORMAL)
              (list (make-corba-opdef
                     :inparams (list type)
                     :name (format "_set_%s" name)))))))


(defun corba-opdef-from-desc (desc)
  (let ((name   (corba-get desc :name))
        (result (corba-get desc :result))
        (inpars nil)
        (outpars nil))
    (unless (eq :tk_void (corba-typecode-kind result))
      (push result outpars))
    (loop for pardesc in (corba-get desc :parameters)
          for tc   = (corba-get pardesc :type)
          for mode = (corba-get pardesc :mode)
          do (unless (eq mode :PARAM_OUT)
               (push tc inpars))
          do (unless (eq mode :PARAM_IN)
               (push tc outpars)))
    (make-corba-opdef
     :name name
     :inparams (nreverse inpars)
     :outparams (nreverse outpars)
     :raises (corba-get desc :exceptions)
     :mode (corba-get desc :mode))))



;;;; New Repository Loading


(defvar corba-local-repository
  (make-hash-table :test #'equal))


(defun corba-has-interface-p (id)
  (gethash id corba-local-repository))


;; Interface: corba-lookup
(defun corba-lookup (id)
  (gethash id corba-local-repository))


;; Example:

(defconst corba-example-rep
  '(repository nil
    (module "::CosNaming"
     (type "::CosNaming::Istring"
      (:tk_alias "IDL:omg.org/CosNaming/Istring:1.0" "Istring" (:tk_string 0)))
     (struct "::CosNaming::NameComponent"
      (:tk_struct "IDL:omg.org/CosNaming/NameComponent:1.0" "NameComponent"
                  (("id" "IDL:omg.org/CosNaming/Istring:1.0")
                   ("kind" "IDL:omg.org/CosNaming/Istring:1.0"))))
     (interface "::CosNaming::BindingIterator" nil
      (:tk_objref "IDL:omg.org/CosNaming/BindingIterator:1.0" "BindingIterator")
      (operation "next_one" :op_normal (:tk_boolean)
                 (("b" :param_out "IDL:omg.org/CosNaming/Binding:1.0")) nil)
      (operation "next_n" :op_normal (:tk_boolean)
                 (("how_many" :param_in (:tk_ulong))
                  ("bl" :param_out "IDL:omg.org/CosNaming/BindingList:1.0"))
                 nil)
      (operation "destroy" :op_normal (:tk_void) nil nil)))))


(defun corba-lookup-interface (id)
  (let ((interface (gethash id corba-local-repository)))
    (if interface
        (if (corba-interface-p interface)
            interface
            (error "ID does not resolve to interface: %a" id))
        (progn (setq interface (make-corba-interface :id id))
               (puthash id interface corba-local-repository)
               interface))))


(defvar corba-loading-interface nil)

(defun corba--addop (opdef)
  (let ((ops (corba-interface-operations corba-loading-interface)))
    (setf (corba-interface-operations corba-loading-interface)
          (nconc ops (list opdef)))))


(defconst corba-load-table
  '((repository . corba-load-container)
    (module . corba-load-container)
    (type . corba-load-type)
    (struct . corba-load-type)
    (union . corba-load-type)
    (exception . corba-load-type)
    (const . corba-load-const)
    (interface . corba-load-interface)
    (operation . corba-load-operation)
    (attribute . corba-load-attribute)))


;; Interface: corba-load-repository
(defun corba-load-repository (repr)
  "Load repository from representation REPR."
  (when repr
    (let ((loader (assq (car repr) corba-load-table)))
      (or loader
          (error "Unimplemented repository representation: %s"
                 repr))
      (funcall (cdr loader) repr))))


(defun corba-load-container (repr)
  ;; (repository () contents..)
  ;; (module name contents..)
  (mapc 'corba-load-repository (cddr repr)))

(defun corba-load-type (repr)
  ;;(type|struct|exception name type)
  (let ((aname (cadr repr)))
    (puthash aname (corba-typecode (nth 2 repr))
             corba-local-repository)))


(defun corba-load-const (repr)
  ;; (const name type value)
  (let ((name (cadr repr))
        (type (corba-typecode (nth 2 repr)))
        (value (nth 3 repr)))
    (puthash name value corba-local-repository)))


(defun corba-load-interface (repr)
  ;;(interface id bases type content..)
  (let ((aname (cadr repr))
        (objref (corba-typecode (nth 3 repr))))
    (let ((corba-loading-interface
           (corba-lookup-interface (cadr objref))))
      (setf (corba-interface-inherit corba-loading-interface)
            (let ((bases (nth 2 repr)))
              (if bases
                  (mapcar #'corba-lookup-interface bases)
                  ;; root inheritance in "::CORBA::Object"
                  (let ((object (corba-lookup-interface
                                 "IDL:omg.org/CORBA/Object:1.0")))
                    (unless (eq object corba-loading-interface)
                      (list object))))))
      (setf (corba-interface-operations corba-loading-interface)
            nil)
      (mapc #'corba-load-repository (nthcdr 4 repr))
      (puthash aname corba-loading-interface corba-local-repository))))


(defun corba-load-operation (repr)
  ;; (operation name mode result-type params exceptions)
  (destructuring-bind (name mode result-type params exceptions)
      (cdr repr)

    ;; result as a pseudo out param
    (unless (eq (car-safe result-type) :tk_void)
      (push (list "" :PARAM_OUT result-type) params))

    (corba--addop
     (make-corba-opdef
      :name name
      :mode (or (car (assq mode '((:op_normal . :OP_NORMAL) (:op_oneway . :OP_ONEWAY))))
                mode)
      :inparams (loop for (nil pmode type) in params
                   unless (memq pmode '(:param_out :PARAM_OUT))
                   collect (corba-typecode type))
      :outparams (loop for (nil pmode type) in params
                    unless (memq pmode '(:param_in :PARAM_IN))
                    collect (corba-typecode type))
      :raises exceptions))))


(defun corba-load-attribute (repr)
  ;;(attribute name mode type)
  (let ((name (cadr repr))
        (mode (nth 2 repr))
        (type (corba-typecode (nth 3 repr))))
    (corba--addop
     (make-corba-opdef
      :name (format "_get_%s" name)
      :outparams (list type)))
    (when (memq mode '(:attr_normal :ATTR_NORMAL))
      (corba--addop
       (make-corba-opdef
        :inparams (list type)
        :name (format "_set_%s" name))))))



;;; Loading from repository


(defun corba-load-interface-id (id)
  (corba-load-interface-def (corba-ir-lookup-id id)))


(defun corba-load-operation-def (opdesc)
  (corba--addop (corba-opdef-from-desc opdesc)))


(defun corba-load-attribute-def (attdesc)
  (mapc #'corba--addop (corba-opdef-from-attr-desc attdesc)))


(defun corba-load-interface-def (idef)
  (let* ((desc-tc (corba-get-typecode "IDL:omg.org/CORBA/InterfaceDef/FullInterfaceDescription:1.0"))
         (corba-intern-all-typecodes t)
         (desc (car (corba-funcall desc-tc "describe_interface" idef)))
         (id (corba-get desc :id))
         (corba-loading-interface (corba-lookup-interface id)))
    (setf (corba-interface-inherit corba-loading-interface)
          (mapcar #'corba-load-interface-id (corba-get desc :base_interfaces)))
    (setf (corba-interface-operations corba-loading-interface) nil)
    (mapc #'corba-load-operation-def (corba-get desc :operations))
    (mapc #'corba-load-attribute-def (corba-get desc :attributes))
    corba-loading-interface))



;;; Prime repository

(corba-load-repository
 '(interface "::CORBA::Object" ()
   (:tk_objref "IDL:omg.org/CORBA/Object:1.0" "Object")
   (operation "_is_a" :op_normal (:tk_boolean)
    (("id" :param_in (:tk_string 0)))
    ())
   (operation "_interface" :op_normal
    (:tk_objref "IDL:omg.org/CORBA/InterfaceDef:1.0" "InterfaceDef")
    () ())
   (operation "_get_interface" :op_normal
    (:tk_objref "IDL:omg.org/CORBA/InterfaceDef:1.0" "InterfaceDef")
    () ())
   (operation "_non_existent" :op_normal (:tk_boolean)
    () () )))


;; To support using corba-use-get-interface without configured repository
(corba-typecode
 '(:tk_struct "IDL:omg.org/CORBA/InterfaceDef/FullInterfaceDescription:1.0"
 "FullInterfaceDescription"
 (("name"
   (:tk_alias "IDL:omg.org/CORBA/Identifier:1.0" "Identifier" (:tk_string 0)))
  ("id"
   (:tk_alias "IDL:omg.org/CORBA/RepositoryId:1.0" "RepositoryId"
    (:tk_string 0)))
  ("defined_in" "IDL:omg.org/CORBA/RepositoryId:1.0")
  ("version"
   (:tk_alias "IDL:omg.org/CORBA/VersionSpec:1.0" "VersionSpec"
    (:tk_string 0)))
  ("operations"
   (:tk_alias "IDL:omg.org/CORBA/OpDescriptionSeq:1.0" "OpDescriptionSeq"
    (:tk_sequence
     (:tk_struct "IDL:omg.org/CORBA/OperationDescription:1.0"
      "OperationDescription"
      (("name" "IDL:omg.org/CORBA/Identifier:1.0")
       ("id" "IDL:omg.org/CORBA/RepositoryId:1.0")
       ("defined_in" "IDL:omg.org/CORBA/RepositoryId:1.0")
       ("version" "IDL:omg.org/CORBA/VersionSpec:1.0")
       ("result" (:tk_typecode))
       ("mode"
        (:tk_enum "IDL:omg.org/CORBA/OperationMode:1.0" "OperationMode"
         ("OP_NORMAL" "OP_ONEWAY")))
       ("contexts"
        (:tk_alias "IDL:omg.org/CORBA/ContextIdSeq:1.0" "ContextIdSeq"
         (:tk_sequence
          (:tk_alias "IDL:omg.org/CORBA/ContextIdentifier:1.0"
           "ContextIdentifier" "IDL:omg.org/CORBA/Identifier:1.0")
          0)))
       ("parameters"
        (:tk_alias "IDL:omg.org/CORBA/ParDescriptionSeq:1.0"
         "ParDescriptionSeq"
         (:tk_sequence
          (:tk_struct "IDL:omg.org/CORBA/ParameterDescription:1.0"
           "ParameterDescription"
           (("name" "IDL:omg.org/CORBA/Identifier:1.0") ("type" (:tk_typecode))
            ("type_def" (:tk_objref "IDL:omg.org/CORBA/IDLType:1.0" "IDLType"))
            ("mode"
             (:tk_enum "IDL:omg.org/CORBA/ParameterMode:1.0" "ParameterMode"
              ("PARAM_IN" "PARAM_OUT" "PARAM_INOUT")))))
          0)))
       ("exceptions"
        (:tk_alias "IDL:omg.org/CORBA/ExcDescriptionSeq:1.0"
         "ExcDescriptionSeq"
         (:tk_sequence
          (:tk_struct "IDL:omg.org/CORBA/ExceptionDescription:1.0"
           "ExceptionDescription"
           (("name" "IDL:omg.org/CORBA/Identifier:1.0")
            ("id" "IDL:omg.org/CORBA/RepositoryId:1.0")
            ("defined_in" "IDL:omg.org/CORBA/RepositoryId:1.0")
            ("version" "IDL:omg.org/CORBA/VersionSpec:1.0")
            ("type" (:tk_typecode))))
          0)))))
     0)))
  ("attributes"
   (:tk_alias "IDL:omg.org/CORBA/AttrDescriptionSeq:1.0" "AttrDescriptionSeq"
    (:tk_sequence
     (:tk_struct "IDL:omg.org/CORBA/AttributeDescription:1.0"
      "AttributeDescription"
      (("name" "IDL:omg.org/CORBA/Identifier:1.0")
       ("id" "IDL:omg.org/CORBA/RepositoryId:1.0")
       ("defined_in" "IDL:omg.org/CORBA/RepositoryId:1.0")
       ("version" "IDL:omg.org/CORBA/VersionSpec:1.0") ("type" (:tk_typecode))
       ("mode"
        (:tk_enum "IDL:omg.org/CORBA/AttributeMode:1.0" "AttributeMode"
         ("ATTR_NORMAL" "ATTR_READONLY")))))
     0)))
  ("base_interfaces"
   (:tk_alias "IDL:omg.org/CORBA/RepositoryIdSeq:1.0" "RepositoryIdSeq"
    (:tk_sequence "IDL:omg.org/CORBA/RepositoryId:1.0" 0)))
  ("type" (:tk_typecode)))))



;;;; corba-new

;;; interface
;; Interface: corba-new
(defun corba-new (type &rest fields)
  "Create a new object of TYPE initiated from FIELDS.
Possible kinds objects are struct, union, request.

Struct: TYPE = repo-id or absolute name
        FIELDS = { slot-key value }*
Union:  TYPE = repo-id or absolute name
        FIELDS = descriminatior-value union-value
Request: TYPE = :request
        FIELDS = request-arglist
see corba-funcall for request-arglist. "
  (cond ((stringp type)
         (let ((tc (corba-lookup-type type)))
           (assert (consp tc))          ; should be tc
           (ecase (car tc)
             (:tk_struct (corba-new-struct tc fields))
             (:tk_union  (apply #'corba-new-union tc fields)))))
        ((eq type :request)
         ;; op obj args..
         ;; result-type op obj args..
         (apply 'corba-object-create-request fields))))



;;;; corba-send

;; Interface: corba-send
(defun corba-send (req-or-op &rest args)
  "(corba-send req &optional no-response)
\(corba-send op obj args..)
\(corba-send result-type op obj args..)
Returns: request"
  (cond ((corba-request-p req-or-op)
         (corba-request-send req-or-op (car args))
         req-or-op)
        (t
         (let ((oneway (if (eq req-or-op :oneway)
                           (progn (setq req-or-op (pop args)) t)))
               (request
                (apply 'corba-object-create-request req-or-op args)))
           (corba-request-send request oneway)
           request))))


;;; corba.el ends here
