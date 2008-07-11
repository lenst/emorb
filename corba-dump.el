
(provide 'corba-dump)

(require 'corba)
(require 'corba-load-ifr)

(eval-when-compile
  (require 'corba-dump-mac))




;;;; TypeCode Dumping


(defvar *tc-dumped* nil)


;;; (defconst corba-typecode-canonizers
;;;   '((:tk_struct . corba-struct-canonize)
;;;     (:tk_alias . corba-type-3-canonize)
;;;     (:tk_union . corba-union-canonize)
;;;     (:tk_value_box . corba-type-3-canonize)
;;;     (:tk_sequence . corba-type-1-canonize)
;;;     (:tk_array . corba-type-1-canonize)))



(defun corba-uncanonize-typecode (tc)
  (let ((old (symbol-function 'corba-typecode)))
    (unwind-protect
         (progn
           (fset 'corba-typecode (symbol-function 'corba-tc-dump))
           (corba-typecode-canonize tc))
      (fset 'corba-typecode old))))


(defun corba-tc-dump (tc)
  (if (not *tc-dumped*)
      tc
      (let ((kind (corba-typecode-kind tc)))
        (if (corba-kind-has-id-p kind) 
            (let ((id (cadr tc)))
              (if (gethash id *tc-dumped*)
                  id
                  (let ((repr (corba-uncanonize-typecode tc)))
                    (puthash id repr *tc-dumped*)
                    repr)))
            (corba-uncanonize-typecode tc)))))



;;;; Repository Dumping


(defun corba-dump-contents (def)
  (mapcar 'corba-ir-dump-1 (car (corba-funcall "contents" def :dk_all t))))


(defvar corba-dumper-table (corba-dumper-table corba-dump-contents))


(defun corba-ir-dump-1 (def)
  (let ((translator nil)
        (table corba-dumper-table)
        (kind (corba-get def "def_kind")))
    (while table
      (let ((keys (caar table)))
        (if (or (eq keys t) (memq kind keys ))
            (setq translator (car table)
                  table nil)
          (setq table (cdr table)))))
    (assert translator)
    (funcall (cdr translator) def)))


(defun corba-ir-dump (def)
  (let ((*tc-dumped* (make-hash-table :test #'equal)))
    (corba-ir-dump-1 def)))



;;; (corba-init '("-ORBInitRef NameService=corbaloc::localhost:4720/NameService"))
;;; (defvar ir (corba-resolve "ir"))
;;; (defvar mod (car (corba-funcall "lookup" ir "CLORB_EX")))
