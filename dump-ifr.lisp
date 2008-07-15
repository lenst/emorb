(in-package :cl-user)

(require "clorb")


(defvar *tc-dumped* nil)

(defun kind-has-id-p (kind)
  (member kind '(:tk_struct :tk_enum :tk_objref :tk_alias :tk_union 
                 :tk_value :tk_value_box :tk_native :tk_local_interface :tk_except
                 :tk_abstract_interface)))

(defun tc-dump (tc)
  (let ((kind (op:kind tc)))
    (if (kind-has-id-p kind) 
      (let ((id (op:id tc)))
        (if (and *tc-dumped* (gethash id *tc-dumped*))
          id
          (progn
            (when *tc-dumped*
              (setf (gethash id *tc-dumped*) t))
            `(,kind ,id ,(op:name tc)
                    ,@(case kind
                        ((:tk_alias :tk_value_box)
                         `(,(tc-dump (op:content_type tc))))
                        ((:tk_struct)
                         (list (loop for i from 0 below (op:member_count tc)
                                     collect (list (op:member_name tc i)
                                                   (tc-dump (op:member_type tc i))))))
                        ((:tk_enum)
                         (list (loop for i from 0 below (op:member_count tc)
                                     collect (op:member_name tc i))))
                        ((:tk_union)
                         (list (tc-dump (op:discriminator_type tc))
                               (op:default_index tc)
                               (loop for i from 0 below (op:member_count tc)
                                  collect (list (op:member_label tc i)
                                                (op:member_name tc i)
                                                (tc-dump (op:member_type tc i)))))))))))
      (case kind
        ((:tk_string :tk_wstring) `(,kind ,(op:length tc)))
        ((:tk_fixed) `(,kind ,(op:fixed_digits tc) ,(op:fixed_scale tc)))
        ((:tk_sequence :tk_array)
         `(,kind ,(tc-dump (op:content_type tc)) ,(op:length tc)))
        (t
         `(,kind))) )))

(defun make-attribute-get (name var)
  (let ((getter (intern (string-upcase name) :op)))
    `(,getter ,var)))

(defun make-translated-form (item var)
  ;; (struct "absolute_name" (> "type" typecode) contents)
  ;; or "absolute_name", (> "type" typecode), ...
  (cond ((null item) nil)
        ((eql item 'contents) `(do-contents ,var))
        ((symbolp item) `',item)
        ((stringp item) (make-attribute-get item var))
        ((and (consp item) (member (car item) '(> @)))
         (let ((attr (make-attribute-get (second item) var)))
           (assert (null (cdddr item)))
           (if (eql (car item) '>)
               (ecase (third item)
                 ((typecode) `(tc-dump ,attr))
                 ((any-value) `(CORBA:any-value ,attr)))
               `(map 'list 
                     (lambda (x) ,(make-translated-form (third item) 'x))
                     ,attr))))
        ((consp item)
         `(cons ,(make-translated-form (car item) var)
                ,(make-translated-form (cdr item) var)))
        (t
         (error "Invalid form item: ~s" item))))

(defun make-dumper (spec)
  ;;(CORBA:StructDef (:dk_struct) (struct "absolute_name" (> "type" typecode) contents))
  (let ((class (pop spec))
        (kinds (pop spec))
        (form (pop spec)))
    (declare (ignore class))
    `(cons ',kinds
           (lambda (def)
             ,(make-translated-form form 'def)))))


(defmacro dumper-table (spec-file)
  `(list ,@(iter (for spec in-file spec-file)
                 (collect (make-dumper spec)))))


(defun ifr-dump (def)
  (let ((*tc-dumped* (make-hash-table :test #'equal))
        (dumper-table nil))
    (labels ((ifr-dump-1 (def)
               (let ((translator
                      (assoc (op:def_kind def) dumper-table
                             :test #L(or (eq !2 t) (member !1 !2)))))
                 (assert translator)
                 (funcall (cdr translator) def)))
             (do-contents (def)
               (map 'list #'ifr-dump-1 (op:contents def :dk_all t))))
      (setq dumper-table (dumper-table "x-repo-rep-meta.lisp"))
      (ifr-dump-1 def))))


(defun main (file)
  (let ((*print-case* :downcase))
    (pprint (ifr-dump (corba:idl file :eval nil)))
    (terpri)))
