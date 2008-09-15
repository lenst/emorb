;;;; dump-ifr.lisp --- Dump Interface Repository to Sexp Representation

(in-package :cl-user)

(require "clorb")


;;;; Get Meta Info from Emacs Lisp files


(defparameter *meta-file* "corba-meta.el")


(defun get-meta (symbol-name)
  (with-open-file (stream *meta-file* :direction :input)
    (let ((*package* (find-package :cl-user))
          (sexp nil))
      (loop
         (setq sexp (read stream))
         (when (and (consp sexp)
                    (eq (car sexp) 'defconst)
                    (string-equal (second sexp) symbol-name))
           (return (second (third sexp))))))))



;;(get-meta 'corba-ifr-repr-meta)



;;;; TypeCode Dumping


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
                        ((:tk_struct :tk_except)
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
                                  collect (list (corba:any-value (op:member_label tc i))
                                                (op:member_name tc i)
                                                (tc-dump (op:member_type tc i)))))))))))
      (case kind
        ((:tk_string :tk_wstring) `(,kind ,(op:length tc)))
        ((:tk_fixed) `(,kind ,(op:fixed_digits tc) ,(op:fixed_scale tc)))
        ((:tk_sequence :tk_array)
         `(,kind ,(tc-dump (op:content_type tc)) ,(op:length tc)))
        (t
         `(,kind))) )))


;;;; Make Repository Dumper Code


(defun make-attribute-get (name var)
  (let ((getter (intern (string-upcase name) :op)))
    `(,getter ,var)))

(defun make-translated-form (item var)
  ;; (struct "absolute_name" (> "type" typecode) contents)
  ;; or "absolute_name", (> "type" typecode), ...
  (cond ((null item) nil)
        ((eql item 'contents) `(do-contents ,var))
        ((keywordp item) (make-attribute-get item var))
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



(defmacro dumper-table ()
  (let ((spec (get-meta 'corba-ifr-repr-meta)))
    `(list ,@(mapcar #'make-dumper spec))))



;;;; Main Dumper

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
      (setq dumper-table (dumper-table))
      (ifr-dump-1 def))))


(defun pp-tc (tc)
  (let ((*tc-dumped* (make-hash-table :test #'equal))
        (*print-case* :downcase))
    (pprint (tc-dump tc)))
  (values))
  

(defun main (file &optional output-file module-name)
  (when (and output-file (not module-name))
    (setq module-name (pathname-name output-file)))
  (let ((dump-sexp (ifr-dump (corba:idl file :eval nil)))
        (*print-case* :downcase))
    (if output-file
        (with-open-file (out output-file :direction :output
                             :if-exists :new-version)
          (format out ";;; Dump of interface repository generated by dump-ifr.lisp~2%")
          (when module-name
            (format out "(provide '~a)~2%" module-name))
          (format out "(require 'corba)~2%(corba-load-repository '")
          (pprint dump-sexp out)
          (format out ")~2%"))
        (pprint dump-sexp))))


#|
 (main "clorb:idl;CosNaming.idl" "corba-load-naming.el")

 (main "clorb:idl;CosEventChannelAdmin.idl" "loadidl-CosEventChannelAdmin.el")
 (main "clorb:examples;hello;hello.idl" "loadidl-clorb_ex-hello.el")
 (main "clorb:idl;cddr-01-queue.idl" "loadidl-cddr_01-queue.el")
 (main "clorb:idl;cddr-01-rest.idl" "loadidl-cddr_01-rest.el")
 (main "/Users/lenst/src/lisp/net/cddr/podcatch/pod.idl" "loadidl-podcatch.el")

|#

