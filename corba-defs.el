;;(require 'corba)
(provide 'corba-defs)

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