;; CORBA Based Rest Interfaces

(require 'corba)
(require 'loadidl-cddr_01-rest)

(defconst crest-sexp "::CDDR_01::Sexp")


(defun crest-from-sexp (sexp)
  (let ((d (corba-union-discriminator sexp))
        (v (corba-union-value sexp)))
    (cond ((eq d 1)                     ; a list
           (mapcar 'crest-from-sexp v))
          ((eq d 2)                     ; symbol
           (corba-make-keyword v))
          ((eq d 0)                     ; any
           (corba-any-value v))
          ((>= d 10)
           v)
          (t
           (error "Invalid union discriminator: %d" d)))))


(defun crest-new-sexp (type val)
  (corba-new crest-sexp type val))


(defun crest-to-sexp (x)
  (cond ((listp x)
         (crest-new-sexp :list_val (mapcar 'crest-to-sexp x)))
        ((integerp x)
         (crest-new-sexp :long_val x))
        ((floatp x)
         (crest-new-sexp :double_val x))
        ((stringp x)
         (crest-new-sexp :string_val x))
        ((keywordp x)
         (crest-new-sexp :symbol_val (substring (symbol-name x) 1)))
        ((eq x t)
         (crest-new-sexp :long_val 1))
        ((corba-object-p x)
         (crest-new-sexp :object_val x))
        (t
         (error "Invalid type: %S" x))))


(defun crest-get (r)
  (crest-from-sexp (car (corba-funcall "get" r))))

(defun crest-put (r x)
  (corba-funcall "put" r (crest-to-sexp x)))

(defun crest-getq (r q)
  (crest-from-sexp
   (car (corba-funcall "getq" r (crest-to-sexp q)))))

(defun crest-delete (r)
  (corba-funcall "delete" r))

(defun crest-insert (r s)
  (crest-from-sexp
   (car (corba-funcall "insert" r (crest-to-sexp s)))))



(provide 'crest)
