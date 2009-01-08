;;;; test-corba.el --- Test cases for corba.el

(require 'corba)


;;;; Union


(progn
  (remhash "IDL:Simple/list:1.0" corba-local-typecode-repository)
  (remhash "IDL:Simple/ToySexpr:1.0" corba-local-typecode-repository)

  (corba-typecode
   '(:tk_union "IDL:Simple/ToySexpr:1.0" "ToySexpr" (:tk_short) -1
     ((1 "atom_val" (:tk_string 0))
      (2 "list_val"
       (:tk_alias "IDL:Simple/list:1.0" "list"
                  (:tk_sequence "IDL:Simple/ToySexpr:1.0" 0))))))

  (let* ((tc-alias (corba-typecode "IDL:Simple/list:1.0"))
         (tc-seq   (nth 3 tc-alias))
         (tc-union (nth 1 tc-seq)))
    (assert (corba-typecode-p tc-union))
    (assert (corba-union-symbols tc-union)
            [:atom_val :list_val])))


(let* ((tc (corba-typecode "IDL:Simple/ToySexpr:1.0"))
       (a (corba-new-union tc :atom_val "Hello")))
  (assert (eq (car-safe a) 'union))
  (assert (eql (cadr a) 1))
  (assert (equal (caddr a) "Hello")))


(let ((a (corba-new "IDL:Simple/ToySexpr:1.0" :atom_val "Hello")))
  (assert (eql (corba-union-discriminator a) 1))
  (assert (equal (corba-union-value a) "Hello"))
  (let ((s (corba-new "IDL:Simple/ToySexpr:1.0"
                      :list_val (list a))))
    (assert (eql (corba-union-discriminator s) 2))
    (assert (equal (corba-union-value s) (list a)))))

(let ((tc (corba-typecode "IDL:Simple/ToySexpr:1.0"))
      (a (corba-new "IDL:Simple/ToySexpr:1.0" :atom_val "Hello")))
  (let ((s (corba-new "IDL:Simple/ToySexpr:1.0"
                      :list_val (list a))))
    (corba-in-work-buffer
      (corba-marshal a tc)
      (goto-char (point-min))
      (assert (eql (corba-read-short) 1))
      (assert (equal (corba-read-string) "Hello"))
      (goto-char (point-min))
      (let ((u (corba-unmarshal tc)))
        (assert (corba-union-p u))
        (assert (equal (corba-union-value u) "Hello"))))))

;; with default

(progn
  (corba-typecode
   '(:tk_union "IDL:Simple/ToySexpr2:1.0" "ToySexpr2" (:tk_short) 1
          ((1 "atom_val" (:tk_string 0))
           (0 "list_val"
            (:tk_alias "IDL:Simple/list2:1.0" "list"
             (:tk_sequence "IDL:Simple/ToySexpr2:1.0" 0))))))
  (let ((l (corba-new "IDL:Simple/ToySexpr2:1.0" :list_val nil)))
    (assert (= (cadr l) 0))))

(corba-typecode '(:tk_alias "IDL:Simple/list2:1.0" "list"
                  (:tk_sequence "IDL:Simple/ToySexpr2:1.0" 0)))

(let ((tc (corba-typecode "IDL:Simple/ToySexpr2:1.0"))
      (l (corba-new "IDL:Simple/ToySexpr2:1.0" :list_val nil)))
  (corba-in-work-buffer
    (corba-marshal l tc)
    (goto-char (point-min))
    (let ((u (corba-unmarshal tc)))
      (assert (corba-union-p u))
      (assert (eql (corba-union-discriminator u) 0))
      (assert (eq (corba-union-value u) nil))))
  (corba-in-work-buffer
    (corba-write-short 99)
    (corba-marshal (list) (corba-typecode "IDL:Simple/list2:1.0"))
    (goto-char (point-min))
    (let ((u (corba-unmarshal tc)))
      (assert (corba-union-p u))
      (assert (/= (corba-union-discriminator u) 1))
      (assert (eq (corba-union-value u) nil)))))

;;;; Exceptions

;;; CORBA Exceptions gets mapped to conditions...
;; CORBA User Exceptions becomes corba-user-exception condition
;;  and data for the condition is (id member-struct)
;; CORBA System Exception becomes corba-system-exception
;;  and data for the condition is (id minor-upper16 minor-lower16 completion-status)

;;; TypeCode representation:
;; (:tk_except id name ((member-name type)*))

(setq tc-e1 (corba-typecode `(:tk_except "IDL:my-except:1.0" "my-except"
                                         (("code" ,corba-tc-ulong)))))

(corba-in-work-buffer
  (corba-write-typecode tc-e1)
  (goto-char (point-min))
  (assert (equal (corba-read-typecode) tc-e1)))

;;(corba-struct-symbols tc-e1)
(corba-in-work-buffer
  (corba-write-string (nth 1 tc-e1))
  (corba-write-ulong 789)
  (goto-char (point-min))
  (let ((e (corba-unmarshal-except (list tc-e1))))
    (message "e=%S" e)
    (assert (equal e `(corba-user-exception
                       ,(nth 1 tc-e1)   ;id
                        [[ "my-except" :code] 789])))))

(corba-in-work-buffer                   ; unknown user exception
  (corba-write-string (nth 1 tc-e1))
  (corba-write-ulong 789)
  (goto-char (point-min))
  (let ((e (corba-unmarshal-except nil)))
    (message "e=%S" e)
    (assert (equal e `(corba-system-exception
                       "IDL:omg.org/CORBA/UNKNOWN:1.0"
                       ,corba-omgvmcid-upper 1 :COMPLETED_YES)))))


;; System Exceptions
(corba-in-work-buffer
  (corba-write-string "IDL:omg.org/CORBA/BAD_PARAM:1.0")
  (corba-write-align 4)
  (corba-write-short 234)
  (corba-write-short corba-omgvmcid-upper)
  (corba-write-ulong 2)
  (goto-char (point-min))
  (let ((s (corba-read-system-exception)))
    (assert (equal s `(corba-system-exception "IDL:omg.org/CORBA/BAD_PARAM:1.0"
                                              ,corba-omgvmcid-upper 234
                                              :COMPLETED_MAYBE)))))


;;;; ANY

(let ((any (corba-any corba-tc-string "hello")))
  (assert (equal (corba-any-value any) "hello"))
  (assert (equal (corba-get any :any-value) "hello"))
  (corba-put any :any-value "fisk")
  (assert (equal (corba-get any :any-value) "fisk"))
  (assert (equal (corba-get corba-tc-string :length) 0))
  (assert (equal (corba-get corba-tc-object :name) "Object")))


;;;; Mutable TC

(let ((tc (make-corba-typecode :tk_sequence '(:tk_null 0))))
  (corba-put tc :content_type corba-tc-string)
  (assert (equal tc `(:tk_sequence ,corba-tc-string 0))))



;;;; noir-resolve

(defun noir-resolve (ns name)
  (corba-funcall corba-tc-object "resolve_str" ns :in corba-tc-string name))



;;;; Services

(defvar corba-services-running t)

(when corba-services-running
  (corba-init)
  (assert (corba-object-p
           (car (corba-funcall corba-tc-object "resolve_str" (corba-get-ns)  
                               :in corba-tc-string "hello-lapps"))))
  ;;(setq id "IDL:omg.org/GIOP/MsgType_1_1:1.0")
  ;;(corba-get-typecode "IDL:omg.org/GIOP/MsgType_1_1:1.0")
  ;;(corba-get-interface "IDL:omg.org/CosNaming/NamingContext:1.0")
  (require 'corba-load-naming)
  (setq id "IDL:omg.org/CosNaming/NamingContext:1.0")
  ;;(setq def (corba-ir-lookup-id id))
  ;;(setq opseq (corba-ir-contents def 7 t))
  ;;(setq irdef (car opseq))
  ;;(corba-opdef-from-ir irdef)
  (require 'corba-load-ifr)
  (setq parseq (corba-typecode "IDL:omg.org/CORBA/ParDescriptionSeq:1.0"))
  ;;(corba-get-attribute irdef "_get_params" parseq)
  (require 'loadidl-clorb_ex-hello)
  (assert (corba-typep (corba-resolve "hello-lapps") "::CLORB_EX::HelloWorld")))
