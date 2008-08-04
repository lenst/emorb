;;;; test-corba.el --- Test cases for corba.el

(require 'corba)


;;;; Enum

;; Enum constants are keywords

;; Enum TC representation:
;; (:tk_enum id name (member-name*))

(let ((tc-e1
       (corba-typecode '(:tk_enum "IDL:my-enum:1.0" "my-enum"
                                  ("foo" "fie" "fumm")))))

  (assert (equal (corba-enum-symbols tc-e1) '(:foo :fie :fumm)))

  (corba-in-work-buffer
    (corba-marshal :fie tc-e1)
    (corba-marshal :fumm tc-e1)
    (goto-char (point-min))
    (assert (= (corba-read-ulong) 1))
    (assert (eq (corba-unmarshal tc-e1) :fumm)))

  (corba-in-work-buffer
    (corba-write-typecode tc-e1)
    (goto-char (point-min))
    (assert (equal (corba-read-typecode) tc-e1))))

;;;; Sequence and Array

;;; Sequences are mapped to lists
;;; Arrays mapped to vectors

;;; TC representations:
;; (:tk_sequence content-type length)
;; (:tk_array content-type length)

(let ((tc-seq `(:tk_sequence ,corba-tc-string 0))
      (tc-arr `(:tk_array ,corba-tc-ulong 10)))
  (let ((s1 '("hej" "hopp" "mopp"))
        (a1 (make-vector 10 99)))
    (dotimes (i 10)
      (aset a1 i (+ (aref a1 i) i)))
    (corba-in-work-buffer
      (corba-marshal s1 tc-seq)
      (corba-marshal a1 tc-arr)
      (goto-char (point-min))
      (let ((s2 (corba-unmarshal tc-seq))
            (a2 (corba-unmarshal tc-arr)))
        (assert (equal s2 s1))
        (assert (equal a2 a1))
        (list s2 a2)))))



;;;; Struct

(let ((tc-s1 (corba-typecode `(:tk_struct "IDL:my-struct:1.0" "my-struct"
                                          (("a" ,corba-tc-string)
                                           ("b" ,corba-tc-ulong))))))

  (assert (equal (corba-struct-symbols tc-s1) [ "my-struct" :a :b]))
  (let ((s (corba-new "IDL:my-struct:1.0" :a "hej" :b 12)))
    (assert (equal (corba-get s :a) "hej"))
    (assert (= (corba-get s :b) 12))
    (corba-put s :b 13)
    (assert (= (corba-get s :b) 13))
    (corba-in-work-buffer
      (corba-write-typecode tc-s1)
      (corba-marshal s tc-s1)
      (goto-char (point-min))
      (assert (equal (corba-read-typecode) tc-s1))
      (assert (equal (corba-unmarshal tc-s1) s)))))



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
