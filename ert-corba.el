;;; ert-corba.el --- Test cases for corba.el

;; Copyright (C) 2009  Cons unlimited

;; Author: Lennart Staflin <lenst@lysator.liu.se>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'ert)
(require 'corba)



;;;; Enum

;; Enum constants are keywords

;; Enum TC representation:
;; (:tk_enum id name (member-name*))

(deftest corba-enum ()
  (let ((tc-e1
         (corba-typecode '(:tk_enum "IDL:my-enum:1.0" "my-enum"
                           ("foo" "fie" "fumm")))))

    (should (equal (corba-enum-symbols tc-e1) '(:foo :fie :fumm)))

    (corba-in-work-buffer
      (corba-marshal :fie tc-e1)
      (corba-marshal :fumm tc-e1)
      (goto-char (point-min))
      (should (= (corba-read-ulong) 1))
      (should (eq (corba-unmarshal tc-e1) :fumm)))

    (corba-in-work-buffer
      (corba-write-typecode tc-e1)
      (goto-char (point-min))
      (should (equal (corba-read-typecode) tc-e1)))) )



;;;; Sequence and Array

;;; Sequences are mapped to lists
;;; Arrays mapped to vectors

;;; TC representations:
;; (:tk_sequence content-type length)
;; (:tk_array content-type length)

(deftest corba-seq ()
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
          (should (equal s2 s1))
          (should (equal a2 a1))
          (list s2 a2))))))



;;;; Struct

;;; Struct TypeCode:
;;;  (:tk_struct id name ((member-name type)*))
;;;  id = Repository ID
;;;
;;; Struct Mapping:
;;;  [keys-vector val1 .. valn]
;;; keys-vector = [name key1 .. key2]
;;; key1 = keyword from member-name1
;;;
;;; Create: (corba-new id :key1 val1 :key2 val2 ...)
;;; Access: (corba-get struct key)
;;;         (corba-put struct key val)
;;;


(deftest corba-struct ()
  (let* ((tc-raw `(:tk_struct "IDL:my-struct:1.0" "my-struct"
                              (("a" ,corba-tc-string)
                               ("b" ,corba-tc-ulong))) )
         (tc-s1 (corba-typecode tc-raw)))
    (should (equal (corba-struct-symbols tc-s1) [ "my-struct" :a :b]))
    (let ((s (corba-new "IDL:my-struct:1.0" :a "hej" :b 12)))
      (should (equal (corba-get s :a) "hej"))
      (should (= (corba-get s :b) 12))
      (corba-put s :b 13)
      (should (= (corba-get s :b) 13))
      (corba-in-work-buffer
        (corba-write-typecode tc-s1)
        (corba-marshal s tc-s1)
        (goto-char (point-min))
        (should (equal (corba-read-typecode) tc-s1))
        (should (equal (corba-unmarshal tc-s1) s))))))



;;;; Union

;;; Representation: (union <discriminator> <value>)

;;; Readers:
;;; corba-union-discriminator u => discriminator
;;; corba-union-value u => value

;;; Creator:
;;;  corba-new repo-id-or-absolute-name label value
;;; where
;;;  label = keyword with name of union member
;;;

;;; TypeCode
;;;
;;; (:tk_union id name discriminator-type default-index
;;;        ((member-label member-name member-type)*))
;;;  where default-index is index of member that is the default
;;;             or -1 if no default
;;;        member-label is of type discriminator-type


(defun corba-test-union-types ()
  ;; Creates a recursive union type for use in test cases
  (remhash "IDL:Simple/list:1.0" corba-local-typecode-repository)
  (remhash "IDL:Simple/ToySexpr:1.0" corba-local-typecode-repository)
  (corba-typecode
   '(:tk_union "IDL:Simple/ToySexpr:1.0" "ToySexpr" (:tk_short) -1
     ((1 "atom_val" (:tk_string 0))
      (2 "list_val"
       (:tk_alias "IDL:Simple/list:1.0" "list"
                  (:tk_sequence "IDL:Simple/ToySexpr:1.0" 0)))))))


(deftest corba-union-tc ()
  (corba-test-union-types)
  (let* ((tc-alias (corba-typecode "IDL:Simple/list:1.0"))
         (tc-seq   (nth 3 tc-alias))
         (tc-union (nth 1 tc-seq)))
    (should (corba-typecode-p tc-union))
    (should (equal (corba-union-symbols tc-union)
                   [:atom_val :list_val]))))


(deftest corba-union-new-union ()
  (corba-test-union-types)
  (let* ((tc (corba-typecode "IDL:Simple/ToySexpr:1.0"))
         (a (corba-new-union tc :atom_val "Hello")))
    (should (eq (car-safe a) 'union))
    (should (eql (cadr a) 1))
    (should (equal (caddr a) "Hello"))))


(deftest corba-union-new ()
  (corba-test-union-types)
  (let ((a (corba-new "IDL:Simple/ToySexpr:1.0" :atom_val "Hello")))
    (should (eql (corba-union-discriminator a) 1))
    (should (equal (corba-union-value a) "Hello"))
    (let ((s (corba-new "IDL:Simple/ToySexpr:1.0"
                        :list_val (list a))))
      (should (eql (corba-union-discriminator s) 2))
      (should (equal (corba-union-value s) (list a))))))


(deftest corba-union-marshal ()
  (corba-test-union-types)
  (let ((tc (corba-typecode "IDL:Simple/ToySexpr:1.0"))
        (a (corba-new "IDL:Simple/ToySexpr:1.0" :atom_val "Hello")))
    (let ((s (corba-new "IDL:Simple/ToySexpr:1.0"
                        :list_val (list a))))
      (corba-in-work-buffer
        (corba-marshal a tc)
        (goto-char (point-min))
        (should (eql (corba-read-short) 1))
        (should (equal (corba-read-string) "Hello"))
        (goto-char (point-min))
        (let ((u (corba-unmarshal tc)))
          (should (corba-union-p u))
          (should (equal (corba-union-value u) "Hello")))))))


;; with default

(defun corba-test-union-types-with-default ()
  ;; Creates a recursive union type for use in test cases
  (remhash "IDL:Simple/list2:1.0" corba-local-typecode-repository)
  (remhash "IDL:Simple/ToySexpr2:1.0" corba-local-typecode-repository)
  (corba-typecode
   '(:tk_union "IDL:Simple/ToySexpr2:1.0" "ToySexpr2" (:tk_short) 1
          ((1 "atom_val" (:tk_string 0))
           (0 "list_val"
            (:tk_alias "IDL:Simple/list2:1.0" "list"
             (:tk_sequence "IDL:Simple/ToySexpr2:1.0" 0)))))))


(deftest corba-union-simple-default ()
  (corba-test-union-types-with-default)
  (let ((l (corba-new "IDL:Simple/ToySexpr2:1.0" :list_val nil)))
    (should (= (cadr l) 0))))


(deftest corba-union-marshal-default ()
  (corba-test-union-types-with-default)
  (let ((tc (corba-typecode "IDL:Simple/ToySexpr2:1.0"))
        (l (corba-new "IDL:Simple/ToySexpr2:1.0" :list_val nil)))
    (corba-in-work-buffer
      (corba-marshal l tc)
      (goto-char (point-min))
      (let ((u (corba-unmarshal tc)))
        (should (corba-union-p u))
        (should (eql (corba-union-discriminator u) 0))
        (should (eq (corba-union-value u) nil))))
    (corba-in-work-buffer
      (corba-write-short 99)
      (corba-marshal (list) (corba-typecode "IDL:Simple/list2:1.0"))
      (goto-char (point-min))
      (let ((u (corba-unmarshal tc)))
        (should (corba-union-p u))
        (should (/= (corba-union-discriminator u) 1))
        (should (eq (corba-union-value u) nil)))))  )



;;;; ANY

;;; Repr: (any <typecode> <value>)

;;; Reader:
;;;  corba-any-typecode any => tc
;;;  corba-any-value any => value
;;; Accessors:
;;;  corba-get any :any-value => value
;;;  corba-get any :any-typecode => tc
;;; 

;;; TyepCode: (:tk_any)

(deftest corba-any ()
  (let ((any (corba-any corba-tc-string "hello")))
    (should (equal (corba-any-value any) "hello"))
    (should (equal (corba-get any :any-value) "hello"))
    (corba-put any :any-value "fisk")
    (should (equal (corba-get any :any-value) "fisk"))
    (should (equal (corba-get corba-tc-string :length) 0))
    (should (equal (corba-get corba-tc-object :name) "Object"))))



;;;; Exceptions

;;; CORBA Exceptions gets mapped to conditions...
;; CORBA User Exceptions becomes corba-user-exception condition
;;  and data for the condition is (id member-struct)
;; CORBA System Exception becomes corba-system-exception
;;  and data for the condition is (id minor-upper16 minor-lower16 completion-status)

;;; TypeCode representation:
;; (:tk_except id name ((member-name type)*))


(deftest corba-user-exception ()
  (let ((tc-e1 (corba-typecode `(:tk_except "IDL:my-except:1.0" "my-except"
                                            (("code" ,corba-tc-ulong))))))

    ;; marshal/unmarshal expception typecode
    (corba-in-work-buffer
      (corba-write-typecode tc-e1)
      (goto-char (point-min))
      (should (equal (corba-read-typecode) tc-e1)))

    ;; Unmarshalling exception
    (corba-in-work-buffer
      ;; exception representation
      (corba-write-string (nth 1 tc-e1))
      (corba-write-ulong 789)
      (goto-char (point-min))
      (let ((e (corba-unmarshal-except (list tc-e1))))
        (should (equal e `(corba-user-exception
                           ,(nth 1 tc-e1) ;id
                           [[ "my-except" :code] 789]))))

      ;; unknown user exception, read same exception, but not listed as valid
      (goto-char (point-min))
      (let ((e (corba-unmarshal-except nil)))
        (should (equal e `(corba-system-exception
                           "IDL:omg.org/CORBA/UNKNOWN:1.0"
                           ,corba-omgvmcid-upper 1 :COMPLETED_YES)))))))


(deftest corba-system-exception ()
  (corba-in-work-buffer
    (corba-write-string "IDL:omg.org/CORBA/BAD_PARAM:1.0")
    (corba-write-align 4)
    (corba-write-short 234)
    (corba-write-short corba-omgvmcid-upper)
    (corba-write-ulong 2)
    (goto-char (point-min))
    (let ((s (corba-read-system-exception)))
      (should (equal s `(corba-system-exception "IDL:omg.org/CORBA/BAD_PARAM:1.0"
                                                ,corba-omgvmcid-upper 234
                                                :COMPLETED_MAYBE))))))


;;;; Recursive TypeCodes

(deftest corba-write-recursive-typecode ()
  (let* ((tc1 (list :tk_sequence nil 0))
         (tc2 `(:tk_struct "IDL:foobar" "foobar"
                           (("a" (:tk_long))
                            ("b" ,tc1)))))
    (setcar (cdr tc1) tc2)
    (corba-in-work-buffer
      (corba-write-typecode tc2))))



(deftest corba-read-recursive-typecode ()
  ;; read a struct tc with a member of with same struct type
  (let (encaps-start)
    (corba-in-work-buffer
      (corba-write-ulong (get :tk_struct 'tk-index))
      (setq encaps-start (point))
      (corba-write-ulong 0)                     ;len
      (corba-write-octet 1)                     ;byte order
      (corba-write-ulong 1) (corba-write-octet 0) ;"" id
      (corba-write-ulong 1) (corba-write-octet 0) ;"" name
      (corba-write-ulong 1)                       ; 1 member
      (corba-write-string "a")
      (corba-write-long -1)
      (corba-write-long (- (point-min) (point))) ; point back to self
      (let ((len (- (point) 9)))
        (goto-char encaps-start)
        (delete-char 4)
        (corba-write-ulong len))
      (goto-char (point-min))
      (let ((tc (corba-read-typecode)))
        (should (eq tc (cadr (car (elt tc 3)))))))))


(deftest corba-read-recursive-typecode-2 ()
  (let* ((tc1 (list :tk_sequence nil 0))
         (tc2 `(:tk_struct "IDL:foobar" "foobar"
                           (("a" (:tk_long))
                            ("b" ,tc1)))))
    (setcar (cdr tc1) tc2)
    (corba-in-work-buffer
      (corba-write-typecode tc1)
      (goto-char (point-min))
      (let ((tc (corba-read-typecode)))
        (should (eq (car tc) :tk_sequence))
        (let* ((s (cadr tc))            ;struct
               (m (elt s 3))            ;members
               (b (elt m 1))            ;b member
               )
          (should (eq (cadr b) tc)))))))



(provide 'ert-corba)
;;; ert-corba.el ends here
