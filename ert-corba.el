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
