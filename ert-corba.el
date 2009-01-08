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

(deftest corba-write-recursive-typecode ()
  (let* ((tc1 (list :tk_sequence nil 0))
        (tc2 `(:tk_struct "IDL:foobar" "foobar"
                          (("a" (:tk_long))
                           ("b" ,tc1)))))
    (setcar (cdr tc1) tc2)
  (corba-in-work-buffer
    (corba-write-typecode tc2))))

(deftest corba-read-recursive-typecode ()
  (let* ((tc1 (list :tk_sequence nil 0))
        (tc2 `(:tk_struct "IDL:foobar" "foobar"
                          (("a" (:tk_long))
                           ("b" ,tc1)))))
    (setcar (cdr tc1) tc2)
  (corba-in-work-buffer
    (corba-write-typecode tc2)
    (goto-char (point-min))
    (let ((tc (corba-read-typecode)))
      (should (eq (car tc) :tk_struct))
      (let* ((m (elt tc 3))
             (b (elt m 1))
             (tc1 (cadr b)))
        (should (eq (car tc1) :tk_sequence))
        (should (eq (cadr tc1) tc)))))))



(provide 'ert-corba)
;;; ert-corba.el ends here
