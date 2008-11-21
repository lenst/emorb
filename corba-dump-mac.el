;;;; corba-dump-mac.el

;; Copyright (C) 2007, 2008 Lennart Staflin

;; Author: Lennart Staflin <lenst@lysator.liu.se>

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.



(provide 'corba-dump-mac)

(require 'corba)
(require 'corba-meta)


(defun corba-make-translated-form (item var)
  ;; (struct "absolute_name" (> "type" typecode) contents)
  ;; or "absolute_name", (> "type" typecode), ...
  (cond ((null item) nil)
        ((eq item 'contents) `(do-contents ,var))
        ((keywordp item) `(corba-get ,var ,item))
        ((symbolp item) `',item)
        ((stringp item) `(corba-get ,var ,item))
        ((and (consp item) (memq (car item) '(> @)))
         (let ((attr `(corba-get ,var ,(second item))))
           (assert (null (cdddr item)))
           (if (eq (car item) '>)
               (ecase (third item)
                 ((typecode) `(corba-tc-dump ,attr))
                 ((any-value) `(corba-any-value ,attr)))
               `(mapcar (lambda (x) ,(corba-make-translated-form (third item) 'x))
                        ,attr))))
        ((consp item)
         `(cons ,(corba-make-translated-form (car item) var)
                ,(corba-make-translated-form (cdr item) var)))
        (t
         (error "Invalid form item: %s" item))))


(defun corba-make-dumper (spec)
  ;;(CORBA:StructDef (:dk_struct) (struct "absolute_name" (> "type" typecode) contents))
  (let ((class (pop spec))
        (kinds (pop spec))
        (form (pop spec)))
    (declare (ignore class))
    `(cons ',kinds
           (lambda (def)
             ,(corba-make-translated-form form 'def)))))


(defmacro corba-dumper-table (content-dumper)
  (subst content-dumper 'do-contents
         `(list ,@(mapcar #'corba-make-dumper corba-ifr-repr-meta))))



;;; corba-dump-mac.el ends here
