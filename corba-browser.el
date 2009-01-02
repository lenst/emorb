;;; corba-browser.el -- Browse CORBA NameService and Interface Repository

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


;;; Code:

(require 'cl)
(require 'corba)
(require 'corba-load-naming)
(require 'tree-widget)


(defun corba-browser-expand-ir (widget)
  (let* ((orb (widget-get widget :orb) )
         (parent (widget-get widget :parent))
         (object (widget-get widget :ir-object)))
    (unless object
      (setq object (corba-get-ir))
      (widget-put widget :ir-object object))
    (let ((kind (corba-get object "def_kind")))
      (message "kind=%S" kind)
      (cons
       `(item ,(format "Kind: %s" kind))
       (case kind
         ((:dk_Constant)
          `((item ,(format "value: %s"
                           (corba-any-value (corba-get object "value"))))))
         ((:dk_Struct :dk_Exception)
          (mapcar (lambda (member)
                    `(item ,(corba-get member :name)))
                  (corba-get object "members")))
         (t
          (mapcar
           (lambda (contained)
             (let ((name (corba-get contained "name")))
               `(tree-widget :tag ,name
                             :expander corba-browser-expand-ir
                             :orb ,orb
                             :ir-object ,contained )))
           (condition-case err
               (car (corba-funcall "contents" object :dk_all t))
             (error
              (unless (string-match "Undefined operation" (cadr err))
                (warn "Error getting contents: %s" err))
              nil)))))))))



(defun corba-browser-get-context (widget)
  (let* ((orb (widget-get widget :orb))
         (name (widget-get widget :ns-name))
         (context
          (if (null name)
              (corba-get-ns)
            (corba-narrow
             (car (corba-funcall "resolve"
                                 (widget-get (widget-get widget :parent)
                                             :ns-context)
                                 name))
             "IDL:omg.org/CosNaming/NamingContext:1.0"))))
    (widget-put widget :ns-context context)
    context))


(defun corba-browser-expand-ns (widget)
  (condition-case err
      (let* ((orb (widget-get widget :orb))
             (context (corba-browser-get-context widget)))
        (if context
            (let  ((result (corba-funcall "list" context 100)))
              (when (second result)
                (corba-funcall "destroy" (second result)))
              (mapcar
               (lambda (binding)
                 (let* ((name (corba-get binding :binding_name))
                        (type (corba-get binding :binding_type))
                        (id   (corba-get (first name) :id))
                        (kind (corba-get (first name) :kind)))
                   (if (eql type :nobject)
                       `(item ,(format "%s.%s" id kind))
                       `(tree-widget
                         :tag ,(format "%s.%s" id kind)
                         :ns-name ,name :orb ,orb
                         :expander corba-browser-expand-ns
                         :has-children t ))))
               (first result)))))
    (error ;;corba-system-exception
     (warn "Can't expand node: %s" err)
     nil)))




(defun corba-browser ()
  (interactive)
  (pop-to-buffer (get-buffer-create "*Browser*"))
  (kill-all-local-variables)
  (let ((inhibit-read-only t))
    (erase-buffer))
  (remove-overlays)
  (widget-insert "CORBA Browser\n\n")
  (let ((orb (corba-init)))
    (widget-create 'tree-widget
                   :tag "CORBA"
                   :open t
                   `(tree-widget
                     :tag "NameService"
                     :expander corba-browser-expand-ns
                     :has-children t
                     :orb ,orb )
                   `(tree-widget
                     :tag "InterfaceRepository"
                     :expander corba-browser-expand-ir
                     :has-children t
                     :orb ,orb )) )
  (use-local-map widget-keymap)
  (widget-setup))



(provide 'corba-browser)

;;; corba-browser.el ends here
