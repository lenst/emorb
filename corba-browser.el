(require 'cl)
(require 'corba)
;;(require 'tree)

(defun corba-browser-expand-ir (widget)
  (let* ((orb (widget-get widget :orb) )
         (parent (widget-get widget :parent))
         (object (widget-get widget :ir-object)))
    (unless object
      (setq object (corba-get-ir))
      (widget-put widget :ir-object object))
    (mapcar
       (lambda (contained)
         (let ((name (car (corba-funcall "_get_name" contained))))
           `(tree-widget :tag ,name
                         :dynargs corba-browser-expand-ir
                         :orb ,orb
                         :ir-object ,contained )))
       (condition-case err
           (car (corba-funcall "contents" object 1 t))
         (error 
          (unless (string-match "Undefined operation" (cadr err))
            (signal (car err) (cdr err))))))))




(defun corba-browser-expand-ns (widget)
  (let* ((orb (widget-get widget :orb))
         (name (widget-get widget :ns-name))
         (context (widget-get widget :ns-context)))
    (cond (name
           (let ((p-context (widget-get (widget-get widget :parent)
                                        :ns-context)))
             (setq context (car (corba-funcall "resolve" p-context name)))))
          (t
           (setq context
                 (corba-object-narrow
                  (corba-orb-resolve-initial-references orb "NameService")
                  "IDL:omg.org/CosNaming/NamingContext:1.0"))))
    (widget-put widget :ns-context context)
    (if (and context
             (corba-object-is-a context
                                "IDL:omg.org/CosNaming/NamingContext:1.0"))
        (let  ((result (corba-funcall "list" context 100)))
          (when (second result)
            (corba-funcall "destroy" (second result)))
          (mapcar
           (lambda (binding)
             (let* ((name (corba-struct-get binding 'binding-name))
                    (type (corba-struct-get binding 'binding-type))
                    (id (corba-struct-get (first name) 'id))
                    (kind (corba-struct-get (first name) 'kind)))
               (if (= type 0)
                   `(item ,(format "%s.%s" id kind))
                 `(tree-widget
                   :tag ,(format "%s.%s" id kind)
                   :ns-name ,name :orb ,orb
                   :dynargs corba-browser-expand-ns
                   :has-children t ))))
           (first result))))))



(defun corba-browser ()
  (interactive)
  (pop-to-buffer (get-buffer-create "*Browser*"))
  (kill-all-local-variables)
  (make-local-variable 'widget-example-repeat)
  (let ((inhibit-read-only t))
    (erase-buffer))
  (remove-overlays)
  (widget-insert "CORBA Browser\n\n")
  (widget-create 'tree-widget
                 :tag "CORBA"
                 :open t
                 `(tree-widget
                   :tag "NameService"
                   :dynargs corba-browser-expand-ns
                   :has-children t
                   :orb nil )
                 '(tree-widget
                   :tag "InterfaceRepository"
                   :dynargs corba-browser-expand-ir
                   :has-children t
                   :orb nil ))
  (use-local-map widget-keymap)
  (widget-setup))

