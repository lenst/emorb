(require 'cl)
(require 'corba)
(require 'tree)

(defun corba-browser-expand-ir (orb path)
  (let ((container
         (if path
             (let ((name (mapconcat 'identity path "::")))
               (car (corba-invoke (corba-get-ir) "lookup" name) ))
           (corba-get-ir))))
    (if container 
        (map 'list
             (lambda (contained)
               (car (corba-invoke contained "_get_name")))
             (car (corba-invoke container "contents" 1 nil))))))

(defun corba-browser-expand-ns (orb path)
  (let ((context
         (corba-orb-resolve-initial-references orb "NameService")))
    (when path
      (let ((name
             (mapcar
              (lambda (n)
                (destructuring-bind (id kind)
                    (split-string n "\t")
                  (corba-struct "IDL:omg.org/CosNaming/NameComponent:1.0"
                                'id id 'kind kind)))
              path)))
        (setq context (car (corba-invoke context "resolve" name)))))
    (if (and context
             (corba-object-is-a context
                                "IDL:omg.org/CosNaming/NamingContext:1.0"))
        (let  ((result (corba-invoke context "list" 100)))
          (when (second result)
            (corba-invoke (second result) "destroy"))
          (map 'list
               (lambda (binding)
                 (let ((name (corba-struct-get binding 'binding-name)))
                   (format "%s\t%s"
                           (corba-struct-get (first name) 'id)
                           (corba-struct-get (first name) 'kind))))
               (first result))))))


(defun corba-browser-expand (node)
  (let* ((names (split-string (car node) "::"))
         (name (car names))
         (orb (corba-orb-init)))
    (cond
     ((equal name "InterfaceRepository")
      (corba-browser-expand-ir orb (cdr names)))
     ((equal name "NameService")
      (corba-browser-expand-ns orb (cdr names))))))


(defvar corba-browser-mode-map
  (make-sparse-keymap))

(defun corba-browser-mode ()
  (use-local-map corba-browser-mode-map)
  (suppress-keymap corba-browser-mode-map)
  (local-set-key " " 'tree-toggle-branch)
  (setq major-mode 'corba-browser
        mode-name "CORBA Browser"))

(defun corba-browser ()
  (interactive)
  (pop-to-buffer (get-buffer-create "*Browser*"))
  (erase-buffer)
  (corba-browser-mode)
  (make-local-variable 'tree-root-nodes)
  (make-local-variable 'tree-seperator)
  (setq tree-seperator "::")
  (setq tree-root-nodes (list "InterfaceRepository" "NameService"))
  (tree-create tree-root-nodes 'corba-browser-expand t)
  (goto-char (point-min)))
