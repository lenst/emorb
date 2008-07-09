(require 'cl)
(require 'corba)
(require 'tree)

(defun irb-root-expand (node)
  (let* ((names nil)
         (ir (corba-get-ir))
         (container ir)
         (name (car node)))
    (setq name (substring name 0 (- (length name) 2)))
    (unless (equal name "")
      (setq container (car (corba-funcall "lookup" ir name) )))
    (setq names
          (if container
              (map 'list
                   (lambda (contained)
                     (car (corba-invoke contained "_get_name")))
                   (car (corba-funcall "contents" container :dk_all nil)))))
    names))

(defvar ir-browser-mode-map
  (make-sparse-keymap))

(defun ir-browser-mode ()
  (use-local-map ir-browser-mode-map)
  (suppress-keymap ir-browser-mode-map)
  (local-set-key " " 'tree-toggle-branch)
  (setq major-mode 'ir-browser
        mode-name "IR Browser"))

(defun ir-browser ()
  (interactive)
  (pop-to-buffer (get-buffer-create "*Interface Repository*"))
  (erase-buffer)
  (ir-browser-mode)
  (make-local-variable 'tree-root-nodes)
  (make-local-variable 'tree-seperator)
  (setq tree-seperator "::")
  (setq tree-root-nodes (list ""))
  (tree-create tree-root-nodes 'irb-root-expand t)
  (goto-char (point-min)))
