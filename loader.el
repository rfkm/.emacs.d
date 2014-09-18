;;; loader.el --- loader

;;; Commentary:

;;; Code:
(defvar my/modules-dir (locate-user-emacs-file "modules"))
(defvar my/modules nil)

(defun my/module-selector (modules)
  "Sort and filter given MODULES according to `my/modules'."
  (delq nil
        (mapcar (lambda (a)
                  (let ((elc (concat a ".elc"))
                        (el (concat a ".el")))
                    (cond ((member elc modules) elc)
                          ((member el modules) el)
                          ((member a modules) a)
                          (t
                           (warn "Unknown module: %s" a)
                           nil))))
                (delq nil my/modules))))


(defun my/load-modules ()
  "Load modules."
  (setq-default init-loader-show-log-after-init t
                init-loader-byte-compile t
                init-loader-default-regexp "\\`\\(?:setup-\\|utils-\\|custom.el\\)"
                init-loader-sort-function 'my/module-selector)
         
  (init-loader-load my/modules-dir))


;; (require 'helm-files)
(defvar my/helm-source-emacs-modules
  `((name . "EmacsModules")
    (candidates . (lambda ()
                    (with-helm-current-buffer
                      (let ((dirs (append (directory-files user-emacs-directory t)
                                          (directory-files my/modules-dir t)))
                            (filter (lambda (d) (string-match "^.*\.el$" d))))
                        (remove-if-not filter dirs)))))
    (type . file)))
(defun my/helm-list-emacs-modules ()
  (interactive)
  (helm-other-buffer 'my/helm-source-emacs-modules "*helm-emacs-modules*"))
(global-set-key (kbd "C-x r .") 'my/helm-list-emacs-modules)

;;; loader.el ends here
