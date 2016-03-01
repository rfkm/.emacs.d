;;; setup-yaml-mode.el --- yaml-mode settings

;;; Commentary:

;;; Code:

(use-package yaml-mode
  :mode "\\.yml$"
  :config
  (defun my/yaml-mode-hook ()
    (add-hook 'before-save-hook 'my/cleanup-buffer-safe nil t))
  (add-hook 'yaml-mode-hook 'my/yaml-mode-hook)

  (when my/use-ergonomic-key-bindings
    (bind-keys :map yaml-mode-map
               ("C-j" . nil)
               ("C-m" . nil))))

;;; setup-yaml-mode.el ends here
