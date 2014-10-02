;;; setup-yaml-mode.el --- yaml-mode settings

;;; Commentary:

;;; Code:

(use-package yaml-mode
  :mode "\\.yml$"
  :config (progn
            (when my/use-ergonomic-key-bindings
              (bind-keys :map yaml-mode-map
                         ("C-j" . nil)
                         ("C-m" . nil)))))

;;; setup-yaml-mode.el ends here
