;;; setup-yaml-mode.el --- yaml-mode settings

;;; Commentary:

;;; Code:

(use-package yaml-mode
  :mode "\\.yml$"
  :config (progn
            (bind-keys :map yaml-mode-map
                       ("C-j" . backward-char)
                       ("C-m" . newline-and-indent))))

;;; setup-yaml-mode.el ends here
