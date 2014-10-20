;;; setup-cc-mode.el --- cc-mode settings

;;; Commentary:

;;; Code:

(use-package cc-mode
  :defer t
  :config (progn
            (when my/use-ergonomic-key-bindings
              (bind-keys :map c-mode-base-map
                         ("C-d" . nil)))))

;;; setup-cc-mode.el ends here
