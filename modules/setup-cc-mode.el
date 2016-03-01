;;; setup-cc-mode.el --- cc-mode settings

;;; Commentary:

;;; Code:

(use-package cc-mode
  :defer t
  :config
  (when my/use-ergonomic-key-bindings
    (bind-keys :map c-mode-base-map
               ("C-d" . nil)))

  (defun my/c-mode-hook ()
    ;; gtags
    (helm-gtags-mode 1)
    (auto-complete-mode 1))

  (add-hook 'c-mode-hook 'my/c-mode-hook))

;;; setup-cc-mode.el ends here
