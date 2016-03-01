;;; setup-eshell.el --- eshell settings

;;; Commentary:

;;; Code:

(use-package eshell
  :defer t
  :config
  (setq eshell-directory-name (locate-user-emacs-file ".eshell/"))
  (defun my/eshell-mode-hook ()
    (when my/use-ergonomic-key-bindings
      (bind-keys :map eshell-mode-map
                 ("C-M-l" . forward-word))))

  (add-hook 'eshell-mode-hook 'my/eshell-mode-hook))

;;; setup-eshell.el ends here
