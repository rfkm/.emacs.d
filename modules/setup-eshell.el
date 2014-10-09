;;; setup-eshell.el --- eshell settings

;;; Commentary:

;;; Code:

(use-package eshell
  :defer t
  :config (progn
            (setq eshell-directory-name (locate-user-emacs-file ".eshell/"))))

;;; setup-eshell.el ends here
