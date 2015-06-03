;;; setup-vcs.el --- vcs configurations

;;; Commentary:

;;; Code:

(use-package magit
  :bind (("C-x v d" . magit-status)
         ("C-x v L" . magit-key-mode-popup-logging))
  :init (progn
          (setq magit-last-seen-setup-instructions "1.4.0"))
  :config (progn
            (setq magit-status-buffer-switch-function 'switch-to-buffer)
            (setq magit-save-some-buffers nil)))

(use-package git-messenger
  :bind ("C-x v p" . git-messenger:popup-message)
  :config (progn
            (add-hook 'git-messenger:popup-buffer-hook 'magit-commit-mode)))

;;; setup-vcs.el ends here
