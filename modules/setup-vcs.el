;;; setup-vcs.el --- vcs configurations

;;; Commentary:

;;; Code:

(use-package magit
  :bind (("C-x v d" . magit-status)
         ("C-x v l" . magit-log-popup))
  :init (progn
          (setq magit-last-seen-setup-instructions "1.4.0"))
  :config (progn
            (setq magit-status-buffer-switch-function 'switch-to-buffer)
            (setq magit-save-some-buffers nil)))

(use-package git-messenger
  :bind ("C-x v p" . git-messenger:popup-message)
  :config (progn
            (add-hook 'git-messenger:popup-buffer-hook 'magit-commit-mode)))

(use-package git-gutter
  :diminish git-gutter-mode
  :config (progn
            (add-to-list 'git-gutter:update-commands 'linum-mode)
            (setq git-gutter:added-sign "•")
            (setq git-gutter:deleted-sign "•")
            (setq git-gutter:modified-sign "•")
            (global-git-gutter-mode)))

;;; setup-vcs.el ends here
