;;; setup-vcs.el --- vcs configurations

;;; Commentary:

;;; Code:

(use-package magit
  :bind (("C-x v d" . magit-status)
         ("C-x v L" . magit-key-mode-popup-logging))
  :init (progn
          (setq magit-status-buffer-switch-function 'switch-to-buffer)
          (setq magit-save-some-buffers nil)))

;;; setup-vcs.el ends here
