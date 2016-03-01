;;; setup-tags.el --- configurations for code tagging systems

;;; Commentary:

;;; Code:

(use-package helm-gtags
  :config
  (bind-keys :map helm-gtags-mode-map
             ("M-." . helm-gtags-find-tag)
             ("M-," . helm-gtags-pop-stack)
             ("M-r" . helm-gtags-find-rtag)))

;;; setup-tags.el ends here
