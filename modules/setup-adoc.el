;;; setup-adoc.el --- adoc-mode settings

;;; Commentary:

;;; Code:

(use-package adoc-mode
  :mode (("\\.adoc\\'" . adoc-mode))
  :config
  (bind-keys :maap adoc-mode-map
             ("C-s-l" . adoc-promote)
             ("C-s-j" . adoc-denote))

  (defun my/adoc-mode-hook ()
    (flyspell-mode +1))
  (add-hook 'adoc-mode-hook 'my/adoc-mode-hook))

;;; setup-adoc.el ends here
