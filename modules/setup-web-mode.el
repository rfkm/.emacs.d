;;; setup-web-mode.el --- web-mode settings

;;; Commentary:

;;; Code:

(use-package web-mode
  :mode "\\.html\\.twig\\'"
  :init (progn
          (setq web-mode-enable-auto-pairing nil)))

;;; setup-web-mode.el ends here
