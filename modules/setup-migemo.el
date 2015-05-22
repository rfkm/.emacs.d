;;; setup-migemo.el --- migemo settings.

;;; Commentary:

;;; Code:

(use-package migemo
  :config (progn
            (if (executable-find "cmigemo")
                (progn
                  (setq migemo-command "cmigemo")
                  (setq migemo-options '("-q" "--emacs"))
                  (setq migemo-dictionary "/usr/local/share/migemo/utf-8/migemo-dict")
                  (setq migemo-user-dictionary nil)
                  (setq migemo-regex-dictionary nil)
                  (setq migemo-coding-system 'utf-8-unix)
                  (migemo-init))
              (setq migemo-isearch-enable-p nil))))

;;; setup-migemo.el ends here
