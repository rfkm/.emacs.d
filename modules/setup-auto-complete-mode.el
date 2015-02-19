;;; setup-auto-complete-mode.el --- auto-complete-mode settings

;;; Commentary:

;;; Code:

(use-package auto-complete
  :config (progn
            ;; (setq ac-dictionary-directories (locate-user-emacs-file "ac-dict"))
            (require 'auto-complete-config)

            (setq-default ac-sources '(ac-source-yasnippet
                                       ac-source-abbrev
                                       ac-source-dictionary
                                       ac-source-words-in-same-mode-buffers))
            (setq ac-comphist-file (locate-user-emacs-file ".ac-comphist.dat"))

            (add-hook 'emacs-lisp-mode-hook 'ac-emacs-lisp-mode-setup)
            (add-hook 'c-mode-common-hook 'ac-cc-mode-setup)
            (add-hook 'ruby-mode-hook 'ac-ruby-mode-setup)
            (add-hook 'css-mode-hook 'ac-css-mode-setup)
            (add-hook 'auto-complete-mode-hook 'ac-common-setup)
            (global-auto-complete-mode t)

            (setq ac-auto-start 2)
            (setq ac-auto-show-menu 0.1)

            (bind-keys :map ac-completing-map
                       ("C-s" . ac-isearch))

            (bind-keys :map ac-mode-map
                       ("C-'" . auto-complete))

            (when my/use-ergonomic-key-bindings
              (bind-keys :map ac-completing-map
                         ("A-i" . ac-previous)
                         ("C-k" . ac-next)))

            (setf (symbol-function 'yas-active-keys)
                  (lambda ()
                    (remove-duplicates (mapcan #'yas--table-all-keys (yas--get-snippet-tables)))))))

;;; setup-auto-complete-mode.el ends here
