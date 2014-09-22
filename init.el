;;; init.el --- My emacs settings

;;; Commentary:

;;; Code:


;; emacs directory
(when load-file-name
  (setq user-emacs-directory (expand-file-name
                              (file-name-directory load-file-name))))

;; bootstrap
(load (locate-user-emacs-file "bootstrap"))

;; load module loader
(load (locate-user-emacs-file "loader"))

;; define modules to load
(setq my/modules `(
                     "setup-basic"
                     ;; "setup-session"
                     "custom"
                     ,(when is-mac "setup-osx")
                     ;; "setup-migemo"
                     ;; "setup-anything"
                     "setup-helm"
                     ;; "setup-tags"
                     "setup-autocomplete"
                     ;; "setup-eshell"
                     ;; "setup-dired"
                     ;; "setup-editor"
                     ;; "setup-yasnippet"
                     ;; "setup-smartrep"
                     ;; "setup-wrap-region-mode"
                     "setup-vcs"
                     ;; "setup-w3m"
                     ;; "setup-experiment"
                     "setup-lisp"
                     ;; "setup-lua"
                     ;; "setup-yaml-mode"
                     ;; "setup-go"
                     ;; "setup-coffee-mode"
                     ;; "setup-scala-mode"
                     ;; "setup-org-mode"
                     ;; "setup-android-mode"
                     ;; "setup-php-mode"
                     ;; "setup-ruby-mode"
                     ;; "setup-scss-mode"
                     ;; "setup-visual-basic-mode"
                     ;; "setup-ace-jump-mode"
                     ;; "setup-quickrun"
                     ;; "setup-appearance"
                     ;; "utils-file"
                     ;; "utils-editting"

                     ;;"setup-evil"
                     "setup-key-bindings"
                     ))

;; load modules
(my/load-modules)



;;; init.el ends here
