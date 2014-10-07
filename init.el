;;; init.el --- My emacs settings

;;; Commentary:

;;; Code:

;; Determine `user-emacs-directory'.
(when load-file-name
  (setq user-emacs-directory (expand-file-name
                              (file-name-directory load-file-name))))


;; Customization variables
(defgroup my/settings nil
  "My settings."
  :group 'emacs)

(defcustom my/use-ergonomic-key-bindings t
  "Non-nil to use ergonomic key bindings.  See setup-key-bindings.el.
You need to restart Emacs after changing the value."
  :group 'my/settings
  :type 'boolean)


;; Loading Cask, configuring paths...
(load (locate-user-emacs-file "bootstrap"))

;; Load modules
(require 'core-loader)

(setq my/modules (list
                  "custom"
                  "setup-basic"
                  "setup-key-bindings"
                  ;; "setup-session"
                  "setup-ui"
                  (when mac? "setup-osx")
                  "setup-migemo"
                  "setup-helm"
                  ;; "setup-tags"
                  "setup-auto-complete-mode"
                  ;; "setup-eshell"
                  "setup-dired"
                  ;; "setup-editor"
                  "setup-yasnippet"
                  ;; "setup-smartrep"
                  ;; "setup-wrap-region-mode"
                  "setup-vcs"
                  ;; "setup-w3m"
                  ;; "setup-experiment"
                  "setup-lisp"
                  "setup-clojure-mode"
                  ;; "setup-lua"
                  "setup-web-mode"
                  "setup-yaml-mode"
                  ;; "setup-go"
                  "setup-coffee-mode"
                  ;; "setup-scala-mode"
                  "setup-org-mode"
                  ;; "setup-android-mode"
                  "setup-php"
                  "setup-ruby-mode"
                  "setup-scss-mode"
                  ;; "setup-visual-basic-mode"
                  "setup-ace-jump-mode"
                  ;; "setup-quickrun"
                  ;; "setup-appearance"
                  ;; "utils-file"
                  ;; "utils-editting"
                  "utils"))

(my/load-modules)

;;; init.el ends here
