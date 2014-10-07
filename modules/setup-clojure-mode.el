;;; setup-clojure-mode.el --- clojure-mode-settings

;;; Commentary:

;;; Code:

;; Clojure
(use-package clojure-mode
  :defer t
  :config
  (progn
    (message "Configuring clojure-mode...")

    (use-package clojure-mode-extra-font-locking)
    (use-package midje-mode)
    (use-package clj-refactor
      :config (cljr-add-keybindings-with-modifier "C-H-"))
    (use-package cider
      :config (progn
                (setq nrepl-hide-special-buffers t)
                (setq cider-repl-history-file (locate-user-emacs-file ".nrepl-history"))

                (use-package cider-eldoc)
                (use-package ac-cider
                  :init
                  (progn
                    (eval-after-load "auto-complete"
                      '(progn (add-to-list 'ac-modes 'cider-mode)
                              (add-to-list 'ac-modes 'cider-repl-mode)))))

                (defun my/cider-mode-hook ()
                  (paredit-mode 1)
                  (rainbow-delimiters-mode 1)
                  (cider-turn-on-eldoc-mode)
                  (ac-flyspell-workaround) ; ?

                  ;; Currently, ac-cider's default source is broken.
                  ;; See: https://github.com/clojure-emacs/ac-cider/issues/13
                  ;; (ac-cider-setup)
                  (add-to-list 'ac-sources '((available . ac-cider-available-p)
                                             (candidate-face . ac-cider-candidate-face)
                                             (selection-face . ac-cider-selection-face)
                                             (prefix . cider-completion-symbol-start-pos)
                                             (document . ac-cider-documentation)
                                             ;; (match . ac-cider-match-everything)
                                             (cache)
                                             (candidates . ac-cider-candidates-everything)
                                             (symbol . "v"))))

                (add-hook 'cider-mode-hook 'my/cider-mode-hook)
                (add-hook 'cider-repl-mode-hook 'my/cider-mode-hook)

                (defun cider-namespace-refresh ()
                  (interactive)
                  (cider-interactive-eval
                   "(require 'clojure.tools.namespace.repl)
(clojure.tools.namespace.repl/refresh)"))

                (when my/use-ergonomic-key-bindings
                  (bind-keys :map cider-mode-map
                             ("C-j" . nil))
                  (bind-keys :map cider-repl-mode-map
                             ("C-j" . nil)))))

    (defun my/clojure-mode-hook ()
      (paredit-mode 1)
      (rainbow-delimiters-mode 1))

    (add-hook 'clojure-mode-hook 'my/clojure-mode-hook)))

;;; setup-clojure-mode.el ends here
