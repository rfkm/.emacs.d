;;; setup-clojure-mode.el --- clojure-mode-settings

;;; Commentary:

;;; Code:

;; Clojure
(use-package clojure-mode
  :mode "\\.boot\\'"
  :config
  (progn
    (use-package clojure-mode-extra-font-locking)
    (use-package align-cljlet
      :init (bind-keys :map clojure-mode-map
                       ("C-c j a l" . align-cljlet)))
    (use-package midje-mode)
    (use-package clj-refactor
      :diminish clj-refactor-mode
      :config (progn
                (cljr-add-keybindings-with-prefix "C-c j")
                (setq cljr-eagerly-build-asts-on-startup nil)
                (setq cljr-populate-artifact-cache-on-startup nil)))
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

                  (ac-cider-setup))

                (add-hook 'cider-mode-hook 'my/cider-mode-hook)
                (add-hook 'cider-repl-mode-hook 'my/cider-mode-hook)

                (defun my/cider-namespace-refresh ()
                  (interactive)
                  (cider-interactive-eval
                   "(require 'clojure.tools.namespace.repl)(clojure.tools.namespace.repl/refresh)"))

                (defun my/cider-reload-project ()
                  (interactive)
                  (cider-interactive-eval
                   "(require 'alembic.still)(alembic.still/load-project)"))

                (defun my/cider-midje-run-autotest ()
                  (interactive)
                  (cider-interactive-eval
                   "(require 'midje.repl)(midje.repl/autotest)"))

                (when my/use-ergonomic-key-bindings
                  (bind-keys :map cider-mode-map
                             ("C-j" . nil))
                  (bind-keys :map cider-repl-mode-map
                             ("C-j" . nil)))))

    (defun my/clojure-mode-hook ()
      (add-hook 'before-save-hook 'my/cleanup-buffer nil t)
      (clj-refactor-mode 1)
      (paredit-mode 1)
      (rainbow-delimiters-mode 1))

    (add-hook 'clojure-mode-hook 'my/clojure-mode-hook)))

;;; setup-clojure-mode.el ends here
