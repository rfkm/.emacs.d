;;; setup-clojure-mode.el --- clojure-mode-settings

;;; Commentary:

;;; Code:

;; Clojure
(use-package clojure-mode
  :mode "\\.boot\\'"
  :config
  (progn
    ;; custom indentations
    (define-clojure-indent
      (let-test-data 1))

    (use-package clojure-mode-extra-font-locking)
    (use-package align-cljlet
      :init (bind-keys :map clojure-mode-map
                       ("C-c j a l" . align-cljlet)))
    ;; (use-package midje-mode)
    (use-package clj-refactor
      :diminish clj-refactor-mode
      :config (progn
                (cljr-add-keybindings-with-prefix "C-c j")
                (setq cljr-eagerly-build-asts-on-startup nil)
                (setq cljr-populate-artifact-cache-on-startup nil)))
    (use-package cider
      :config (progn
                (setq nrepl-hide-special-buffers nil)
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

                (defun my/cider-test-clear-last-results ()
                  (interactive)
                  (setq cider-test-last-results '(dict)))

                ;; Hack for https://github.com/clojure-emacs/cider/issues/1452
                (defun my/cider-freeze-repl-overlay (&rest _)
                  (dolist (ov (overlays-at (1- (cider-repl--end-of-line-before-input-start))))
                    (when (member #'ansi-color-freeze-overlay (overlay-get ov 'modification-hooks)) ; ensure ov is crated by ansi-color
                      (unless (member #'ansi-color-freeze-overlay (overlay-get ov 'insert-behind-hooks))
                        (push #'ansi-color-freeze-overlay (overlay-get ov 'insert-behind-hooks))))))

                (advice-add 'cider-repl--emit-interactive-output :after #'my/cider-freeze-repl-overlay)

                (defun my/zou-go ()
                  (interactive)
                  (if current-prefix-arg
                      (progn
                        (save-some-buffers)
                        (cider-interactive-eval
                         "(zou.framework.repl/reset)"))
                    (cider-interactive-eval
                     "(zou.framework.repl/go)")))

                (when my/use-ergonomic-key-bindings
                  (bind-keys :map cider-mode-map
                             ("C-j" . nil)
                             ("s-;" . my/zou-go))
                  (bind-keys :map cider-repl-mode-map
                             ("C-j" . nil)
                             ("s-;" . my/zou-go)))))

    (defun my/clojure-mode-hook ()
      (add-hook 'before-save-hook 'my/cleanup-buffer nil t)
      (clj-refactor-mode 1)
      (paredit-mode 1)
      (rainbow-delimiters-mode 1))

    (add-hook 'clojure-mode-hook 'my/clojure-mode-hook)))

;;; setup-clojure-mode.el ends here
