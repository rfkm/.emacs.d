;;; setup-lisp.el --- lisp-mode config.

;;; Commentary:

;;; Code:

;; rainbow-delimiters

;; paredit
(use-package paredit
  :config (progn
            (bind-keys :map paredit-mode-map
                       ("C-k"   . next-line)
                       ("C-d"   . paredit-kill)
                       ("C-o"   . paredit-forward-delete)
                       ("C-M-o" . paredit-forward-kill-word)
                       ("C-u"   . paredit-backward-delete)
                       ("C-M-u" . paredit-backward-kill-word)
                       ("C-j"   . backward-char)
                       ("C-M-u" . backward-kill-word))

            ;; Enable `paredit-mode' in the minibuffer, during `eval-expression'.
            (defun conditionally-enable-paredit-mode ()
              (if (eq this-command 'eval-expression)
                  (paredit-mode 1)))
            (add-hook 'minibuffer-setup-hook 'conditionally-enable-paredit-mode)))

;; el-doc
(setq eldoc-idle-delay 0.2)
(setq eldoc-minor-mode-string "")

;; hooks
(defun my/lisp-mode-defaults ()
  (paredit-mode 1)
  (rainbow-delimiters-mode 1)
  (turn-on-eldoc-mode))

(defun my/lisp-mode-hook ()
  (my/lisp-mode-defaults))

(add-hook 'emacs-lisp-mode-hook 'my/lisp-mode-hook)
(add-hook 'lisp-interaction-mode-hook 'my/lisp-mode-hook)
(add-hook 'lisp-mode-hook 'my/lisp-mode-hook)
(add-hook 'ielm-mode-hook 'my/lisp-mode-hook)

(add-to-list 'auto-mode-alist '("Cask\\'" . emacs-lisp-mode))

;; C-x F, C-x K, C-x V
(find-function-setup-keys)

;; clojure
(use-package clojure-mode
  :defer t
  :config 
  (progn 
    (use-package clojure-mode-extra-font-locking)
    (use-package midje-mode
      :init
      (add-hook 'clojure-mode-hook 'midje-mode))
    (use-package clj-refactor
      :config
      (add-hook 'clojure-mode-hook (lambda ()
                                     (clj-refactor-mode 1))))
    (use-package ac-cider
      :init
      (progn
        (add-hook 'cider-mode-hook 'ac-flyspell-workaround)
        (add-hook 'cider-mode-hook 'ac-cider-setup)
        (add-hook 'cider-repl-mode-hook 'ac-cider-setup)
        (eval-after-load "auto-complete"
          '(add-to-list 'ac-modes 'cider-mode))))
    ))

(use-package ace-jump-mode
  :bind ("C-." . ace-jump-mode)
  :config
  (message "Yay, ace-jump-mode was actually loaded!"))

;; clojure-mode
;; (bundle! clojure-mode
;;   (require 'clojure-mode-extra-font-locking)
;;   (add-to-list 'auto-mode-alist '("\\.cljs$" . clojure-mode))
;;   (bundle midje-mode
;;     (add-hook 'clojure-mode-hook 'midje-mode))
;;   ;; (bundle! elpa:clj-refactor)


;;   (eval-after-load-compile 'clojure-mode

;;     (defun my/clojure-mode-hook ()
;;       (add-hook 'before-save-hook 'my/cleanup-buffer nil t)
;;       (require 'clj-refactor)
;;       (clj-refactor-mode 1)
;;       ;; (cljr-add-keybindings-with-prefix "C-c C-v")
;;       (cljr-add-keybindings-with-modifier "C-H-")
;;       ;; (clojure-test-mode 1)
;;       (my/lisp-defaults))

;;     (add-hook 'clojure-mode-hook 'my/clojure-mode-hook)

;;     ))

;; (bundle ac-nrepl
;;   (add-hook 'cider-repl-mode-hook 'ac-nrepl-setup)
;;   (add-hook 'cider-mode-hook 'ac-nrepl-setup))

;; (add-to-list 'package-pinned-packages '(cider . "melpa-stable") t)
;; (bundle elpa:cider

;;   ;; (bundle! nrepl-ritz
;;   ;;   (define-key cider-interaction-mode-map (kbd "C-c C-j") 'cider-javadoc)
;;   ;;   (define-key cider-mode-map (kbd "C-c C-j") 'cider-javadoc)
;;   ;;   (define-key cider-interaction-mode-map (kbd "C-c C-a") 'cider-apropos)
;;   ;;   (define-key cider-mode-map (kbd "C-c C-a") 'cider-apropos))
;;   ;; (bundle! vitalreactor/nrepl-inspect
;;   ;;   (define-key nrepl-mode-map (kbd "C-c i") 'nrepl-inspect))
;;   (eval-after-load-compile 'cider
;;     (setq nrepl-hide-special-buffers t)
;;     (setq cider-repl-popup-stacktraces t)
;;     (setq cider-repl-history-file (locate-user-emacs-file "nrepl-history"))

;;     (define-key cider-mode-map (kbd "C-j") nil)
;;     (define-key cider-mode-map (kbd "M-RET") 'cider-newline-and-indent)
;;     (add-hook 'cider-interaction-mode-hook 'cider-turn-on-eldoc-mode)

;;     (defun cider-namespace-refresh ()
;;       (interactive)
;;       (cider-interactive-eval
;;        "(require 'clojure.tools.namespace.repl)
;;     (clojure.tools.namespace.repl/refresh)"))
;;     (defun my/cider-mode-hook ()
;;       (define-key cider-mode-map (kbd "C-c C-d") 'ac-nrepl-popup-doc)
;;       (run-hooks 'lisp-interaction-mode-hook))
;;     (add-hook 'cider-mode-hook 'my/cider-mode-hook)))




;;; setup-lisp.el ends here
