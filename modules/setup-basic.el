;;; setup-basic.el --- basic settings.

;;; Commentary:

;; References:
;; https://github.com/technomancy/better-defaults/blob/master/better-defaults.el
;; https://github.com/magnars/.emacs.d/blob/master/sane-defaults.el

;;; Code:

;; Keep emacs Custom-settings in separate file
(setq custom-file (locate-user-emacs-file "modules/custom.el"))

;; Write backup files to own directory
(setq backup-directory-alist `(("." . ,(locate-user-emacs-file ".backups"))))
(setq vc-make-backup-files t)

;; Default major mode
(setq major-mode 'text-mode)
(setq initial-scratch-message ";; foooo")

;; No splash screen
(setq inhibit-startup-message t)

;; bookmarks
(use-package bookmark
  :defer t
  :config (setq bookmark-default-file (locate-user-emacs-file ".bookmarks")
                bookmark-save-flag 1))

;; auto save
(setq auto-save-list-file-prefix (concat user-emacs-directory ".auto-save-list/.saves-"))

;; Language
(set-language-environment 'Japanese)
(prefer-coding-system 'utf-8-unix)
(set-default-coding-systems 'utf-8-unix)
(set-buffer-file-coding-system 'utf-8-unix)
(setq buffer-file-coding-system 'utf-8-unix)

;; Performance
(setq gc-cons-threshold 20000000)
(setq jit-lock-stealth-time 16
      jit-lock-defer-contextually t
      jit-lock-stealth-nice 0.5)
(setq-default font-lock-multiline t)

;; Ignore case
(setq read-file-name-completion-ignore-case t)

;; CUA
(cua-mode 1)
(setq cua-enable-cua-keys nil)

;; linum-mode
(setq linum-delay t)
(defadvice linum-schedule (around my-linum-schedule () activate)
  (run-with-idle-timer 0.2 nil #'linum-update-current))
(setq linum-format "  %d ")
(eval-after-load "auto-complete"
  ;; Disable line number updates while auto-comple window is
  ;; displayed.
  '(ac-linum-workaround))
(global-linum-mode)

;; display time on mode line
(display-time-mode 1)

;; scroll
(setq scroll-conservatively 10000
      scroll-margin 0
      scroll-step 1
      scroll-preserve-screen-position t)

;; Allow pasting selection outside of Emacs
(setq x-select-enable-clipboard t)

;; Auto refresh buffers
(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t)

;; Show keystrokes in progress
(setq echo-keystrokes 0.1)

;; Transparently open compressed files
(auto-compression-mode 1)

;; Enable syntax highlighting for older Emacsen that have it off
(global-font-lock-mode 1)

;; Answering just 'y' or 'n' will do
(defalias 'yes-or-no-p 'y-or-n-p)

;; Show active region
(transient-mark-mode 1)

;; Remove text in active region if inserting text
(delete-selection-mode 1)

;; Show the current column number
(column-number-mode 1)

;; Show the current line number
(line-number-mode 1)

;; Lines should be 80 characters wide, not 72
(setq fill-column 80)

;; Save a list of recent files visited.
(setq recentf-save-file (locate-user-emacs-file ".recentf"))
(setq recentf-max-saved-items 2000)
(recentf-mode 1)

;; Never insert tabs
(set-default 'indent-tabs-mode nil)
(setq-default tab-width 4)

;; Show me empty lines after buffer end
(set-default 'indicate-empty-lines t)

;; Don't break lines for me, please
(setq-default truncate-lines t)

;; Fontify org-mode code blocks
(setq org-src-fontify-natively t)

;; Sentences do not need double spaces to end. Period.
(set-default 'sentence-end-double-space nil)

;; Easily navigate sillycased words
(global-subword-mode 1)

;; No lock files
(setq create-lockfiles nil)

;; tramp
;; (require 'tramp)
;; (defadvice tramp-handle-vc-registered (around tramp-handle-vc-registered-around activate)
;;   (let ((vc-handled-backends '(SVN Git))) ad-do-it))

;; (add-to-list 'tramp-default-proxies-alist
;;              '(nil "\\`root\\'" "/ssh:%h:"))
;; (add-to-list 'tramp-default-proxies-alist
;;              '("localhost" nil nil))
;; (add-to-list 'tramp-default-proxies-alist
;;              '((regexp-quote (system-name)) nil nil))

;; winner-mode
(winner-mode 1)

(require 'server)
(unless (server-running-p)
  (server-start))

;; spell checking
(setq-default ispell-program-name "aspell")

;; smart pairing for all
(electric-pair-mode 1)

;; Hippie expand
(setq hippie-expand-try-functions-list '(try-expand-dabbrev
                                         try-expand-dabbrev-all-buffers
                                         try-expand-dabbrev-from-kill
                                         try-complete-file-name-partially
                                         try-complete-file-name
                                         try-expand-all-abbrevs
                                         try-expand-list
                                         try-expand-line
                                         try-complete-lisp-symbol-partially
                                         try-complete-lisp-symbol))

;; whitespace
(setq whitespace-line-column 80)
(setq whitespace-style '(face tabs empty trailing))

(global-whitespace-mode +1)
;; (add-hook 'before-save-hook 'whitespace-cleanup nil t)

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-ignore-buffers-re "*[^*]+*")

;; anzu
(use-package anzu
  :idle (global-anzu-mode 1))

;; Flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)

;; open-junk-file
(use-package open-junk-file
  :bind ("C-c s-j" . open-junk-file)
  :config (setq open-junk-file-format "~/Dropbox/junk/%Y%m%d-%H%M%S."))


;; shell-pop
(use-package shell-pop
  :bind ("C-x C-z" . shell-pop)
  :config (progn
            (shell-pop--set-shell-type 'shell-pop-shell-type '("eshell" "*eshell*" (lambda () (eshell))))))

;; expand-region
(use-package expand-region
  :bind ("C-@" . er/expand-region))

;; multiple-cursors
(use-package multiple-cursors
  :bind (("C-<" . mc/mark-previous-like-this)
         ("C->" . mc/mark-next-like-this)
         ("C-*" . mc/mark-all-like-this)))

;; jump-char
(when my/use-ergonomic-key-bindings
  (use-package jump-char
    :bind (("C-f" . jump-char-forward)
           ("M-f" . jump-char-backward))))

;; undo-tree
(use-package undo-tree
  :bind ("C-M-/" . undo-tree-redo)
  :idle (progn
          (setq-default undo-tree-mode-lighter nil)
          (setq undo-limit 600000)
          (setq undo-strong-limit 900000)
          (global-undo-tree-mode 1)))

;; popwin
(use-package popwin
  :init (progn
          (popwin-mode 1)))

;; helm-ag
(use-package helm-ag
  :init (progn
          (defun my/projectile-helm-ag ()
            (interactive)
            (helm-ag (projectile-project-root)))
          (bind-key "C-x G" 'my/projectile-helm-ag)))

;; quickrun -- Execute editing buffer
(use-package quickrun
  :bind (("s-q" . quickrun)
         ("s-Q" . quickrun-shell)))

;;; setup-basic.el ends here
