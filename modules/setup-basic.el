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
(setq auto-save-file-name-transforms `((".*" ,(locate-user-emacs-file ".backups/") t)))
(setq vc-make-backup-files t)

;; No splash screen
(setq inhibit-startup-message t)

;; bookmarks
(use-package bookmark
  :defer t
  :config (setq bookmark-default-file (locate-user-emacs-file ".bookmarks")
                bookmark-save-flag 1))

;; auto save
(setq auto-save-list-file-prefix (concat user-emacs-directory ".auto-save-list/.saves-"))

;; Default coding system
(prefer-coding-system 'utf-8-unix)

;; Performance
(setq gc-cons-threshold 50000000)
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
(bind-keys ("M-L" . linum-mode))

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
(diminish 'subword-mode)

;; No lock files
(setq create-lockfiles nil)

(defun my/make-directory-maybe (filename &optional wildcards)
  (unless (file-exists-p filename)
    (let ((dir (file-name-directory filename)))
      (unless (file-exists-p dir)
        (make-directory dir t)))))

(advice-add #'find-file :before #'my/make-directory-maybe)

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
(use-package flyspell-mode
  :config
  (bind-keys :map flyspell-mode-map
             ("C-;" . nil)))

;; smart pairing for all
(electric-pair-mode 1)

(use-package abbrev
  :config
  (setq abbrev-file-name (locate-user-emacs-file ".abbrev_defs")))

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
(use-package whitespace
  :diminish global-whitespace-mode
  :config
  (setq whitespace-line-column 80)
  (setq whitespace-style '(face tabs empty trailing tab-mark)))


(global-whitespace-mode +1)
;; (add-hook 'before-save-hook 'whitespace-cleanup nil t)

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-ignore-buffers-re "*[^*]+*")

;; anzu
(use-package anzu
  :defer 2
  :diminish anzu-mode
  :config (global-anzu-mode 1))

;; Flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)

;; open-junk-file
(use-package open-junk-file
  :bind ("C-c s-j" . open-junk-file)
  :config (setq open-junk-file-format "~/Dropbox/junk/%Y%m%d-%H%M%S."))


;; shell-pop
(use-package shell-pop
  :bind ("C-x C-z" . shell-pop)
  :config
  (shell-pop--set-shell-type 'shell-pop-shell-type '("eshell" "*eshell*" (lambda () (eshell)))))

;; expand-region
(use-package expand-region
  :bind ("C-@" . er/expand-region))

;; multiple-cursors
(use-package multiple-cursors
  :bind (("C-<" . mc/mark-previous-like-this)
         ("C->" . mc/mark-next-like-this)
         ("C-*" . mc/mark-all-like-this)
         ("C-\"" . mc/edit-lines)))

;; jump-char
(when my/use-ergonomic-key-bindings
  (use-package jump-char
    :bind (("C-f" . jump-char-forward)
           ("M-f" . jump-char-backward))))

;; undo-tree
(use-package undo-tree
  :bind ("C-M-/" . undo-tree-redo)
  :config
  (setq-default undo-tree-mode-lighter nil)
  (setq undo-limit 600000)
  (setq undo-strong-limit 900000)
  (global-undo-tree-mode 1))

;; popwin
(use-package popwin
  :config
  (popwin-mode 1))

;; quickrun -- Execute editing buffer
(use-package quickrun
  :bind (("s-q" . quickrun)
         ("s-Q" . quickrun-shell)))


;; olivetti
(use-package olivetti
  :diminish olivetti-mode
  :bind (("s-c" . olivetti-mode))
  :config
  (setq-default olivetti-body-width 100)
  (bind-keys :map olivetti-mode-map
             ("C-s-0" . olivetti-expand)
             ("C-s-9" . olivetti-shrink)))

(use-package auto-dim-other-buffers
  :diminish auto-dim-other-buffers-mode
  :config
  (auto-dim-other-buffers-mode +1))

(use-package ido
  :config
  (ido-mode -1)
  (setq ido-enable-flex-matching t))

(use-package ido-ubiquitous-mode
  :config
  (ido-ubiquitous-mode +1))

(use-package ido-vertical-mode
  :config
  (ido-vertical-mode +1)
  (setq ido-vertical-show-count t)

  (defun my/define-ido-vetical-define-keys ()
    (when my/use-ergonomic-key-bindings
      (bind-keys :map ido-completion-map
                 ("A-i" . ido-prev-match) ; C-i
                 ("C-k" . ido-next-match))))

  (add-hook 'ido-setup-hook 'my/define-ido-vetical-define-keys))

(use-package highlight-symbol
  :diminish highlight-symbol-mode
  :config
  (setq highlight-symbol-idle-delay 0.5)
  (add-hook 'prog-mode-hook 'highlight-symbol-mode))

;;; setup-basic.el ends here
