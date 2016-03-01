;;; setup-ui.el --- ui settings

;;; Commentary:

;;; Code:
(setq visible-bell t
      font-lock-maximum-decoration t
      truncate-partial-width-windows nil)

(use-package paren
  :init
  (setq show-paren-style 'mixed)
  (show-paren-mode 1))

(use-package hl-line
  :init (global-hl-line-mode 1))

(blink-cursor-mode -1)

;; Display time mode
(setq display-time-string-forms '((format
                                   "%s/%s(%s) %s:%s" month day dayname 24-hours minutes)))
(display-time-mode t)

(setq split-width-threshold 200)

(when window-system
  ;; Theme
  (load-theme 'gruvbox)

  (use-package powerline
    :config
    (setq powerline-default-separator 'contour)
    (powerline-default-theme))

  ;; Font settings
  ;;  Font width checker:
  ;;    The both edges of two lines below should be aligned.
  ;;    |あいうえおかきくけこさしすせそ🍺|
  ;;    |''''''''''''''''''''''''''''''''|
  ;;
  (let* ((size 14)
         (asciifont "Fira Code")
         ;; (jpfont "Ricty")
         (jpfont "Rounded M+ 1ms")      ; my mod font
         (emojifont "Apple Color Emoji")
         (fontspec (font-spec :family asciifont))
         (jp-fontspec (font-spec :family jpfont))
         (emoji-fontspec (font-spec :family emojifont)))
    (set-face-attribute 'default nil :family asciifont :height (* size 10) :weight 'light)
    (setq face-font-rescale-alist nil)
    (add-to-list 'face-font-rescale-alist `(,(regexp-quote jpfont) . 1.2))
    (add-to-list 'face-font-rescale-alist `(,emojifont . 0.95))
    (set-fontset-font nil 'symbol emoji-fontspec nil)
    (set-fontset-font nil 'japanese-jisx0213.2004-1 jp-fontspec)
    (set-fontset-font nil 'japanese-jisx0213-2 jp-fontspec)
    (set-fontset-font nil 'katakana-jisx0201 jp-fontspec) ; 半角カナ
    (set-fontset-font nil '(#x0080 . #x024F) fontspec) ; 分音符付きラテン
    (set-fontset-font nil '(#x0370 . #x03FF) fontspec) ; ギリシャ文字
    )

  ;; Frame alphe
  (defun my/change-frame-alpha (delta)
    (let* ((current-alpha (or (frame-parameter (selected-frame) 'alpha) 100))
           (new-alpha (max 10 (min 100 (+ current-alpha delta)))))
      (message (number-to-string new-alpha))
      (set-frame-parameter nil 'alpha new-alpha)))

  (defun my/inc-frame-alpha ()
    (interactive)
    (my/change-frame-alpha +2))

  (defun my/dec-frame-alpha ()
    (interactive)
    (my/change-frame-alpha -2))

  (bind-keys
   ("s-<up>" . my/inc-frame-alpha)
   ("s-<down>" . my/dec-frame-alpha))

  (add-hook 'after-init-hook 'toggle-frame-maximized))

;; CLI
(unless window-system
  (progn
    ;; (load-theme 'solarized-dark)
    ))

;;; setup-ui.el ends here
