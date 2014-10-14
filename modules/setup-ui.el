;;; setup-ui.el --- ui settings

;;; Commentary:

;;; Code:
(setq visible-bell t
      font-lock-maximum-decoration t
      truncate-partial-width-windows nil)

(use-package paren
  :init (progn (setq show-paren-style 'mixed)
               (show-paren-mode 1)))

;; (use-package hl-line
;;   :init (global-hl-line-mode 1))

;; (blink-cursor-mode -1)

;; Display time mode
(setq display-time-string-forms '((format
                                   "%s/%s(%s) %s:%s" month day dayname 24-hours minutes)))
(display-time-mode t)

(setq split-width-threshold 200)

(when window-system
  ;; Theme
  (load-theme 'solarized-light)

  ;; Font settings
  ;;
  ;;    2 byte char width = 1 byte char width * 2
  ;;    あいうえおかきくけこさしすせそ
  ;;    ''''''''''''''''''''''''''''''
  (let* ((size 14)                      ; [9/10/12/14/15/17/19/20/...]
         (asciifont "Ricty")            ; ASCII fonts
         (jpfont "Ricty")               ; Japanese fonts
         (h (* size 10))
         (fontspec (font-spec :family asciifont))
         (jp-fontspec (font-spec :family jpfont)))
    (set-face-attribute 'default nil :family asciifont :height h)
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
   ("s-<down>" . my/dec-frame-alpha)))

;; CLI
(unless window-system
  (progn
    ;; (load-theme 'solarized-dark)
    ))

;;; setup-ui.el ends here
