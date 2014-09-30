;;; setup-key-bindings.el --- Global key bindings

;;; Commentary:

;;; Code:
(require 'bind-key)

;; (defmacro my/swap-keys (a b)
;;   "Swap A for B."
;;   `(progn
;;      (keyboard-translate ,a ,b)
;;      (keyboard-translate ,b ,a)))

;; Key translation
(when window-system
  (keyboard-translate ?\C-i ?\A-i))

(bind-keys*
 ("C-t" . other-window)
 ("C-M-t" . other-frame))

(bind-keys
 ("C-l" . forward-char)
 ("C-j" . backward-char)
 ("A-i" . previous-line)		; C-i
 ("C-k" . next-line)
 ("C-M-l" . forward-word)
 ("C-M-j" . backward-word)
 
 ("C-d" . kill-whole-line)
 ("M-l" . recenter)
 
 ("C-e" . set-mark-command)
 ("C-;" . move-end-of-line)
 ("C-h" . move-beginning-of-line)
 ("<f1>" . help-command)
 
 ("C-u" . delete-backward-char)
 ("C-M-u" . backward-kill-word)
 ("C-a" . universal-argument)
 ("C-o" . delete-char)
 ("C-M-o" . kill-word)
 
 ("C-p" . cua-scroll-down)
 ("C-n" . cua-scroll-up)
 ("s-TAB" . (lambda () (interactive) (scroll-down 1)))
 ("C-s-k" . (lambda () (interactive) (scroll-up 1)))
 
 ("s-k" . kill-this-buffer)
 ("s-u" . revert-buffer)
 
 ("M-/" . hippie-expand))

;; isearch-mode
(bind-keys
 :map isearch-mode-map
 ("C-u" . isearch-delete-char))

;; minibuffer
(dolist (map (list minibuffer-local-map
                   minibuffer-local-ns-map
                   minibuffer-local-completion-map
                   minibuffer-local-must-match-map
                   minibuffer-local-shell-command-map
                   minibuffer-local-filename-completion-map))
  (bind-key "C-j" 'backward-char map))

;;; setup-key-bindings.el ends here
