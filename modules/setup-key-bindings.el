;;; setup-key-bindings.el --- Global key bindings

;;; Commentary:

;;; Code:
(require 'bind-key)

;; (defmacro my/swap-keys (a b)
;;   "Swap A for B."
;;   `(progn
;;      (keyboard-translate ,a ,b)
;;      (keyboard-translate ,b ,a)))

(bind-keys*
 ("C-t" . other-window)
 ("C-M-t" . other-frame)
 ("C-x C-r" . my/rename-current-buffer-file))

(bind-keys
 ("M-l" . recenter)
 ("s-TAB" . (lambda () (interactive) (scroll-down 1)))
 ("C-s-k" . (lambda () (interactive) (scroll-up 1)))
 ("s-k" . kill-this-buffer)
 ("s-u" . revert-buffer)
 ("M-/" . hippie-expand)
 ("s-l" . goto-line)
 ("C-c d" . my/duplicate-current-line-or-region))

;; Ergonomic key bindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when my/use-ergonomic-key-bindings
  (when window-system
    (keyboard-translate ?\C-i ?\A-i))

  (bind-keys
   ("C-l" . forward-char)
   ("C-j" . backward-char)
   ("A-i" . previous-line)              ; C-i
   ("C-k" . next-line)
   ("C-M-l" . forward-word)
   ("C-M-j" . backward-word)

   ("C-d" . kill-whole-line)

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
   ("C-n" . cua-scroll-up))

  ;; comint
  (with-eval-after-load "comint"
    (bind-keys
     :map comint-mode-map
     ("C-M-l" . forward-word)))

  ;; isearch-mode
  (bind-keys
   :map isearch-mode-map
   ("C-u" . isearch-delete-char))

  (bind-keys
   :map Buffer-menu-mode-map
   ("C-k" . nil))

  (bind-keys
   :map widget-field-keymap
   ("C-k" . nil)
   ("C-d" . widget-kill-line)
   ("C-e" . nil)
   ("C-;" . widget-end-of-line))

  ;; minibuffer
  (dolist (map (list minibuffer-local-map
                     minibuffer-local-ns-map
                     minibuffer-local-completion-map
                     minibuffer-local-must-match-map
                     minibuffer-local-shell-command-map
                     minibuffer-local-filename-completion-map))
    (bind-key "C-j" 'backward-char map)))

;;; setup-key-bindings.el ends here
