;;; setup-yasnippet.el --- yasnippet config.

;;; Commentary:

;; Load and initialize yasnippet

;;; Code:

(use-package yasnippet
  :idle (progn
          (setq yas-snippet-dirs (list (expand-file-name (locate-user-emacs-file "misc/snippets"))
                                       yas-installed-snippets-dir))
          (yas-global-mode 1)

          (eval-after-load "helm"
            '(use-package helm-c-yasnippet
               :bind ("C-c y" . helm-yas-complete)))))


(defun my/guess-user-name-from-git ()
  (s-trim (shell-command-to-string "git config --get user.name")))

(defun my/guess-user-email-from-git ()
  (s-trim (shell-command-to-string "git config --get user.email")))

;; (yas/setup "~/.emacs.d/plugin/yasnippet-0.6.1c")

;; (yas/initialize)


;; Develop in ~/emacs.d/snippets, but also
;; include snippets that come with yasnippet

;; (mapc 'yas/load-directory yas/root-directory)
;; (custom-set-variables '(yas-trigger-key "TAB"))

;; ;; Include snippets for Buster.js
;; (require 'buster-snippets)

;; ;; Jump to end of snippet definition
;; (define-key yas/keymap (kbd "C-<return>") 'yas/exit-all-snippets)

;; ;; Inter-field navigation
;; (defun yas/goto-end-of-active-field ()
;;   (interactive)
;;   (let* ((snippet (car (yas/snippets-at-point)))
;;         (position (yas/field-end (yas/snippet-active-field snippet))))
;;     (if (= (point) position)
;;         (move-end-of-line-or-next-line)
;;       (goto-char position))))

;; (defun yas/goto-start-of-active-field ()
;;   (interactive)
;;   (let* ((snippet (car (yas/snippets-at-point)))
;;         (position (yas/field-start (yas/snippet-active-field snippet))))
;;     (if (= (point) position)
;;         (move-start-of-line-or-prev-line)
;;       (goto-char position))))

;; (define-key yas/keymap (kbd "C-e") 'yas/goto-end-of-active-field)
;; (define-key yas/keymap (kbd "C-a") 'yas/goto-start-of-active-field)

;; No dropdowns please, yas
;; (setq yas-prompt-functions '(yas/ido-prompt yas/completing-prompt))



;;; setup-yasnippet.el ends here
