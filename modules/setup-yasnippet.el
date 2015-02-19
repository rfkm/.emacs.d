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
            '(progn (use-package helm-c-yasnippet
                      :bind ("C-c y" . helm-yas-complete))

                    (defun my/yas-helm-prompt (prompt choices &optional display-fn)
                      "Use helm to select a snippet. Put this into `yas/prompt-functions.'"
                      (interactive)
                      (setq display-fn (or display-fn 'identity))
                      (if (require 'helm-config)
                          (let (tmpsource cands result rmap)
                            (setq cands (mapcar (lambda (x) (funcall display-fn x)) choices))
                            (setq rmap (mapcar (lambda (x) (cons (funcall display-fn x) x)) choices))
                            (setq tmpsource
                                  (list
                                   (cons 'name prompt)
                                   (cons 'candidates cands)
                                   '(action . (("Expand" . (lambda (selection) selection))))
                                   ))
                            (setq result (helm-other-buffer '(tmpsource) "*helm-select-yasnippet"))
                            (if (null result)
                                (signal 'quit "user quit!")
                              (cdr (assoc result rmap))))
                        nil))
                    (setq yas-prompt-functions '(my/yas-helm-prompt yas-ido-prompt yas-no-prompt))))))


;; snippet helper functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my/guess-user-name-from-git ()
  "Return user's name from git-config."
  (s-trim (shell-command-to-string "git config --get user.name")))

(defun my/guess-user-email-from-git ()
  "Return user's email address from git-config."
  (s-trim (shell-command-to-string "git config --get user.email")))

;;; setup-yasnippet.el ends here
