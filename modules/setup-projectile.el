;;; setup-projectile.el --- projectile settings

;;; Commentary:

;;; Code:

(use-package projectile
  :bind (("C-c p p" . projectile-switch-project)
         ("C-x g" . projectile-find-file)
         ("C-x G" . my/projectile-helm-ag)
         ("C-x B" . projectile-switch-to-buffer))
  :init
  (progn
    (when mac?
      (autoload #'my/projectile-sourcetree "projectile" nil t)
      (bind-keys
       ("C-c p G" . my/projectile-sourcetree))))
  :config
  (progn
    (setq projectile-completion-system 'helm)
    (setq projectile-use-git-grep t)
    (setq projectile-cache-file (locate-user-emacs-file ".projectile.cache"))
    (setq projectile-known-projects-file (locate-user-emacs-file ".projectile-bookmarks.eld"))
    (projectile-load-known-projects)
    (projectile-global-mode 1)

    (defun my/projectile-helm-ag ()
      (interactive)
      (helm-ag (projectile-project-root)))

    (when mac?
      (defun my/projectile-sourcetree ()
        "Open the current project with SouceTree."
        (interactive)
        (projectile-with-default-dir (projectile-project-root)
          (call-process "open" nil nil nil "." "-a" "SourceTree"))))))

;;; setup-projectile.el ends here
