;;; setup-projectile.el --- projectile settings

;;; Commentary:

;;; Code:

(eval-when-compile (require 'projectile))

(use-package projectile
  :config
  (setq projectile-completion-system 'helm)
  (setq projectile-use-git-grep t)
  (setq projectile-cache-file (locate-user-emacs-file ".projectile.cache"))
  (setq projectile-known-projects-file (locate-user-emacs-file ".projectile-bookmarks.eld"))
  (setq projectile-switch-project-action 'projectile-dired)
  (projectile-load-known-projects)
  (projectile-global-mode 1)

  (defun my/projectile-helm-ag ()
    (interactive)
    (helm-ag (projectile-project-root)))

  (bind-keys :map projectile-mode-map
             ("C-x G" . my/projectile-helm-ag)
             ("C-x g" . projectile-find-file)
             ("C-x B" . projectile-switch-to-buffer))

  (bind-keys ("C-c p p" . projectile-switch-project))

  (defun my/projectile-sourcetree ()
    "Open the current project with SouceTree."
    (interactive)
    (projectile-with-default-dir (projectile-project-root)
      (call-process "open" nil nil nil "." "-a" "SourceTree")))

  (bind-keys :map projectile-mode-map
             ("C-c p G" . my/projectile-sourcetree)))

;;; setup-projectile.el ends here
