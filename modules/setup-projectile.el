;;; setup-projectile.el --- projectile settings

;;; Commentary:

;;; Code:

(use-package projectile
  :bind (("C-c p p" . projectile-switch-project)
         ("C-x g" . projectile-find-file))
  :config
  (progn
    (setq projectile-completion-system 'helm)
    (setq projectile-use-git-grep t)
    (setq projectile-cache-file (locate-user-emacs-file ".projectile.cache"))
    (setq projectile-known-projects-file (locate-user-emacs-file ".projectile-bookmarks.eld"))
    (projectile-load-known-projects)
    (projectile-global-mode 1)

    (when mac?
      (defun my/projectile-sourcetree ()
        "Open the current project with SouceTree."
        (interactive)
        (projectile-with-default-dir (projectile-project-root)
          (call-process "open" nil nil nil "." "-a" "SourceTree")))

      (bind-keys :map projectile-command-map
                 ("G" . my/projectile-sourcetree)))))

;;; setup-projectile.el ends here
