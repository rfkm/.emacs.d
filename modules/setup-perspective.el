;;; setup-perspective.el --- persp-mode settings

;;; Commentary:

;;; Code:

(use-package persp-mode
  :init
  (persp-mode +1)
  :config
  (custom-set-variables `(persp-keymap-prefix ,(kbd "C-z")))
  (setq persp-save-dir (expand-file-name ".persp-confs/" user-emacs-directory))
  (setq persp-add-on-switch-or-display t)

  ;; Do not resume
  (setq persp-auto-resume-time 0)

  (use-package helm
    :commands my/helm-persp-switch
    :config
    (defvar my/helm-source-perspectives
      (helm-build-sync-source "Perspectives"
        :candidates #'persp-names
        :action #'persp-switch
        :fuzzy-match t))

    (defvar my/helm-source-perspectives-not-found
      (helm-build-dummy-source "Create perspective"
        :action #'persp-switch))

    (defun my/helm-persp-switch ()
      (interactive)
      (helm :sources '(my/helm-source-perspectives
                       my/helm-source-perspectives-not-found))))

  (use-package projectile
    :config
    (defun my/projectile-persp-switch-project (project-to-switch)
      (interactive (list (projectile-completing-read "Switch to project: "
                                                     (projectile-relevant-known-projects))))
      (let* ((name (file-name-nondirectory (directory-file-name project-to-switch)))
             (persp (gethash name *persp-hash*)))
        (cond
         ((and persp (not (equal persp (get-frame-persp))))
          (persp-switch name))

         ((not persp)
          (let ((frame (selected-frame)))
            (persp-switch name)
            (projectile-switch-project-by-name project-to-switch))))))

    (bind-key "C-c p P" 'my/projectile-persp-switch-project projectile-mode-map))

  (bind-keys :map persp-key-map
             ("s" . my/projectile-persp-switch-project)
             ("d" . persp-remove-buffer)
             ("k" . persp-kill)
             ("c" . persp-switch)
             ("b" . my/helm-persp-switch))
  (bind-key "C-c p P" 'my/projectile-persp-switch-project))

;;; setup-perspective.el ends here
