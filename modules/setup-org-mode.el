;;; setup-org-mode.el --- org-mode settings

;;; Commentary:

;;; Code:

(use-package org
  :config (progn
            (setq org-directory "~/Dropbox/org")
            (setq org-default-notes-file (concat org-directory "/notes.org"))
            (setq org-agenda-files (list org-directory))
            (setq org-startup-folded nil)
            (setq org-return-follows-link t)
            (setq org-log-done 'time)
            (setq org-mobile-inbox-for-pull "~/Dropbox/memo/flagged.org")
            (setq org-mobile-directory "~/Dropbox/MobileOrg")

            (bind-keys
             ("C-c l" . org-store-link)
             ("C-c a" . org-agenda)
             ("C-c r" . org-capture))

            (when my/use-ergonomic-key-bindings
              (bind-keys :map org-mode-map
                         ("C-k" . nil)
                         ("C-e" . nil)
                         ("C-j" . nil)
                         ("C-a" . nil)
                         ("C-h" . org-beginning-of-line)
                         ("C-;" . org-end-of-line)
                         ("C-s-l" . org-metaright)
                         ("C-s-j" . org-metaleft)
                         ("C-S-s-l" . org-shiftmetaright)
                         ("C-S-s-j" . org-shiftmetaleft)
                         ("C-m" . org-return-indent)))

            ;; TODO: Export settings
            ))

;;; setup-org-mode.el ends here
