;;; setup-org-mode.el --- org-mode settings

;;; Commentary:

;;; Code:

(use-package org
  :defer t
  :config (progn
            (message "Configuring org-mode...")
            (setq org-directory "~/Dropbox/org")
            (setq org-default-notes-file (concat org-directory "/notes.org"))
            (setq org-agenda-files (list org-directory))
            (setq org-startup-folded nil)
            (setq org-return-follows-link t)
            (setq org-log-done 'time)
            (setq org-mobile-inbox-for-pull "~/Dropbox/memo/flagged.org")
            (setq org-mobile-directory "~/Dropbox/MobileOrg")
            (setq org-imenu-depth 3)

            (bind-keys
             ("C-c l" . org-store-link)
             ("C-c a" . org-agenda)
             ("C-c r" . org-capture))

            (defun my/org-mode-hook ()
              (add-hook 'before-save-hook 'whitespace-cleanup nil t))

            (add-hook 'org-mode-hook 'my/org-mode-hook)


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

            ;; LaTeX
            (setq org-latex-pdf-process '("ptex2pdf -l -ot '-synctex=1 -shell-escape' %f"
                                          "ptex2pdf -l -ot '-synctex=1 -shell-escape' %f"
                                          "ptex2pdf -l -ot '-synctex=1 -shell-escape' %f"))

            (add-to-list 'org-latex-classes '("jsarticle" "\\documentclass[a4paper]{jsarticle}"
                                              ("\\section{%s}" . "\\section*{%s}")
                                              ("\\subsection{%s}" . "\\subsection*{%s}")
                                              ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                                              ("\\paragraph{%s}" . "\\paragraph*{%s}")
                                              ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
            (setq org-latex-default-class "jsarticle")
            (setq org-latex-listings nil) ; TODO: use minted
            ))

;;; setup-org-mode.el ends here
