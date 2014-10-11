;;; setup-org-mode.el --- org-mode settings

;;; Commentary:

;;; Code:

(use-package org
  :defer t
  :config
  (progn
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
    (setq org-use-sub-superscripts '{})

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

    ;; org-babel
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((emacs-lisp . t)
       (ruby       . t)
       (clojure    . t)
       (js         . t)
       (ditaa      . t)
       (dot        . t)
       (gnuplot    . t)
       (sh         . t)))

    ;; Exporting
    (use-package ox
      :defer t
      :config
      (progn
        (setq org-export-with-sub-superscripts '{})

        ;; LaTeX
        (use-package ox-latex
          :defer t
          :config
          (progn
            (setq org-latex-pdf-process
                  '("ptex2pdf -l -ot '-synctex=1 -shell-escape' %f"
                    "ptex2pdf -l -ot '-synctex=1 -shell-escape' %f"
                    "ptex2pdf -l -ot '-synctex=1 -shell-escape' %f"))

            (add-to-list 'org-latex-classes
                         '("jsarticle" "\\documentclass{jsarticle}"
                           ("\\section{%s}" . "\\section*{%s}")
                           ("\\subsection{%s}" . "\\subsection*{%s}")
                           ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                           ("\\paragraph{%s}" . "\\paragraph*{%s}")
                           ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
            (add-to-list 'org-latex-classes
                         '("jsreport" "\\documentclass[report]{jsbook}"
                           ("\\part{%s}" . "\\part*{%s}")
                           ("\\chapter{%s}" . "\\chapter*{%s}")
                           ("\\section{%s}" . "\\section*{%s}")
                           ("\\subsection{%s}" . "\\subsection*{%s}")
                           ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))
            (add-to-list 'org-latex-classes
                         '("jsbook" "\\documentclass{jsbook}"
                           ("\\part{%s}" . "\\part*{%s}")
                           ("\\chapter{%s}" . "\\chapter*{%s}")
                           ("\\section{%s}" . "\\section*{%s}")
                           ("\\subsection{%s}" . "\\subsection*{%s}")
                           ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))

            (setq org-latex-default-class "jsarticle")
            (setq org-latex-packages-alist `(("" ,(expand-file-name (locate-user-emacs-file "misc/latex/common")))))

            (setq org-latex-listings 'minted)
            (setq org-latex-minted-options '(("mathescape" "")
                                             ;; ("linenos" "")
                                             ("numbersep" "5pt")
                                             ("bgcolor" "minted_bg")
                                             ;; ("frame" "lines")
                                             ("fontsize" "\\scriptsize")
                                             ("fontfamily" "courier")
                                             ("framesep" "2mm")))))

        ;; TODO: More backends
        ))))

;;; setup-org-mode.el ends here
