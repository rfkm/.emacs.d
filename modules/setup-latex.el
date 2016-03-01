;;; setup-latex.el --- latex settings

;;; Commentary:

;;; Code:

;; TODO: platform agnostic settings
(use-package yatex
  :mode (("\\.tex$" . yatex-mode)
         ("\\.ltx$" . yatex-mode)
         ("\\.cls$" . yatex-mode)
         ("\\.sty$" . yatex-mode)
         ("\\.clo$" . yatex-mode)
         ("\\.bbl$" . yatex-mode))
  :init
  (setq auto-mode-alist
        (append '(("\\.tex$" . yatex-mode)
                  ("\\.ltx$" . yatex-mode)
                  ("\\.cls$" . yatex-mode)
                  ("\\.sty$" . yatex-mode)
                  ("\\.clo$" . yatex-mode)
                  ("\\.bbl$" . yatex-mode)) auto-mode-alist))
  (setq tex-start-of-header (concat "^\\s *" "\\\\" "document\\(style\\|class\\)"))
  (setq YaTeX-inhibit-prefix-letter t)
  (setq YaTeX-kanji-code nil)
  (setq YaTeX-use-LaTeX2e t)
  (setq YaTeX-use-AMS-LaTeX t)
  (setq YaTeX-hilit-sectioning-face '(dodgerblue/yellow slateblue/yellow))
  (setq YaTeX-dvi2-command-ext-alist
        '(("Preview\\|TeXShop\\|TeXworks\\|Skim\\|mupdf\\|xpdf\\|Firefox\\|Adobe" . ".pdf")))
  (setq tex-command "ptex2pdf -l -ot '-synctex=1 -shell-escape'")
  (setq bibtex-command (cond ((string-match "uplatex\\|-u" tex-command) "upbibtex")
                             ((string-match "platex" tex-command) "pbibtex")
                             ((string-match "lualatex\\|luajitlatex\\|xelatex" tex-command) "bibtexu")
                             ((string-match "pdflatex\\|latex" tex-command) "bibtex")
                             (t "pbibtex")))
  (setq makeindex-command (cond ((string-match "uplatex\\|-u" tex-command) "mendex")
                                ((string-match "platex" tex-command) "mendex")
                                ((string-match "lualatex\\|luajitlatex\\|xelatex" tex-command) "texindy"
                                 ((string-match "pdflatex\\|latex" tex-command) "makeindex")
                                 (t "mendex"))))
  (setq dvi2-command "/usr/bin/open -a Skim")
  (setq dviprint-command-format "/usr/bin/open -a \"Adobe Reader\" `echo %s | sed -e \"s/\\.[^.]*$/\\.pdf/\"`")
  (defun skim-forward-search ()
    (interactive)
    (process-kill-without-query
     (start-process
      "displayline"
      nil
      "/Applications/Skim.app/Contents/SharedSupport/displayline"
      (number-to-string (save-restriction
                          (widen)
                          (count-lines (point-min) (point))))
      (expand-file-name
       (concat (file-name-sans-extension (or YaTeX-parent-file
                                             (save-excursion
                                               (YaTeX-visit-main t)
                                               buffer-file-name)))
               ".pdf"))
      buffer-file-name)))
  (add-hook 'yatex-mode-hook
            '(lambda ()
               (define-key YaTeX-mode-map (kbd "C-c s") 'skim-forward-search)))
  (add-hook 'yatex-mode-hook
            '(lambda ()
               (auto-fill-mode -1))))

;;; setup-latex.el ends here
