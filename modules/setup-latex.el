;;; setup-latex.el --- latex settings

;;; Commentary:

;;; Code:

(use-package yatex
  :mode (("\\.tex$" . yatex-mode)
         ("\\.ltx$" . yatex-mode)
         ("\\.cls$" . yatex-mode)
         ("\\.sty$" . yatex-mode)
         ("\\.clo$" . yatex-mode)
         ("\\.bbl$" . yatex-mode))
  :init (progn
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
          (setq tex-command "/usr/texbin/ptex2pdf -l -ot '-synctex=1 -shell-escape'")
          (setq bibtex-command (cond ((string-match "uplatex\\|-u" tex-command) "/usr/texbin/upbibtex")
                                     ((string-match "platex" tex-command) "/usr/texbin/pbibtex")
                                     ((string-match "lualatex\\|luajitlatex\\|xelatex" tex-command) "/usr/texbin/bibtexu")
                                     ((string-match "pdflatex\\|latex" tex-command) "/usr/texbin/bibtex")
                                     (t "/usr/texbin/pbibtex")))
          (setq makeindex-command (cond ((string-match "uplatex\\|-u" tex-command) "/usr/texbin/mendex")
                                        ((string-match "platex" tex-command) "/usr/texbin/mendex")
                                        ((string-match "lualatex\\|luajitlatex\\|xelatex" tex-command) "/usr/texbin/texindy"
                                         ((string-match "pdflatex\\|latex" tex-command) "/usr/texbin/makeindex")
                                         (t "/usr/texbin/mendex"))))
          (setq dvi2-command "/usr/bin/open -a Skim")
          (setq dviprint-command-format "/usr/bin/open -a \"Adobe Reader\" `echo %s | sed -e \"s/\\.[^.]*$/\\.pdf/\"`")
          (defun skim-forward-search ()
            (interactive)
            (progn
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
                buffer-file-name))))
          (add-hook 'yatex-mode-hook
                    '(lambda ()
                       (define-key YaTeX-mode-map (kbd "C-c s") 'skim-forward-search)))
          (add-hook 'yatex-mode-hook
                    '(lambda ()
                       (auto-fill-mode -1)))))

;;; setup-latex.el ends here
