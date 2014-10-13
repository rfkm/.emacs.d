;;; setup-ruby-mode.el --- ruby-mode settings

;;; Commentary:

;;; Code:

(use-package ruby-mode
  :mode (("\\.rb\\'"       . ruby-mode)
         ("\\.rake\\'"     . ruby-mode)
         ("Rakefile\\'"    . ruby-mode)
         ("\\.gemspec\\'"  . ruby-mode)
         ("\\.ru\\'"       . ruby-mode)
         ("Gemfile\\'"     . ruby-mode)
         ("Guardfile\\'"   . ruby-mode)
         ("Capfile\\'"     . ruby-mode)
         ("\\.thor\\'"     . ruby-mode)
         ("\\.rabl\\'"     . ruby-mode)
         ("Thorfile\\'"    . ruby-mode)
         ("Vagrantfile\\'" . ruby-mode)
         ("\\.jbuilder\\'" . ruby-mode)
         ("Podfile\\'"     . ruby-mode)
         ("\\.podspec\\'"  . ruby-mode)
         ("Puppetfile\\'"  . ruby-mode)
         ("Berksfile\\'"   . ruby-mode)
         ("Appraisals\\'"  . ruby-mode))
  :interpreter (("ruby" . ruby-mode))
  :config (progn
            (use-package ruby-end
              :init (setq ruby-end-insert-newline nil))
            (use-package ruby-block)
            (use-package inf-ruby)
            (use-package ruby-tools
              :config (when my/use-ergonomic-key-bindings
                        (bind-keys :map ruby-tools-mode-map
                                   ("C-;" . nil))))

            (defun my/ruby-mode-hook ()
              (add-hook 'before-save-hook 'my/cleanup-buffer nil t)
              (ruby-block-mode +1)
              (inf-ruby-minor-mode +1)
              (ruby-tools-mode +1))

            (add-hook 'ruby-mode-hook 'my/ruby-mode-hook)))

;;; setup-ruby-mode.el ends here
