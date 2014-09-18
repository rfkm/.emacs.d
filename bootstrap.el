;;; bootstrap.el

(when load-file-name
  (setq user-emacs-directory (expand-file-name
                              (file-name-directory load-file-name))))

;; Cask
(let* ((brewed-suffix "echo $(brew --prefix)/share/emacs/site-lisp/cask.el")
       (brewed-path (and (executable-find "brew")
                         (replace-regexp-in-string (rx (* (any " \t\n")) eos)
                                                   ""
                                                   (shell-command-to-string brewed-suffix))))
       (pred (lambda (x) (and (file-exists-p x) x)))
       (cask-path (car (delq nil (mapcar pred `("~/.cask/cask.el"
                                                  ,brewed-path))))))
  (if cask-path
      (progn (require 'cask cask-path)
             (cask-initialize))
    (error "Can't find cask.el")))

;; Turn off mouse interface early in startup to avoid momentary display
(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
  (when (fboundp mode) (funcall mode -1)))

;; Set Emacs' `exec-path' and $PATH from the shell path
(setq exec-path-from-shell-variables '("PATH" "MANPATH" "GOPATH"))
(exec-path-from-shell-initialize)

;; Add external projects to load path
(add-to-list 'load-path (expand-file-name "site-lisp" user-emacs-directory))
(defun update-load-path ()
  (interactive)
  (dolist (project (directory-files (expand-file-name "site-lisp" user-emacs-directory) t "\\w+"))
   (when (file-directory-p project)
     (add-to-list 'load-path project))))
(update-load-path)

;;; bootstrap.el ends here
