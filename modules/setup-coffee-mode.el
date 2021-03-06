;;; setup-coffee-mode.el --- coffee-mode settings

;;; Commentary:

;;; Code:

(use-package coffee-mode
  :defer t
  :config
  (bind-keys :map coffee-mode-map
             ("M-r" . coffee-compile-buffer))

  (defun my/coffee-mode-hook ()
    ;; I'm so sorry but I prefer hard-tab in CoffeeScript.
    ;; See: http://stackoverflow.com/questions/2316677/can-git-automatically-switch-between-spaces-and-tabs
    (setq-local indent-tabs-mode t)
    (setq-local coffee-indent-tabs-mode t)
    (setq-local tab-width 4)
    (setq-local coffee-tab-width 4)

    (setq coffee-args-compile '("-bc"))
    (setq coffee-debug-mode t)

    (and (file-exists-p (buffer-file-name))
         (file-exists-p (coffee-compiled-file-name))
         (coffee-cos-mode t)))

  (add-hook 'coffee-mode-hook 'my/coffee-mode-hook))

;;; setup-coffee-mode.el ends here
