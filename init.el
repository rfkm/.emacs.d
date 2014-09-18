;;; init.el --- My emacs settings

;;; Commentary:

;;; Code:

;; lootstrap
(load (concat (file-name-directory load-file-name) "bootstrap"))

;; load module loader
(load (concat user-emacs-directory "loader"))

;; define modules to load
(setq my/modules `())

;; load modules
(my/load-modules)

;;; init.el ends here
