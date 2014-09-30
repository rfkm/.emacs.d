;;; setup-php.el --- php config.

;;; Commentary:

;; Configuration for php-mode.

;;; Code:

;; (bundle php-eldoc)
;; (bundle elpa:geben)
;; (bundle jinja2-mode)
;; (add-to-list 'auto-mode-alist '("\\.php$" . php-mode))
;; (add-to-list 'auto-mode-alist '("\\.ctp$" . sgml-mode))

;; Use jinja2-mode as tiwg-mode.
;; (add-to-list 'auto-mode-alist '("\\.twig$" . jinja2-mode))


(use-package php-mode
  :defer t
  :config (progn
          (setq php-template-compatibility nil)
          (setq php-mode-coding-style 'symfony2)
          (bind-keys :map php-mode-map
                     ("C-d" . kill-whole-line))
          (defun my/php-mode-hook ()
            ;; gtags
            (ggtags-mode 1)

            ;; autocomplete
            (make-local-variable 'ac-sources)
            (setq ac-sources '(
                               ac-source-dictionary
                               ac-source-words-in-same-mode-buffers
                               ac-source-yasnippet
                               ac-source-gtags
                               ac-source-filename
                               ))
            (auto-complete-mode 1))
          (add-hook 'php-mode-hook 'my/php-mode-hook)))

(defun my/guess-php-namespace ()
  (->> default-directory
    f-split
    (--drop-while (not (equal it "src")))
    rest
    (s-join "\\")))

(defun my/guess-php-class-name ()
  (f-no-ext (f-filename (buffer-file-name))))

(defun my/guess-sf2-service-name ()
  (->> (buffer-file-name)
    f-split
    (--drop-while (not (equal it "src")))
    rest
    (s-join "")
    f-no-ext
    s-snake-case))

;;; setup-php.el ends here
