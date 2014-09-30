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


'(eval-after-load-compile 'php-mode
                          (defun my/php-mode-hook ()
                           ;; gtags
                           (gtags-mode 1)

                           ;; autocomplete
                           (make-local-variable 'ac-sources)
                           (setq ac-sources '(
                                              ac-source-dictionary
                                              ac-source-words-in-same-mode-buffers
                                              ac-source-yasnippet
                                              ac-source-gtags
                                              ac-source-filename
                                              ))
                           (auto-complete-mode 1)
                           ;; reduce font lock keywords
                           (make-local-variable 'font-lock-keywords)
                           (setq font-lock-keywords php-font-lock-keywords-2)
                           (c-set-style "user")
                           (c-toggle-auto-hungry-state t)
                           (c-set-offset 'case-label' 4)
                           (c-set-offset 'arglist-intro' 4)
                           (c-set-offset 'arglist-cont-nonempty' 4)
                           (c-set-offset 'arglist-close' 0)
                           (setq tab-width 4
                                 c-basic-offset 4
                                 c-hanging-comment-ender-p nil
                                 indent-tabs-mode nil)
                           (setq c-hanging-braces-alist
                                 '(
                                   (class-open nil)
                                   (class-close nil)
                                   (defun-open before after)
                                   (defun-close nil)
                                   (inline-open nil)
                                   (inline-close nil)
                                   (brace-list-open nil)
                                   (brace-list-close nil)
                                   (block-open nil)
                                   (block-close nil)
                                   (substatement-open before after)
                                   (statement-case-open before after)
                                   (extern-lang-open nil)
                                   (extern-lang-close nil)
                                   )))
                         (add-hook 'php-mode-hook 'my/php-mode-hook)

                         (define-key php-mode-map (kbd "C-d") 'kill-whole-line)
                         ;; (define-key php-mode-map "\C-m" 'newline-and-indent)
                         ;; (define-key php-mode-map "\C-\M-j" 'backward-word)



                         )


;; ;; CakePHP2
;; (bundle k1LoW/emacs-cake2
;;         (eval-after-load-compile 'k1LoW/emacs-cake2
;;                                  (global-cake2 t)
;;                                  (cake2-set-default-keymap)
;;                                  (define-key cake2-key-map (kbd "C-u C-c V") nil)
;;                                  (define-key cake2-key-map (kbd "C-u C-c") nil)
;;                                  (define-key cake2-key-map (kbd "C-u") nil)
;;                                  (define-key cake2-key-map (kbd "C-u") 'delete-backward-char)
;;                                  '(add-hook 'cake2-hook
;;                                             #'(lambda ()
;;                                                 (setq yas-mode-symbol 'cake2)))))

;;; setup-php.el ends here
