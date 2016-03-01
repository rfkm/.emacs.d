;;; setup-helm.el --- helm settings

;;; Commentary:

;;; Code:

(use-package helm
  :init (require 'helm-config)
  :diminish helm-migemo-mode
  :bind (("s-a" . helm-mini)
         ("C-x b" . helm-buffers-list)
         ("C-x r b" . helm-bookmarks)
         ("M-x" . helm-M-x)
         ("M-y" . helm-show-kill-ring))
  :config
  (when my/use-ergonomic-key-bindings
    (bind-keys :map helm-map
               ("C-k" . helm-next-line)
               ("A-i" . helm-previous-line)
               ("C-d" . helm-delete-minibuffer-contents)
               ("C-n" . helm-next-page)
               ("C-p" . helm-previous-page)
               ("C-l" . helm-next-source)
               ("C-j" . helm-previous-source)))

  (setq helm-quick-update t
        helm-google-suggest-use-curl-p t
        helm-google-suggest-search-url
        "http://www.google.co.jp/search?hl=ja&num=100&as_qdr=y5&ie=utf-8&oe=utf-8&q="
        helm-google-suggest-url
        "http://google.co.jp/complete/search?ie=utf-8&oe=utf-8&hl=ja&output=toolbar&q="
        helm-buffers-fuzzy-matching t
        helm-ff-transformer-show-only-basename nil)

  (helm-migemo-mode 1)
  (helm-descbinds-mode 1))

(use-package helm-ag
  :config (progn
            (setq helm-ag-insert-at-point 'symbol)))

(use-package helm-swoop
  :bind (("M-i" . helm-swoop)
         ("M-I" . helm-swoop-back-to-last-point)
         ("C-c M-i" . helm-multi-swoop)
         ("C-x M-i" . helm-multi-swoop-all))

  :config
  (setq helm-swoop-speed-or-color t)
  (setq helm-swoop-move-to-line-cycle t)
  (bind-keys :map helm-swoop-map
             ("M-i" . helm-multi-swoop-all-from-helm-swoop)
             ("M-m" . helm-multi-swoop-current-mode-from-helm-swoop)))

;;; setup-helm.el ends here
