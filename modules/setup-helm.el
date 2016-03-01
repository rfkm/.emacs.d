;;; setup-helm.el --- helm settings

;;; Commentary:

;;; Code:

(use-package helm
  :init (require 'helm-config)
  :bind (("s-a" . helm-mini)
         ("C-x b" . helm-buffers-list)
         ("C-x r b" . helm-bookmarks)
         ("M-x" . helm-M-x)
         ("M-y" . helm-show-kill-ring))
  :config (progn
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

            (helm-descbinds-mode 1)))

(use-package helm-ag
  :config (progn
            (setq helm-ag-insert-at-point 'symbol)))

;;; setup-helm.el ends here
