;;; setup-helm.el --- helm settings

;;; Commentary:

;;; Code:
(require 'helm)
(require 'helm-grep)
(require 'helm-files)
(require 'helm-descbinds)
(require 'helm-config)

(bind-keys ("s-a" . helm-mini)
           ("s-g" . helm-google-suggest)
           ("C-x b" . helm-buffers-list)
           ("C-x r b" . helm-bookmarks)
           ("M-x" . helm-M-x)
           ("M-y" . helm-show-kill-ring))

(bind-keys :map helm-map
           ("C-k" . helm-next-line)
           ("A-i" . helm-previous-line)
           ("C-d" . helm-delete-minibuffer-contents)
           ("C-n" . helm-next-page)
           ("C-p" . helm-previous-page)
           ("C-l" . helm-next-source)
           ("C-j" . helm-previous-source))

(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))

(setq helm-google-suggest-search-url
      "http://www.google.co.jp/search?hl=ja&num=100&as_qdr=y5&ie=utf-8&oe=utf-8&q="
      helm-google-suggest-url
      "http://google.co.jp/complete/search?ie=utf-8&oe=utf-8&hl=ja&output=toolbar&q="
      helm-quick-update t
      helem-buffers-fuzzy-matching t
      helm-ff-transformer-show-only-basename nil)

(helm-descbinds-mode)

;;; setup-helm.el ends here
