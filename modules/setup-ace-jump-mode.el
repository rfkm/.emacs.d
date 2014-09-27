;;; setup-ace-jump-mode.el --- ace-jump-mode config.

;;; Commentary:
;; Configuration for ace-jump-mode.

;;; Code:
(eval-when-compile (require 'cl))

(defun my/add-keys-to-ace-jump-mode (prefix c &optional mode)
  (define-key global-map
    (read-kbd-macro (concat prefix (string c)))
    `(lambda ()
       (interactive)
       (funcall (if (eq ',mode 'word)
                    #'ace-jump-word-mode
                  #'ace-jump-char-mode) ,c))))

(loop for c from ?! to ?~ do (my/add-keys-to-ace-jump-mode "H-" c))
(loop for c from ?! to ?~ do (my/add-keys-to-ace-jump-mode "H-M-" c 'word))

;;; setup-ace-jump-mode.el ends here
