;;; setup-osx.el --- for Mac

;;; Commentary:

;;; Code:

;; Modifier keys
(setq ns-command-modifier (quote meta))
(setq ns-right-command-modifier (quote hyper))
(setq ns-alternate-modifier (quote super))
(setq ns-right-alternate-modifier (quote alt))

;; for emacs-mac-port
(setq mac-command-modifier (quote meta))
(setq mac-right-command-modifier (quote hyper))
(setq mac-option-modifier (quote super))
(setq mac-right-alternate-modifier (quote alt))

(setq ns-use-srgb-colorspace t)

;; Don't open new frame
(setq ns-pop-up-frames nil)

;; Don't use native fullscreen
(setq ns-use-native-fullscreen nil)

(when (eq window-system 'mac)
  (defun mac-toggle-frame-fullscreen ()
    (interactive)
    (let* ((cur (frame-parameter (selected-frame) 'fullscreen))
           (new-param (if (eq cur 'fullboth) nil 'fullboth)))
      (set-frame-parameter (selected-frame) 'fullscreen new-param))))


(bind-key "M-<f10>" (-first #'fboundp '(mac-toggle-frame-fullscreen
                                        toggle-frame-fullscreen
                                        ns-toggle-fullscreen)))

;; Use GNU's ls
(when (executable-find "gls")
  (setq insert-directory-program "gls"))

;; Use trash can
(setq delete-by-moving-to-trash t
      trash-directory "~/.Trash")

(defun my/dired-mode-trash-hook ()
  "Determine the location of trash can according to current directory."
  (set (make-local-variable 'trash-directory)
       (let ((path (expand-file-name default-directory)))
         (if (string-match "^/Volumes/\\([^/\n]+\\)" path)
             (concat "/Volumes/" (match-string 1 path) "/.Trashes/" (number-to-string (user-real-uid)))
           "~/.Trash"))))
(add-hook 'dired-after-readin-hook 'my/dired-mode-trash-hook)

;; AppleScript
(defun my/chrome-reload ()
  "Reload Google Chrome."
  (interactive)
  (shell-command (concat "osascript "  (locate-user-emacs-file "misc/applescripts/chrome.scpt reload"))))
(bind-key "s-r" 'my/chrome-reload)

;; Enable clipboard sharing on CUI Emacs
(unless window-system
  (defvar my/prev-yanked-text nil "*previous yanked text")
  (setq interprogram-cut-function
        (lambda (text &optional push)
          (let ((process-connection-type nil))
            (let ((proc (start-process "pbcopy" nil "pbcopy")))
              (process-send-string proc text)
              (process-send-eof proc)
              ))))

  (setq interprogram-paste-function
        (lambda ()
          (let ((text (shell-command-to-string "pbpaste")))
            (if (string= my/prev-yanked-text text)
                nil
              (setq my/prev-yanked-text text))))))

;;; setup-mac.el ends here
