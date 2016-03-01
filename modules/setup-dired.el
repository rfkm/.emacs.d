;;; setup-dired.el --- dired settings

;;; Commentary:

;;; Code:

(defun my/dired-do-quicklook ()
  "Preview with Quick Look."
  (interactive)
  (let* ((files (if current-prefix-arg
                    (list (dired-get-filename t t))
                  (dired-get-marked-files t)))
         (current-line-file (dired-get-filename t t))
         (ht (-split-on current-line-file files))
         (files (append (list current-line-file) (cadr ht) (car ht)))
         (process (get-process "qlmanage_ps")))
    (when process
      (kill-process process))
    (apply 'start-process "qlmanage_ps" nil "qlmanage" "-px" files)))

(defun my/dired-do-zip (filelist output-filename)
  "Compress marked files with zip. With prefix arg, create
achived files for each files."
  (interactive
   (list (dired-get-marked-files t)
         (let ((default-output-file-name
                 (concat default-directory (car (dired-get-marked-files t)) ".zip")))
           (read-file-name (format "filename[%s]:" default-output-file-name) nil default-output-file-name))))
  (unless (string-match "\\.zip$" output-filename)
    (setq output-filename
          (concat output-filename ".zip")))
  (save-window-excursion
    (message "processing... : %S" output-filename)
    (or (when (string-empty-p (file-name-nondirectory output-filename))
          (message "ERROR: Output filename is empty."))
        (when (file-exists-p output-filename)
          (not (y-or-n-p (concat "file " output-filename " exists. overwrite the file ?"))))
        (cond ((and current-prefix-arg (> (length filelist) 1))
               (dolist (filename filelist)
                 (my/dired-do-zip (list filename) (concat default-directory filename ".zip"))))
              (t
               (dired-do-shell-command (concat "zip -r " (shell-quote-argument (expand-file-name output-filename)) " *") nil filelist)
               (revert-buffer)
               (message "COMPLETE"))))))

(defun my/dired-mark-region (start end &optional arg)
  "Mark all files in region.  With prefix argument, unflag all
those files."
  (interactive "r\nP")
  (let ((dired-marker-char (if arg ?\040 ?*))) ; \040 = SPC
    (dired-mark-files-in-region
     (save-excursion
       (goto-char start)
       (line-beginning-position))
     end)))

(defun my/dired-back-to-start-of-files ()
  (interactive)
  (let ((current-point (point)))
    (dired-move-to-filename)
    (when (eq current-point (point))
      (goto-char (point-at-bol)))))

(defun my/dired-mode-hook ()
  (bind-keys :map dired-mode-map
             ([remap move-beginning-of-line] . my/dired-back-to-start-of-files)
             ("C-c C-z" . my/dired-do-zip)
             ("* r" . my/dired-mark-region))
  (when mac?
    (bind-keys :map dired-mode-map
               ("SPC" . my/dired-do-quicklook))))

(add-hook 'dired-mode-hook 'my/dired-mode-hook)

;; wdired-mode
(use-package wdired
  :defer t
  :config
  (defun my/wdired-mode-hook ()
    (when my/use-ergonomic-key-bindings
      (bind-keys :map wdired-mode-map
                 ("C-j" . nil)
                 ("C-o" . nil)
                 ("C-n" . nil)
                 ("C-p" . nil))))
  (add-hook 'wdired-mode-hook 'my/wdired-mode-hook))

;;; setup-dired.el ends here
