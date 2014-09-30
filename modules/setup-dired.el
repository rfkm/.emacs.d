;;; setup-dired.el --- dired settings

;;; Commentary:

;;; Code:

(defun my/dired-do-quicklook (files)
  "preview with Quick Look."
  (interactive
   (list (setq files (if current-prefix-arg
                         (list (dired-get-filename t t))
                       (dired-get-marked-files t)))))
  (let* ((current-line-file (dired-get-filename t t))
         (former-list (member current-line-file files))
         (process (get-process "qlmanage_ps")))
    (when process
      (kill-process process))
    (when former-list
      (dolist (ele former-list)
        (delete ele files))
      (setq files (append former-list files))
      )
    (apply 'start-process "qlmanage_ps" nil "qlmanage" "-px" files)))

(defun my/dired-do-tar-cvzf (output-filename arg)
  "マークしたファイルを tar cvzf でまとめて圧縮する."
  (interactive "FOUTPUT FILENAME: \nP")
  (or (when (string= "" (file-name-nondirectory output-filename))
        ;; 出力ファイル名が入力されていなかったらエラーとする.
        (message "ERROR: Output filename is empty."))
      (when (file-exists-p
             (setq output-filename
                   (concat output-filename (unless (string-match "\\.t\\(ar\\.\\)?gz$" output-filename) ".tar.gz"))))
        ;; 出力ファイル名と同じ名前のファイルが, 既にあった場合, 上書きするか問い合わせる.
        (not (y-or-n-p (concat "file " output-filename " exists. overwrite the file ?"))))
      ;; 実際に圧縮をする.
      (let (add-file)
        (when arg
          (while (y-or-n-p "ADD FILE?")
            (add-to-list 'add-file (file-relative-name (read-file-name "" nil nil t)))))
        (dolist (ppp (dired-get-marked-files t))
          (dired-do-shell-command (concat "tar cvzf " output-filename ppp ".tar.gz" " *") nil
                                  (list ppp))))))

(defun my/dired-do-zip (filelist output-filename)
  ;; (interactive "FOUTPUT FILENAME: \nP")
  "マークしたファイルをzipでまとめて圧縮する.前置引数を与えた場合は、各ファイルごとに圧縮ファイルを作成する."
  (interactive
   (list (setq filelist (dired-get-marked-files t))
         (let ((default-output-file-name
                 (concat default-directory (car filelist) ".zip")))
           (read-file-name (format "filename[%s]:" default-output-file-name) nil default-output-file-name))))
  (save-window-excursion
    (message "processing... : %S" output-filename)
    (or (when (string= "" (file-name-nondirectory output-filename))
          (message "ERROR: Output filename is empty."))
        (when (file-exists-p (setq output-filename
                                   (concat output-filename (unless (string-match "\\.zip$" output-filename) ".zip"))))
          (not (y-or-n-p (concat "file " output-filename " exists. overwrite the file ?"))))
        (cond ((and current-prefix-arg (> (length filelist) 1))
               (dolist (filename filelist)
                 (my/dired-do-zip (list filename) (concat default-directory filename ".zip"))))
              (t
               (dired-do-shell-command (concat "zip -r " (shell-quote-argument output-filename) " *") nil filelist)
               (revert-buffer)
               (message "COMPLETE"))))))

(defun my/dired-do-unarchive-with-password (filelist password)
  "マークの付いたパスワード付き書庫を解凍する."
  (interactive
   (list (setq filelist (dired-get-marked-files t))
         (read-passwd "archive password: " nil)))

  (message "%S" (list filelist password))
  (save-window-excursion
    (let (commandlist firstfile)
      (dolist (filename filelist)
        (message "processing...: %S" filename)
        (let ((command nil))
          (cond ((string-match "\\.zip$" filename)
                 (setq command
                       (concat
                        "unzip "
                        (when (and password (not (string= "" password)))
                          (concat "-P " password " "))
                        (if commandlist
                            (shell-quote-argument filename)
                          "*")
                        (when current-prefix-arg
                          (concat " -d " (file-name-sans-extension (shell-quote-argument filename)) "/"))
                        (format " && growlnotify -a 'Emacs' -m 'done - %s' || growlnotify -a 'Emacs' -m 'failed - %s'" filename filename))))
                ((string-match "^\\(?:.+\\.part0*1\\|[^.]+\\)\\.rar$" filename)
                 (setq command
                       (concat
                        "unrar x "
                        (when (and password (not (string= "" password)))
                          (concat "-p" password " "))
                        (if commandlist
                            (shell-quote-argument filename)
                          "*")
                        (when current-prefix-arg
                          (concat " " (file-name-sans-extension (shell-quote-argument filename)) "/"))
                        (format " && growlnotify -a 'Emacs' -m 'done - %s' || growlnotify -a 'Emacs' -m 'failed - %s'" filename filename)))))
          (message "filename: %S  command: %S" filename command)
          (when command
            (when (null commandlist) (setq firstfile filename))
            (add-to-list 'commandlist command)
            )))
      (when commandlist
        (message "%s" (mapconcat 'identity commandlist ";"))
        (dired-do-async-shell-command (mapconcat 'identity commandlist ";") nil (list firstfile))))))

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
             ("C-c C-u" . my/dired-do-unarchive-with-password)
             ("* r" . my/dired-mark-region))
  (when mac?
    (bind-keys :map dired-mode-map
               ("SPC" . my/dired-do-quicklook))))

(add-hook 'dired-mode-hook 'my/dired-mode-hook)

;; wdired-mode
(use-package wdired
  :defer t
  :config (progn
            (defun my/wdired-mode-hook ()
              (bind-keys :map wdired-mode-map
                         ("C-j" . nil)
                         ("C-o" . nil)
                         ("C-n" . nil)
                         ("C-p" . nil)))
            (add-hook 'wdired-mode-hook 'my/wdired-mode-hook)))

;;; setup-dired.el ends here
