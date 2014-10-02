;;; utils.el --- utility functions

;;; Commentary:

;;; Code:

(defun my/duplicate-region (num &optional start end)
  "Duplicates the region bounded by START and END NUM times.
If no START and END is provided, the current region-beginning and
region-end is used. Adds the duplicated text to the kill ring."
  (interactive "p")
  (let* ((start (or start (region-beginning)))
         (end (or end (region-end)))
         (region (buffer-substring start end)))
    (kill-ring-save start end)
    (goto-char end)
    (dotimes (i num)
      (insert region))))

(defun my/duplicate-current-line (num)
  "Duplicate the current line NUM times."
  (interactive "p")
  (my/duplicate-region num (point-at-bol) (1+ (point-at-eol)))
  (goto-char (1- (point))))

(defun my/duplicate-current-line-or-region (arg)
  "Duplicates the current line or region ARG times.
If there's no region, the current line will be duplicated."
  (interactive "p")
  (if (region-active-p)
      (my/duplicate-region arg)
    (my/duplicate-current-line arg)))

;;; utils.el ends here
