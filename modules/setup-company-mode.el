;;; setup-company-mode.el --- company-mode settings

;;; Commentary:

;;; Code:

(require 'cl-lib)

(defmacro my/save-mouse-position (&rest body)
  (let ((m (gensym)))
    `(let ((,m (mouse-pixel-position)))
       (prog1 (progn ,@body)
         (set-mouse-pixel-position (selected-frame) (cadr ,m) (cddr ,m))))))

(use-package company
  ;; :init
  ;; (global-company-mode +1)
  :config
  (setq company-selection-wrap-around t)
  (setq company-idle-delay 0.2)
  (setq company-dabbrev-code-everywhere t)
  (setq company-dabbrev-code-other-buffers t)
  (setq company-dabbrev-other-buffers 'all)

  (add-to-list 'company-backends
               '(company-capf :with company-dabbrev
                              :with company-yasnippet))

  (bind-keys :map company-active-map
             ("A-i" . company-select-previous-or-abort)
             ("C-k" . company-select-next-or-abort)
             ("C-s" . company-filter-candidates)
             ("<tab>" . company-complete-selection)
             ("C-h" . nil))
  (bind-keys :map company-search-map
             ("A-i" . company-select-previous)
             ("C-k" . company-select-next))
  (bind-key "C-'" 'company-complete)

  (use-package company-quickhelp
    :init
    (company-quickhelp-mode +1)
    :config
    (setq company-quickhelp-delay 0.1)

    ;; Better multi-monitor support
    (defun my/force-reset-mouse-position-advice (f &rest args)
      (my/save-mouse-position
       (set-mouse-pixel-position (selected-frame) 0 0)
       (apply f args)))

    (advice-add 'pos-tip-show :around 'my/force-reset-mouse-position-advice))

  ;; (use-package helm-company
  ;;   :config
  ;;   (bind-keys :map company-active-map
  ;;              ("C-s" . helm-company)))
  )

;;; setup-company-mode.el ends here
