;;; setup-experiment.el --- experimental settings

;;; Commentary:

;;; Code:

(use-package e2wm
  :bind ("M-+" . e2wm:start-management)
  :config (progn
            (bind-keys :map e2wm:pst-minor-mode-keymap
                       ("<M-left>" . e2wm:dp-code)       ; codeへ変更
                       ("<M-right>"  . e2wm:dp-two)      ; twoへ変更
                       ("<M-up>"    . e2wm:dp-doc)       ; docへ変更
                       ("<M-down>"  . e2wm:dp-dashboard) ; dashboardへ変更
                       ("C-."       . e2wm:pst-history-forward-command) ; 履歴を進む
                       ("C-,"       . e2wm:pst-history-back-command) ; 履歴をもどる
                       ;; ("prefix L"  . ielm)
                       ("M-m"       . e2wm:pst-window-select-main-command))

            (setq e2wm:c-code-recipe
                  '(| (:left-max-size 60)
                      (- (:upper-size-ratio 0.7)
                         files history)
                      (- (:upper-size-ratio 0.7)
                         (| (:right-max-size 60)
                            main imenu)
                         sub)))))


'(bundle e2wm
         (global-set-key (kbd "M-+") 'e2wm:start-management)

         (eval-after-load-compile 'e2wm
                                  (e2wm:add-keymap
                                   e2wm:pst-minor-mode-keymap
                                   '(("<M-left>" . e2wm:dp-code) ; codeへ変更
                                     ("<M-right>"  . e2wm:dp-two)  ; twoへ変更
                                     ("<M-up>"    . e2wm:dp-doc)  ; docへ変更
                                     ("<M-down>"  . e2wm:dp-dashboard) ; dashboardへ変更
                                     ("C-."       . e2wm:pst-history-forward-command) ; 履歴を進む
                                     ("C-,"       . e2wm:pst-history-back-command) ; 履歴をもどる
                                     ("prefix L"  . ielm)
                                     ("M-m"       . e2wm:pst-window-select-main-command)
                                     ) e2wm:prefix-key)
                                  (e2wm:add-keymap
                                   e2wm:dp-two-minor-mode-map
                                   '(("prefix I" . info)
                                     ("C->"       . e2wm:dp-two-right-history-forward-command) ; 右側の履歴を進む
                                     ("C-<"       . e2wm:dp-two-right-history-back-command) ; 右側の履歴を進む
                                     ) e2wm:prefix-key)

                                  ;; レイアウト
                                  ;; (setq e2wm:c-clj-recipe
                                  ;;       '(- (:upper-size-ratio 0.8)
                                  ;;           (| left
                                  ;;              (- (:upper-size-ratio 0.9)
                                  ;;                 right history))
                                  ;;           sub))
                                  ;; for 1440x900以上 (default)
                                  ;; (setq e2wm:c-code-recipe
                                  ;;   '(| (:left-max-size 35)
                                  ;;       (- (:upper-size-ratio 0.7)
                                  ;;          files history)
                                  ;;       (- (:upper-size-ratio 0.7)
                                  ;;          (| (:right-max-size 30)
                                  ;;             main imenu)
                                  ;;          sub)))

                                  ;; (setq e2wm:c-code-winfo
                                  ;;   '((:name main)
                                  ;;     (:name files :plugin files)
                                  ;;     (:name history :plugin history-list)
                                  ;;     (:name sub :buffer "*info*" :default-hide t)
                                  ;;     (:name imenu :plugin imenu :default-hide nil))
                                  ;;   )
                                  (setq e2wm:c-code-recipe
                                        '(| (:left-max-size 35)
                                            (- (:upper-size-ratio 0.7)
                                               files history)
                                            (- (:upper-size-ratio 0.7)
                                               (| (:left-size-ratio 0.7)
                                                  main
                                                  (- nrepl result))
                                               sub)))

                                  ;; (setq e2wm:c-clj-recipe
                                  ;;       '(- (:upper-size-ratio 0.8)
                                  ;;           (| )
                                  ;;           sub))

                                  ;; (setq e2wm:c-clj-winfo
                                  ;;       '((:name left )
                                  ;;         (:name right )
                                  ;;         (:name sub :buffer "*Help*" :default-hide t)
                                  ;;         (:name history :plugin history-list :default-hide nil)))
                                  (setq e2wm:c-code-winfo
                                        '((:name main)
                                          (:name files :plugin files)
                                          (:name history :plugin history-list)
                                          (:name sub :buffer "*info*" :default-hide t)
                                          (:name nrepl :buffer "*nrepl*" :default-hide t)
                                          (:name result :buffer "*nrepl-result*" :default-hide t)
                                          )
                                        )


                                  ))

(use-package helm-glogs
  :bind ("s-g" . helm-glogs))

;;; setup-experiment.el ends here
