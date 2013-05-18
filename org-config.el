;;; Package --- Summary
;;; Code:
;;; Commentary:

(eval-after-load 'org
  '(progn
     (defvar awang/org-dir "E:/Users/awang/Dropbox/Org")
     (setq org-directory awang/org-dir)

     (define-key global-map "\C-cc" 'org-capture)

     (setq org-capture-templates
           '(
             ("t" "Todo" entry (file+headline (expand-file-name "mygtd.org" awang/org-dir) "Tasks")
              "* TODO %^{Brief Description} %^g\n%?\nAdded: %U")
             ("p" "Private" entry (file (expand-file-name "privnotes.org" awang/org-dir))
              "\n* %^{topic} %T \n%i%?\n")
             ("j" "Journal" entry (file+datetree (expand-file-name "journal.org" awang/org-dir))
              "* %?\nEntered on %U\n %i\n %a")
             ("w" "WordOfDay" entry (file (expand-file-name "wotd.org" awang/org-dir))
              "\n* %^{topic} \n%i%?\n")
             ))

     (setq org-agenda-custom-commands
           '(
             ("P" "Projects"
              ((tags "PROJECT")))

             ("H" "Office and Home Lists"
              ((agenda)
               (tags-todo "OFFICE")
               (tags-todo "HOME")
               (tags-todo "COMPUTER")
               (tags-todo "PHONE")
               (tags-todo "READING")))

             ("D" "Daily Action List"
              (
               (agenda "" ((org-agenda-ndays 1)
                           (org-agenda-sorting-strategy
                            (quote ((agenda time-up priority-down tag-up) )))
                           (org-deadline-warning-days 0)
                           ))))
             )
           )

     (setq org-refile-targets
           '(
             ("mygtd.org" :maxlevel . 1)
             ("someday.org" :level . 2)
             ))

     ;; org misc settings
     (setq org-agenda-files (list (expand-file-name "mygtd.org" awang/org-dir))
           org-agenda-ndays 7
           org-deadline-warning-days 7
           org-agenda-show-all-dates t
           org-agenda-skip-deadline-if-done t
           org-agenda-skip-scheduled-if-done t
           org-agenda-start-on-weekday nil
           org-reverse-note-order t)

     (add-hook 'org-mode-hook (lambda ()
                                (when prelude-guru
                                  (guru-mode +1))))
     (add-hook 'org-agenda-mode-hook 'hl-line-mode)))

(provide 'org-config)

;;; org-config.el ends here
