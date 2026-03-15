;;; init-org.el --- Configurations for `org-mode' -*- lexical-binding: t -*-

;;; Calendar

(use-package calendar
  :commands calendar
  :custom
  (calendar-mark-holidays-flag t)
  (calendar-time-display-form
   '( 24-hours ":" minutes
      (when time-zone (format "(%s)" time-zone))))
  (calendar-week-start-day 1)           ; Monday
  (calendar-date-style 'iso)
  (calendar-time-zone-style 'numeric))  ; Emacs 28.1

;;; Org

(use-package org
  :hook ((org-mode . turn-on-auto-fill)
         (org-mode . turn-on-org-cdlatex))
  :bind (("C-c o l" . org-store-link)
         :map org-mode-map
         ("$" . math-delimiters-insert)
         ("M-g o" . consult-org-heading)
         :map org-cdlatex-mode-map
         ("`" . nil)
         (";" . cdlatex-math-symbol))
  :custom
  ;; General settings
  (org-directory (expand-file-name "~/org"))
  (org-read-date-prefer-future 'time)
  (org-log-done 'time)
  (org-log-into-drawer t)
  (org-log-redeadline 'time)
  (org-log-reschedule 'time)
  (org-todo-keywords '((sequence "TODO(t)" "HOLD(h@/!)" "|" "CANCELED(c@)" "DONE(d!)")))
  (org-enforce-todo-dependencies t)
  (org-enforce-todo-checkbox-dependencies t)
  ;; Appearance
  (org-highlight-latex-and-related '(latex entities))
  (org-hide-emphasis-markers t)
  (org-hide-leading-stars t)
  (org-pretty-entities t)
  (org-auto-align-tags nil)
  (org-tags-column 0)
  ;; Keybinding behavior
  (org-special-ctrl-a/e t)
  (org-M-RET-may-split-line '((default . nil)))
  (org-insert-heading-respect-content t)
  :config
  (setf (alist-get "\\.pdf\\'" org-file-apps nil nil #'equal) 'emacs)
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp")))

(use-package org-capture
  :bind (("C-c o c" . org-capture)
         ("C-c o i" . org-capture-inbox))
  :custom
  (org-capture-templates
   `(("i" "Inbox" entry (file "inbox.org")
      ,(concat "* TODO %^{Task}\n"
               ":PROPERTIES:\n"
               ":CREATED: %U\n"
               ":CAPTURED: %a\n"
               ":END:\n"
               "%?"))
     ("e" "Event" entry (file+headline "agenda.org" "Events")
      ,(concat "* %^{Event}\n"
               "SCHEDULED: %^{Scheduled}T\n"
               ":PROPERTIES:\n"
               ":CREATED: %U\n"
               ":CAPTURED: %a\n"
               ":END:\n"
               "%?"))
     ("d" "Deadline" entry (file+headline "agenda.org" "Deadlines")
      ,(concat "* %^{Task}\n"
               "DEADLINE: %^{Deadline}T\n"
               ":PROPERTIES:\n"
               ":CREATED: %U\n"
               ":CAPTURED: %a\n"
               ":END:\n"
               "%?"))
     ("c" "Contact" entry (file "contacts.org")
      ,(concat "* %^{Name}\n"
               ":PROPERTIES:\n"
               ":CREATED: %U\n"
               ":CAPTURED: %a\n"
               ":PHONE: %^{Phone}\n"
               ":EMAIL: %^{Email}\n"
               ":ADDRESS: %^{Address}\n"
               ":BIRTHDAY: %^{Birthday +1y}u\n"
               ":END:\n"
               "%?"))))
  :config
  (defun org-capture-inbox ()
    "Store a link of the current location and create an inbox `org-capture'."
    (interactive)
    (call-interactively 'org-store-link)
    (org-capture nil "i")))

(use-package org-refile
  :after org
  :custom
  (org-refile-targets '(("projects.org" . (:regexp . "\\(?:\\(?:Note\\|Task\\)s\\)"))
                        ("agenda.org" . (:level . 2))))
  (org-refile-use-outline-path 'file)
  (org-outline-path-complete-in-steps nil)
  (org-refile-allow-creating-parent-nodes 'confirm))

(use-package org-agenda
  :bind ("C-c o a" . org-agenda)
  :custom
  (org-agenda-files
   (mapcar (lambda (file) (concat org-directory "/" file))
           '("agenda.org" "contacts.org")))
  (org-agenda-window-setup 'current-window)
  (org-deadline-past-days 365)
  (org-scheduled-past-days 365)
  (org-agenda-skip-timestamp-if-done t)
  (org-agenda-skip-deadline-if-done t)
  (org-agenda-skip-scheduled-if-done t))

(use-package org-contacts
  :ensure t
  :commands (org-contacts org-contacts-agenda org-contacts-export-as-vcard)
  :custom
  (org-contacts-files (list (concat org-directory "/contacts.org"))))

(use-package denote
  :ensure t
  :hook ((dired-mode . denote-dired-mode)
         (after-init . denote-rename-buffer-mode))
  :bind (("C-c n n" . denote)
         ("C-c n N" . denote-type)
         ("C-c n o" . denote-open-or-create)
         ("C-c n r" . denote-rename-file)
         ("C-c n R" . denote-rename-file-using-front-matter)
         ("C-c n l" . denote-link)
         ("C-c n L" . denote-add-links)
         ("C-c n b" . denote-backlinks)
         ("C-c n d" . denote-dired)
         :map dired-mode-map
         ("C-c C-d C-i" . denote-dired-link-marked-notes)
         ("C-c C-d C-r" . denote-dired-rename-files)
         ("C-c C-d C-k" . denote-dired-rename-marked-files-with-keywords)
         ("C-c C-d C-R" . denote-dired-rename-marked-files-using-front-matter)))

(use-package consult-denote
  :ensure t
  :demand t
  :bind (("C-c n f" . consult-denote-find)
         ("C-c n g" . consult-denote-grep))
  :config
  (consult-denote-mode))

(when (package-installed-p 'pdf-tools)
  (use-package org-pdftools
    :ensure t
    :hook (org-mode . org-pdftools-setup-link)))

(use-package org-appear
  :ensure t
  :after org
  :hook ((org-mode . org-appear-mode)
         (org-mode . (lambda ()
                      (add-hook 'meow-insert-enter-hook
                                #'org-appear-manual-start
                                nil
                                t)
                      (add-hook 'meow-insert-exit-hook
                                #'org-appear-manual-stop
                                nil
                                t))))
  :custom
  (org-appear-trigger 'manual)
  (org-appear-autoemphasis t)
  (org-appear-autolinks t)
  (org-appear-autosubmarkers t)
  (org-appear-autoentities t)
  (org-appear-autokeywords nil)
  (org-appear-inside-latex t))

(use-package org-modern
  :ensure t
  :after org
  :hook ((org-mode . org-modern-mode)
         (org-agenda-finalize . org-modern-agenda))
  :custom
  (org-modern-hide-stars nil) ; org-indent-mode doesn't behave well with other values
  (org-modern-block-indent t))

(provide 'init-org)
;;; init-org.el ends here
