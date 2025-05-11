;;; init-org.el --- Configurations for `org-mode' -*- lexical-binding: t -*-

;;; Calendar

(use-package calendar
  :commands calendar
  :custom
  (calendar-mark-holidays-flag t)
  (calendar-time-display-form
   '( 24-hours ":" minutes
      (when time-zone (format "(%s)" time-zone))))
  (calendar-week-start-day 1)      ; Monday
  (calendar-date-style 'iso)
  (calendar-time-zone-style 'numeric)) ; Emacs 28.1

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
  (org-directory (expand-file-name "~/Documents/org"))
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
      ,(concat "* TODO %?\n"
               "/Created on/ %U\n"))
     ("m" "Meeting" entry  (file+olp "agenda.org" "Future" "Meetings")
      ,(concat "* %? :meeting:\n"
               "SCHEDULED: %^{Meeting}T\n"
               "/Created on/ %U\n"))
     ("a" "Appointment" entry (file+olp "agenda.org" "Future" "Appointments")
      ,(concat "* %? :appointment:\n"
               "SCHEDULED: %^{Appointment}T\n"
               "/Created on/ %U\n"))
     ("e" "Event")
     ("es" "Scheduled" entry (file+olp "agenda.org" "Future" "Events")
      ,(concat "* %? :event:%^g\n"
               "SCHEDULED: %^{Event}T\n"
               "/Created on/ %U\n"))
     ("ed" "Deadline" entry (file+olp "agenda.org" "Future" "Events")
      ,(concat "* %? :event:%^g\n"
               "DEADLINE: %^{Event}T\n"
               "/Created on/ %U\n"))))
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
  (org-agenda-files `(,org-directory))
  (org-agenda-window-setup 'current-window)
  (org-deadline-past-days 365)
  (org-scheduled-past-days 365)
  (org-agenda-skip-timestamp-if-done t)
  (org-agenda-skip-deadline-if-done t)
  (org-agenda-skip-scheduled-if-done t))

(use-package denote
  :ensure t
  :hook (dired-mode . denote-dired-mode)
  :bind (("C-c n n" . denote)
         ("C-c n N" . denote-type)
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
         ("C-c C-d C-R" . denote-dired-rename-marked-files-using-front-matter))
  :custom
  (denote-directory (expand-file-name "~/Documents/notes"))
  (denote-rename-buffer-mode t))

(use-package consult-denote
  :ensure t
  :bind (("C-c n f" . consult-denote-find)
         ("C-c n g" . consult-denote-grep))
  :custom
  (consult-denote-mode t))

(use-package org-noter
  :ensure t
  :bind (("C-c o n" . org-noter)
         :map org-noter-doc-mode-map
         ("M-p" . org-noter-sync-prev-note)
         ("M-." . org-noter-sync-current-note)
         ("M-n" . org-noter-sync-next-note)
         ("C-M-p" . org-noter-sync-prev-page-or-chapter)
         ("C-M-." . org-noter-sync-current-page-or-chapter)
         ("C-M-n" . org-noter-sync-next-page-or-chapter)
         :map org-noter-notes-mode-map
         ("M-p" . org-noter-sync-prev-note)
         ("M-." . org-noter-sync-current-note)
         ("M-n" . org-noter-sync-next-note)
         ("C-M-p" . org-noter-sync-prev-page-or-chapter)
         ("C-M-." . org-noter-sync-current-page-or-chapter)
         ("C-M-n" . org-noter-sync-next-page-or-chapter))
  :custom
  (org-noter-swap-window t)
  (org-noter-always-create-frame nil)
  (org-noter-separate-notes-from-heading t)
  (org-noter-default-notes-file-names '("projects.org"))
  (org-noter-notes-search-path
   (list org-directory
         denote-directory
         (expand-file-name "references" denote-directory)))
  :config
  (require 'pdf-macs)
  (defun org-noter-pdf--show-arrow ()
    ;; From `pdf-util-tooltip-arrow'.
    (pdf-util-assert-pdf-window)
    (let* (x-gtk-use-system-tooltips
           (arrow-top  (aref org-noter--arrow-location 2)) ; % of page
           (arrow-left (aref org-noter--arrow-location 3))
           (image-top  (if (floatp arrow-top)
                           (round (* arrow-top  (cdr (pdf-view-image-size)))))) ; pixel location on page (magnification-dependent)
           (image-left (if (floatp arrow-left)
                           (floor (* arrow-left (car (pdf-view-image-size))))))
           (dx (or image-left
                   (+ (or (car (window-margins)) 0)
                      (car (window-fringes)))))
           (dy (or image-top 0))
           (pos (list dx dy dx (+ dy (* 2 (frame-char-height)))))

           (tooltip-frame-parameters
            `((border-width . 0)
              (internal-border-width . 0)
              ,@tooltip-frame-parameters))
           (tooltip-hide-delay 3))
      (setq dy (max 0 (- dy
                         (cdr (pdf-view-image-offset))
                         (window-vscroll nil t)
                         (frame-char-height))))
      (when (overlay-get (pdf-view-current-overlay) 'before-string)
        (let* ((e (window-inside-pixel-edges))
               (xw (pdf-util-with-edges (e) e-width))
               (display-left-margin (/ (- xw (car (pdf-view-image-size t))) 2)))
          (cl-incf dx display-left-margin)))
      (setq dx (max 0 (+ dx org-noter-arrow-horizontal-offset)))
      (pdf-util-tooltip-in-window
       (propertize
        " " 'display (propertize
                      "\u2192" ;; right arrow
                      'display '(height 2)
                      'face `(:foreground
                              ,org-noter-arrow-foreground-color
                              :background
                              ,(if (bound-and-true-p pdf-view-midnight-minor-mode)
                                   (cdr pdf-view-midnight-colors)
                                 org-noter-arrow-background-color))))
       dx dy))))

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
