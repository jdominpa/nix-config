;;; jdp-modules-org.el --- Configurations for `org-mode' -*- lexical-binding: t -*-

(use-package org
  :hook ((org-mode . turn-on-auto-fill)
         (org-mode . turn-on-org-cdlatex))
  :bind (("C-c o a" . org-agenda)
         ("C-c o c" . org-capture)
         ("C-c o l" . org-store-link)
         ("C-c i" . jdp-org-capture-inbox)
         :map org-mode-map
         ("$" . math-delimiters-insert)
         ("M-g o" . consult-org-heading)
         :map org-cdlatex-mode-map
         ("`" . nil)
         (";" . cdlatex-math-symbol))
  :custom
  ;; Appearance
  (org-highlight-latex-and-related '(latex script entities))
  (org-hide-emphasis-markers t)
  (org-hide-leading-stars t)
  (org-pretty-entities t)
  ;; Keybinding behavior
  (org-special-ctrl-a/e t)
  (org-M-RET-may-split-line '((default . nil)))
  (org-insert-heading-respect-content t)
  ;; Agenda
  (org-directory "~/Documents/org")
  (org-agenda-files (list (file-name-concat org-directory "inbox.org")
                          (file-name-concat org-directory "agenda.org")))
  (org-log-done 'time)
  (org-log-into-drawer t)
  (org-todo-keywords '((sequence "TODO(t)" "HOLD(h!)" "|" "CANCELLED(c!)" "DONE(d!)")))
  (org-capture-templates `(("i" "Inbox" entry (file "inbox.org")
                            ,(concat "* TODO %?\n"
                                     "/Created on/ %U"))
                           ("m" "Meeting" entry  (file+headline "agenda.org" "Future")
                            ,(concat "* %? :meeting:\n"
                                     "SCHEDULED: %^{Meeting date}t"))))
  :config
  (setf (alist-get "\\.pdf\\'" org-file-apps nil nil #'equal) 'emacs)
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (defun jdp-org-capture-inbox ()
    "Store a link of the current location and create an inbox `org-capture'."
    (interactive)
    (call-interactively 'org-store-link)
    (org-capture nil "i")))

(use-package org-pdftools
  :ensure t
  :if (package-installed-p 'pdf-tools)
  :after org
  :hook (org-mode . org-pdftools-setup-link))

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
  :custom
  (global-org-modern-mode t))

(provide 'jdp-modules-org)
;;; jdp-modules-org.el ends here
