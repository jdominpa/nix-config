;;; jdp-modules-write.el --- Configurations for writing prose, `LaTeX-mode' and `org-mode' -*- lexical-binding: t -*-

;;; General settings for writing prose
(use-package emacs
  :custom
  (default-input-method "catalan-prefix")
  (default-transient-input-method "catalan-prefix"))

(use-package text-mode
  :hook (text-mode . turn-on-auto-fill)
  :custom
  (fill-column 80))

;;; Spellchecking
(use-package jinx
  :ensure t
  :hook (text-mode . jinx-mode)
  :bind (:map jinx-mode-map
              ("M-$" . jinx-correct)
              ("C-M-$" . jinx-languages)))

;;; Improved PDF viewing
(use-package pdf-tools
  :ensure t
  :bind (:map pdf-view-mode-map
              ("d" . pdf-view-midnight-minor-mode))
  :custom
  (pdf-view-display-size 'fit-page)
  (pdf-view-midnight-colors '("#ffffff" . "#000000"))
  :init
  (pdf-loader-install))

;;; `LaTeX-mode'
(use-package math-delimiters
  :commands (math-delimiters-no-dollars math-delimiters-insert))

(use-package cdlatex
  :ensure t
  :defer t
  :custom
  (cdlatex-math-symbol-prefix ?\;)
  (cdlatex-takeover-dollar nil)
  (cdlatex-takeover-parenthesis nil)
  (cdlatex-use-dollar-to-ensure-math nil)
  (cdlatex-sub-super-scripts-outside-math-mode nil)
  (cdlatex-math-modify-alist '((?B "\\mathbb" "\\textbf" t nil nil)
                               (?F "\\mathfrak" "\\textfrak" t nil nil)))
  (cdlatex-math-symbol-alist '((?+ "\\cup" "\\oplus" "\\bigoplus")
                               (?& "\\cap" "\\wedge")
                               (?* "\\times" "\\otimes" "\\bigotimes")
                               (?o "\\omega" "\\circ")
                               (?x "\\chi" "\\xrightarrow")))
  :config
  ;; Integrate yasnippet with cdlatex if it is installed
  (with-eval-after-load 'yasnippet
    (use-package yasnippet
      :hook ((cdlatex-tab . yas-expand)
             (cdlatex-tab . jdp-cdlatex-in-yas-field))
      :bind (:map yas-keymap
                  ("TAB" . jdp-yas-next-field-or-cdlatex))
      :config
      (defun jdp-cdlatex-in-yas-field ()
        ;; Check if we're at the end of the Yas field
        (when-let* ((_ (overlayp yas--active-field-overlay))
                    (end (overlay-end yas--active-field-overlay)))
          (if (>= (point) end)
              ;; Call yas-next-field if cdlatex can't expand here
              (let ((s (thing-at-point 'sexp)))
                (unless (and s (assoc (substring-no-properties s)
                                      cdlatex-command-alist-comb))
                  (yas-next-field-or-maybe-expand)
                  t))
            ;; otherwise expand and jump to the correct location
            (let (cdlatex-tab-hook minp)
              (setq minp
                    (min (save-excursion (cdlatex-tab)
                                         (point))
                         (overlay-end yas--active-field-overlay)))
              (goto-char minp) t))))
      (defun jdp-yas-next-field-or-cdlatex nil
        "Jump to the next Yas field correctly with cdlatex active."
        (interactive)
        (if (or (bound-and-true-p cdlatex-mode)
                (bound-and-true-p org-cdlatex-mode))
            (cdlatex-tab)
          (yas-next-field-or-maybe-expand))))))

;; NOTE: This package declaration must be after the math-delimiters declaration.
;;  Otherwise `math-delimiters-insert' won't be autoloaded properly and it won't
;;  be available in `LaTeX-mode'.  The same can be said about the `org' package
;;  further below.
(use-package latex
  :ensure auctex
  :hook ((LaTeX-mode . turn-on-cdlatex)
         (LaTeX-mode . turn-on-cdlatex-electricindex)
         (LaTeX-mode . prettify-symbols-mode))
  :bind (:map LaTeX-mode-map
              ("$" . math-delimiters-insert)
              ("M-n" . TeX-next-error)
              ("M-p" . TeX-previous-error))
  :custom
  (TeX-source-correlate-mode t)
  (TeX-source-correlate-start-server t)
  (TeX-newline-function 'reindent-then-newline-and-indent)
  :config
  (setcdr (assq 'output-pdf TeX-view-program-selection)
          '("PDF Tools"))
  (add-hook 'TeX-after-compilation-finished-functions
            #'TeX-revert-document-buffer))

(use-package reftex
  :after auctex
  :hook (LaTeX-mode . turn-on-reftex)
  :custom
  (reftex-plug-into-AUCTeX t)
  (reftex-label-alist
   '(("theorem"     ?T "thm:"  "~\\ref{%s}" t ("theorem")     -3)
     ("proposition" ?P "prop:" "~\\ref{%s}" t ("proposition") -3)
     ("lemma"       ?L "lem:"  "~\\ref{%s}" t ("lemma")       -3)
     ("corollary"   ?C "cor:"  "~\\ref{%s}" t ("corollary")   -3)
     ("remark"      ?R "rem:"  "~\\ref{%s}" t ("remark")      -3)
     ("definition"  ?D "defn:" "~\\ref{%s}" t ("definition")  -3)
     AMSTeX)))

;;; `org-mode'
(use-package org
  :hook (org-mode . turn-on-org-cdlatex)
  :bind (:map org-mode-map
              ("$" . math-delimiters-insert)
              ("M-g o" . consult-org-heading)
         :map org-cdlatex-mode-map
              ("`" . nil)
              (";" . cdlatex-math-symbol))
  :custom
  ;; Appearance
  (org-startup-indented t)
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
  (org-agenda-files (list (file-name-concat org-directory "agenda.org")))
  (org-log-done 'time)
  (org-log-into-drawer t)
  (org-todo-keywords '((sequence "TODO(t)" "HOLD(h!)" "|" "CANCEL(c!)" "DONE(d!)")))
  :config
  (setf (alist-get "\\.pdf\\'" org-file-apps nil nil #'equal) 'emacs)
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp")))

(use-package org-pdftools
  :ensure t
  :after (org pdf-tools)
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

(provide 'jdp-modules-write)
;;; jdp-modules-write.el ends here
