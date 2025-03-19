;;; General settings for writing prose
(use-package emacs
  :custom
  (fill-column 80)
  (default-input-method "catalan-prefix")
  (default-transient-input-method "catalan-prefix"))

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
  :hook (pdf-view-mode . pdf-view-themed-minor-mode)
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
  :after yasnippet
  :hook ((cdlatex-tab . yas-expand)
         (cdlatex-tab . jdp-cdlatex-in-yas-field))
  :bind (:map cdlatex-mode-map
              ("$" . nil))
  :custom
  (cdlatex-math-symbol-prefix ?\;)
  (cdlatex-takeover-parenthesis nil)
  (cdlatex-sub-super-scripts-outside-math-mode nil)
  (cdlatex-math-modify-alist '((?B "\\mathbb" nil t nil nil)
                               (?k "\\mathfrak" nil t nil nil)))
  (cdlatex-math-symbol-alist '((?+ "\\cup" "\\oplus" "\\bigoplus")
                               (?& "\\cap" "\\wedge")
                               (?* "\\times" "\\otimes" "\\bigotimes")
                               (?o "\\omega" "\\circ")
                               (?x "\\chi" "\\xrightarrow")))
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
  (use-package yasnippet
    :ensure t
    :bind (:map yas-keymap
                ("TAB" . jdp-yas-next-field-or-cdlatex))
    :config
    (defun jdp-yas-next-field-or-cdlatex nil
      "Jump to the next Yas field correctly with cdlatex active."
      (interactive)
      (if (or (bound-and-true-p cdlatex-mode)
              (bound-and-true-p org-cdlatex-mode))
          (cdlatex-tab)
        (yas-next-field-or-maybe-expand)))))

;; NOTE: This package declaration must be after the math-delimiters declaration.
;;  Otherwise `math-delimiters-insert' won't be autoloaded properly and it won't
;;  be available in `LaTeX-mode'.  The same can be said about the `org' package
;;  further below.
(use-package latex
  :ensure auctex
  :hook ((LaTeX-mode . turn-on-cdlatex)
         (LaTeX-mode . turn-on-auto-fill)
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
  :hook ((org-mode . turn-on-auto-fill)
         (org-mode . turn-on-org-cdlatex))
  :bind (:map org-mode-map
              ("$" . math-delimiters-insert)
              ("M-g o" . consult-org-heading)
         :map org-cdlatex-mode-map
              ("`" . nil)
              (";" . cdlatex-math-symbol))
  :custom
  (org-startup-indented t)
  (org-highlight-latex-and-related '(latex script entities))
  (org-special-ctrl-a/e t)
  (org-hide-emphasis-markers t)
  (org-hide-leading-stars t)
  (org-pretty-entities t)
  (org-pretty-entities-include-sub-superscripts nil)
  :config
  (setf (alist-get "\\.pdf\\'" org-file-apps nil nil #'equal) 'emacs)
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp")))

(provide 'jdp-core-write)
