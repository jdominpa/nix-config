;;; init-latex.el --- Configurations for `LaTeX-mode' -*- lexical-binding: t -*-

;;; Input method settings

(use-package emacs
  :custom
  (fill-column 78)
  (sentence-end-double-space nil) ; use a single space after a sentence
  (default-input-method "catalan-prefix")
  (default-transient-input-method "catalan-prefix"))

;;; Spellchecking

(use-package jinx
  :ensure t
  :bind (([remap ispell-word] . jinx-correct)
         :map jinx-mode-map
         ("M-$" . jinx-correct)
         ("C-M-$" . jinx-languages)))

;;; Improved PDF viewing with `pdf-tools'

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
  :vc (:url "https://github.com/oantolin/math-delimiters"
       :rev :newest)
  :commands (math-delimiters-no-dollars math-delimiters-insert)
  :custom
  (math-delimiters-compressed-display-math nil))

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
  ;; NOTE: order of lazytab and yasnippet matter. Otherwise the
  ;; `cdlatex-tab-hook' won't be ordered properly.
  ;; Use org tables with cdlatex
  (use-package lazytab
    :vc (:url "https://github.com/karthink/lazytab"
              :rev :newest)
    :demand t
    :bind (:map orgtbl-mode-map
                ("TAB" . lazytab-org-table-next-field-maybe)
                ([tab] . lazytab-org-table-next-field-maybe))
    :config
    (add-hook 'cdlatex-tab-hook #'lazytab-cdlatex-or-orgtbl-next-field 90)
    (dolist (cmd '(("bmat" "Insert bmat env"
                    "\\begin{bmatrix} ? \\end{bmatrix}"
                    lazytab-position-cursor-and-edit
                    nil nil t)
                   ("pmat" "Insert pmat env"
                    "\\begin{pmatrix} ? \\end{pmatrix}"
                    lazytab-position-cursor-and-edit
                    nil nil t)
                   ("tbl" "Insert table"
                    "\\begin{table}\n\\centering ? \\caption{}\n\\end{table}\n"
                    lazytab-position-cursor-and-edit
                    nil t nil)))
      (push cmd cdlatex-command-alist))
    (cdlatex-reset-mode))
  ;; Integrate yasnippet with cdlatex if it is installed
  (use-package yasnippet
    :if (package-installed-p 'yasnippet)
    :hook ((cdlatex-tab . cdlatex-in-yas-field)
           (cdlatex-tab . yas-expand))
    :bind (:map yas-keymap
                ("TAB" . yas-next-field-or-cdlatex)
                ([tab] . yas-next-field-or-cdlatex))
    :config
    (defun cdlatex-in-yas-field ()
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
    (defun yas-next-field-or-cdlatex nil
      "Jump to the next Yas field correctly with cdlatex active."
      (interactive)
      (if (or (bound-and-true-p cdlatex-mode)
              (bound-and-true-p org-cdlatex-mode))
          (cdlatex-tab)
        (yas-next-field-or-maybe-expand)))))

;; NOTE: This package declaration must be after the math-delimiters declaration.
;;  Otherwise `math-delimiters-insert' won't be autoloaded properly and it won't
;;  be available in `LaTeX-mode'.  The same can be said for the `org' package.
(use-package latex
  :ensure auctex
  :hook ((LaTeX-mode . turn-on-cdlatex)
         (LaTeX-mode . turn-on-cdlatex-electricindex)
         (LaTeX-mode . turn-on-auto-fill)
         (LaTeX-mode . prettify-symbols-mode)
         (LaTeX-mode . jinx-mode))
  :bind (:map LaTeX-mode-map
              ("$" . math-delimiters-insert)
              ("M-n" . TeX-next-error)
              ("M-p" . TeX-previous-error))
  :custom
  (TeX-auto-save t)
  (TeX-parse-self t)
  (TeX-auto-untabify t)
  (TeX-source-correlate-mode t)
  (TeX-source-correlate-start-server t)
  (TeX-newline-function 'reindent-then-newline-and-indent)
  :config
  (setcdr (assq 'output-pdf TeX-view-program-selection)
          '("PDF Tools"))
  (add-hook 'TeX-after-compilation-finished-functions
            #'TeX-revert-document-buffer)
  (setf (alist-get '(major-mode . TeX-special-mode)
                   display-buffer-alist)
        `((display-buffer-at-bottom)
          (window-height . ,(lambda (win) (fit-window-to-buffer win (floor (frame-height) 3)))))))

(use-package preview
  :after latex
  :hook (LaTeX-mode . preview-larger-previews)
  :config
  (defun preview-larger-previews ()
    "Increases `LaTeX-mode' preview scale."
    (setq preview-scale-function
          (lambda ()
            (* 1.25
               (funcall (preview-scale-from-face)))))))

(use-package reftex
  :after latex
  :hook (LaTeX-mode . turn-on-reftex)
  :custom
  (reftex-insert-label-flags '("se" "sfte"))
  (reftex-plug-into-AUCTeX t)
  (reftex-label-alist
   '(("theorem"     ?T "thm:"  "~\\ref{%s}" t ("theorem")     -3)
     ("proposition" ?P "prop:" "~\\ref{%s}" t ("proposition") -3)
     ("lemma"       ?L "lem:"  "~\\ref{%s}" t ("lemma")       -3)
     ("corollary"   ?C "cor:"  "~\\ref{%s}" t ("corollary")   -3)
     ("remark"      ?R "rem:"  "~\\ref{%s}" t ("remark")      -3)
     ("definition"  ?D "defn:" "~\\ref{%s}" t ("definition")  -3)
     AMSTeX)))

(provide 'init-latex)
;;; init-latex.el ends here
