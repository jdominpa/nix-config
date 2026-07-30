;;; -*- lexical-binding: t -*-

;; [jinx] Spellchecking
(use-package jinx
  :ensure t
  :bind (([remap ispell-word] . jinx-correct)
         :map jinx-mode-map
         ("M-$" . jinx-correct)
         ("C-M-$" . jinx-languages)))

;; [pdf-tools] Improved PDF viewing
(use-package pdf-tools
  :ensure t
  :bind (:map pdf-view-mode-map
              ("d" . pdf-view-midnight-minor-mode))
  :init
  (pdf-loader-install)
  :config
  (setq-default pdf-view-display-size 'fit-page
                pdf-view-midnight-colors '("#ffffff" . "#000000")))

;; [math-delimiters] Insert \( when pressing $ key
(use-package math-delimiters
  :vc (:url "https://github.com/oantolin/math-delimiters"
       :rev :newest)
  :commands (math-delimiters-no-dollars math-delimiters-insert)
  :config
  (setq math-delimiters-compressed-display-math nil))

;; [auctex]
(use-package latex
  :ensure auctex
  :hook ((LaTeX-mode . turn-on-cdlatex)
         (LaTeX-mode . turn-on-cdlatex-electricindex)
         (LaTeX-mode . turn-on-auto-fill)
         (LaTeX-mode . TeX-source-correlate-mode)
         (LaTeX-mode . (lambda ()
                         (setq-local fill-column 85)))
         (LaTeX-mode . prettify-symbols-mode)
         (LaTeX-mode . jinx-mode))
  :bind (:map LaTeX-mode-map
              ("$" . math-delimiters-insert)
              ("M-n" . TeX-next-error)
              ("M-p" . TeX-previous-error))
  :config
  (setq
   ;; Just save, don't ask before each compilation
   TeX-save-query nil
   ;; Parse on save
   TeX-auto-save t
   ;; Parse on load
   TeX-parse-self t
   ;; Use hidden directories for AUCTeX files
   TeX-auto-local ".auctex-auto"
   TeX-style-local ".auctex-style"
   TeX-auto-untabify t
   TeX-newline-function 'reindent-then-newline-and-indent
   ;; Don't start the emacs server when correlating sources
   TeX-source-correlate-start-server nil)
  (setcdr (assq 'output-pdf TeX-view-program-selection)
          '("PDF Tools"))
  (add-hook 'TeX-after-compilation-finished-functions
            #'TeX-revert-document-buffer))

;; [cdlatex] Tab navigation in latex files
(use-package cdlatex
  :ensure t
  :after latex
  :init
  ;; NOTE: these need to be set at init time, otherwise they don't take effect
  ;; properly
  (setq cdlatex-math-symbol-prefix ?\;
        cdlatex-takeover-dollar nil
        cdlatex-takeover-parenthesis nil)
  :config
  (setq cdlatex-use-dollar-to-ensure-math nil
        cdlatex-sub-super-scripts-outside-math-mode nil
        cdlatex-math-modify-alist '((?B "\\mathbb" "\\textbf" t nil nil)
                                    (?F "\\mathfrak" "\\textfrak" t nil nil))
        cdlatex-math-symbol-alist '((?+ "\\cup" "\\oplus" "\\bigoplus")
                                    (?& "\\cap" "\\wedge")
                                    (?* "\\times" "\\otimes" "\\bigotimes")
                                    (?o "\\omega" "\\circ")
                                    (?x "\\chi" "\\xrightarrow")))
  ;; [lazytab] Use org tables with cdlatex
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
    (cdlatex-reset-mode)))

(use-package reftex
  :after latex
  :hook (LaTeX-mode . turn-on-reftex)
  :config
  (setq reftex-insert-label-flags '("sfe" "sfte")
        reftex-plug-into-AUCTeX t)
  (setopt reftex-label-alist
          '(("theorem"     ?T "thm:"  "~\\ref{%s}" t ("theorem")     -3)
            ("proposition" ?P "prop:" "~\\ref{%s}" t ("proposition") -3)
            ("lemma"       ?L "lem:"  "~\\ref{%s}" t ("lemma")       -3)
            ("corollary"   ?C "cor:"  "~\\ref{%s}" t ("corollary")   -3)
            ("remark"      ?R "rem:"  "~\\ref{%s}" t ("remark")      -3)
            ("definition"  ?D "defn:" "~\\ref{%s}" t ("definition")  -3)
            AMSTeX)))

(use-package preview
  :after latex
  :hook (LaTeX-mode . +writing-preview-larger-previews-h)
  :config
  (defun +writing-preview-larger-previews-h ()
    "Increases `LaTeX-mode' preview scale."
    (setq preview-scale-function
          (lambda ()
            (* 1.25
               (funcall (preview-scale-from-face)))))))
