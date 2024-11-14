;;; General programming settings

;; Tabs and indentation
(use-package emacs
  :custom
  (tab-always-indent 'complete)
  (tab-width 4)
  (indent-tabs-mode nil))

;; Configure 'electric' behaviour
(use-package electric
  :custom
  (electric-quote-context-sensitive t)
  (electric-quote-replace-double t)
  (electric-pair-mode t)
  (electric-indent-mode t))

;; Parentheses
(use-package paren
  :custom
  (show-paren-style 'parenthesis)
  (show-paren-context-when-offscreen 'overlay)
  (show-paren-mode t))

;; Auto fill comments
(use-package emacs
  :hook ((prog-mode . goto-address-prog-mode)
         (prog-mode . (lambda ()
                        (setq-local comment-auto-fill-only-comments t)))))

;; Eldoc
(use-package eldoc
  :custom
  (global-eldoc-mode t)
  (eldoc-echo-area-use-multiline-p nil))

;; Handle performance for long lines
(use-package so-long
  :custom
  (global-so-long-mode t))

;; Emacs native LSP client
(use-package eglot
  :bind (:map eglot-mode-map
              ("C-c l R" . eglot-reconnect)
              ("C-c l s" . eglot-shutdown)
              ("C-c l S" . eglot-shutdown-all)
              ("C-c l r" . eglot-rename)
              ("C-c l f" . eglot-format)
              ("C-c l i" . imenu)
              ("C-c l a" . eglot-code-actions)
              ("C-c l d" . flymake-show-buffer-diagnostics)
              ("C-c l D" . flymake-show-project-diagnostics))
  :custom (eglot-ignored-server-capabilities '(:documentHighlightProvider)))

(use-package consult-eglot
  :ensure t
  :after (consult eglot)
  :bind (:map eglot-mode-map
              ("M-g s" . consult-eglot-symbols)
              ("M-g M-s" . consult-eglot-symbols)))

;; Flymake
(use-package flymake
  :bind (:map flymake-mode-map
              ("C-c ! d" . flymake-show-buffer-diagnostics)
              ("C-c ! D" . flymake-show-project-diagnostics)
              ("C-c ! n" . flymake-goto-next-error)
              ("C-c ! p" . flymake-goto-prev-error))
  :custom
  (flymake-no-changes-timeout nil)
  (flymake-proc-compilation-prevents-syntax-check t)
  (flymake-start-on-flymake-mode t)
  (flymake-start-on-save-buffer t)
  (flymake-wrap-around nil)
  (flymake-mode-line-format
   '("" flymake-mode-line-exception flymake-mode-line-counters)))

;; Highlight comment keywords
(use-package hl-todo
  :ensure t
  :hook (prog-mode . hl-todo-mode))

;; Color delimiters
(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

;; Expand lisp macros
(use-package macrostep
  :ensure t
  :bind (:map emacs-lisp-mode-map
              ("C-c e" . macrostep-expand)))

;;; Programming language modes configurations 

;; C/C++ (cc-mode)
(use-package cc-mode
  :hook (c-mode . eglot-ensure)
  :bind (:map c-mode-base-map
              ("TAB" . nil))
  :custom
  (c-default-style "k&r")
  (c-basic-offset 4))

(use-package just-mode
  :ensure t)

;; Markdown (markdown-mode)
(use-package markdown-mode
  :ensure t
  :mode "\\.md\\'")

;; Nix (nix-mode)
(use-package nix-mode
  :ensure t
  :mode "\\.nix\\'"
  :custom
  (nix-prettify-global-mode t))

(provide 'jdp-core-proglangs)
