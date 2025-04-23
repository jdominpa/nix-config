;;; jdp-modules-prog.el --- Configurations for `prog-mode' and programming languages -*- lexical-binding: t -*-

;;; General programming settings

;; Tabs and indentation
(use-package emacs
  :custom
  (tab-always-indent 'complete)
  (tab-width 4)
  (indent-tabs-mode nil))

;; Configure `electric' behaviour
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

;; Enable automatic completion in `prog-mode' buffers
(use-package prog-mode
  :hook ((prog-mode . goto-address-prog-mode)
         (prog-mode . (lambda ()
                        (with-eval-after-load 'corfu
                          (setq-local corfu-auto t))))))

;; Eldoc
(use-package eldoc
  :custom
  (eldoc-echo-area-use-multiline-p nil)
  (global-eldoc-mode t))

;; Handle performance for long lines
(use-package so-long
  :custom
  (global-so-long-mode t))

;; Tree-sitter
(use-package treesit
  :config
  (dolist (mapping '((c-mode . c-ts-mode)
                     (c++-mode . c++-ts-mode)))
    (add-to-list 'major-mode-remap-alist mapping)))

;; Emacs native LSP client
(use-package eglot
  :functions eglot-ensure
  :commands eglot
  :hook ((c-mode c-ts-mode nix-mode) . eglot-ensure)
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
  :custom
  (eglot-autoshutdown t)
  (eglot-events-buffer-size '(:size 20000 :format short))
  (eglot-ignored-server-capabilities
   '(:documentHighlightProvider))
  :config
  (with-eval-after-load 'cape
    (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)))

(use-package eglot-booster
  :disabled t ; FIXME: native compilation doesn't work on macOS Sequoia 15.4
  :vc (:url "https://github.com/jdtsmith/eglot-booster")
  :after eglot
  :config
  (eglot-booster-mode))

(use-package consult-eglot
  :ensure t
  :after (consult eglot)
  :bind (:map eglot-mode-map
         ("M-g s" . consult-eglot-symbols)))

;; Flymake
(use-package flymake
  :bind (:map flymake-mode-map
              ("M-n" . flymake-goto-next-error)
              ("M-p" . flymake-goto-prev-error)
              ("C-c ! d" . flymake-show-buffer-diagnostics)
              ("C-c ! D" . flymake-show-project-diagnostics))
  :custom
  (flymake-no-changes-timeout nil)
  (flymake-proc-compilation-prevents-syntax-check t)
  (flymake-start-on-flymake-mode t)
  (flymake-start-on-save-buffer t)
  (flymake-show-diagnostics-at-end-of-line 'short)
  (flymake-mode-line-format
   '("" flymake-mode-line-exception flymake-mode-line-counters)))

;; Highlight comment keywords
(use-package hl-todo
  :ensure t
  :hook (prog-mode . hl-todo-mode))

;; Colored delimiters
(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

;; Expand lisp macros
(use-package macrostep
  :ensure t
  :bind (:map emacs-lisp-mode-map
              ("C-c e" . macrostep-expand)))

;; Direnv integration
(use-package envrc
  :ensure t
  :hook (prog-mode . envrc-mode))

;;; Programming language modes configurations

;; C/C++
(use-package cc-mode
  :defer t
  :bind (:map c-mode-base-map
              ("TAB" . nil))
  :custom
  (c-default-style "k&r")
  (c-basic-offset 4))

(use-package c-ts-mode
  :defer t
  :custom
  (c-ts-mode-indent-offset 4))

;; Elisp
(use-package elisp-mode
  :hook (emacs-lisp-mode . (lambda ()
                             (setq-local sentence-end-double-space t))))

;; Justfile
(use-package just-mode
  :ensure t
  :defer t)

;; Markdown
(use-package markdown-mode
  :ensure t
  :defer t
  :custom
  (markdown-hide-markup t))

;; Nix
(use-package nix-mode
  :ensure t
  :defer t
  :hook (nix-mode . nix-prettify-mode))

(provide 'jdp-modules-prog)
;;; jdp-modules-prog.el ends here
