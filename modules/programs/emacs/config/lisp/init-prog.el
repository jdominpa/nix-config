;;; init-prog.el --- Configurations for `prog-mode' and programming languages -*- lexical-binding: t -*-

;;; General programming settings

;; Tabs and indentation
(use-package emacs
  :custom
  (tab-always-indent 'complete)
  (tab-width 4)
  :config
  (indent-tabs-mode -1))

;; Configure `electric' behaviour
(use-package electric
  :custom
  (electric-quote-context-sensitive t)
  (electric-quote-replace-double t)
  :config
  (electric-pair-mode)
  (electric-indent-mode))

;; Parentheses
(use-package paren
  :custom
  (show-paren-style 'parenthesis)
  (show-paren-context-when-offscreen 'child-frame)
  :config
  (show-paren-mode))

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
  (eldoc-documentation-strategy 'eldoc-documentation-compose)
  (eldoc-doc-buffer-separator
   (concat "\n"
           (propertize
            (concat
             (propertize "---" 'display " ")
             "\n")
            'face '(:strike-through t :extend t)
            'font-lock-face
            '(:strike-through t :extend t))))
  :config
  (global-eldoc-mode))

(use-package eldoc-box
  :ensure t
  :bind (("C-c h" . eldoc-box-help-at-point)
		 :map eglot-mode-map
		 ("C-h ." . eldoc-box-help-at-point)))

;; Handle performance for long lines
(use-package so-long
  :config
  (global-so-long-mode))

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
  :hook (eglot-managed-mode . jdp-eglot-capf)
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
  (eglot-code-action-indications '(margin mode-line))
  :config
  (fset #'jsonrpc--log-event #'ignore)
  (with-eval-after-load 'orderless
	(add-to-list 'completion-category-overrides
				 '(eglot (styles . (orderless basic))))
	(add-to-list 'completion-category-overrides
				 '(eglot-capf (styles . (orderless basic)))))
  (with-eval-after-load 'cape
	(advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)
	(defun jdp-eglot-capf ()
	  (setq-local completion-at-point-functions
				  (list (cape-capf-super
						 #'eglot-completion-at-point
						 #'tempel-expand))))))

(use-package eglot-booster
  :vc (:url "https://github.com/jdtsmith/eglot-booster"
            :rev :newest)
  :if (executable-find "emacs-lsp-booster")
  :after eglot
  :custom
  ;; FIXME: see https://github.com/blahgeek/emacs-lsp-booster/issues/43
  (eglot-booster-io-only t)
  :config
  (eglot-booster-mode))

(use-package consult-eglot
  :ensure t
  :after (consult eglot)
  :bind (:map eglot-mode-map
         ("M-g s" . consult-eglot-symbols)))

(use-package eglot-tempel
  :ensure t
  :config
  (eglot-tempel-mode))

;; Flymake
(use-package flymake
  :bind (:map flymake-mode-map
              ("M-n" . flymake-goto-next-error)
              ("M-p" . flymake-goto-prev-error)
              ("C-c ! d" . flymake-show-buffer-diagnostics)
              ("C-c ! D" . flymake-show-project-diagnostics))
  :custom
  (flymake-proc-compilation-prevents-syntax-check t)
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
  :hook (c-mode . eglot-ensure)
  :bind (:map c-mode-base-map
              ("TAB" . nil)
              ([tab] . nil))
  :custom
  (c-default-style "k&r")
  (c-basic-offset 4))

(use-package c-ts-mode
  :hook (c-ts-mode . eglot-ensure)
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

;; Lean 4

(use-package nael
  :ensure t
  :hook (nael-mode . eglot-ensure))

;; Markdown
(use-package markdown-mode
  :ensure t
  :defer t
  :custom
  (markdown-hide-markup t))

;; Nix
(use-package nix-mode
  :ensure t
  :hook ((nix-mode . eglot-ensure)
		 (nix-mode . nix-prettify-mode)))

;; Rust
(use-package rustic
  :ensure t
  :init
  (remove-hook 'rustic-mode-hook 'flycheck-mode)
  :custom
  (rustic-lsp-client 'eglot)
  :config
  (setq rustic-format-on-save t))

(provide 'init-prog)
;;; init-prog.el ends here
