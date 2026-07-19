;;; -*- lexical-binding: t -*-

;; Enable automatic completion in `prog-mode' buffers
(use-package prog-mode
  :hook ((prog-mode . prettify-symbols-mode)
         (prog-mode . (lambda ()
                        (with-eval-after-load 'corfu
                          (setq-local corfu-auto t))))))

;; [compile]
(use-package compile
  :hook ((compilation-filter . +prog-truncate-compilation-buffer-h)
         (compilation-filter . +prog-colorize-compilation-buffer-h))
  :config
  (setq compilation-always-kill t       ; kill compilation process before starting another
        compilation-ask-about-save nil  ; save all buffers on `compile'
        compilation-scroll-output 'first-error)
  (defun +prog-truncate-compilation-buffer-h (&optional _string)
    "Rate-limit `comint-truncate-buffer' in `compilation-mode' buffers."
    (require 'comint)
    (when (> (buffer-size)
             (* 80 comint-buffer-maximum-size))
      (let ((gc-cons-threshold most-positive-fixnum)
            (gc-cons-percentage 1.0))
        (with-silent-modifications
          (comint-truncate-buffer)))))
  (defun +prog-colorize-compilation-buffer-h ()
    "Apply ANSI color codes to a `compilation-mode' buffer."
    (require 'ansi-color)
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region compilation-filter-start (point)))))

;; [comment] Comment over empty lines
(setq comment-empty-lines t)

;; [xref] Cross reference
(use-package xref
  :config
  (setq xref-search-program 'ripgrep
        xref-history-storage 'xref-window-local-history))

;; [eglot] Emacs native LSP client
(use-package eglot
  :functions eglot-ensure
  :commands eglot
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
  :custom-face (eglot-highlight-symbol-face ((t (:underline t)))) ; TODO: testing this
  :preface
  (defconst +prog-eglot-auto-start-modes
    '( c-mode c++-mode c-ts-mode c++-ts-mode
       nael-mode nix-ts-mode rust-mode rust-ts-mode)
    "List of major modes in which Eglot should automatically start.")
  :init
  (dolist (mode +prog-eglot-auto-start-modes)
    (add-hook (intern (format "%s-hook" mode)) #'eglot-ensure))
  :config
  (setq eglot-events-buffer-config '(:size 0 :format full)
        eglot-autoshutdown t
        eglot-documentation-renderer 'markdown-ts-view-mode
        ;; eglot-ignored-server-capabilities '(:documentHighlightProvider)
        eglot-code-action-indications nil)
  ;; (fset #'jsonrpc--log-event #'ignore)
  (with-eval-after-load 'cape
    (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)))

;; [consult-eglot] Consult support for eglot
(use-package consult-eglot
  :ensure t
  :after (consult eglot)
  :bind (:map eglot-mode-map
         ([remap xref-find-apropos] . consult-eglot-symbols)))

(use-package eglot-tempel
  :ensure t
  :after (eglot tempel)
  :config
  (eglot-tempel-mode 1))

;; [eldoc]
(use-package eldoc
  :hook (after-init . global-eldoc-mode)
  :config
  (setq eldoc-echo-area-display-truncation-message t
        eldoc-echo-area-prefer-doc-buffer t
        eldoc-echo-area-use-multiline-p nil
        eldoc-documentation-strategy 'eldoc-documentation-compose-eagerly
        eldoc-help-at-pt t
        eldoc-doc-buffer-separator (concat "\n"
                                           (propertize
                                            (concat
                                             (propertize "---" 'display " ")
                                             "\n")
                                            'face '(:strike-through t :extend t)
                                            'font-lock-face
                                            '(:strike-through t :extend t)))))

(use-package eldoc-box
  :ensure t
  :bind (("C-c ." . eldoc-box-help-at-point) ; meow leader keybind
         ("C-h h" . eldoc-box-help-at-point)))

;; [dape] Debug Adapter Protocol client
(use-package dape
  :ensure t
  :commands dape
  :bind (:map prog-mode-map
              ("C-c D" . dape))
  :init
  (setq dape-buffer-window-arrangement 'right))

;; [flymake] On-the-fly syntax checker
(use-package flymake
  :hook (prog-mode . +prog-flymake-mode-unless-eglot-h)
  :bind (:map flymake-mode-map
              ;; TODO: review these keybinds
              ("M-n" . flymake-goto-next-error)
              ("M-p" . flymake-goto-prev-error)
              ("C-c ! d" . flymake-show-buffer-diagnostics)
              ("C-c ! D" . flymake-show-project-diagnostics))
  :preface
  (defun +prog-flymake-mode-unless-eglot-h ()
    "Enable `flymake-mode' unless the major mode of the current buffer is in
`+prog-eglot-auto-start-modes'."
    (unless (memq major-mode +prog-eglot-auto-start-modes)
      (flymake-mode 1)))
  :config
  (setq flymake-show-diagnostics-at-end-of-line 'short
        flymake-mode-line-format '("" flymake-mode-line-exception flymake-mode-line-counters)))

;; [treesit]
(use-package treesit
  :when (treesit-available-p)
  :config
  (setopt treesit-enabled-modes t)
  (setq treesit-auto-install-grammar 'always
        treesit-font-lock-level 4))

;; [envrc] Direnv integration
(use-package envrc
  :ensure t
  :hook (prog-mode . envrc-mode))

;; [indent-bars] Show indent guides
(use-package indent-bars
  :ensure t
  :hook (prog-mode . indent-bars-mode)
  :config
  (setopt
   indent-bars-pattern "."
   indent-bars-pad-frac 0.1
   indent-bars-width-frac 0.1
   indent-bars-zigzag nil
   indent-bars-highlight-current-depth '(:blend 0.825)
   indent-bars-display-on-blank-lines nil
   indent-bars-no-descend-lists 'skip
   indent-bars-treesit-support t
   indent-bars-treesit-wrap '((c argument_list
                                 parameter_list
                                 init_declarator
                                 parenthesized_expression)
                              (rust arguments parameters)
                              (toml table array comment))
   indent-bars-treesit-scope '((rust trait_item impl_item
                                     macro_definition macro_invocation
                                     struct_item enum_item mod_item
                                     const_item let_declaration
                                     function_item for_expression
                                     if_expression loop_expression
                                     while_expression match_expression
                                     match_arm call_expression
                                     token_tree token_tree_pattern
                                     token_repetition))))

;;; Programming languages

;; C/C++
(use-package cc-mode
  :config
  (setq c-basic-offset 4)
  (c-set-offset 'case-label '+))

(use-package c-ts-mode
  :hook (;; NOTE: on-type formatting does not work correctly with
         ;; electric-indent-mode and clangd
         (c-ts-mode . (lambda ()
                        (setq-local eglot-ignored-server-capabilities
                                    (append eglot-ignored-server-capabilities
                                            '(:documentOnTypeFormattingProvider))))))
  :config
  (setopt c-ts-mode-indent-style 'k&r)
  (setq c-ts-indent-offset 4))

;; [elisp]
(use-package elisp-mode
  :hook (emacs-lisp-mode . (lambda ()
                             (setq-local sentence-end-double-space t))))

;; [justfile]
(use-package just-ts-mode
  :ensure t
  :defer t)

;; [lean-4]
(use-package nael
  :ensure t
  :defer t)

;; [markdown]
(use-package markdown-ts-mode
  :mode ("\\.md\\'" "\\.markdown\\'"))

;; [nix]
(use-package nix-ts-mode
  :ensure t
  :mode "\\.nix\\'"
  :config
  (setq treesit-font-lock-level 4))

;; [rust]
(use-package rust-mode
  :ensure t
  :defer t
  :init
  (setq rust-mode-treesitter-derive t))

(use-package rustic
  :ensure t
  :after rust-mode
  :init
  (remove-hook 'rustic-mode-hook 'flycheck-mode)
  :config
  (setq rustic-lsp-client 'eglot
        rustic-format-on-save t))
