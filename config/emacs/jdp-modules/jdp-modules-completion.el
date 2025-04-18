;;; jdp-modules-completion.el --- Configurations for minibuffer and in-buffer completions -*- lexical-binding: t -*-

;;; Minibuffer configurations and Vertico
(use-package minibuffer
  :custom
  (completion-styles '(basic substring initials flex orderless))
  (completion-category-overrides
   '((file (styles . (basic partial-completion orderless)))
     (library (styles . (basic substring)))
     (embark-keybinding (styles . (basic substring)))
     (imenu (styles . (basic substring orderless)))
     (consult-location (styles . (basic substring orderless)))
     (kill-ring (styles . (emacs22 orderless)))
     (eglot . (styles . (emacs22 substring orderless)))))
  (completion-pcm-leading-wildcard t)
  (read-buffer-completion-ignore-case t)
  (read-file-name-completion-ignore-case t)
  (enable-recursive-minibuffers t)
  (read-answer-short t)
  (resize-mini-windows t)
  (minibuffer-eldef-shorten-default t)
  (file-name-shadow-mode t)
  (minibuffer-depth-indicate-mode t)
  (minibuffer-electric-default-mode t)
  :config
  (setq completion-category-defaults nil
        completion-ignore-case t))

(use-package savehist
  :custom
  (savehist-file (locate-user-emacs-file "savehist"))
  (history-length 100)
  (history-delete-duplicates t)
  (savehist-save-minibuffer-history t)
  (savehist-mode t))

(use-package vertico
  :ensure t
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy)
  :custom
  (vertico-reverse-mode t)
  (vertico-mode t))

;;; Completion annotations
(use-package marginalia
  :ensure t
  :custom
  (marginalia-mode t))

;;; Orderless completion style
(use-package orderless
  :ensure t
  :demand t
  :bind (:map minibuffer-local-completion-map
         ("?" . nil)
         ("SPC" . nil))
  :custom
  (orderless-matching-styles '(orderless-prefixes
                               orderless-regexp)))

;;; Enhanced minibuffer commands
(use-package consult
  :ensure t
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :bind (;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command) ; orig. `repeat-complex-command'
         ("C-x t b" . consult-buffer-other-tab) ; orig. `switch-to-buffer-other-tab'
         ("C-x r b" . consult-bookmark)         ; orig. `bookmark-jump'
         ("C-x M-m" . consult-minor-mode-menu)
         ("C-x M-k" . consult-kmacro)
         ;; C-c bindings in `mode-specific-map'
         ("C-c b" . consult-buffer)
         ;; M-g bindings in `goto-map'
         ("M-g a" . consult-org-agenda)
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)
         ("M-g g" . consult-goto-line)   ; orig. `goto-line'
         ("M-g M-g" . consult-goto-line) ; orig. `goto-line'
         ("M-g o" . consult-outline)
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s f" . consult-find)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s i" . consult-info)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-s e" . consult-isearch-history) ; orig. `isearch-edit-string'
         ("M-s l" . consult-line)     ; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi) ; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history) ; orig. `next-matching-history-element'
         ("M-r" . consult-history) ; orig. `previous-matching-history-element'
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store) ; orig. `abbrev-prefix-mark' (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop) ; orig. `yank-pop'
         ("M-X" . consult-mode-command)
         :map consult-narrow-map
         ("?" . consult-narrow-help))
  :custom
  (completion-in-region-function #'consult-completion-in-region)
  (register-preview-function #'consult-register-format)
  (register-preview-delay 0.8)
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref)
  (consult-narrow-key "<"))

;;; Switch to directories
(use-package consult-dir
  :ensure t
  :bind (("C-x C-d" . consult-dir)
         :map minibuffer-local-completion-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file)))

;;; Embark
(use-package embark
  :ensure t
  :bind (("C-." . embark-act)
         ("M-." . embark-dwim)
         ("C-h B" . embark-bindings))
  :custom
  (prefix-help-command #'embark-prefix-help-command))

(use-package embark-consult
  :ensure t
  :defer t)

;;; In-buffer completion
(use-package corfu
  :ensure t
  :custom
  (corfu-min-width 20)
  (global-corfu-mode t)
  (corfu-popupinfo-delay '(1.25 . 0.5))
  (corfu-popupinfo-mode t)
  (corfu-history-mode t)
  :config
  (with-eval-after-load 'savehist
    (add-to-list 'savehist-additional-variables 'corfu-history)))

;;; Completion backends
(use-package cape
  :ensure t
  :after corfu
  :bind ("C-c c" . cape-prefix-map)
  :init
  (dolist (backend '(cape-dabbrev cape-abbrev cape-file cape-history))
    (add-hook 'completion-at-point-functions backend)))

;;; Completion popup icons
(use-package kind-icon
  :ensure t
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

;;; Snippets
(use-package yasnippet
  :ensure t
  :custom
  (yas-global-mode t))

(provide 'jdp-modules-completion)
;;; jdp-modules-completion.el ends here
