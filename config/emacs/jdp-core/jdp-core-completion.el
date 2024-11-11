;;; Minibuffer configurations and Vertico
(use-package minibuffer
  :custom
  (completion-styles '(basic substring initials flex orderless))
  (completion-category-overrides '((file (styles . (basic partial-completion orderless)))
                                   (project-file (styles . (basic substring partial-completion orderless)))
                                   (kill-ring (styles . (emacs22 orderless)))
                                   (eglot . (styles . (emacs22 substring orderless)))))
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
  (vertico-mode t)
  (vertico-reverse-mode t))

;;; Completion annotations
(use-package marginalia
  :ensure t
  :custom (marginalia-mode t))

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
  :bind (;; C-x bindings
         ("C-x M-:" . consult-complex-command)
         ("C-x M-m" . consult-minor-mode-menu)
         ("C-x M-k" . consult-kmacro)
         ([remap switch-to-buffer] . consult-buffer)
         ;; M-g bindings
         ([remap goto-line] . consult-goto-line)
         ;; M-s bindings
         ("M-s M-f" . consult-find)
         ("M-s M-g" . consult-grep)
         ("M-s M-i" . consult-imenu)
         ("M-s M-l" . consult-line)
         ("M-s M-m" . consult-mark)
         ("M-s M-s" . consult-outline)
         ("M-s M-y" . consult-yank-pop)
         :map consult-narrow-map
         ("?" . consult-narrow-help))
  :custom
  (register-preview-delay 0.8)
  (consult-preview-key 'any)
  (consult-narrow-key ">")
  :config
  (setq register-preview-function #'consult-register-format))

;;; Switch to directories
(use-package consult-dir
  :ensure t
  :bind ("C-x C-d" . consult-dir)
  :custom
  (consult-dir-shadow-filenames nil))

;;; In-buffer completion
(use-package corfu
  :ensure t
  :custom
  (global-corfu-mode t)
  (corfu-popupinfo-delay '(1.25 . 0.5))
  (corfu-popupinfo-mode t)
  :config
  (with-eval-after-load 'meow
    (add-hook 'meow-insert-exit-hook 'corfu-quit)))

;;; Completion backends
(use-package cape
  :ensure t
  :after corfu
  :bind ("C-c c" . cape-prefix-map)
  :init
  (dolist (backend '(cape-dabbrev cape-elisp-symbol cape-file))
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
  :custom (yas-global-mode t))

(provide 'jdp-core-completion)
