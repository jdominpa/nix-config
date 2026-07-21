;;; -*- lexical-binding: t -*-

;; [shell]
(use-package shell
  :defer t
  :config
  (setq shell-command-prompt-show-cwd t
        shell-input-autoexpand 'input
        shell-highlight-undef-enable t
        shell-kill-buffer-on-exit t
        comint-scroll-to-bottom-on-input t
        comint-input-autoexpand 'input
        comint-prompt-read-only t
        comint-completion-autolist t
        comint-input-ignoredups t))

;; [eshell] Emacs command shell
(use-package esh-mode
  :defer t
  :config
  (setq
   ;; Banner message
   eshell-banner-message nil
   ;; Scrolling
   eshell-scroll-to-bottom-on-input 'all
   eshell-scroll-to-bottom-on-output 'all
   ;; Exiting eshell
   eshell-kill-processes-on-exit t
   eshell-hist-ignoredups t
   eshell-input-filter #'eshell-input-filter-initial-space
   ;; [em-glob]
   eshell-glob-case-insensitive t
   eshell-error-if-no-glob t
   ;; Prefer eshell functions
   eshell-prefer-lisp-functions t
   ;; Visual commands require a proper terminal. Eshell can't handle that
   eshell-visual-subcommands '(("git" "help" "lg" "log" "diff" "show"))
   ;; Behave like common shells
   eshell-cmpl-ignore-case t
   eshell-cmpl-cycle-completions nil
   eshell-cd-on-directory t))

;; [ghostel] Terminal emulator
(use-package ghostel
  :ensure t
  :hook (ghostel-mode-hook . (lambda () (hl-line-mode -1)))
  :bind (:map ghostel-semi-char-mode-map
         ("C-s"  . consult-line)
         ("C-k"  . +term/ghostel-send-C-k-and-kill)
         ;; Use M-n/p to go up/down in the shell history
         ("M-p" . (lambda () (interactive) (ghostel-send-key "p" "ctrl")))
         ("M-n" . (lambda () (interactive) (ghostel-send-key "n" "ctrl")))
         :map project-prefix-map
         ("t" . ghostel-project)
         ("T" . ghostel-project-list-buffers))
  :config
  (setq password-cache t
        password-cache-expiry 300
        ghostel-ignore-cursor-change t)
  (defun +term/ghostel-send-C-k-and-kill ()
    "Send `C-k' to ghostel.  Like normal Emacs `C-k', kill to end of line
and put content in kill-ring."
    (interactive)
    (kill-ring-save (point) (line-end-position))
    (ghostel-send-key "k" "ctrl"))
  (add-to-list 'project-switch-commands '(ghostel-project "Ghostel") t)
  (add-to-list 'project-switch-commands '(ghostel-project-list-buffers "Ghostel buffers") t)
  (add-to-list 'ghostel-eval-cmds '("magit-status-setup-buffer" magit-status-setup-buffer)))
