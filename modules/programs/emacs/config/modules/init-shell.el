;;; init-shell.el --- Configurations for Emacs' terminal and shell packages -*- lexical-binding: t -*-

;;; Shell

(use-package shell
  :defer t
  :custom
  (shell-command-prompt-show-cwd t)
  (shell-input-autoexpand 'input)
  (shell-highlight-undef-enable t)
  (shell-kill-buffer-on-exit t)
  (comint-scroll-to-bottom-on-input t)
  (comint-input-autoexpand 'input)
  (comint-prompt-read-only t)
  (comint-completion-autolist t)
  (comint-input-ignoredups t))

;;; Eshell

(use-package eshell
  :defer t
  :custom
  (eshell-cd-on-directory t)
  (eshell-hist-ignoredups t)
  (eshell-save-history-on-exit t)
  (eshell-scroll-to-bottom-on-input t)
  (password-cache t)
  (password-cache-expiry 300))

(provide 'init-shell)
;;; init-shell.el ends here
