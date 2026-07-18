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

;;; Eshell

(use-package eshell
  :defer t
  :config
  (setq eshell-cd-on-directory t
        eshell-hist-ignoredups t
        eshell-save-history-on-exit t
        eshell-scroll-to-bottom-on-input t))

;;; Ghostel (terminal emulator)

(use-package ghostel
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
