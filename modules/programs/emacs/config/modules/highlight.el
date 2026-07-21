;;; -*- lexical-binding: t -*-

;; [hl-line] Highlight current line
(use-package hl-line
  :hook (after-init . global-hl-line-mode)
  :config
  (setopt hl-line-sticky-flag nil))

;; [show-paren-mode] Highlight matching parens
(use-package paren
  :hook (after-init . show-paren-mode)
  :config
  (setq show-paren-style 'parenthesis
        show-paren-when-point-inside-paren t
        show-paren-context-when-offscreen 'child-frame
        show-paren-delay 0.2))

;; [whitespace] Show visualize TAB, (HARD) SPC, newline
(use-package whitespace
  :hook ((prog-mode conf-mode) . whitespace-mode)
  :config
  ;; only show bad whitespace
  (setopt whitespace-style
          '(face trailing tabs spaces empty indentation space-after-tab space-before-tab)))

;; [display-fill-column-indicator] Show a line at 80 char
(use-package display-fill-column-indicator
  :hook (prog-mode . display-fill-column-indicator-mode))

;; [rainbow-delimiters] Color delimiters according to their depth
(use-package rainbow-delimiters
  :ensure t
  :hook ((prog-mode conf-mode) . rainbow-delimiters-mode)
  :config
  (setq rainbow-delimiters-max-face-count 5))

;; [rainbow-mode] Colorize color names in buffers
(use-package rainbow-mode
  :ensure t
  :functions (rainbow-turn-off rainbow-colorize-match rainbow-x-color-luminance)
  :bind (:map help-mode-map
              ("r" . rainbow-mode))
  :hook ((prog-mode conf-mode) . rainbow-mode))

;; [hl-todo] Highlight TODO and similar keywords in comments and strings
(use-package hl-todo
  :ensure t
  :hook ((prog-mode conf-mode) . hl-todo-mode)
  :config
  (setq hl-todo-require-punctuation t
        hl-todo-highlight-punctuation ":"))

;; [goggles] Highlight modified region
(use-package goggles
  :ensure t
  :hook ((prog-mode text-mode) . goggles-mode)
  :config
  (setq goggles-pulse t))

;; [pulse] Highlight line at cursor after switching window
(use-package pulse
  :config
  (setq pulse-delay 0.1
        pulse-iterations 2)
  (defun +highlight-pulse-momentary-line-a (&rest _)
    "Pulse the current line."
    (pulse-momentary-highlight-one-line (point)))
  (defun +highlight--pulse-momentary (&rest _)
    "Pulse the region or the current line."
    (if (fboundp 'xref-pulse-momentarily)
        (xref-pulse-momentarily)
      (+highlight-pulse-momentary-line-a)))
  (defun +highlight-recenter-and-pulse-a (&rest _)
    "Recenter and pulse the region or the current line."
    (recenter)
    (+highlight--pulse-momentary))
  (defun +highlight-recenter-and-pulse-line-a (&rest _)
    "Recenter and pulse the current line."
    (recenter)
    (+highlight-pulse-momentary-line-a))
  (dolist (cmd '(aw-select
                 bookmark-jump
                 delete-frame
                 delete-other-frames
                 delete-window
                 delete-other-windows
                 dired-goto-file
                 dired-maybe-insert-subdir
                 dired-up-directory
                 other-window
                 quit-window
                 recenter-top-bottom
                 switch-to-buffer
                 tab-bar-select-tab
                 tab-close
                 tab-new
                 tab-next
                 tab-previous))
    (advice-add cmd :after #'+highlight-pulse-momentary-line-a))
  (dolist (cmd '(pop-to-mark-command
                 pop-global-mark))
    (advice-add cmd :after #'+highlight-recenter-and-pulse-a))
  (dolist (cmd '(compile-goto-error
                 occur-mode-goto-occurrence
                 occur-mode-goto-occurrence-other-window))
    (advice-add cmd :after #'+highlight-recenter-and-pulse-line-a)))
