;;; init-highlight.el --- Configurations related with highlighting text, lines, etc. -*- lexical-binding: t -*-

;; [hl-line] Highlight current line
(use-package hl-line
  :hook ((dired-mode ibuffer occur-mode) . hl-line-mode)
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
          '(face trailing empty indentation space-before-tab space-after-tab)))

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
  (setq-default goggles-pulse t))

;; [pulse] Highlight line at cursor after switching window
(use-package pulse
  :init
  (setq pulse-delay 0.1
        pulse-iterations 2)
  (defun +pulse-momentary (&rest _)
    "Pulse the region or the current line."
    (if (fboundp 'xref-pulse-momentarily)
        (xref-pulse-momentarily)
      (pulse-momentary-highlight-one-line (point))))
  ;; FIXME: don't pulse when switching to/from the minibuffer
  (defun +pulse-momentary-line-a (&rest _)
    "Pulse the current line."
    (pulse-momentary-highlight-one-line (point)))
  (defun +recenter-and-pulse-a (&rest _)
    "Recenter and pulse the region or the current line."
    (recenter)
    (+pulse-momentary))
  (defun +recenter-and-pulse-line-a (&rest _)
    "Recenter and pulse the current line."
    (recenter)
    (pulse-momentary-highlight-one-line (point)))
  (dolist (cmd '(recenter-top-bottom
                 other-window switch-to-buffer aw-select
                 tab-bar-select-tab
                 delete-window delete-other-windows
                 delete-frame delete-other-frames))
    (advice-add cmd :after #'+pulse-momentary-line-a))
  (dolist (cmd '(pop-to-mark-command
                 pop-global-mark))
    (advice-add cmd :after #'+recenter-and-pulse-a))
  (dolist (cmd '(compile-goto-error))
    (advice-add cmd :after #'+recenter-and-pulse-line-a)))

(provide 'init-highlight)
;;; init-highlight.el ends here
