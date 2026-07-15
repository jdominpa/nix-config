;;; init-ui.el --- Configurations for Emacs' UI (theme, font, ...) -*- lexical-binding: t -*-

;; Optimization
(use-package emacs
  :config
  (setq
   ;; Inhibits fontification while receiving input, which should help a little with scrolling performance
   redisplay-skip-fontification-on-input t
   ;; TODO: test if this affects memory by a lot
   ;; Font compacting can be terribly expensive, but may increase memory use
   inhibit-compacting-font-caches t
   ;; Don't show cursor or region in other windows
   highlight-nonselected-windows nil)
  (setopt cursor-in-non-selected-windows nil)
  ;; [cursor] Disable blinking
  (blink-cursor-mode -1)
  ;; Allow [resize] by pixels
  (setq frame-resize-pixelwise t
        window-resize-pixelwise t)
  ;; Frame title
  (setq frame-title-format "Emacs")
  ;; Suppress GUI features for consistency
  (setq use-dialog-box nil
        use-file-dialog nil))

;; [modus-themes]
(use-package modus-themes
  :ensure t
  :config
  (setq custom-safe-themes t ; don't prompt to confirm theme safety
        modus-themes-mixed-fonts t
        modus-themes-bold-constructs t
        modus-themes-italic-constructs t)
  (load-theme 'modus-vivendi t))

;; [auto-dark] Switch light/dark theme according to system setting
(use-package auto-dark
  :ensure t
  :hook (after-init . auto-dark-mode)
  :config
  (setq auto-dark-allow-osascript t)
  (setopt auto-dark-themes '((modus-vivendi) (modus-operandi))))

;; [fontaine] Font configuration presets
(use-package fontaine
  :ensure t
  :when (display-graphic-p)
  :demand t
  :hook (after-init . (lambda ()
                        (fontaine-set-preset (or (fontaine-restore-latest-preset)
                                                 'default))))
  :config
  (setq fontaine-presets
        '((default)
          (medium
           :default-height 140)
          (large
           :default-height 160)
          (t
           :default-family "Aporetic Sans Mono"
           :default-weight regular
           :default-height 130
           :fixed-pitch-family "Aporetic Sans Mono"
           :variable-pitch-family "Aporetic Serif")))
  :config
  (fontaine-mode 1))

(use-package face-remap
  :bind (("C-x C-0" . global-text-scale-adjust) ; swap the default keybinds
         ("C-x C--" . global-text-scale-adjust)
         ("C-x C-+" . global-text-scale-adjust)
         ("C-x C-=" . global-text-scale-adjust)
         ("C-x C-M-0" . text-scale-adjust)
         ("C-x C-M--" . text-scale-adjust)
         ("C-x C-M-+" . text-scale-adjust)
         ("C-x C-M-=" . text-scale-adjust)))

;; [which-key]
(use-package which-key
  :hook (after-init . which-key-mode)
  :config
  (setq which-key-preserve-window-configuration t
        which-key-idle-delay 0.6
        which-key-idle-secondary-delay 0.2))

;; [spacious-padding]
(use-package spacious-padding
  :ensure t
  :hook (after-init . spacious-padding-mode)
  :config
  (setq spacious-padding-widths
        '( :fringe-width 4
           :internal-border-width 0
           :mode-line-width 1
           :right-divider-width 1)
        spacious-padding-subtle-frame-lines t))

;;; Mode line

(use-package mode-line
  :custom
  (mode-line-right-align-edge 'right-margin)
  (mode-line-position '("%l,%c"))
  (project-mode-line t)
  (mode-line-collapse-minor-modes
   '(not
     defining-kbd-macro
     envrc-mode
     flymake-mode))
  :config
  (setq-default mode-line-format
                '("%e" mode-line-front-space
                  (meow-mode (:eval (meow-indicator)))
                  " "
                  mode-line-mule-info
                  mode-line-client
                  mode-line-modified
                  mode-line-remote
                  mode-line-window-dedicated
                  mode-line-frame-identification
                  (project-mode-line +mode-line-project-format)
                  +mode-line-buffer-identification
                  "  "
                  mode-line-position
                  (vc-mode (" " vc-mode))
                  "  "
                  mode-line-modes
                  mode-line-misc-info
                  mode-line-end-spaces)))

(provide 'init-ui)
;;; init-ui.el ends here
