;;; init-ui.el --- Configurations for Emacs' UI (theme, font, ...) -*- lexical-binding: t -*-

;;; Modus themes

(use-package modus-themes
  :ensure t
  :custom
  (modus-themes-mixed-fonts t)
  (modus-themes-bold-constructs t)
  (modus-themes-italic-constructs t)
  (modus-themes-completions '((t . (bold))))
  (modus-themes-prompts '(bold))
  :config
  (load-theme 'modus-vivendi t))

;;; Light/dark theme according to system setting

(use-package auto-dark
  :ensure t
  :custom
  (auto-dark-allow-osascript t)
  (custom-safe-themes t)
  (auto-dark-themes '((modus-vivendi) (modus-operandi)))
  :config
  (auto-dark-mode))

;;; Font configuration

(use-package fontaine
  :ensure t
  :if (display-graphic-p)
  :demand t
  :hook (after-init . (lambda ()
                        (fontaine-set-preset (or (fontaine-restore-latest-preset)
                                                 'default))))
  :custom
  (fontaine-presets '((default)
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
  (fontaine-mode))

(use-package face-remap
  :bind (("C-x C-0" . global-text-scale-adjust) ; swap the default keybinds
         ("C-x C--" . global-text-scale-adjust)
         ("C-x C-+" . global-text-scale-adjust)
         ("C-x C-=" . global-text-scale-adjust)
         ("C-x C-M-0" . text-scale-adjust)
         ("C-x C-M--" . text-scale-adjust)
         ("C-x C-M-+" . text-scale-adjust)
         ("C-x C-M-=" . text-scale-adjust)))

;;; Which-key

(use-package which-key
  :custom
  (which-key-preserve-window-configuration t)
  (which-key-idle-delay 0.6)
  (which-key-idle-secondary-delay 0.2)
  :config
  (which-key-mode))

;;; Mode line

(use-package jdp-mode-line
  :custom
  (ring-bell-function 'ignore)
  (mode-line-right-align-edge 'right-margin)
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
                  (project-mode-line jdp-mode-line-project-format)
                  jdp-mode-line-buffer-identification
                  (vc-mode (" " vc-mode))
                  "  "
                  mode-line-modes
                  mode-line-misc-info
                  mode-line-end-spaces)))

(provide 'init-ui)
;;; init-ui.el ends here
