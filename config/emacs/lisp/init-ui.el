;;; init-ui.el --- Configurations for Emacs' UI (theme, font, ...) -*- lexical-binding: t -*-

;;; Modus theme
(use-package modus-themes
  :ensure t
  :demand t
  :bind ([f5] . modus-themes-toggle)
  :custom
  (modus-themes-mixed-fonts t)
  (modus-themes-bold-constructs t)
  (modus-themes-italic-constructs t)
  (modus-themes-completions '((t . (bold))))
  (modus-themes-prompts '(bold))
  :config
  (load-theme 'modus-vivendi t))

;;; Font configuration
(use-package fontaine
  :ensure t
  :if (display-graphic-p)
  :hook (after-init . (lambda ()
                        (fontaine-set-preset (or (fontaine-restore-latest-preset)
                                                 'default))))
  :custom
  (fontaine-presets '((default)
                      (medium
                       :default-height 150)
                      (large
                       :default-height 165)
                      (t
                       :default-family "Aporetic Sans Mono"
                       :default-weight regular
                       :default-height 130
                       :fixed-pitch-family "Aporetic Sans Mono"
                       :variable-pitch-family "Aporetic Serif")))
  :custom
  (fontaine-mode t))

(use-package face-remap
  :bind (("C-x C-0" . global-text-scale-adjust) ; swap the default keybinds
         ("C-x C--" . global-text-scale-adjust)
         ("C-x C-+" . global-text-scale-adjust)
         ("C-x C-=" . global-text-scale-adjust)
         ("C-x C-M-0" . text-scale-adjust)
         ("C-x C-M--" . text-scale-adjust)
         ("C-x C-M-+" . text-scale-adjust)
         ("C-x C-M-=" . text-scale-adjust)
         :map ctl-x-x-map
         ("v" . variable-pitch-mode)))

;;; Spacious padding
(use-package spacious-padding
  :ensure t
  :if (display-graphic-p)
  :bind ([f8] . spacious-padding-mode)
  :custom
  (spacious-padding-widths
   '(:mode-line-width 1
     :right-divider-width 1
     :internal-border-width 10))
  (spacious-padding-subtle-mode-line t)
  (spacious-padding-mode t))

;;; Which-key
(use-package which-key
  :custom
  (which-key-preserve-window-configuration t)
  (which-key-idle-delay 0.6)
  (which-key-idle-secondary-delay 0.2)
  (which-key-mode t))

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
                  mode-line-end-spaces))
  
  (with-eval-after-load 'spacious-padding
    (defun jdp/mode-line-spacious-indicators ()
      "Set box attribute to `mode-line-indicator-button' if
spacious-padding is enabled."
      (if (bound-and-true-p spacious-padding-mode)
          (set-face-attribute 'jdp-mode-line-indicator-button nil :box t)
        (set-face-attribute 'jdp-mode-line-indicator-button nil :box 'unspecified)))

    ;; Run it at startup and then afterwards whenever `spacious-padding-mode' is
    ;; toggled on/off.
    (jdp/mode-line-spacious-indicators)
    (add-hook 'spacious-padding-mode-hook #'jdp/mode-line-spacious-indicators)))

(provide 'init-ui)
;;; init-ui.el ends here
