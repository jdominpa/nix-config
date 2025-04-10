;;; jdp-modules-ui.el --- Configurations for Emacs' UI (theme, font, ...) -*- lexical-binding: t -*-

;;; Modus theme
(use-package modus-themes
  :ensure t
  :demand t
  :bind ([f5] . modus-themes-toggle)
  :custom
  (modus-themes-mixed-fonts t)
  (modus-themes-variable-pitch-ui t)
  (modus-themes-bold-constructs t)
  (modus-themes-italic-constructs t)
  (modus-themes-slanted-constructs t)
  (modus-themes-completions '((t . (extrabold))))
  (modus-themes-prompts '(extrabold))
  :config
  (load-theme 'modus-vivendi t))

;;; Font configuration
(use-package fontaine
  :ensure t
  :if (display-graphic-p)
  :bind ("C-c f" . fontaine-set-preset)
  :custom
  (fontaine-presets '((regular
                       :default-height 130)
                      (large
                       :default-height 150)
                      (t
                       :default-family "Iosevka Comfy"
                       :fixed-pitch-family nil
                       :variable-pitch-family "Iosevka Comfy Duo")))
  :custom
  (fontaine-mode t)
  :config
  (fontaine-set-preset (or (fontaine-restore-latest-preset) 'regular)))

;;; Spacious padding
(use-package spacious-padding
  :ensure t
  :if (display-graphic-p)
  :bind ([f8] . spacious-padding-mode)
  :custom
  (spacious-padding-widths
   '(:mode-line-width 3
     :right-divider-width 1
     :internal-border-width 10))
  (spacious-padding-subtle-mode-line t)
  (spacious-padding-mode t))

;;; Which-key
(use-package which-key
  :custom
  (which-key-show-prefix 'bottom)
  (which-key-popup-type 'minibuffer)
  (which-key-preserve-window-configuration t)
  (which-key-idle-delay 0.6)
  (which-key-idle-secondary-delay 0.2)
  (which-key-mode t))

;;; Modeline
(use-package jdp-modeline
  :custom
  (ring-bell-function 'ignore)
  (mode-line-right-align-edge 'right-margin)
  :config
  (setq-default mode-line-format
                '("%e"
                  jdp-modeline-kbd-macro
                  jdp-modeline-narrow
                  jdp-modeline-buffer-status
                  jdp-modeline-window-dedicated-status
                  jdp-modeline-input-method
                  jdp-modeline-meow-mode
                  jdp-modeline-buffer-identification
                  jdp-modeline-major-mode
                  jdp-modeline-process
                  jdp-modeline-vc-branch
                  jdp-modeline-eglot
                  jdp-modeline-flymake
                  jdp-modeline-envrc-status
                  mode-line-format-right-align ; Emacs 30
                  jdp-modeline-misc-info))
  
  (with-eval-after-load 'spacious-padding
    (defun jdp-modeline-spacious-indicators ()
      "Set box attribute to `'jdp-modeline-indicator-button' if spacious-padding is enabled."
      (if (bound-and-true-p spacious-padding-mode)
          (set-face-attribute 'jdp-modeline-indicator-button nil :box t)
        (set-face-attribute 'jdp-modeline-indicator-button nil :box 'unspecified)))

    ;; Run it at startup and then afterwards whenever
    ;; `spacious-padding-mode' is toggled on/off.
    (jdp-modeline-spacious-indicators)
    (add-hook 'spacious-padding-mode-hook #'jdp-modeline-spacious-indicators)))

;;; Battery display
(use-package battery
  :custom
  (display-battery-mode t))

;;; Date and time display
(use-package time
  :custom
  (display-time-format " %a %e %b, %H:%M ")
  (display-time-interval 60)
  (display-time-default-load-average nil)
  (display-time-mode t))

(provide 'jdp-modules-ui)
;;; jdp-modules-ui.el ends here
