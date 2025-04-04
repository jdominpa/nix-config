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
   '(:right-divider-width 1
     :internal-border-width 10))
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
(use-package mini-echo
  :ensure t
  :custom
  (ring-bell-function 'ignore)
  (mode-line-position-column-line-format '("%l:%c,%p"))
  (mini-echo-persistent-rule
   '(:long ("meow" "shrink-path" "major-mode" "vcs"
            "buffer-position" "eglot" "flymake" "mise" "envrc")
     :short ("meow" "major-mode" "buffer-name" "buffer-position")))
  (mini-echo-persistent-function #'jdp-mini-echo-persistent-detect)
  (mini-echo-separator " | ")
  (mini-echo-mode t)
  :config
  (defun jdp-mini-echo-persistent-detect ()
    (with-current-buffer (current-buffer)
      (pcase major-mode
        ((guard (or (memq major-mode '(git-commit-elisp-text-mode git-rebase-mode))
                    (string-match-p "\\`magit-.*-mode\\'" (symbol-name major-mode))))
         '(:both ("meow" "major-mode" "project")))
        ((guard (and (fboundp 'popper-display-control-p)
                     (popper-display-control-p (current-buffer))))
         '(:both ("meow" "popper")))
        ('diff-mode '(:both ("meow" "major-mode")))
        ('ibuffer-mode '(:both ("meow" "major-mode")))
        ('dired-mode '(:both ("meow" "major-mode" "dired")))
        ('treesit--explorer-tree-mode '(:both ("meow" "major-mode" "treesit-explorer")))
        ('xwidget-webkit-mode '(:long ("meow" "shrink-path")
                                      :short ("meow" "buffer-name")))
        (_ nil)))))

(provide 'jdp-modules-ui)
;;; jdp-modules-ui.el ends here
