;;; -*- lexical-binding: t -*-

;; Optimization
(use-package emacs
  :config
  (setq
   ;; Allow [resize] by pixels
   frame-resize-pixelwise t
   window-resize-pixelwise t
   ;; Frame title
   frame-title-format "Emacs"
   ;; Suppress GUI features for consistency
   use-dialog-box nil
   use-file-dialog nil
   ;; Inhibits fontification while receiving input, which should help a little with scrolling performance
   redisplay-skip-fontification-on-input t
   ;; TODO: test if this affects memory by a lot
   ;; Font compacting can be terribly expensive, but may increase memory use
   inhibit-compacting-font-caches t
   ;; Don't show cursor or region in other windows
   highlight-nonselected-windows nil)
  (setopt cursor-in-non-selected-windows nil)
  ;; [cursor] Disable blinking
  (blink-cursor-mode -1))

;; [modus-themes]
(use-package modus-themes
  :ensure t
  :config
  (setq custom-safe-themes t ; don't prompt to confirm theme safety
        modus-themes-mixed-fonts t
        modus-themes-bold-constructs t
        modus-themes-italic-constructs t)
  (load-theme 'modus-vivendi t))

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
        '( :internal-border-width 0
           :mode-line-width 1
           :tab-width 1
           :right-divider-width 1
           :fringe-width 4)
        spacious-padding-subtle-frame-lines t))

(defcustom +mode-line-string-truncate-length 33
  "String length after which truncation should be done in small windows."
  :type 'natnum)

(defun +mode-line--string-truncate-p (str)
  "Return non-nil if STR should be truncated."
  (and (stringp str)
       (not (string-empty-p str))
       (not (string-blank-p str))
       (> (length str) +mode-line-string-truncate-length)))

(defun +mode-line--string-cut-middle (str)
  "Return truncated STR, if appropriate, else return STR.
Cut off the middle of STR by counting half of
`+mode-line-string-truncate-length' both from its beginning
and end."
  (let ((half (floor (- +mode-line-string-truncate-length 3) 2)))
    (if (+mode-line--string-truncate-p str)
        (concat (substring str 0 half) "..." (substring str (- half)))
      str)))

(defvar-local +mode-line-project-format
    '(:eval
      (when-let* ((project-name (format-mode-line project-mode-line-format))
                  ((and (buffer-file-name) ; check if buffer is a file buffer
                        (not (string-empty-p project-name)))))
        (propertize (concat (string-trim project-name) "/")
                    'face 'mode-line-buffer-id)))
  "Mode line construct to display the current project.  Meant to be used in
conjunction with `+mode-line-buffer-name'.")

(defvar-local +mode-line-buffer-identification
    '(:eval
      (propertize (+mode-line--string-cut-middle (buffer-name))
                  'face 'mode-line-buffer-id
                  'help-echo "Buffer name
mouse-1: Previous buffer\nmouse-3: Next buffer"
                  'mouse-face 'mode-line-highlight
                  'local-map mode-line-buffer-identification-keymap))
  "Mode line construct for identifying the buffer being displayed.")

;; NOTE 2025-04-10: The `risky-local-variable' is critical, as those variables
;; will not work without it.
(dolist (construct '(+mode-line-project-format
                     +mode-line-buffer-identification))
  (put construct 'risky-local-variable t))

;; [mode-line] Personal modeline config
(use-package emacs
  :config
  (setq-default
   mode-line-right-align-edge 'right-margin
   mode-line-position '("%l,%c")
   project-mode-line t
   mode-line-collapse-minor-modes '(not defining-kbd-macro
                                        envrc-mode
                                        flymake-mode)
   mode-line-format '("%e" mode-line-front-space
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
