;;; jdp-modules-emacs.el --- General configurations for Emacs -*- lexical-binding: t -*-

;;; General settings
(use-package emacs
  :bind (("C-z" . nil)
         ("C-x C-z" . nil)
         ("M-o" . other-window)
         ;; Help commands
         ("C-h K" . describe-keymap)    ; orig. `Info-goto-emacs-key-command-node'
         ("C-h c" . describe-char)      ; orig. `describe-key-briefly'
         ("C-h y" . describe-personal-keybindings)
         ;; Commands for lines
         ("M-\\" . cycle-spacing)
         ;; Commands for text manipulation
         ("M-c" . capitalize-dwim)
         ("M-l" . downcase-dwim)
         ("M-u" . upcase-dwim)
         ;; Keymap for buffers
         :map ctl-x-x-map
         ("l" . visual-line-mode))
  :custom
  (help-window-select t)
  (next-error-recenter '(nil)))

;;; Track recently visited files and directories
(use-package recentf
  :custom
  (recentf-save-file (locate-user-emacs-file "recentf"))
  (recentf-exclude '("/tmp/" "/ssh:" ".gz" ".xz" ".zip"))
  (recentf-mode t))

;;; Mouse configuration
(use-package mouse
  :custom
  (mouse-wheel-scroll-amount
   '(1
     ((shift) . 5)
     ((meta) . 0.5)
     ((control) . text-scale)))
  (mouse-wheel-progressive-speed nil)
  (mouse-wheel-follow-mouse t)
  (mouse-wheel-mode t))

;;; Scrolling behaviour
(use-package emacs
  :custom
  (scroll-margin 0)
  (scroll-conservatively 1)
  (scroll-preserve-screen-position 'always))

;;; `repeat-mode'
(use-package repeat
  :custom
  (repeat-exit-timeout 5)
  (set-mark-command-repeat-pop t)
  (repeat-mode t))

;;; `auto-revert-mode'
(use-package autorevert
  :custom
  (auto-revert-mode t))

;;; Emacs server
(use-package server
  :hook (emacs-startup . (lambda ()
                           (unless (server-running-p)
                             (server-start)))))

;;; Save cursor position
(use-package saveplace
  :custom
  (save-place-file (locate-user-emacs-file "saveplace"))
  (save-place-forget-unreadable-files t)
  (save-place-mode t))

(provide 'jdp-modules-emacs)
;;; jdp-modules-emacs.el ends here
