;;; General settings
(use-package emacs
  :demand t
  :bind (("C-x C-z" . nil)
         ("C-z" . nil)
         ("M-o" . other-window)
         ;; Help commands
         ("C-h y" . describe-personal-keybindings)
         ("C-h K" . describe-keymap)
         ("C-h c" . describe-char)
         ;; Commands for lines
         ("M-\\" . cycle-spacing)
         ;; Commands for text manipulation
         ("M-c" . capitalize-dwim)
         ("M-l" . downcase-dwim)
         ("M-u" . upcase-dwim))
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

;;; Repeat mode
(use-package repeat
  :custom
  (repeat-exit-timeout 5)
  (set-mark-command-repeat-pop t)
  (repeat-mode t))

;;; Auto revert mode
(use-package autorevert
  :custom
  (auto-revert-mode t))

;;; Emacs server
(use-package server
  :config
  (add-hook 'emacs-startup-hook
            (lambda ()
              (require 'server)
              (unless (server-running-p)
                (server-start)))))

;;; Save cursor position
(use-package saveplace
  :custom
  (save-place-file (locate-user-emacs-file "saveplace"))
  (save-place-forget-unreadable-files t)
  (save-place-mode t))

(provide 'jdp-core-emacs)
