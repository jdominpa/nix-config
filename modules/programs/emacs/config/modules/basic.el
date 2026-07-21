;;; -*- lexical-binding: t -*-

(use-package emacs
  :bind (("C-z" . nil)
         ("C-x C-z" . nil)
         ("C-x k" . kill-current-buffer)
         ("C-x K" . kill-buffer)
         ;; Help commands
         ("C-h K" . describe-keymap)    ; orig. `Info-goto-emacs-key-command-node'
         ("C-h c" . describe-char)      ; orig. `describe-key-briefly'
         ("C-h y" . describe-personal-keybindings)
         ;; Commands for lines
         ("M-\\" . cycle-spacing)
         ;; Commands for text manipulation
         ("M-c" . capitalize-dwim)
         ("M-l" . downcase-dwim)
         ("M-u" . upcase-dwim))
  :config
  (setq-default
   ;; Select help window after opening it
   help-window-select t
   ;; Backup settings
   create-lockfiles nil
   backup-directory-alist `(("." . ,(expand-file-name "backups/" user-emacs-directory)))
   vc-make-backup-files t
   version-control t
   backup-by-copying t
   delete-old-versions t
   kept-new-versions 5
   ;; Auto-save
   auto-save-default t
   auto-save-include-big-deletions t
   auto-save-list-file-prefix (expand-file-name "autosaves/" user-emacs-directory)
   auto-save-file-name-transforms `((".*" ,auto-save-list-file-prefix t))
   ;; Disable [bidirectional text] scanning for a modest performance boost
   ;; Will improve long line display performance
   bidi-inhibit-bpa t
   bidi-paragraph-direction 'left-to-right
   bidi-display-reordering 'left-to-right
   ;; Smaller threshold to improve long line performance
   long-line-threshold 10000
   large-hscroll-threshold 10000
   syntax-wholeline-max 2000
   ;; Send custom.el to oblivion
   custom-file (make-temp-file "emacs-custom-")
   ;; Always follow links when visiting a [symbolic link]
   find-file-visit-truename t
   vc-follow-symlinks t
   ring-bell-function 'ignore
   ;; Set [fill-column] indicator to 80 chars
   fill-column 80
   ;; Sentence end
   sentence-end-double-space nil
   ;; [tab]
   ;; Indent with 4 spaces by default.
   indent-tabs-mode nil
   tab-width 4
   ;; Indent first, otherwise run completion-at-point
   tab-always-indent 'complete
   ;; Use y-or-n to replace yes-or-no
   use-short-answers t
   ;; Inhibit switching out from `y-or-n-p' and `read-char-choice'
   y-or-n-p-use-read-key t
   read-char-choice-use-read-key t
   ;; Disable the "same file" warning, just redirect to the existing buffer
   find-file-suppress-same-file-warnings t
   ;; POSIX standard [newline]
   require-final-newline 'visit-save
   ;; Don't prompt for confirmation when creating a new file or buffer
   confirm-nonexistent-file-or-buffer nil
   ;; What-cursor-position
   what-cursor-show-names t
   ;; List only applicable commands
   read-extended-command-predicate #'command-completion-default-include-p
   ;; Enable [disabled cmds]
   disabled-command-function nil)
  ;; Encoding and locale
  (prefer-coding-system 'utf-8-unix)
  (setq default-input-method "catalan-prefix"
        default-transient-input-method "catalan-prefix"))

;; History
;;; [saveplace] save last visited place
(use-package saveplace
  :hook (after-init . save-place-mode)
  :config
  (setq save-place-file (expand-file-name "saveplace" user-emacs-directory)
        save-place-forget-unreadable-files t))

;;; [recentf] recently visited files
(use-package recentf
  :bind ("C-x C-r" . recentf-open-files)
  :hook (after-init . recentf-mode)
  :config
  (setopt recentf-save-file (expand-file-name "recentf" user-emacs-directory))
  (setq recentf-exclude '("/tmp/" "/ssh:" ".gz" ".xz" ".zip")))

;; [so-long] Workaround for long one-line files
(use-package so-long
  :hook (after-init . global-so-long-mode))

;; Scrolling
(use-package emacs
  :bind (("C-v" . +basic/scroll-window-up)
         ("M-v" . +basic/scroll-window-down)
         ("C-M-v" . +basic/scroll-other-window-up)
         ("C-M-S-v" . +basic/scroll-other-window-down))
  :config
  (setq-default
   ;; Performant and rapid scrolling
   fast-but-imprecise-scrolling t
   ;; Keep 3 lines when scrolling
   scroll-step 0
   scroll-margin 3
   scroll-up-aggressively 0.01
   scroll-down-aggressively 0.01
   scroll-conservatively 101
   scroll-preserve-screen-position t
   next-screen-context-lines 3
   ;; [hscroll]
   auto-hscroll-mode t
   hscroll-step 0
   hscroll-margin 2)
  (defun +basic/scroll-window-up (&optional arg)
    "Scroll window up ARG lines; or half the window height if ARG is nil."
    (interactive "^P")
    (scroll-up-command (or arg
                           (floor (window-body-height) 2))))
  (defun +basic/scroll-window-down (&optional arg)
    "Scroll window down ARG lines; or half the window height if ARG is nil."
    (interactive "^P")
    (scroll-down-command (or arg
                             (floor (window-body-height) 2))))
  (defun +basic/scroll-other-window-up (&optional arg)
    "Scroll the next window up ARG lines; or half the window height if ARG is
nil."
    (interactive "^P")
    (with-selected-window (other-window-for-scrolling)
      (+basic/scroll-window-up arg)))
  (defun +basic/scroll-other-window-down (&optional arg)
    "Scroll the next window down ARG lines; or half the window height if ARG
is nil."
    (interactive "^P")
    (with-selected-window (other-window-for-scrolling)
      (+basic/scroll-window-down arg)))
  (dolist (cmd '(+basic/scroll-window-up
                 +basic/scroll-window-down
                 +basic/scroll-other-window-up
                 +basic/scroll-other-window-down))
    (put cmd 'scroll-command t)))

;; [repeat] Enable repeatable commands
(use-package repeat
  :hook (after-init . repeat-mode)
  :config
  (setq repeat-exit-timeout 5
        set-mark-command-repeat-pop t))

;; [mouse] Mouse settings
(use-package mouse
  :when (display-graphic-p)
  :hook (after-init . mouse-wheel-mode)
  :config
  (setopt mouse-wheel-scroll-amount
          '(2
            ((shift) . hscroll)
            ((meta) . 10)
            ((control) . text-scale)))
  (setq mouse-wheel-progressive-speed nil
        mouse-wheel-follow-mouse t))

;; [server] Emacs server
(use-package server
  :hook (emacs-startup . (lambda ()
                           (unless (server-running-p)
                             (server-start)))))

;; [environment variables]
(use-package exec-path-from-shell
  :ensure t
  :when (or (memq window-system '(mac ns x pgtk))
            (unless (memq system-type '(ms-dos windows-nt))
              (daemonp)))
  :hook (after-init . exec-path-from-shell-initialize)
  :config
  (add-to-list 'exec-path-from-shell-variables "SSH_AUTH_SOCK"))
