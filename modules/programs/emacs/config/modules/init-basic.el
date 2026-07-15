;;; init-basic.el --- General configurations for Emacs -*- lexical-binding: t -*-

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
         ("M-u" . upcase-dwim))
  :config
  (setopt
   help-window-select t
   kill-do-not-save-duplicates t
   next-error-recenter '(nil)
   mode-require-final-newline 'visit-save
   scroll-error-top-bottom t
   ;; Backup settings
   create-lockfiles nil
   backup-directory-alist `(("." . ,(locate-user-emacs-file "backups/")))
   vc-make-backup-files t
   version-control t
   backup-by-copying t
   delete-old-versions t
   kept-new-versions 5
   tramp-backup-directory-alist backup-directory-alist
   ;; Auto-save
   auto-save-default t
   auto-save-include-big-deletions t
   auto-save-list-file-prefix (locate-user-emacs-file "autosaves/")
   auto-save-file-name-transforms (list (list "\\`/[^/]*:\\([^/]*/\\)*\\([^/]*\\)\\'"
                                              ;; Prefix tramp autosaves to prevent conflicts with local ones
                                              (concat auto-save-list-file-prefix "tramp-\\2") t)
                                        (list ".*" auto-save-list-file-prefix t))
   ;; Send custom.el to oblivion
   custom-file (make-temp-file "emacs-custom-")
   ;; Always follow links when visiting a [symbolic link]
   find-file-visit-truename t
   vc-follow-symlinks t
   ring-bell-function 'ignore
   ;; Set `fill-column' indicator to 80 chars
   fill-column 80
   ;; Sentence end
   sentence-end-double-space nil
   ;; Default input method
   default-input-method "catalan-prefix"
   default-transient-input-method "catalan-prefix"
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
   )
  (setq blink-cursor-mode nil)

  (setq-default
   ;; Disable [bidirectional text] scanning for a modest performance boost.
   ;; Will improve long line display performance
   bidi-inhibit-bpa t
   bidi-paragraph-direction 'left-to-right
   bidi-display-reordering 'left-to-right
   ;; Smaller threshold to improve long line performance
   long-line-threshold 10000
   large-hscroll-threshold 10000
   syntax-wholeline-max 2000)
  )

;; Handle performance for long lines
(use-package so-long
  :config
  (global-so-long-mode))

;;; Track recently visited files and directories

(use-package recentf
  :custom
  (recentf-save-file (locate-user-emacs-file "recentf"))
  (recentf-exclude '("/tmp/" "/ssh:" ".gz" ".xz" ".zip"))
  :config
  (recentf-mode))

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
  :config
  (mouse-wheel-mode))

;;; Scrolling behaviour

(use-package emacs
  :custom
  (scroll-margin 0)
  (scroll-conservatively 1)
  (scroll-preserve-screen-position t)
  (next-screen-context-lines 3))

;;; `repeat-mode'

(use-package repeat
  :custom
  (repeat-exit-timeout 5)
  (set-mark-command-repeat-pop t)
  :config
  (repeat-mode))

;;; `auto-revert-mode'

(use-package autorevert
  :config
  (global-auto-revert-mode))

;;; `whitespace-mode'

(use-package whitespace
  :bind ("C-c z" . delete-trailing-whitespace))

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
  :config
  (save-place-mode))

;;; Transient menu for toggling modes

(use-package transient
  :ensure t
  :defines toggle-modes
  :bind ([f5]  . toggle-modes)
  :config
  (transient-define-prefix toggle-modes ()
    "Turn on and off various frequently used modes."
    [["Appearance"
      ("t" "toggle dark/light theme" modus-themes-toggle)
      ("p" "prettify symbols" (lambda () (interactive)
                              (if (derived-mode-p 'org-mode)
                                  (org-toggle-pretty-entities)
                                (call-interactively
                                 #'prettify-symbols-mode))))
      ("vl" "visual line mode" visual-line-mode)
      ("vt" "truncate lines" toggle-truncate-lines)
      ("vp" "variable pitch" variable-pitch-mode)]
     ["Org"
      :if-derived org-mode
      ("om" "modern" org-modern-mode)
      ("op" "pretty entities" org-toggle-pretty-entities)
      ("oe" "emphasis markers" (lambda () (interactive)
                                 (if (bound-and-true-p org-appear-mode)
                                     (setq-local org-hide-emphasis-markers nil)
                                   (setq-local org-hide-emphasis-markers t))
                                 (org-appear-mode 'toggle)))
      ("oi" "indent" org-indent-mode)
      ("on" "numbers" org-num-mode)]
     ["Markdown"
      :if-derived markdown-mode
      ("oe" "emphasis markers" markdown-toggle-markup-hiding)
      ("ou" "url" markdown-toggle-url-hiding)
      ("os" "src" markdown-toggle-fontify-code-blocks-natively)]
     ["Editing"
      ("r" "read only" read-only-mode)
      ("n" "display line numbers" display-line-numbers-mode)
      ("M-q" "auto fill" auto-fill-mode)
      ("fc" "fill column" set-fill-column)
      ("i" "ispell" jinx-mode)]
     ["Highlight"
      ("hl" "line" hl-line-mode)
      ("hw" "whitespace" whitespace-mode)
      ("hd" "delimiters" rainbow-delimiters-mode)]
     ["Code"
      ("a" "autocomp" (lambda () (interactive)
                        (setq-local corfu-auto (not corfu-auto))
                        (message "corfu-auto is now %s" corfu-auto))
       :transient t)
      ("g" "diff-hl" (lambda (&optional arg) (interactive "P")
                       (if (null arg) (diff-hl-mode 'toggle)
                         (let ((ref (read-string "Reference revision for diff-hl: ")))
                           (setq-local diff-hl-reference-revision ref)
                           (diff-hl-mode) (diff-hl-update)
                           (message "Showing changes against %s" ref)))))
      ("fm" "flymake" (lambda (arg) (interactive "P")
                        (if (not arg)
                            (call-interactively #'flymake-mode)
                          (let* ((linters (remq t flymake-diagnostic-functions))
                                 (active (completing-read-multiple
                                          "Exclude linters: "
                                          linters nil t)))
                            (dolist (linter active)
                              (remove-hook 'flymake-diagnostic-functions
                                           (intern-soft linter) :local))
                            (call-interactively #'flymake-mode)))))]]))

(provide 'init-basic)
;;; init-basic.el ends here
