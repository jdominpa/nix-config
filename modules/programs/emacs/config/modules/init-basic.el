;;; init-basic.el --- General configurations for Emacs -*- lexical-binding: t -*-

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
  (setq-default
   ;; Select help window after opening it
   help-window-select t
   kill-do-not-save-duplicates t
   ;; Backup settings
   create-lockfiles nil
   backup-directory-alist `(("." . ,(locate-user-emacs-file "backups/")))
   vc-make-backup-files t
   version-control t
   backup-by-copying t
   delete-old-versions t
   kept-new-versions 5
   ;; Auto-save
   auto-save-default t
   auto-save-include-big-deletions t
   auto-save-list-file-prefix (locate-user-emacs-file "autosaves/")
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
  (setq save-place-file (locate-user-emacs-file "saveplace")
        save-place-forget-unreadable-files t))

;;; [recentf] recently visited files
(use-package recentf
  :bind ("C-x C-r" . recentf-open-files)
  :hook (after-init . recentf-mode)
  :config
  (setopt recentf-save-file (locate-user-emacs-file "recentf"))
  (setq recentf-exclude '("/tmp/" "/ssh:" ".gz" ".xz" ".zip")))

;; [so-long] Workaround for long one-line files
(use-package so-long
  :hook (after-init . global-so-long-mode))

;; Scrolling
(use-package emacs
  :bind (("C-v" . +scroll-window)
         ("M-v" . +scroll-window-down)
         ("C-M-v" . +scroll-other-window)
         ("C-M-S-v" . +scroll-other-window-down))
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
  (defvar +scrolling-lines (/ (window-height) 3)
    "Number of lines to scroll with scroll commands")
  (defun +scroll-window () (interactive) (scroll-up +scrolling-lines))
  (defun +scroll-window-down () (interactive) (scroll-down +scrolling-lines))
  (defun +scroll-other-window () (interactive) (scroll-other-window +scrolling-lines))
  (defun +scroll-other-window-down () (interactive) (scroll-other-window-down +scrolling-lines)))

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

;; [autorevert] Update changed buffers automatically
(use-package autorevert
  :hook (after-init . global-auto-revert-mode))

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

;; TODO: maybe remove/reorganize this
;; Transient menu for toggling modes
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
