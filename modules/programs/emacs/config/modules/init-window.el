;;; init-window.el --- Configurations for managing windows -*- lexical-binding: t -*-

;;; Unique buffer names

(use-package uniquify
  :custom
  (uniquify-buffer-name-style 'forward)
  (uniquify-strip-common-suffix t)
  (uniquify-after-kill-buffer-p t))

;;; Window rules and other tweaks

;;;###autoload
(defun buffer-mode (&optional buffer-or-name)
  "Returns the major mode associated with a buffer.
If buffer-or-name is nil return current buffer's mode."
  (buffer-local-value 'major-mode
                      (if buffer-or-name
                          (get-buffer buffer-or-name)
                        (current-buffer))))

(defvar jdp-help-modes-list
  '(help-mode
    TeX-special-mode)
  "List of major-modes used in documentation buffers.")

(defvar jdp-man-modes-list
  '(Man-mode
    woman-mode)
  "List of major-modes used in Man-type buffers.")

(defvar jdp-message-modes-list
  '(compilation-mode)
  "List of major-modes used in message buffers.")

(defvar jdp-repl-modes-list
  '(eshell-mode
    shell-mode
    term-mode)
  "List of major-modes used in REPL buffers.")

(defvar jdp-repl-names-list
  '("^\\*\\(?:.*?-\\)\\{0,1\\}e*shell[^z-a]*\\(?:\\*\\|<[[:digit:]]+>\\)$"
    "^\\*term.*\\*$")
  "List of buffer names used in REPL buffers.")

(defvar jdp-occur-grep-modes-list
  '(occur-mode
    grep-mode
    flymake-diagnostics-buffer-mode
    flymake-project-diagnostics-mode)
  "List of major-modes used in occur-type buffers.")

(use-package window
  :bind (("C-x }" . enlarge-window)
         ("C-x {" . shrink-window)
         ("C-x >" . enlarge-window-horizontally) ; override `scroll-right'
         ("C-x <" . shrink-window-horizontally)  ; override `scroll-left'
         :map resize-window-repeat-map
         ("}" . enlarge-window)
         ("{" . shrink-window)
         (">" . enlarge-window-horizontally)
         ("<" . shrink-window-horizontally))
  :custom
  (window-combination-resize t)
  (switch-to-buffer-in-dedicated-window 'pop)
  (display-buffer-alist
   '(;; Top windows
     ((lambda (buf act)
        (member (buffer-mode buf) jdp-occur-grep-modes-list))
      (display-buffer-reuse-window
       display-buffer-in-direction
       display-buffer-in-side-window)
      (side . top)
      (slot . 5)
      (window-height . (lambda (win)
						 (fit-window-to-buffer win 20 10)))
      (direction . above)
      (body-function . select-window))
     ;; Side windows
     ((lambda (buf act)
        (member (buffer-mode buf) jdp-man-modes-list))
      nil
      (body-function . select-window))
     ("\\*Apropos\\*"
      (display-buffer-in-side-window)
      (window-width . 65)
      (side . right)
      (slot . -2)
      (dedicated . t)
      (body-function . select-window))
     ((lambda (buf act)
        (member (buffer-mode buf) jdp-help-modes-list))
      (display-buffer-reuse-window
       display-buffer-in-side-window
       display-buffer-in-direction)
      (body-function . select-window)
      (window-width . 77)
      (direction . below)
      (side . right)
      (slot . 2)
      (window-parameters . ((split-window . #'ignore))))
     ;; Bottom windows
     ("\\*Backtrace\\*"
      (display-buffer-in-side-window)
      (window-height . 0.2)
      (side . bottom)
      (slot . -9))
     ("\\*RefTex"
      (display-buffer-in-side-window)
      (window-height . 0.25)
      (side . bottom)
      (slot . -9))
     ((lambda (buf act)
        (member (buffer-mode buf) jdp-message-modes-list))
      (display-buffer-at-bottom
       display-buffer-in-side-window)
      (window-height . 0.25)
      (side . bottom)
      (slot . -6))
     ("\\*Messages\\*"
      (display-buffer-at-bottom
       display-buffer-in-side-window
       display-buffer-in-direction)
      (window-height . (lambda (win)
                         (fit-window-to-buffer win (floor (frame-height) 5))))
      (side . bottom)
      (direction . below)
      (slot . -6)
      (body-function . select-window)
      (window-parameters . ((split-window . #'ignore))))
     ("\\*\\(?:Warnings\\|Compile-Log\\)\\*"
      (display-buffer-at-bottom
       display-buffer-in-side-window
       display-buffer-in-direction)
      (window-height . (lambda (win)
                         (fit-window-to-buffer win (floor (frame-height) 5))))
      (side . bottom)
      (direction . below)
      (slot . -5)
      (window-parameters . ((split-window . #'ignore))))
     ("\\*Async Shell Command\\*"
      (display-buffer-in-side-window)
      (window-height . 0.2)
      (side . bottom)
      (slot . -4)
      (window-parameters . ((no-other-window . t))))
     ("[Oo]utput\\*"
      (display-buffer-in-side-window)
      (window-height . (lambda (win)
                         (fit-window-to-buffer win (floor (frame-height) 2.5))))
      (side . bottom)
      (slot . -4))
     ("\\*Completions\\*"
      (display-buffer-in-side-window)
      (window-height . 0.2)
      (side . bottom)
      (slot . -2))
     ("\\*\\(?:Org \\(?:Select\\|Note\\)\\|Agenda Commands\\)\\*"
      (display-buffer-below-selected
       display-buffer-in-side-window)
      (body-function . select-window)
      (window-height . (lambda (win) (fit-window-to-buffer win nil 12)))
      (side . bottom)
      (slot . -2)
      (preserve-size . (nil . t))
      (window-parameters . ((mode-line-format . none))))
     ("\\(?:\\*Capture\\*\\|CAPTURE-.*\\)"
      (display-buffer-reuse-mode-window
       display-buffer-below-selected
       display-buffer-in-side-window)
      (window-height . 0.33)
      (side . bottom)
      (slot . -2)
      (preserve-size . (nil . t)))
     ("\\*\\(?:Calendar\\|Bookmark Annotation\\|ert\\).*"
      (display-buffer-reuse-mode-window
       display-buffer-in-side-window)
      (window-height . fit-window-to-buffer)
      (body-function . select-window)
      (side . bottom)
      (slot . -2)
      (preserve-size . (nil . t)))
     ((lambda (buf act)
        (or (seq-some (lambda (regex)
                        (string-match-p regex buf))
                      jdp-repl-names-list)
            (seq-some (lambda (mode)
                        (equal (buffer-mode buf) mode))
                      jdp-repl-modes-list)))
      (display-buffer-reuse-window
       display-buffer-in-direction
       display-buffer-in-side-window)
      (body-function . select-window)
      (window-height . 0.35)
      (window-width . 0.4)
      (direction . below)
      (side . bottom)
      (slot . 1))
     ("^\\*eldoc.*\\*$"
      (display-buffer-reuse-window
       display-buffer-in-direction
       display-buffer-in-side-window)
      (window-width . 82)
      (direction . below)
      (side . bottom)
      (slot . 2)
      (window-parameters . ((dedicated . t)
                            (split-window . #'ignore)
                            (no-other-window . t)
                            (mode-line-format . none))))
     ((lambda (buf act)
        (member (buffer-mode buf) '(ibuffer-mode bookmark-bmenu-mode)))
      (display-buffer-below-selected)
      (body-function . select-window)
      (direction . below)
      (window-height . (lambda (win) (fit-window-to-buffer win 30 7)))
      (side . bottom)
      (slot . 2))
     ((derived-mode . reb-mode)         ; M-x re-builder
      (display-buffer-reuse-mode-window
       display-buffer-below-selected)
      (window-height . 4)
      (dedicated . t)
      (preserve-size . (t . t))))))

;;; ace-window

(use-package ace-window
  :ensure t
  :bind ([remap other-window] . ace-window)
  :custom
  (aw-keys '(?a ?s ?d ?f ?g ?h ?k ?l ?\;)))

;;; Popup windows (popper)

(use-package popper
  :ensure t
  :bind (("C-`" . popper-toggle)
         ("M-`" . popper-cycle)
         ("C-M-`" . popper-toggle-type)
         :repeat-map popper-repeat-map
         ("`" . popper-cycle)
         ("~" . popper-cycle-backwards))
  :bind-keymap
  :custom
  (popper-reference-buffers
   (append jdp-help-modes-list
           jdp-message-modes-list
           jdp-man-modes-list
           jdp-repl-modes-list
           jdp-repl-names-list
           jdp-occur-grep-modes-list
           '(Custom-mode
             messages-buffer-mode)
           '(("^\\*Warnings\\*$" . hide)
             ("^\\*Compile-Log\\*$" . hide)
             "^\\*Backtrace\\*"
             "^\\*Apropos"
             "^Calc:"
             "^\\*eldoc\\*"
             "\\*TeX errors\\*"
             "\\*TeX Help\\*"
             "\\*Shell Command Output\\*"
             "\\*Async Shell Command\\*"
             "[Oo]utput\\*"
             "\\*Completions\\*")))
  (popper-display-control 'user)
  :config
  (defvar popper-repeat-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "`") #'popper-cycle)
      (define-key map (kbd "~") #'popper-cycle-backwards)
      map)
    "Repeat map used for popper commands.")
  (put 'popper-toggle 'repeat-map 'popper-repeat-map)
  (put 'popper-cycle 'repeat-map 'popper-repeat-map)
  (put 'popper-cycle-backwards 'repeat-map 'popper-repeat-map)
  (popper-echo-mode)
  (popper-mode))

(provide 'init-window)
;;; init-window.el ends here
