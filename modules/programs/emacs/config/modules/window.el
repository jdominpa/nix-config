;;; -*- lexical-binding: t -*-

;;; Unique buffer names

(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'forward
        uniquify-strip-common-suffix t
        uniquify-after-kill-buffer-p t))

;; [window] Window rules
(defvar +window--help-modes-list
  '(help-mode
    TeX-special-mode)
  "List of major modes used in documentation buffers.")

(defvar +window--man-modes-list
  '(Man-mode
    woman-mode)
  "List of major modes used in Man-type buffers.")

(defvar +window--message-modes-list
  '(compilation-mode)
  "List of major modes used in message buffers.")

(defvar +window--repl-modes-list
  '(eshell-mode
    shell-mode
    term-mode
    ghostel-mode)
  "List of major modes used in REPL buffers.")

(defvar +window--occur-modes-list
  '(occur-mode
    grep-mode
    flymake-diagnostics-buffer-mode
    flymake-project-diagnostics-mode)
  "List of major modes used in occur-type buffers.")

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
  :init
  (defun +window--buffer-major-mode (buffer-or-name)
    "Return the major mode associated with a buffer. If BUFFER-OR-NAME is
nil, return current buffer's major mode."
    (buffer-local-value 'major-mode
                        (if buffer-or-name
                            (get-buffer buffer-or-name)
                          (current-buffer))))
  (setq fit-window-to-buffer-horizontally t
        window-combination-resize t
        switch-to-buffer-in-dedicated-window 'pop
        display-buffer-alist
        '(((lambda (buf act)
             (member (+window--buffer-major-mode buf) +window--man-modes-list))
           (display-buffer-reuse-mode-window)
           (body-function . select-window))
          ;; Top windows
          ((lambda (buf act)
             (member (+window--buffer-major-mode buf) +window--occur-modes-list))
           (display-buffer-reuse-mode-window
            display-buffer-in-side-window)
           (body-function . select-window)
           (window-height . (lambda (win)
						      (fit-window-to-buffer win (floor (frame-height) 4) 10)))
           (side . top)
           (slot . 0))
          ;; Side windows
          ((lambda (buf act)
             (member (+window--buffer-major-mode buf) +window--help-modes-list))
           (display-buffer-reuse-mode-window
            display-buffer-in-side-window)
           (body-function . select-window)
           (window-width . (lambda (win)
                             (fit-window-to-buffer win (floor (frame-width) 4))))
           (side . right)
           (slot . 0)
           (window-parameters . ((split-window . #'ignore))))
          ;; Bottom windows
          ("\\*RefTex"
           (display-buffer-in-side-window)
           (window-height . 0.25)
           (side . bottom)
           (slot . -5))
          ((lambda (buf act)
             (member (+window--buffer-major-mode buf) +window--message-modes-list))
           (display-buffer-at-bottom
            display-buffer-in-side-window)
           (window-height . (lambda (win)
                              (fit-window-to-buffer win (floor (frame-height) 4))))
           (side . bottom)
           (slot . -4))
          ("\\*\\(?:Messages\\|Warnings\\|Compile-Log\\)\\*"
           (display-buffer-at-bottom
            display-buffer-in-side-window
            display-buffer-in-direction)
           (window-height . (lambda (win)
                              (fit-window-to-buffer win (floor (frame-height) 4))))
           (direction . below)
           (side . bottom)
           (slot . -3)
           (body-function . select-window)
           (window-parameters . ((split-window . #'ignore))))
          ("[Oo]utput\\*"
           (display-buffer-in-side-window)
           (window-height . (lambda (win)
                              (fit-window-to-buffer win (floor (frame-height) 4) 10)))
           (side . bottom)
           (slot . -2))
          ("\\*\\(?:Org \\(?:Select\\|Note\\)\\|Agenda Commands\\)\\*"
           (display-buffer-below-selected
            display-buffer-in-side-window)
           (window-height . (lambda (win)
                              (fit-window-to-buffer win nil 12)))
           (side . bottom)
           (slot . -1)
           (preserve-size . (nil . t))
           (window-parameters . ((mode-line-format . none))))
          ("\\(?:\\*Capture\\*\\|CAPTURE-.*\\)"
           (display-buffer-reuse-mode-window
            display-buffer-below-selected
            display-buffer-in-side-window)
           (window-height . 0.33)
           (side . bottom)
           (slot . -1)
           (preserve-size . (nil . t)))
          ("\\*\\(?:Calendar\\|Bookmark Annotation\\|ert\\).*"
           (display-buffer-reuse-mode-window
            display-buffer-in-side-window)
           (window-height . fit-window-to-buffer)
           (side . bottom)
           (slot . -1)
           (preserve-size . (nil . t)))
          ((lambda (buf act)
             (member (+window--buffer-major-mode buf) +window--repl-modes-list))
           (display-buffer-reuse-mode-window
            display-buffer-in-direction
            display-buffer-in-side-window)
           (body-function . select-window)
           (window-height . 0.33)
           (direction . below)
           (side . bottom)
           (slot . 1))
          ((derived-mode . reb-mode)    ; [re-builder]
           (display-buffer-reuse-mode-window
            display-buffer-below-selected)
           (window-height . 4)
           (dedicated . t)
           (preserve-size . (t . t))))))

;; *Warnings*
;; [ace-window] Switch windows avy-style
(use-package ace-window
  :ensure t
  :bind ([remap other-window] . ace-window)
  :custom
  (aw-keys '(?a ?s ?d ?f ?g ?h ?k ?l ?\;)))

;; [popper] Popup windows
(use-package popper
  :ensure t
  :bind (("C-`" . popper-toggle)
         ("M-`" . popper-cycle)
         ("C-M-`" . popper-toggle-type)
         :repeat-map popper-repeat-map
         ("`" . popper-cycle)
         ("~" . popper-cycle-backwards))
  :hook (after-init . popper-mode)
  :init
  (setq popper-reference-buffers
        (append +window--help-modes-list
                +window--message-modes-list
                +window--man-modes-list
                +window--repl-modes-list
                +window--occur-modes-list
                '(Custom-mode
                  messages-buffer-mode
                  reb-mode)
                '("\\*Warnings\\*" "\\*Compile-Log\\*"
                  "\\*Backtrace\\*"
                  "[Oo]utput\\*$" "\\*Pp Eval Output\\*$"
                  "\\*Shell Command Output\\*" "\\*Async Shell Command\\*"
                  "\\*Completions\\*"
                  "\\*Apropos\\*"
                  "\\*Calendar\\*"
                  "\\*TeX errors\\*"
                  "\\*TeX Help\\*")))
  :config
  (setq popper-display-control 'user)
  (put 'popper-toggle 'repeat-map 'popper-repeat-map)
  (put 'popper-cycle 'repeat-map 'popper-repeat-map)
  (put 'popper-cycle-backwards 'repeat-map 'popper-repeat-map))

(use-package popper-echo
  :after popper
  :hook (popper-mode . popper-echo-mode))
