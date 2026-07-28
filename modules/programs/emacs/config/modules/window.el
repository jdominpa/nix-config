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

  (defmacro +window--major-mode-list-predicate (mode-type)
    "Create a predicate that checks whether the major mode of a buffer is in the list of MODE-TYPE.
Meant to be used in `display-buffer-alist'."
    (let ((fn-name (intern (format "+window--%s-modes-p" mode-type)))
          (list-name (intern (format "+window--%s-modes-list" mode-type))))
      `(defun ,fn-name (buf _act)
         ,(format "Return non-nil when the major mode of BUF is a member of `%s'." list-name)
         (provided-mode-derived-p (+window--buffer-major-mode buf) ,list-name))))
  (+window--major-mode-list-predicate "help")
  (+window--major-mode-list-predicate "man")
  (+window--major-mode-list-predicate "message")
  (+window--major-mode-list-predicate "repl")
  (+window--major-mode-list-predicate "occur")

  (setq fit-window-to-buffer-horizontally t
        window-combination-resize t
        switch-to-buffer-in-dedicated-window 'pop
        display-buffer-alist
        '((+window--man-modes-p
           (display-buffer-reuse-mode-window)
           (body-function . select-window))
          ;; Top windows
          (+window--occur-modes-p
           (display-buffer-reuse-mode-window
            display-buffer-in-side-window)
           (body-function . select-window)
           (window-height . (lambda (win)
                              (fit-window-to-buffer win (floor (frame-height) 4) 10)))
           (side . top)
           (slot . 0))
          ;; Side windows
          (+window--help-modes-p
           (display-buffer-reuse-mode-window
            display-buffer-in-side-window)
           (body-function . select-window)
           (window-width . (lambda (win)
                             (fit-window-to-buffer win (floor (frame-width) 4))))
           (side . right)
           (slot . 0)
           (window-parameters . ((split-window . ignore))))
          ;; Bottom windows
          ("\\*RefTex"
           (display-buffer-in-side-window)
           (window-height . 0.25)
           (side . bottom)
           (slot . -5))
          (+window--message-modes-p
           (display-buffer-at-bottom
            display-buffer-in-side-window)
           (window-height . 0.33)
           (side . bottom)
           (slot . -4)
           (bump-use-time . t))
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
           (window-parameters . ((split-window . ignore))))
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
          (+window--repl-modes-p
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
         :repeat-map +window-popper-repeat-map
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
  (put 'popper-toggle 'repeat-map '+window-popper-repeat-map))

(use-package popper-echo
  :after popper
  :hook (popper-mode . popper-echo-mode))
