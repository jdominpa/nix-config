;;; init-window.el --- Configurations for managing windows -*- lexical-binding: t -*-

;;; Unique buffer names

(use-package uniquify
  :custom
  (uniquify-buffer-name-style 'forward)
  (uniquify-strip-common-suffix t)
  (uniquify-after-kill-buffer-p t))

;;; Window rules and other tweaks

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
   '(;; No window
     ("\\`\\*\\(Warnings\\|Org Links\\)\\*\\'"
      (display-buffer-no-window)
      (allow-no-window . t))
     ;; Bottom side window
     ("\\*\\(Org \\(Select\\|Note\\)\\|Agenda Commands\\)\\*"
      (display-buffer-in-side-window)
      (dedicated . t)
      (side . bottom)
      (slot . 0)
      (window-parameters . ((mode-line-format . none))))
     ("\\*Backtrace\\*"
      (display-buffer-in-side-window)
      (window-height . 0.2)
      (side . bottom)
      (slot . -1))
     ("\\*RefTex"
      (display-buffer-in-side-window)
      (window-height . 0.25)
      (side . bottom)
      (slot . -1))
     ("\\*Messages\\*"
      (display-buffer-at-bottom display-buffer-in-side-window display-buffer-in-direction)
      (window-height . (lambda (win)
                         (fit-window-to-buffer win (floor (frame-height) 3))))
      (side . bottom)
      (direction . below)
      (slot . -1)
      (body-function . select-window)
      (window-parameters . ((split-window . #'ignore))))
     ;; Bottom buffer (NOT side window)
     ((or . ((derived-mode . flymake-diagnostics-buffer-mode)
             (derived-mode . flymake-project-diagnostics-mode)))
      (display-buffer-reuse-mode-window display-buffer-at-bottom)
      (window-height . 0.33)
      (dedicated . t)
      (preserve-size . (t . t)))
     ("\\*Embark Actions\\*"
      (display-buffer-reuse-mode-window display-buffer-below-selected)
      (window-height . fit-window-to-buffer)
      (window-parameters . ((no-other-window . t)
                            (mode-line-format . none))))
     ("\\*Embark \\(?:Export\\|Collect\\).*\\*"
      (display-buffer-in-direction)
      (window-height . (lambda (win)
                         (fit-window-to-buffer win (floor (frame-height) 3))))
      (direction . below)
      (window-parameters . ((split-window . #'ignore))))
     ((major-mode . TeX-special-mode)
      (display-buffer-at-bottom)
      (window-height . (lambda (win)
                         (fit-window-to-buffer win (floor (frame-height) 3)))))
     ;; Below current window
     ("\\(\\*Capture\\*\\|CAPTURE-.*\\)"
      (display-buffer-reuse-mode-window display-buffer-below-selected)
      (window-height . 0.33))
     ((derived-mode . reb-mode)         ; M-x re-builder
      (display-buffer-reuse-mode-window display-buffer-below-selected)
      (window-height . 4)             ; note this is literal lines, not relative
      (dedicated . t)
      (preserve-size . (t . t)))
     ("\\*\\(Calendar\\|Bookmark Annotation\\|ert\\).*"
      (display-buffer-reuse-mode-window display-buffer-below-selected)
      (dedicated . t)
      (window-height . fit-window-to-buffer)))))

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
         ("C-M-`" . popper-toggle-type))
  :custom
  (popper-reference-buffers
   '(help-mode
     Man-mode woman-mode
     "^\\*eshell.*\\*$" eshell-mode
     "^\\*shell.*\\*$" shell-mode
     "^\\*term.*\\*$" term-mode
     occur-mode grep-mode
     Custom-mode
     compilation-mode
     messsages-buffer-mode
     ("^\\*Warnings\\*$" . hide)
     ("^\\*Compile-log\\*$" . hide)
     "^\\*Backtrace\\*"
     "\\*Async Shell Command\\*"
     "\\*TeX errors\\*"
     "\\*TeX help\\*"
     "\\*Completions\\*"
     "[Oo]utput\\*"))
  (popper-display-control 'user)
  (popper-mode t)
  (popper-echo-mode t))

(provide 'init-window)
;;; init-window.el ends here
