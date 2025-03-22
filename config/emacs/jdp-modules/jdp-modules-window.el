;;; Unique buffer names
(use-package emacs
  :bind (;; Whitespace and line numbers modes
         ([f6] . whitespace-mode)
         ([f7] . display-line-numbers-mode)
         ("C-c z" . delete-trailing-whitespace))
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
  (switch-to-buffer-in-dedicated-window 'pop))

(use-package ace-window
  :ensure t
  :bind ([remap other-window] . ace-window))

(provide 'jdp-modules-window)
