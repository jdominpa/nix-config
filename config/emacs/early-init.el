;;; early-init.el --- Early Init File -*- lexical-binding: t -*-

;; Resize the frame pixelwise
(customize-set-variable 'frame-resize-pixelwise t)

;; Do not resize the frame at this early stage.
(customize-set-variable 'frame-inhibit-implied-resize t)

;; Frame title
(customize-set-variable 'frame-title-format '("%b"))

;; Disable GUI elements
(custom-set-variables '(menu-bar-mode nil)
                      '(tool-bar-mode nil)
                      '(scroll-bar-mode nil)
                      '(use-dialog-box t)
                      '(use-file-dialog nil)
                      '(use-short-answers t)
                      '(inhibit-startup-screen t))

;; Speed up startup
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 1024 1024 20)
                  gc-cons-percentage 0.2)
            (garbage-collect)))

;; Initialise installed packages
(customize-set-variable 'package-enable-at-startup t)

(provide 'early-init)
;;; early-init.el ends here
