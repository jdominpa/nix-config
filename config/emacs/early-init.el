;;; early-init.el --- Early init file -*- lexical-binding: t -*-

(use-package emacs
  :custom
  (package-enable-at-startup t)    ; Initialise installed packages
  (frame-resize-pixelwise t)       ; Resize the frame pixelwise
  (frame-inhibit-implied-resize t) ; Do not resize the frame at this early stage.
  (frame-title-format '("%b"))     ; Frame title
  ;; GUI elements
  (tool-bar-mode nil)
  (menu-bar-mode nil)
  (scroll-bar-mode nil)
  (use-dialog-box t)
  (use-file-dialog nil)
  (use-short-answers t)
  (inhibit-startup-screen t))

;; Speed up startup
(use-package emacs
  :hook (emacs-startup . (lambda ()
                           (setq gc-cons-threshold (* 1024 1024 20)
                                 gc-cons-percentage 0.2)
                           (garbage-collect)))
  :custom
  (gc-cons-threshold most-positive-fixnum)
  (gc-cons-percentage 0.6))

(provide 'early-init)
;;; early-init.el ends here
