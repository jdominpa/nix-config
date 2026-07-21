;;; -*- lexical-binding: t -*-

;; Defer GC during startup, then restore sane runtime defaults later
(defvar +core-gc-cons-threshold (* 2 1024 1024)
  "Default `gc-cons-threshold' after startup.")
(defvar +core-gc-cons-percentage 0.2
  "Default `gc-cons-percentage' after startup.")

(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

(defun +core-gc-restore-defaults-h ()
  "Restore sane GC settings (`gc-cons-threshold' and `gc-cons-percentage')
after startup."
  (setq gc-cons-threshold +core-gc-cons-threshold
        gc-cons-percentage +core-gc-cons-percentage))

(add-hook 'emacs-startup-hook #'+core-gc-restore-defaults-h 95)

;; Keep early startup quiet unless we're debugging init
(setq ad-redefinition-action 'accept
      jka-compr-verbose init-file-debug
      native-comp-async-report-warnings-errors init-file-debug
      native-comp-warning-on-missing-source init-file-debug
      warning-suppress-types '((defvaralias) (lexical-binding))
      warning-inhibit-types '((files missing-lexbind-cookie)))

;; Increase process read size before any package can start a subprocess
(setq read-process-output-max (* 1024 1024))

;; In non-interactive sessions, prioritize .el file. It saves IO time
(setq load-prefer-newer noninteractive)

;; Do not resize the frame at this early stage
(setq frame-inhibit-implied-resize t)

;; Inhibit startup screen and message
(setq inhibit-startup-screen t
      inhibit-startup-echo-area-message t
      inhibit-startup-buffer-menu t
      inhibit-x-resources t
      inhibit-default-init t
      initial-scratch-message nil
      initial-major-mode 'fundamental-mode)
(advice-add #'display-startup-echo-area-message :override #'ignore)
(advice-add #'display-startup-screen :override #'ignore)

;; Suppress flashing at startup
(setq-default inhibit-redisplay t
              inhibit-message t)
(defun +core-inhibit-restore-defaults-h ()
  "Restore the value of the variables `inhibit-redisplay' and
`inhibit-message' to nil (default)."
  (setq-default inhibit-redisplay nil
                inhibit-message nil)
  (unless (daemonp)
    (redraw-frame)))
(add-hook 'window-setup-hook #'+core-inhibit-restore-defaults-h)

;; Disable toolbars and menu bars
(push '(tab-bar-lines . 1) default-frame-alist)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(push '(internal-border-width . 0) default-frame-alist)
(when (featurep 'ns)
  (push '(ns-transparent-titlebar . t) default-frame-alist))
; Set these to nil so users don't have to toggle the modes twice to reactivate
(setq menu-bar-mode nil
      tool-bar-mode nil
      scroll-bar-mode nil
      tab-bar-mode t)

;; Avoid toolbar setup work during startup. It is unnecessary while the toolbar
;; is disabled, and can be reconstructed if `tool-bar-mode' is enabled later
(when (fboundp 'tool-bar-setup)
  (advice-add #'tool-bar-setup :override #'ignore))

;; Case-insensitive pass over `auto-mode-alist' is time wasted
(setq auto-mode-case-fold nil)
