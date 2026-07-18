;;; -*- lexical-binding: t -*-

(defun +echo-startup-time-info-h ()
  "Echoes in the minibuffer information about startup time."
  (message "window-setup: %.3fs, after-init: %.3fs"
           (float-time (time-subtract nil before-init-time))
           (float-time (time-subtract after-init-time before-init-time))))
(add-hook 'window-setup-hook #'+echo-startup-time-info-h)

;; Load and configure `use-package'
(eval-when-compile
  (require 'use-package))
(use-package use-package
  :config
  (setopt use-package-enable-imenu-support t)
  (setq use-package-verbose init-file-debug
        use-package-expand-minimally (not init-file-debug)
        use-package-compute-statistics init-file-debug
        debug-on-error init-file-debug
        debug-on-quit init-file-debug))

(use-package package
  :config
  (setq
   package-archives '(("gnu-elpa" . "https://elpa.gnu.org/packages/")
                      ("gnu-elpa-devel" . "https://elpa.gnu.org/devel/")
                      ("melpa" . "https://melpa.org/packages/")
                      ("nongnu" . "https://elpa.nongnu.org/nongnu/"))
   package-archive-priorities '(("gnu-elpa" . 3)
                                ("melpa" . 2)
                                ("nongnu" . 1))))

(defvar +modules (list
                  'basic
                  'meow
                  'completion
                  'dired
                  'editor
                  'email
                  'highlight
                  'latex
                  (when (eq system-type 'darwin) 'macos)
                  'org
                  'prog
                  'search
                  'term
                  'ui
                  'vc
                  'window)
  "List of modules to be loaded by init.el.")

(let ((modules-directory (expand-file-name "modules/" user-emacs-directory)))
  (dolist (module +modules)
    (when module
      (load-file (concat modules-directory
                         (symbol-name module)
                         ".el")))))
