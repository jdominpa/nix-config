;;; -*- lexical-binding: t -*-

;; Naming conventions:
;;   +core                     core namespace (init.el/early-init.el)
;;   +MODULE                   module namespace for variables/functions defined in modules/module.el
;;   +NAMESPACE-SYMBOLNAME     public variable or function
;;   +NAMESPACE--SYMBOLNAME    private/internal variable or function
;;   +MODULE/NAME              interactive command (invoked through a keybind or M-x)
;;   +MODULE-NAME-h            non-interactive function meant to be used as a hook
;;   +MODULE-NAME-a            function designed to be used as advice to other functions
;;   +MODULE-NAME-p            predicate function

(defun +core-echo-startup-time-info-h ()
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

(defvar +core-modules (list
                       'basic
                       'completion
                       'dired
                       'editor
                       'email
                       'highlight
                       (when (eq system-type 'darwin) 'macos)
                       'meow
                       'org
                       'prog
                       'search
                       'tab-bar
                       'term
                       'ui
                       'vc
                       'window
                       'writing)
  "List of modules to be loaded by init.el.")

(let ((modules-directory (expand-file-name "modules/" user-emacs-directory)))
  (dolist (module +core-modules)
    (when module
      (load-file (concat modules-directory
                         (symbol-name module)
                         ".el")))))
