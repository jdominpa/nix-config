;;; init.el --- Init file -*- lexical-binding: t -*-

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

;; `modules' is for emacs configuration modules
;; `lisp' is for custom elisp files and third party packages
(mapc
 (lambda (string)
   (add-to-list 'load-path (locate-user-emacs-file string)))
 '("lisp" "modules"))

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

(defvar +init-files (list
                     'init-basic
                     'init-meow
                     'init-completion
                     'init-dired
                     'init-editor
                     'init-email
                     'init-highlight
                     'init-latex
                     (when (eq system-type 'darwin) 'init-mac)
                     'init-org
                     'init-prog
                     'init-search
                     'init-term
                     'init-ui
                     'init-vc
                     'init-window)
  "List of init files to be loaded by init.el.")

(dolist (file +init-files)
  (when file
    (require file)))

(provide 'init)
;;; init.el ends here
