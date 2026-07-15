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
  :custom
  (use-package-enable-imenu-support t)
  (use-package-verbose init-file-debug)
  (use-package-expand-minimally (not init-file-debug))
  (use-package-compute-statistics init-file-debug)
  (debug-on-error init-file-debug)
  (debug-on-quit init-file-debug))

;; Basic settings
(use-package emacs
  :custom
  ;; Frame settings
  (frame-resize-pixelwise t)    ; Resize the frame pixelwise
  (frame-title-format "Emacs")  ; Frame title
  ;; GUI elements
  (use-dialog-box t)
  (use-file-dialog nil)
  (use-short-answers t)
  (blink-cursor-mode nil)
  ;; Send custom.el file to oblivion
  (custom-file (make-temp-file "emacs-custom-"))
  ;; Backup settings
  (make-backup-files nil)
  (create-lockfiles nil)
  ;; Enable all commands
  (disabled-command-function nil))

;; `modules' is for emacs configuration modules
;; `lisp' is for custom elisp files and third party packages
(mapc
 (lambda (string)
   (add-to-list 'load-path (locate-user-emacs-file string)))
 '("lisp" "modules"))

(use-package package
  :custom
  (package-archives '(("gnu-elpa" . "https://elpa.gnu.org/packages/")
                      ("gnu-elpa-devel" . "https://elpa.gnu.org/devel/")
                      ("melpa" . "https://melpa.org/packages/")
                      ("nongnu" . "https://elpa.nongnu.org/nongnu/")))
  (package-archive-priorities '(("gnu-elpa" . 3)
                                ("melpa" . 2)
                                ("nongnu" . 1))))

;; Set up `load-path' and environmental variables correctly
(use-package exec-path-from-shell
  :ensure t
  :config
  (dolist (var '("SSH_AUTH_SOCK" "NIX_PATH" "NIX_PROFILES" "NIX_USER_PROFILE_DIR" "NIX_SSL_CERT_FILE"))
    (add-to-list 'exec-path-from-shell-variables var))
  (when (or (memq window-system '(mac ns x pgtk))
            (unless (memq system-type '(ms-dos windows-nt))
              (daemonp)))
    (exec-path-from-shell-initialize)))

(defvar +init-files (list
                     'init-basic
                     'init-meow
                     'init-completion
                     'init-dired
                     'init-email
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
