;;; init.el --- Init file -*- lexical-binding: t -*-

;;; Load and configure `use-package'

(eval-when-compile
  (require 'use-package))
(require 'bind-key)

(use-package use-package
  :custom
  (use-package-verbose init-file-debug)
  (use-package-expand-minimally (not init-file-debug))
  (use-package-compute-statistics init-file-debug)
  (debug-on-error init-file-debug)
  (debug-on-quit init-file-debug))

;;; Basic settings

(use-package emacs
  :hook (emacs-startup . (lambda ()
                           (setq gc-cons-threshold (* 800000 2)
                                 gc-cons-percentage 0.1)
                           (garbage-collect)))
  :custom
  ;; Speed up startup
  (gc-cons-threshold most-positive-fixnum)
  (gc-cons-percentage 0.6)
  ;; Frame settings
  (frame-resize-pixelwise t)       ; Resize the frame pixelwise
  (frame-inhibit-implied-resize t) ; Do not resize the frame at this early stage.
  (frame-title-format "%b")        ; Frame title
  ;; GUI elements
  (tool-bar-mode nil)
  (menu-bar-mode nil)
  (scroll-bar-mode nil)
  (use-dialog-box t)
  (use-file-dialog nil)
  (use-short-answers t)
  (inhibit-startup-screen t)
  (initial-buffer-choice t) ; Always start with *scratch* buffer
  (blink-cursor-mode nil)
  ;; Send custom.el file to oblivion
  (custom-file (make-temp-file "emacs-custom-"))
  ;; Backup settings
  (make-backup-files nil)
  (create-lockfiles nil)
  ;; Silence native compilation warning messages
  (native-comp-async-report-warnings-errors 'silent)
  ;; Enable all commands
  (disabled-command-function nil))

;; `lisp' is for emacs configuration modules
;; `site-lisp' is for custom elisp files and third party packages
(mapc
 (lambda (string)
   (add-to-list 'load-path (locate-user-emacs-file string)))
 '("lisp" "site-lisp"))

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

(require 'init-meow)
(require 'init-emacs)
(require 'init-ui)
(require 'init-completion)
(require 'init-search)
(require 'init-dired)
(require 'init-window)
(require 'init-vc)
(require 'init-shell)
(require 'init-latex)
(require 'init-org)
(require 'init-prog)


;;; System specific settings

(use-package emacs
  :if (eq system-type 'darwin)
  :custom
  (ns-command-modifier 'meta))

(provide 'init)
;;; init.el ends here
