;;; init.el --- Init file -*- lexical-binding: t -*-

(use-package emacs
  :custom
  (custom-file (make-temp-file "emacs-custom-")) ; Send custom.el file to oblivion
  ;; Some basic settings
  (initial-buffer-choice t)             ; Always start with *scratch* buffer
  (blink-cursor-mode nil)
  ;; Backups
  (make-backup-files nil)
  (create-lockfiles nil)
  ;; Silence native compilation warning messages
  (native-comp-async-report-warnings-errors 'silent)
  :config
  (setq disabled-command-function nil))

;; "jdp-modules" is for emacs configuration modules
;; "jdp-lisp" is used for custom elisp files
(mapc
 (lambda (string)
   (add-to-list 'load-path (locate-user-emacs-file string)))
 '("jdp-modules" "jdp-lisp"))

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

(require 'jdp-modules-meow)
(require 'jdp-modules-emacs)
(require 'jdp-modules-ui)
(require 'jdp-modules-completion)
(require 'jdp-modules-search)
(require 'jdp-modules-dired)
(require 'jdp-modules-window)
(require 'jdp-modules-vc)
(require 'jdp-modules-shell)
(require 'jdp-modules-latex)
(require 'jdp-modules-org)
(require 'jdp-modules-prog)


;;; System specific settings

(when (eq system-type 'darwin)
  (setq ns-command-modifier 'meta
        ns-option-modifier 'control))

(provide 'init)
;;; init.el ends here
