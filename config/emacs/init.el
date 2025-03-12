;;; init.el --- Personal init file -*- lexical-binding: t -*-

(use-package emacs
  :custom
  (custom-file (make-temp-file "emacs-custom-")) ; Send custom.el file to oblivion
  ;; Some basic settings
  (initial-buffer-choice t)             ; Always start with *scratch* buffer
  (blink-cursor-mode nil)
  ;; Backups
  (make-backup-files nil)
  (create-lockfiles nil)
  :config
  (setq disabled-command-function nil))

;; "jdp-core" is for emacs configuration modules
;; "jdp-lisp" is used for custom elisp files
(mapc
 (lambda (string)
   (add-to-list 'load-path (locate-user-emacs-file string)))
 '("jdp-core" "jdp-lisp"))

;; Don't automatically show native compilation warning messages
(when (native-comp-available-p)
  (customize-set-variable 'native-comp-async-report-warnings-errors 'silent))

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

(require 'jdp-core-meow)
(require 'jdp-core-emacs)
(require 'jdp-core-ui)
(require 'jdp-core-completion)
(require 'jdp-core-search)
(require 'jdp-core-dired)
(require 'jdp-core-window)
(require 'jdp-core-git)
(require 'jdp-core-shell)
(require 'jdp-core-write)
(require 'jdp-core-prog)


;;; System specific settings

(when (eq system-type 'darwin)
  (setq mac-command-modifier 'meta
        mac-option-modifier 'control))

(provide 'init)
;;; init.el ends here
