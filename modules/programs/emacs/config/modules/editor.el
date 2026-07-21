;;; -*- lexical-binding: t -*-

(use-package emacs
  :hook (after-save . executable-make-buffer-file-executable-if-script-p)
  :config
  (setq
   ;; Do not add duplicates of the last element to the kill-ring
   kill-do-not-save-duplicates t
   ;; Save clipboard contents into kill-ring before replacing them
   save-interprogram-paste-before-kill t))

;; [autorevert] Update changed buffers automatically
(use-package autorevert
  :hook ((find-file dired-initial-position) . +editor-auto-revert-mode-init-h)
  :config
  (setopt auto-revert-use-notify nil)
  (setq auto-revert-verbose t
        auto-revert-stop-on-user-input nil
        ;; Only prompt for confirmation when buffer is unsaved
        revert-without-query '("."))

  (defun +editor-auto-revert-mode-init-h (&rest _)
    "Enable `+editor-auto-revert-mode' exactly once. This function is meant
to be added as a hook to `find-file-hook' and `dired-initial-position-hook'."
    (+editor-auto-revert-mode 1)
    (remove-hook 'find-file-hook #'+editor-auto-revert-mode-init-h)
    (remove-hook 'dired-initial-position-hook #'+editor-auto-revert-mode-init-h))

  (define-minor-mode +editor-auto-revert-mode
    "A lazy and more performant alternative to `global-auto-revert-mode'."
    :global t
    (when global-auto-revert-mode
      (setq +editor-auto-revert-mode nil))
    (let ((fn (if +editor-auto-revert-mode #'add-hook #'remove-hook)))
      (funcall fn 'window-buffer-change-functions #'+editor-auto-revert-current-buffer-h)
      (funcall fn 'window-selection-change-functions #'+editor-auto-revert-current-buffer-h)
      (if +editor-auto-revert-mode
          (add-function :after after-focus-change-function #'+editor-auto-revert-visible-buffers-h)
        (remove-function after-focus-change-function #'+editor-auto-revert-visible-buffers-h))
      (funcall fn 'after-save-hook #'+editor-auto-revert-visible-buffers-h)
      (funcall fn 'server-switch-hook #'+editor-auto-revert-current-buffer-h)))

  (defun +editor--visible-buffers ()
    "Return a list of visible buffers across all visible frames (not buried)."
    (let (buffers)
      (walk-windows (lambda (window)
                      (push (window-buffer window) buffers))
                    'no-minibuf 'visible)
      (delete-dups buffers)))

  (defun +editor-auto-revert-current-buffer-h (&rest _)
    "Auto revert current buffer, if necessary."
    (unless (or auto-revert-mode
                (active-minibuffer-window)
                (and buffer-file-name
                     auto-revert-remote-files
                     (file-remote-p buffer-file-name nil t)))
      (dlet ((auto-revert-mode t))
        (auto-revert-handler))))

  (defun +editor-auto-revert-visible-buffers-h (&rest _)
    "Auto revert stale buffers in visible windows, if necessary."
    (dolist (buf (+editor--visible-buffers))
      (with-current-buffer buf
        (+editor-auto-revert-current-buffer-h)))))

;; [ws-butler] Remove trailing whitespace on edited lines
(use-package ws-butler
  :ensure t
  :hook ((prog-mode markdown-mode) . ws-butler-mode))

;; [editorconfig] Respect project-local formatting rules
(use-package editorconfig
  :hook (find-file . editorconfig-mode))

;; [elec-pair] Automatic delimiter pairing
(use-package elec-pair
  :hook (after-init . electric-pair-mode))

;; [electric] Automatic indentation
(use-package electric
  :hook (after-init . electric-indent-mode)
  :config
  (setq electric-quote-context-sensitive t
        electric-quote-replace-double t))

;; Alternative to [hungry-delete]
(setq backward-delete-char-untabify-method 'hungry)

;; [subword] Handle camelCase
(use-package subword
  :hook ((prog-mode minibuffer-setup) . subword-mode))
