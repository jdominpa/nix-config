;;; init-editor.el --- General settings for editing text -*- lexical-binding: t -*-

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
  ;; TODO: change this to an advice like in doom
  :hook ((find-file dired-initial-position-hook) . +auto-revert-mode)
  :config
  (setopt auto-revert-use-notify nil)
  (setq auto-revert-verbose t
        auto-revert-stop-on-user-input nil
        ;; Only prompt for confirmation when buffer is unsaved
        revert-without-query '("."))

  (defun +auto-revert-visible-buffers ()
    "Return visible buffers across all frames."
    (let (buffers)
      (walk-windows (lambda (window)
                      (push (window-buffer window) buffers))
                    'no-minibuf t)
      (delete-dups buffers)))

  (defun +auto-revert-buffer-h (&rest _)
    "Auto revert current buffer, if necessary."
    (unless (or auto-revert-mode
                (active-minibuffer-window)
                (and buffer-file-name
                     auto-revert-remote-files
                     (file-remote-p buffer-file-name nil t)))
      (dlet ((auto-revert-mode t))
        (auto-revert-handler))))

  (defun +auto-revert-buffers-h (&rest _)
    "Auto revert stale buffers in visible windows, if necessary."
    (dolist (buf (+auto-revert-visible-buffers))
      (with-current-buffer buf
        (+auto-revert-buffer-h))))

  (define-minor-mode +auto-revert-mode
    "A lazy and more performant alternative to `global-auto-revert-mode'."
    :global t
    (when global-auto-revert-mode
      (setq +auto-revert-mode nil))
    (let ((fn (if +auto-revert-mode #'add-hook #'remove-hook)))
      (funcall fn 'window-buffer-change-functions #'+auto-revert-buffer-h)
      (funcall fn 'window-selection-change-functions #'+auto-revert-buffer-h)
      (funcall fn 'after-focus-change-function #'+auto-revert-buffers-h)
      (funcall fn 'after-save-hook #'+auto-revert-buffers-h)
      (funcall fn 'server-switch-hook #'+auto-revert-buffer-h))))

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

(provide 'init-editor)
;;; init-editor.el ends here
