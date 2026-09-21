;;; -*- lexical-binding: t -*-

;; [agent-shell] ACP-driven agent sessions in native emacs buffers
(use-package agent-shell
  :ensure t
  :hook (agent-shell-mode . +ai-agent-shell-subscribe-idle-h)
  :bind (("C-c i i" . agent-shell)
         ("C-c i t" . agent-shell-toggle)
         ("C-c i c" . agent-shell-prompt-compose)
         ("C-c i w" . agent-shell-send-dwim))
  :config
  (setq agent-shell-preferred-agent-config '(preselect . claude-code)
        agent-shell-session-restore-verbosity 'full
        agent-shell-show-welcome-message nil
        agent-shell-header-style 'text
        agent-shell-markdown-table-zebra-stripe nil
        agent-shell-file-display-action '((display-buffer-reuse-window
                                           display-buffer-pop-up-window))
        agent-shell-idle-timeout '((permission-request . 10)
                                   (turn-complete . 60)))

  (defun +ai--agent-shell-notify (title body)
    "Show a desktop notification with TITLE and BODY."
    (cond ((featurep 'dbusbind)
           (require 'notifications)
           (notifications-notify :title title :body body :app-name "Emacs"))
          ((fboundp 'ns-do-applescript)
           ;; `ns-do-applescript' allows us to send a osascript notification as
           ;; a notification sent from emacs.
           (ns-do-applescript
            (format "display notification %S with title %S" body title)))
          (t (message "%s: %s" title body)
             ;; `ring-bell-function' is `ignore' globally. Re-enable the bell
             ;; temporarily for the notification.
             (let ((ring-bell-function nil))
               (ding)))))

  (defun +ai--agent-shell-notify-idle (event)
    "Notify when EVENT reports an agent shell idle awaiting input."
    (let* ((data (alist-get :data event))
           (buffer (alist-get :buffer data)))
      (when (and (buffer-live-p buffer)
                 (not (and (get-buffer-window buffer 'visible)
                           (frame-focus-state))))
        (+ai--agent-shell-notify
         (buffer-name buffer)
         (pcase (alist-get :idle-event data)
           ('permission-request "Waiting for permission")
           ('turn-complete "Turn complete")
           (other (format "Waiting (%s)" other)))))))

  (defun +ai-agent-shell-subscribe-idle-h ()
    "Subscribe the current agent shell to idle notifications."
    (agent-shell-subscribe-to
     :shell-buffer (current-buffer)
     :event 'idle
     :on-event #'+ai--agent-shell-notify-idle)))

(use-package agent-recall
  :ensure t
  :hook (agent-shell-mode . agent-recall-track-sessions)
  :bind (("C-c i v" . agent-recall-browse)
         ("C-c i r" . agent-recall-resume))
  :config
  (setq agent-recall-search-paths (mapcar #'expand-file-name
                                          '("~/dev" "~/.emacs.d"))
        agent-recall-search-function 'consult-ripgrep
        agent-recall-browse-sort 'modified-desc
        agent-recall-resume-continue-transcript t))
