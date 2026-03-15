;;; init-email.el --- Configurations for email -*- lexical-binding: t -*-

(use-package message
  :hook (message-setup . message-sort-headers)
  :custom
  (mail-user-agent 'message-user-agent)
  (message-mail-user-agent t)
  (message-elide-ellipsis "\n> [... %l lines elided]\n")
  (message-signature "\n\n-- \nJoan Domingo Pasarin")
  (mail-signature "\n\n-- \nJoan Domingo Pasarin")
  (message-citation-line-function #'message-insert-formatted-citation-line)
  (message-citation-line-format "On %a, %b %d %Y, at %H:%M:%S, %N wrote:")
  (message-ignored-cited-headers "")
  (message-kill-buffer-on-exit t)
  (message-wide-reply-confirm-recipients t)
  :config
  (defun jdp-message-citation-time-format (orig &rest args)
    "Advice for the `message-insert-formatted-citation-line' function so that
english locale is used to format the time/date."
    (let ((system-time-locale "C"))
      (apply orig args)))
  (advice-add 'message-insert-formatted-citation-line :around
              #'jdp-message-citation-time-format))

(use-package sendmail
  :after message
  :custom
  (sendmail-program (executable-find "msmtp"))
  (send-mail-function 'sendmail-send-it)
  (message-sendmail-envelope-from 'header))

(use-package mu4e
  :if (executable-find "mu")
  :bind (("C-c m" . mu4e)
         ("C-x m" . mu4e-compose-new))
  :custom
  (user-mail-address "work@jdompas.com")
  (user-full-name "Joan Domingo Pasarin")
  (mu4e-change-filenames-when-moving t)
  (mu4e-get-mail-command "mbsync -a")
  (mu4e-drafts-folder "/mail/Drafts")
  (mu4e-sent-folder "/mail/Sent")
  (mu4e-refile-folder "/mail/Archive")
  (mu4e-trash-folder "/mail/Trash")
  (mu4e-maildir-shortcuts
   '((:maildir "/mail/Inbox" :key ?i)
	 (:maildir "/mail/Drafts" :key ?d)
	 (:maildir "/mail/Sent" :key ?s)
	 (:maildir "/mail/Archive" :key ?a)
	 (:maildir "/mail/Trash" :key ?t))))

(provide 'init-email)
;;; init-email.el ends here
