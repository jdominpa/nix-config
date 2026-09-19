;;; -*- lexical-binding: t -*-

;; [agent-shell] ACP-driven agent sessions in native emacs buffers
(use-package agent-shell
  :ensure t
  :bind (("C-c i i" . agent-shell)
         ("C-c i t" . agent-shell-toggle)
         ("C-c i p" . agent-shell-prompt-compose)
         ("C-c i w" . agent-shell-send-dwim)))

(use-package agent-recall
  :ensure t
  :hook (agent-shell-mode . agent-shell-track-sessions)
  :bind (("C-c i h" . agent-recall-browse)
         ("C-c i r" . agent-recall-resume))
  :config
  (setq agent-recall-search-paths (mapcar #'expand-file-name
                                          '("~/dev" "~/.emacs.d"))
        agent-recall-search-function 'consult-ripgrep
        agent-recall-browse-sort 'modified-desc
        agent-recall-resume-continue-transcript t))
