;;; -*- lexical-binding: t -*-

;; [tab-bar]
(use-package tab-bar
  :defer t
  :config
  (setq tab-bar-separator ""
        tab-bar-new-tab-choice t
        tab-bar-tab-name-truncated-max 20
        tab-bar-auto-width nil
        tab-bar-tab-hints t
        tab-bar-tab-name-format-functions '(tab-bar-tab-name-format-hints
                                            tab-bar-tab-name-format-truncated
                                            (lambda (name &rest _) (concat " " name " "))
                                            tab-bar-tab-name-format-face)
        tab-bar-format '(tab-bar-format-tabs))
  (setopt tab-bar-close-button-show nil
          tab-bar-select-tab-modifiers '(meta)))
