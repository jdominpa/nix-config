;;; jdp-modules-vc.el --- Configurations for version control frameworks -*- lexical-binding: t -*-

;;; Version control framework
(use-package vc
  :defer t
  :custom
  (vc-follow-symlinks t))

;;; `diff-hl-mode'
(use-package diff-hl
  :ensure t
  :custom
  (diff-hl-draw-borders nil)
  (global-diff-hl-mode t))

;;; Interactive git front-end
(use-package magit
  :ensure t
  :hook (git-commit-mode . goto-address-mode)
  :bind ("C-c g" . magit-status)
  :custom
  (magit-diff-refine-hunk t))

(provide 'jdp-modules-vc)
;;; jdp-modules-vc.el ends here
