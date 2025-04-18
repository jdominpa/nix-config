;;; jdp-modules-vc.el --- Configurations for version control frameworks -*- lexical-binding: t -*-

;;; Version control framework
(use-package vc
  :defer t
  :custom
  (vc-follow-symlinks t))

;;; `ediff'
(use-package ediff
  :commands (ediff-buffers ediff-files)
  :custom
  (ediff-split-window-function 'split-window-horizontally)
  (ediff-window-setup-function 'ediff-setup-windows-plain))

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

(use-package magit-todos
  :ensure t
  :after magit
  :custom
  (magit-todos-mode t))

(provide 'jdp-modules-vc)
;;; jdp-modules-vc.el ends here
