;;; init-dired.el --- Configurations for `dired' and `ibuffer' -*- lexical-binding: t -*-

;;; Dired file manager
(use-package dired
  :hook (dired-mode . hl-line-mode)
  :custom
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'always)
  (delete-by-moving-to-trash t)
  (dired-listing-switches "-aGFhlv --group-directories-first --time-style=long-iso")
  (dired-dwim-target t)
  (dired-auto-revert-buffer #'dired-directory-changed-p)
  (dired-mouse-drag-files t))

(use-package dired-aux
  :after dired
  :custom
  (dired-isearch-filenames 'dwim)
  (dired-create-destination-dirs 'ask)
  (dired-create-destination-dirs-on-trailing-dirsep t)
  (dired-vc-rename-file t)
  (dired-do-revert-buffer t))

(use-package dired-x
  :bind (("C-x C-j" . dired-jump)
         ("C-x 4 C-j" . dired-jump-other-window))
  :custom
  (dired-clean-up-buffers-too t)
  (dired-clean-confirm-killing-deleted-buffers t))

(use-package wdired
  :after dired
  :custom
  (wdired-allow-to-change-permissions t))

;;; Ibuffer (dired-like buffer list manager)
(use-package ibuffer
  :hook (ibuffer . hl-line-mode)
  :bind ("C-x C-b" . ibuffer)
  :custom
  (ibuffer-expert t)
  (ibuffer-display-summary nil)
  (ibuffer-show-empty-filter-groups nil)
  (ibuffer-human-readable-size t)
  (ibuffer-movement-cycle nil)
  (ibuffer-default-sorting-mode 'filename/process)
  (ibuffer-old-time 48))

(provide 'init-dired)
;;; init-dired.el ends here
