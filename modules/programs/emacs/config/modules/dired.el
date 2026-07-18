;;; -*- lexical-binding: t -*-

;; [dired] File manager
(use-package dired
  :hook (dired-mode . turn-on-gnus-dired-mode)
  :config
  (setopt dired-mouse-drag-files t)
  (setq
   ;; Always delete and copy recursively
   dired-recursive-copies 'always
   dired-recursive-deletes 'top
   ;; Move between two dired buffers quickly
   dired-dwim-target t
   ;; Don't prompt for reverting, just do it
   dired-auto-revert-buffer #'dired-buffer-stale-p
   ;; Don't hide symlink targets in hide mode
   dired-hide-details-hide-symlink-targets nil
   ;; Don't display available disk space
   dired-free-space nil
   ;; Flags passed to ls command:
   ;;   `-a' list all files and directories
   ;;   `-h' use human readable size like 1K, 234M, 2G, etc.
   ;;   `-l' long listing format
   ;;   `-v' natural sort of (version) numbers in text
   dired-listing-switches "-ahlv")
  (when (eq system-type 'darwin)
    (if (executable-find "gls")
        ;; Use GNU ls as `gls' from `coreutils' if available
        (setq insert-directory-program "gls")
      ;; Suppress warning: "ls does not support --dired"
      (setq dired-use-ls-dired nil)))
  (when (or (not (eq system-type 'darwin))
            (executable-find "gls"))
    ;; Show directories first
    (setq dired-listing-switches (concat dired-listing-switches
                                         " --group-directories-first"))))

;; [dired-aux] Extra dired functionality
(use-package dired-aux
  :after dired
  :config
  (setq dired-isearch-filenames 'dwim
        dired-create-destination-dirs 'ask
        dired-create-destination-dirs-on-trailing-dirsep t
        dired-vc-rename-file t
        dired-do-revert-buffer t))

;; [dired-x] Extra dired functionality
(use-package dired-x
  :bind (("C-x C-j" . dired-jump)
         ("C-x 4 C-j" . dired-jump-other-window)
         :map dired-mode-map
         ("." . dired-omit-mode))
  :config
  (setq
   dired-omit-verbose nil
   ;; Hide dot files
   dired-omit-files "^\\..*\\'"
   dired-clean-up-buffers-too t
   ;; Disable the prompt about killing a Dired buffer of a deleted directory
   dired-clean-confirm-killing-deleted-buffers t))

;; [wdired] Editable dired buffers
(use-package wdired
  :after dired
  :config
  (setq wdired-allow-to-change-permissions t))

;; [dired-hacks] Several additional extensions for dired
(use-package dired-hacks
  :ensure t
  :after dired
  :init
  (use-package dired-subtree
    :bind (:map dired-mode-map
                ("TAB" . dired-subtree-toggle))
    :config
    (setq dired-subtree-line-prefix "  |  "))
  (use-package dired-collapse
    :hook (dired-mode . dired-collapse-mode)))

;; [ibuffer] dired-like buffer list manager
(use-package ibuffer
  :bind ("C-x C-b" . ibuffer)
  :config
  (setq ibuffer-show-empty-filter-groups nil
        ibuffer-human-readable-size t
        ibuffer-movement-cycle nil))
