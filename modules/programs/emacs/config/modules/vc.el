;;; -*- lexical-binding: t -*-

;; [vc-mode] Version control interface
(use-package vc
  :config
  (setq vc-allow-async-diff t
        vc-allow-rewriting-published-history t
        vc-dir-auto-hide-up-to-date 'revert))

;; [ediff]
(use-package ediff
  :commands (ediff-buffers ediff-files)
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain
        ediff-split-window-function 'split-window-horizontally
        ediff-merge-split-window-function 'split-window-horizontally
        ediff-highlight-all-diffs t
        ;; Turn off whitespace checking
        ediff-diff-options "-w"))

;; [diff-hl] Highlight uncommitted changes using vc
(use-package diff-hl
  :ensure t
  :hook ((after-init . global-diff-hl-mode)
         (vc-dir-mode . diff-hl-dir-mode)
         (dired-mode . diff-hl-dired-mode))
  ;; :bind (:map diff-hl-mode-map
  ;;             ("C-c v v" . diff-hl-show-hunk)
  ;;             ("C-c v r" . diff-hl-revert-hunk)
  ;;             ("C-c v [" . diff-hl-previous-hunk)
  ;;             ("C-c v ]" . diff-hl-next-hunk)
  ;;             ("C-c v s" . diff-hl-stage-current-hunk)
  ;;             ("C-c v u" . diff-hl-undo-revert-hunk))
  :config
  (setq
   ;; Reduce load on remote
   diff-hl-disable-on-remote t
   ;; A slightly faster algorithm for diffing
   vc-git-diff-switches '("--histogram")
   ;; Use margins in terminal frames where fringes don't exist
   diff-hl-fallback-to-margin t)
  (with-eval-after-load 'magit
    (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh)))

;; [magit] Interactive git front-end
(use-package magit
  :ensure t
  :hook (git-commit-mode . goto-address-mode)
  :bind ("C-x g" . magit-status)
  :config
  (setq
   magit-diff-refine-hunk t
   ;; Don't autosave repo buffers, it's too magical
   magit-save-repository-buffers nil))

;; [magit-todos] Show TODOs in magit
(use-package magit-todos
  :ensure t
  :after magit
  :hook (magit-mode . magit-todos-mode))

;; [browser-at-remote] Open github/gitlab/bitbucket page
(use-package browse-at-remote
  :ensure t
  :bind (:map vc-prefix-map
              ("B" . browse-at-remote)))
