;;; -*- lexical-binding: t -*-

;; This file contains the elisp code needed to correctly integrate the emacs
;; configuration in `./config' with the rest of the nix wrapper.

;; Point `magit-git-executable' to the wrapper's git executable correctly
;; (on darwin it would point to /usr/bin/git otherwise).
(with-eval-after-load 'magit
  (setq magit-git-executable "@git@"))

;; `exec-path-from-shell-initialize' replaces `exec-path' and `PATH' with the
;; login shell's values, which know nothing about the store paths the wrapper
;; puts there for the subprocesses emacs spawns. Merge them back in front.
(defun +nix-restore-runtime-path-a (&rest _)
  "Put the wrapper's binaries back in front of `exec-path' and `PATH'."
  (let ((dirs (split-string "@runtimePath@" path-separator t)))
    (setq exec-path (seq-uniq (append dirs exec-path)))
    (setenv "PATH"
            (string-join
             (seq-uniq (append dirs (split-string (getenv "PATH") path-separator t)))
             path-separator))))

(advice-add #'exec-path-from-shell-initialize :after #'+nix-restore-runtime-path-a)
