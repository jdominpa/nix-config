;;; init-search.el --- Configurations for searching buffer/file contents -*- lexical-binding: t -*-

;;; Isearch and occur

(use-package isearch
  :bind (:map minibuffer-local-isearch-map
         ("M-/" . isearch-complete-edit)
         :map isearch-mode-map
         ("M-/" . isearch-complete))
  :custom
  (search-whitespace-regexp ".*?")
  (isearch-lax-whitespace t)
  (isearch-regexp-lax-whitespace nil)
  (search-highlight t)
  (isearch-lazy-highlight t)
  (isearch-lazy-count t)
  (lazy-count-prefix-format "(%s/%s) ")
  (lazy-count-suffix-format nil)
  (isearch-yank-on-move 'shift)
  (isearch-allow-scroll 'unlimited)
  (isearch-repeat-on-direction-change t))

(use-package replace
  :hook (occur-mode . hl-line-mode))

;;; Grep

(use-package grep
  :custom
  (grep-use-headings t))

;;; Avy for navigation within the screen contents

(use-package avy
  :ensure t
  :bind (("C-;" . avy-goto-char-timer)
         ("C-c ;" . avy-goto-char-timer) ; meow leader keybind
         :map isearch-mode-map
         ("C-;" . avy-isearch))
  :custom
  (avy-single-candidate-jump nil)
  :config
  (with-eval-after-load 'embark
    (defun avy-embark-act (pt)
      "Use Embark to act on the item at PT."
      (unwind-protect
          (save-excursion
            (goto-char pt)
            (embark-act))
        (select-window
         (cdr (ring-ref avy-ring 0)))
        t))
    (add-to-list 'avy-dispatch-alist '(?\. . avy-embark-act))))

(provide 'init-search)
;;; init-search.el ends here
