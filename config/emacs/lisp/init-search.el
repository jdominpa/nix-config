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

;;; Avy for navigation within the screen contents
(use-package avy
  :ensure t
  :bind (("M-j" . avy-goto-char-timer)
         :map isearch-mode-map
         ("M-j" . avy-isearch)))

(provide 'init-search)
;;; init-search.el ends here
