;;; -*- lexical-binding: t -*-

;; [isearch]
(use-package isearch
  :bind (:map minibuffer-local-isearch-map
         ("M-/" . isearch-complete-edit)
         :map isearch-mode-map
         ("M-/" . isearch-complete)
         ([remap isearch-delete-char] . isearch-del-char))
  :config
  (setq
   ;; One space can represent a sequence of whitespaces
   isearch-lax-whitespace t
   ;; Direction change
   isearch-repeat-on-direction-change t
   ;; M-< and M-> move to the first/last occurrence of the current search string
   isearch-allow-motion t
   isearch-motion-changes-direction t
   ;; Lazy count
   isearch-lazy-count t
   lazy-highlight-buffer t
   ;; Match counter
   lazy-count-prefix-format "(%s/%s) "
   lazy-count-suffix-format nil
   isearch-yank-on-move 'shift))

(use-package grep
  :config
  (setq grep-use-headings t))

;; [goto-addr] Click to open URL
(use-package goto-addr
  :hook ((text-mode . goto-address-mode)
         (prog-mode . goto-address-prog-mode)))

;; [avy] Jump with several keystrokes
(use-package avy
  :ensure t
  :bind (("C-," . avy-goto-char-timer)
         ("C-c ," . avy-goto-char-timer) ; meow leader keybind
         :map isearch-mode-map
         ("C-," . avy-isearch))
  :config
  (with-eval-after-load 'embark
    (defun +search-avy-embark-act (pt)
      "Use Embark to act on the item at PT."
      (unwind-protect
          (save-excursion
            (goto-char pt)
            (embark-act))
        (select-window
         (cdr (ring-ref avy-ring 0)))
        t))
    (add-to-list 'avy-dispatch-alist '(?\. . +search-avy-embark-act))))
