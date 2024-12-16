;;; Meow setup
(use-package meow
  :ensure t
  :config
  (defun meow-setup ()
    (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
    (meow-motion-overwrite-define-key
     '("k" . meow-next)
     '("l" . meow-prev)
     '("<escape>" . ignore))
    (meow-leader-define-key
     ;; SPC k/l will run the original command in MOTION state.
     '("k" . "H-k")
     '("l" . "H-l")
     ;; Use SPC (0-9) for digit arguments.
     '("1" . meow-digit-argument)
     '("2" . meow-digit-argument)
     '("3" . meow-digit-argument)
     '("4" . meow-digit-argument)
     '("5" . meow-digit-argument)
     '("6" . meow-digit-argument)
     '("7" . meow-digit-argument)
     '("8" . meow-digit-argument)
     '("9" . meow-digit-argument)
     '("0" . meow-digit-argument)
     '("/" . meow-keypad-describe-key)
     '("?" . meow-cheatsheet)
     ;; Window management
     '("s" . split-window-right)
     '("w" . split-window-below)
     '("q" . delete-window)
     '("a" . delete-other-windows)
     ;; Buffer management
     '("b" . switch-to-buffer)
     `("p" . ,project-prefix-map))
    (meow-normal-define-key
     ;; Numeric arguments
     '("0" . meow-expand-0)
     '("9" . meow-expand-9)
     '("8" . meow-expand-8)
     '("7" . meow-expand-7)
     '("6" . meow-expand-6)
     '("5" . meow-expand-5)
     '("4" . meow-expand-4)
     '("3" . meow-expand-3)
     '("2" . meow-expand-2)
     '("1" . meow-expand-1)
     '("-" . negative-argument)
     ;; Movement
     '("j" . meow-left)
     '("k" . meow-next)
     '("l" . meow-prev)
     '(";" . meow-right)
     '("i" . meow-back-word)
     '("o" . meow-next-word)
     '("I" . meow-back-symbol)
     '("O" . meow-next-symbol)
     '("z" . meow-find)
     '("x" . meow-till)
     '("S" . meow-goto-line)
     ;; Edit
     '("e" . meow-insert)
     '("E" . meow-open-above)
     '("r" . meow-append)
     '("R" . meow-open-below)
     '("f" . meow-change)
     '("t" . meow-delete)
     '("T" . meow-backward-delete)
     '("u" . meow-undo)
     '("U" . meow-undo-in-selection)
     ;; Kill region
     '("d" . meow-kill)
     ;; Save region
     '("c" . meow-save)
     ;; Yank
     '("v" . meow-yank)
     '("V" . meow-yank-pop)
     ;; Replace
     '("b" . meow-replace)
     '("B" . meow-query-replace-regexp)
     ;; Selection
     '("h" . meow-reverse)
     '("J" . meow-left-expand)
     '("K" . meow-next-expand)
     '("L" . meow-prev-expand)
     '(":" . meow-right-expand)
     '("s" . meow-line)
     '("w" . meow-block)
     '("W" . meow-to-block)
     '("m" . meow-join)
     '("g" . meow-cancel-selection)
     '("p" . meow-pop-selection)
     ;; Grab
     '("G" . meow-grab)
     '("P" . meow-pop-grab)
     '("y" . meow-swap-grab)
     '("Y" . meow-sync-grab)
     ;; Search
     '("n" . meow-search)
     '("/" . meow-visit)
     '("a" . meow-mark-word)
     '("A" . meow-mark-symbol)
     ;; Thing
     '("," . meow-beginning-of-thing)
     '("." . meow-end-of-thing)
     '("<" . meow-inner-of-thing)
     '(">" . meow-bounds-of-thing)
     ;; Actions
     '("q" . meow-quit)
     '("Q" . kill-current-buffer)
     '("'" . repeat)
     '("<escape>" . ignore)))

  ;; Modeline state indicators
  (customize-set-variable 'meow-replace-state-name-list
                          '((normal . "<N>")
                            (motion . "<M>")
                            (keypad . "<K>")
                            (insert . "<I>")
                            (beacon . "<B>")))

  ;; Meow extra `thing' definitions
  (meow-thing-register 'angle
                       '(pair ("<") (">"))
                       '(pair ("<") (">")))
  (meow-thing-register 'inline-math
                       '(pair ("\\(") ("\\)"))
                       '(pair ("\\(") ("\\)")))
  (customize-set-variable 'meow-char-thing-table
                          '((?f . round)
                            (?d . square)
                            (?s . curly)
                            (?a . angle)
                            (?v . paragraph)
                            (?c . line)
                            (?x . buffer)
                            (?z . visual-line)
                            (?r . string)
                            (?e . symbol)
                            (?w . defun)
                            (?q . window)
                            (?g . sentence)
                            (?m . inline-math)))

  (meow-setup)
  (meow-global-mode t))

(provide 'jdp-core-meow)
