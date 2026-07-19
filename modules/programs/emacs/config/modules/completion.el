;;; -*- lexical-binding: t -*-

;;; Minibuffer

(use-package minibuffer
  :hook ((after-init . file-name-shadow-mode)
         (after-init . minibuffer-depth-indicate-mode)
         (after-init . minibuffer-electric-default-mode)
         (minibuffer-setup . cursor-intangible-mode))
  :config
  (setq
   completions-sort 'historical
   completion-ignore-case t
   completion-pcm-leading-wildcard t
   read-buffer-completion-ignore-case t
   read-file-name-completion-ignore-case t
   enable-recursive-minibuffers t
   read-answer-short t
   resize-mini-windows t
   minibuffer-default-prompt-format " [%s]")
  (setopt
   minibuffer-eldef-shorten-default t
   ;; Keep the cursor out of the read-only portions of the minibuffer
   minibuffer-prompt-properties '( read-only t
                                   intangible t
                                   cursor-intangible t
                                   face minibuffer-prompt)))

;; [savehist] Save minibuffer history
(use-package savehist
  :hook (after-init . savehist-mode)
  :config
  (setq savehist-file (expand-file-name "savehist" user-emacs-directory)
        history-length 100
        history-delete-duplicates t
        savehist-save-minibuffer-history t))

;; [vertico]
(use-package vertico
  :ensure t
  :hook (after-init . vertico-mode))

(use-package vertico-reverse
  :after vertico
  :hook (vertico-mode . vertico-reverse-mode))

(use-package vertico-directory
  :after vertico
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package vertico-quick
  :after vertico
  :bind (:map vertico-map
              ("C-," . vertico-quick-jump)))

;; [marginalia] Completion annotations
(use-package marginalia
  :ensure t
  :after vertico
  :hook (vertico-mode . marginalia-mode))

;;; Completion matching styles

(use-package orderless
  :ensure t
  :config
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles basic partial-completion))
                                        (eglot (styles orderless))
                                        (eglot-capf (styles orderless)))))

;;; Actions and search commands

(use-package embark
  :ensure t
  :bind (("C-." . embark-act)
         ("C-h B" . embark-bindings))
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  (setq embark-verbose-indicator-display-action
        `(display-buffer-in-side-window
          (side . bottom)
          (window-height . fit-window-to-buffer)
          (body-function . ,(lambda (win)
                              (with-selected-window win
                                (and mode-line-format
                                     (setq-local mode-line-format nil)))))))
  (setf (alist-get "^\\*Embark \\(?:Export\\|Collect\\).*\\*"
                   display-buffer-alist nil nil 'equal)
        '((display-buffer-in-direction)
          (window-height . (lambda (win) (fit-window-to-buffer win (floor (frame-height) 3))))
          (direction . below)
          (window-parameters . ((split-window . #'ignore)))))
  (setf (alist-get "^\\*Embark \\(?:Export\\|Collect\\).*Variables\\*"
                   display-buffer-alist nil nil 'equal)
        '((display-buffer-in-side-window)
          (body-function . (lambda (win) (select-window win)))
          (window-width . 74)
          (side . right)
          (slot . 5)
          (window-parameters . ((split-window . #'ignore))))))

(use-package embark-consult
  :ensure t
  :after (embark consult))

(use-package consult
  :ensure t
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :bind (([remap bookmark-jump] . consult-bookmark)
         ([remap list-registers] . consult-register)
         ([remap goto-line] . consult-goto-line)
         ([remap imenu] . consult-imenu)
         ([remap load-theme] . consult-theme)
         ([remap man] . consult-man)
         ([remap recentf-open-files] . consult-recent-file)
         ([remap switch-to-buffer] . consult-buffer)
         ([remap switch-to-buffer-other-tab] . consult-buffer-other-tab)
         ([remap switch-to-buffer-other-window] . consult-buffer-other-window)
         ([remap switch-to-buffer-other-frame] . consult-buffer-other-frame)
         ([remap yank-pop] . consult-yank-pop)
         ([remap repeat-complex-command] . consult-complex-command)
         ([remap project-list-buffers] . consult-project-buffer)
         ;; [goto-map] M-g bindings
         :map goto-map
         ("a" . consult-org-agenda)
         ("e" . consult-compile-error)
         ("f" . consult-flymake)
         ("i" . consult-imenu)
         ("I" . consult-imenu-multi)
         ("o" . consult-outline)
         ("m" . consult-mark)
         ("k" . consult-global-mark)
         ;; [search-map] M-s bindings
         :map search-map
         ("e" . consult-isearch-history) ; isearch integration
         ("f" . consult-fd)
         ("g" . consult-grep)
         ("G" . consult-git-grep)
         ("i" . consult-info)
         ("k" . consult-keep-lines)
         ("l" . consult-line)
         ("L" . consult-line-multi)
         ("r" . consult-ripgrep)
         ("u" . consult-focus-lines)
         :map isearch-mode-map
         ("M-s e" . consult-isearch-history) ; orig. `isearch-edit-string'
         ("M-s l" . consult-line)     ; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi) ; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history) ; orig. `next-matching-history-element'
         ("M-r" . consult-history) ; orig. `previous-matching-history-element'
         :map consult-narrow-map
         ("?" . consult-narrow-help))
  :config
  (setq consult-narrow-key "<"
        consult-async-min-input 2)
  ;; [consult-register] Configure the register formatting
  (setq register-preview-function #'consult-register-format)
  ;; This adds thin lines, sorting and hides the mode line of the window
  (advice-add #'register-preview :override #'consult-register-window)
  ;; [consult-xref] Use consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  ;; Better preview
  (consult-customize
   consult-ripgrep consult-grep consult-git-grep
   consult-bookmark consult-recent-file consult-buffer
   :preview-key "M-,")
  (consult-customize
   consult-theme
   :preview-key '("M-," :debounce 0.6 any)))

;; [consult-dir] Insert path quickly in the minibuffer
(use-package consult-dir
  :ensure t
  :bind (([remap list-directory] . consult-dir)
         :map minibuffer-local-completion-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file)))

;;; In-buffer completion

(use-package corfu
  :ensure t
  :hook (after-init . global-corfu-mode)
  :config
  (setopt text-mode-ispell-word-completion nil)
  (setq corfu-min-width 20))

(use-package corfu-history
  :after corfu
  :hook (corfu-mode . corfu-history-mode)
  :config
  (with-eval-after-load 'savehist
    (add-to-list 'savehist-additional-variables 'corfu-history)))

(use-package corfu-popupinfo
  :after corfu
  :hook (corfu-mode . corfu-popupinfo-mode)
  :config
  (setq corfu-popupinfo-delay '(nil . 1)))

(use-package corfu-quick
  :after corfu
  :bind (:map corfu-map
              ("C-," . corfu-quick-complete)))

(use-package cape
  :ensure t
  :hook ((TeX-mode LaTeX-mode org-mode markdown-mode) . +completion-add-tex-capf-h)
  :init
  (setq-default completion-at-point-functions
                (append completion-at-point-functions (list #'cape-file #'cape-dabbrev)))
  (defun +completion-add-tex-capf-h ()
    "Add `cape-tex' Capf to `completion-at-point-functions'."
    (add-hook 'completion-at-point-functions #'cape-tex nil t)))

(use-package kind-icon
  :ensure t
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

;;; Snippets and abbreviations

(use-package tempel
  :ensure t
  :bind (:map tempel-map
              ("M-{" . nil)
              ("M-}" . nil)
              ([remap backward-paragraph] . nil)
              ([remap forward-paragraph] . nil)
              ([remap backward-sentence] . tempel-previous)
              ([remap forward-sentence] . tempel-next))
  :hook ((prog-mode conf-mode text-mode) . +completion-add-tempel-capf-h)
  :init
  (defun +completion-add-tempel-capf-h ()
    "Add trigger Capf calls `tempel-complete' with trigger key / to
`completion-at-point-functions'."
    (add-hook 'completion-at-point-functions (cape-capf-trigger #'tempel-complete ?/) nil t))
  :config
  (setq tempel-path (expand-file-name "templates/*.eld" user-emacs-directory)))

(use-package abbrev
  :hook ((text-mode prog-mode) . abbrev-mode))
