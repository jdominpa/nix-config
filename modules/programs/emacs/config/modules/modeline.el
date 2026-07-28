;;;  -*- lexical-binding: t; -*-

(require 'let-alist)
(require 'project)


;;; Customization

(defgroup +modeline nil
  "A minimal and aesthetic mode-line."
  :group 'mode-line)

(defcustom +modeline-window-width-limit 85
  "Limit of the window width.

If `window-width' is smaller than the limit some information won't be
displayed.  It can be an integer or a float number.  A nil value means
no limit."
  :type '(choice integer
                 float
                 (const :tag "Disable" nil))
  :group '+modeline)

(defcustom +modeline-lhs-segment-list
  '(window-state
    meow
    macro
    buffer-info
    remote-host
    buffer-position)
  "Segments shown on the left hand side of the mode-line.
Call `+modeline-refresh' after changing this."
  :type '(repeat symbol)
  :group '+modeline)

(defcustom +modeline-rhs-segment-list
  '(compilation
    misc-info
    eglot
    input-method
    buffer-encoding
    major-mode
    process
    vcs
    flymake)
  "Segments shown on the right hand side of the mode-line.
Call `+modeline-refresh' after changing this."
  :type '(repeat symbol)
  :group '+modeline)

(defcustom +modeline-vcs-max-length 15
  "The maximum displayed length of the branch name of version control."
  :type 'integer
  :group '+modeline)


;;; Faces

(defgroup +modeline-faces nil
  "Faces of the `+modeline' module."
  :group '+modeline
  :group 'faces)

(defface +modeline-notice
  '((t (:inherit mode-line-emphasis :slant normal)))
  "Face for factual state indicators that are not warnings."
  :group '+modeline-faces)

(defface +modeline-info
  '((t (:inherit success)))
  "Face for info-level messages in the mode-line."
  :group '+modeline-faces)

(defface +modeline-warning
  '((t (:inherit compilation-mode-line-run)))
  "Face for warnings in the mode-line."
  :group '+modeline-faces)

(defface +modeline-urgent
  '((t (:inherit compilation-mode-line-fail)))
  "Face for errors in the mode-line."
  :group '+modeline-faces)

(defface +modeline-debug
  '((t (:inherit shadow :slant italic)))
  "Face for debug-level messages."
  :group '+modeline-faces)

(defface +modeline-panel
  '((t (:inherit highlight)))
  "Face for highlighted background segments."
  :group '+modeline-faces)

(defface +modeline-macro
  '((t (:inherit (bold +modeline-panel))))
  "Face for the macro recording indicator."
  :group '+modeline-faces)

(defface +modeline-buffer-name
  '((t (:inherit (mode-line-buffer-id bold))))
  "Face for the file name portion of the buffer identification."
  :group '+modeline-faces)

(defface +modeline-buffer-modified
  '((t (:inherit (+modeline-warning bold))))
  "Face for the buffer name when the buffer is modified."
  :group '+modeline-faces)

(defface +modeline-buffer-major-mode
  '((t (:inherit bold)))
  "Face for the major-mode segment in the mode-line."
  :group '+modeline-faces)

(defface +modeline-host
  '((t (:inherit italic)))
  "Face for remote host segment in the mode-line."
  :group '+modeline-faces)

(defface +modeline-meow-insert-state
  '((t (:inherit +modeline-info :weight normal)))
  "Face for the insert state tag in meow indicator."
  :group '+modeline-faces)

(defface +modeline-meow-beacon-state
  '((t (:inherit +modeline-urgent :weight normal)))
  "Face for the beacon state tag in meow indicator."
  :group '+modeline-faces)

(defface +modeline-meow-keypad-state
  '((t (:inherit +modeline-warning :weight normal)))
  "Face for the keypad state tag in meow indicator."
  :group '+modeline-faces)

(defface +modeline-meow-motion-state
  '((t (:inherit +modeline-debug :weight normal)))
  "Face for the motion state tag in meow indicator."
  :group '+modeline-faces)

(defface +modeline-compilation
  '((t (:inherit compilation-mode-line-run :slant italic)))
  "Face for ongoing compilation process."
  :group '+modeline-faces)


;;; Helpers

;; Active window tracking:
;;   Emacs dims the mode-line's default face in unselected windows on its own,
;;   but faces set explicitly by `propertize' are not dimmed.  Tracking the
;;   genuinely-selected window lets segments opt out of their own colour.

(defvar +modeline--active-window (selected-window)
  "The window that is really selected, as opposed to merely being redrawn.")

(defun +modeline-update-active-window-h (&rest _)
  "Record the selected window, ignoring the minibuffer."
  (let ((win (selected-window)))
    (unless (or (eq win +modeline--active-window)
                (minibuffer-window-active-p win))
      (setq +modeline--active-window win)
      (force-mode-line-update t))))
(add-hook 'window-selection-change-functions #'+modeline-update-active-window-h)
(add-function :after after-focus-change-function #'+modeline-update-active-window-h)

(defsubst +modeline--active-window-p ()
  "Return non-nil if the window currently being drawn is the selected one."
  (eq (selected-window) +modeline--active-window))

(defun +modeline--limited-window-width-p ()
  "Return non-nil when the window is narrower than `+modeline-window-width-limit'."
  (cond ((integerp +modeline-window-width-limit)
         (<= (window-total-width) +modeline-window-width-limit))
        ((floatp +modeline-window-width-limit)
         (<= (/ (window-total-width) (float (frame-width)))
             +modeline-window-width-limit))))

(defsubst +modeline--face (&optional face)
  "Return FACE when the window is active, `mode-line-inactive' otherwise.
If FACE is nil, fallback to `mode-line-active'."
  (if (+modeline--active-window-p)
      (or face 'mode-line-active)
    'mode-line-inactive))

(defsubst +modeline--display-text (text)
  "Return TEXT, propertized with `mode-line-inactive' if the window is inactive.
Otherwise, leave TEXT as is."
  (if (+modeline--active-window-p)
      text
    (propertize text 'face 'mode-line-inactive)))

(defconst +modeline--vspc (propertize " " 'display '((space :relative-width 0.5)))
  "Single thin space.")

(defsubst +modeline--truncate-file-name (str)
  "Return STR's first character or first two characters if hidden."
  (substring str 0 (if (string-prefix-p "." str) 2 1)))

(defun +modeline--shrink-path (path)
  "Return a fish-style shortened version of PATH.
Each directory component is shrinked to its first character, preserving
the leading dot in case of hidden directories.  Leading and trailing
slashes are preserved."
  (if (string-empty-p path)
      ""
    (let* ((abbrev (abbreviate-file-name path))
           (split (string-split abbrev "/" 'omit-nulls))
           (shrunk (string-join
                    (mapcar #'+modeline--truncate-file-name split) "/")))
      (concat (when (string-prefix-p "/" abbrev) "/")
              shrunk
              (when (string-suffix-p "/" abbrev) "/")))))


;;; Segment definition macro

(defvar +modeline--fn-alist nil
  "Alist of segment name and the function implementing it.")

(defvar +modeline--var-alist nil
  "Alist of segment name and a variable holding a mode-line construct.")

(defmacro +modeline-def-segment (name &rest body)
  "Define mode-line segment NAME with BODY.

If BODY is a single symbol, NAME becomes an alias for that variable and
its value is spliced into the mode-line directly.  Otherwise BODY is
wrapped in a function, which is byte-compiled immediately -- segments
run on every redisplay, so interpreted ones are noticeable."
  (declare (indent defun) (doc-string 2))
  (let* ((sym (intern (format "+modeline--%s-segment" name)))
         (docstring (if (stringp (car body))
                        (pop body)
                      (format "Mode-line segment `%s'." name))))
    (if (and (car body)
             (symbolp (car body))
             (null (cdr body)))
        `(setf (alist-get ',name +modeline--var-alist) ',(car body))
      `(progn
         (defun ,sym () ,docstring ,@body)
         (setf (alist-get ',name +modeline--fn-alist) ',sym)
         ,(unless (bound-and-true-p byte-compile-current-file)
            `(let (byte-compile-warnings)
               (unless (and (fboundp 'subr-native-elisp-p)
                            (subr-native-elisp-p (symbol-function ',sym)))
                 (byte-compile ',sym))))))))


;;; Left hand side segments

;; Dedicate window state segment

(+modeline-def-segment window-state
  "Indicator for whether the window is a (strongly) dedicated window."
  (when (window-dedicated-p)
    (let ((text (if (eq (window-dedicated-p) t) "D" "d"))
          (help-echo (if (eq (window-dedicated-p) t)
                         "Window strongly dedicated to its buffer\nmouse-1: Toggle"
                       "Window dedicated to its buffer\nmouse-1: Toggle")))
      (concat
       " "
       (propertize
        text
        'face (+modeline--face '+modeline-notice)
        'help-echo help-echo
        'mouse-face 'mode-line-highlight
        'local-map mode-line-window-dedicated-keymap)
       " "))))

;; Meow mode segment

(+modeline-def-segment meow
  "Indicator for the current meow mode."
  (when (and (bound-and-true-p meow-mode)
             meow--indicator)
    (let ((face (pcase meow--current-state
                  ('normal nil) ; use the fallback face of `+modeline--face'
                  ('insert '+modeline-meow-insert-state)
                  ('beacon '+modeline-meow-beacon-state)
                  ('keypad '+modeline-meow-keypad-state)
                  ('motion '+modeline-meow-motion-state)
                  (_ nil))))
      (propertize
       (substring-no-properties meow--indicator)
       'face (+modeline--face face)))))

;; Macro segment

(+modeline-def-segment macro
  "Macro recording indicator."
  (when (and (+modeline--active-window-p)
             (or defining-kbd-macro executing-kbd-macro))
    (let ((sep (propertize " " 'face '+modeline-panel))
          (vsep (propertize +modeline--vspc 'face '+modeline-panel)))
      (concat
       sep
       (propertize "Macro"
                   'face '+modeline-macro)
       vsep
       (propertize ">" 'face '+modeline-panel)
       sep))))

;; Buffer information segment

(defsubst +modeline--escape (str)
  "Escape %-constructs in STR.
Strings returned from an `:eval' form are still scanned for
%-constructs, so any text taken from a buffer or file name has to be
escaped."
  (string-replace "%" "%%" str))

(defsubst +modeline--unescape (str)
  "Undo `+modeline--escape' on STR.
Tooltips are not scanned for %-constructs, so escaped text has to be
restored before it is handed to `help-echo'."
  (string-replace "%%" "%" str))

(defsubst +modeline--escape-propertized (str)
  "Escape %-constructs in STR, preserving its text properties.
`string-replace' returns a bare string, which would discard the faces
that `format-mode-line' produces, so this is the variant to use on text
that has already been propertized by someone else.

The replacement is built from the match rather than written literally,
so that the escaped \"%%\" inherits the properties of the \"%\" it stands
for: the mode-line reads the face of a %-construct from the position of
its leading \"%\", and a literal replacement would land there bare."
  (replace-regexp-in-string "%" (lambda (m) (concat m m)) str t t))

(defvar-local +modeline--buffer-id nil
  "Cached (PREFIX . NAME) for the current buffer.")

(defun +modeline-update-buffer-id-h (&rest _)
  "Compute and cache the buffer identification.
The car is the project and subpath prefix, or nil; the cdr is the file
name.  Both are %-escaped and free of text properties."
  (setq +modeline--buffer-id
        (or (ignore-errors
              (when-let* ((file (buffer-file-name (buffer-base-buffer)))
                          ((not (file-remote-p file)))
                          (project (project-current))
                          (root (expand-file-name (project-root project)))
                          (relative-path (file-relative-name file root))
                          ((not (string-prefix-p ".." relative-path))))
                (cons (+modeline--escape
                       (concat (file-name-nondirectory (directory-file-name root))
                               "/"
                               (+modeline--shrink-path
                                (or (file-name-directory relative-path) ""))))
                      (+modeline--escape (file-name-nondirectory relative-path)))))
            (cons nil (+modeline--escape (buffer-name))))))
(add-hook 'find-file-hook  #'+modeline-update-buffer-id-h)
(add-hook 'after-save-hook #'+modeline-update-buffer-id-h)
(add-hook 'clone-indirect-buffer-hook #'+modeline-update-buffer-id-h)
(advice-add 'rename-buffer :after #'+modeline-update-buffer-id-h)
(advice-add 'set-visited-file-name :after #'+modeline-update-buffer-id-h)

(defun +modeline--buffer-state ()
  "Read-only and narrowing indicators, when they apply."
  (let ((state (concat
                (when buffer-read-only
                  (propertize "%1*" 'face (+modeline--face '+modeline-notice)))
                (when (buffer-narrowed-p)
                  (propertize "><" 'face (+modeline--face '+modeline-notice))))))
    (unless (string-empty-p state)
      (concat state
              +modeline--vspc))))

(defun +modeline--buffer-name ()
  "Propertized buffer identification, project-relative where possible."
  (let* ((id (or +modeline--buffer-id
                 (+modeline-update-buffer-id-h)))
         (prefix (car id))
         (name (cdr id)))
    (propertize
     (concat (when (and prefix
                        (not (+modeline--limited-window-width-p)))
               (+modeline--display-text prefix))
             (propertize name
                         'face (+modeline--face (if (and buffer-file-name
                                                         (buffer-modified-p))
                                                    '+modeline-buffer-modified
                                                  '+modeline-buffer-name))))
     'help-echo (concat (+modeline--unescape name)
                        "\nmouse-1: Previous buffer\nmouse-3: Next buffer")
     'mouse-face 'mode-line-highlight
     'local-map mode-line-buffer-identification-keymap)))

(+modeline-def-segment buffer-info
  "Buffer state indicators followed by the buffer name."
  (concat " "
          (+modeline--buffer-state)
          (+modeline--buffer-name)))

;; Remote host segment

(+modeline-def-segment remote-host
  "Hostname for remote buffers."
  (when default-directory
    (when-let* ((host (file-remote-p default-directory 'host)))
      (propertize (concat "@" host)
                  'face (+modeline--face '+modeline-host)))))

;; In-buffer position segment

(+modeline-def-segment buffer-position
  "Line and column numbers and buffer percentatge position."
  (let ((face (+modeline--face))
        (help-echo "Buffer position and percentage\n\
mouse-1: Display Line and Column Mode Menu"))
    `(" "
      ;; Line and column
      (:propertize
       (line-number-mode
        (column-number-mode "%l:%c" "L%l")
        (column-number-mode "C%c"))
       face ,face
       help-echo ,help-echo
       mouse-face mode-line-highlight
       local-map mode-line-column-line-number-mode-map)
      (:eval (when (or line-number-mode column-number-mode) " "))
      ;; Percent position
      (:propertize
       ("" mode-line-percent-position)
       face ,face
       help-echo ,help-echo
       mouse-face mode-line-highlight
       local-map mode-line-column-line-number-mode-map)
      " ")))

;;; Right hand side segments

;; In-progress compilation segment

(+modeline-def-segment compilation
  "Indicator for ongoing compilation."
  (when (bound-and-true-p compilation-in-progress)
    (concat
     " "
     (propertize "[Compiling]"
                 'face (+modeline--face '+modeline-compilation)
                 'help-echo "Ongoing compilation\nmouse-1: Goto Buffer"
                 'mouse-face 'mode-line-highlight
                 'local-map (when (fboundp 'compilation-goto-in-progress-buffer)
                              (make-mode-line-mouse-map
                               'mouse-1
                               #'compilation-goto-in-progress-buffer)))
     " ")))

;; Misc. info segment

(+modeline-def-segment misc-info
  "Mode-line construct for miscellaneous information.
By default, this shows the information specified by `global-mode-string'."
  (when (not (+modeline--limited-window-width-p))
    ;; `format-mode-line' has already turned "%%" into a literal "%", which the
    ;; scan of this segment's own return value would read as a construct again.
    (+modeline--display-text
     (+modeline--escape-propertized (format-mode-line mode-line-misc-info)))))

;; Eglot state segment

(defvar-local +modeline--eglot nil
  "Cached alist of eglot state with keys SERVER, NICK, HELP-ECHO and LOCAL-MAP.
Only what cannot change while a server is managing the buffer is cached
here; the pending-request count and the last error are read live by the
segment.")

(defun +modeline-update-eglot-h ()
  "Update eglot cached state for the mode-line."
  (setq +modeline--eglot
        (when-let* ((server (and (eglot-managed-p) (eglot-current-server))))
          (let* ((nick (eglot-project-nickname server))
                 (server-info (eglot--server-info server))
                 (server-name (or (plist-get server-info :name)
                                  (jsonrpc-name server)
                                  ""))
                 (major-modes (or (eglot--major-modes server) "")))
            `((server . ,server)
              (nick . ,nick)
              (help-echo . ,(format "Eglot connected [%s]\n%s %s
mouse-1: Display minor mode menu
mouse-3: LSP server control menu"
                                    nick server-name major-modes))
              (local-map . ,(let ((map (make-sparse-keymap)))
                              (define-key map [mode-line mouse-1] eglot-menu)
                              (define-key map [mode-line mouse-3] eglot-server-menu)
                              map)))))))
(add-hook 'eglot-managed-mode-hook #'+modeline-update-eglot-h)

(defun +modeline--eglot-pending-count (server)
  "Get count of pending eglot requests to SERVER."
  (if (fboundp 'jsonrpc-continuation-count)
      (jsonrpc-continuation-count server)
    (hash-table-count (jsonrpc--request-continuations server))))

(defun +modeline--eglot-face (server nick)
  "Return the face for SERVER, whose project nickname is NICK.
This is evaluated on redisplay rather than cached: at the moment eglot
starts managing a buffer there is by definition no error and nothing
pending, so a cached face could only ever report a healthy server."
  (cond ((jsonrpc-last-error server) '+modeline-urgent)
        ((plusp (+modeline--eglot-pending-count server)) '+modeline-warning)
        (nick '+modeline-info)
        (t '+modeline-notice)))

(with-eval-after-load 'eglot
  (setq mode-line-misc-info
        (delq (assq 'eglot--managed-mode mode-line-misc-info)
              mode-line-misc-info)))

(+modeline-def-segment eglot
  "The lsp server and eglot state."
  (when (and (bound-and-true-p eglot--managed-mode)
             +modeline--eglot)
    (let-alist +modeline--eglot
      (concat
       " "
       (propertize eglot-menu-string
                   'face (+modeline--face (+modeline--eglot-face .server .nick))
                   'help-echo .help-echo
                   'mouse-face 'mode-line-highlight
                   'local-map .local-map)
       " "))))

;; Input method segment

(+modeline-def-segment input-method
  "The current input method."
  (when current-input-method
    (concat " "
            (propertize current-input-method-title
                        'face (+modeline--face '+modeline-notice)
                        'help-echo (concat "Current input method: "
                                           current-input-method
                                           "\n\
mouse-2: Disable input method\n\
mouse-3: Describe current input method")
                        'mouse-face 'mode-line-highlight
                        'local-map mode-line-input-method-map)
            " ")))

;; Buffer encoding segment

(defvar-local +modeline--buffer-encoding nil
  "Cached end-of-line style and coding system of the current buffer.")

(defun +modeline-update-buffer-encoding-h (&rest _)
  "Compute and cache the buffer's eol style and coding system."
  (setq +modeline--buffer-encoding
        (let* (;; eol type
               (eol (coding-system-eol-type buffer-file-coding-system))
               ;; coding system
               (sys (coding-system-plist buffer-file-coding-system))
               (sym (if (memq (plist-get sys :category)
                              '(coding-category-undecided coding-category-utf-8))
                        'utf-8
                      (plist-get sys :name)))
               (parts (delq nil
                            (list (pcase eol
                                    (0 "LF")
                                    (1 "CRLF")
                                    (2 "CR")
                                    (_ nil))
                                  (upcase (symbol-name sym))))))
          (string-join parts " "))))
(add-hook 'find-file-hook #'+modeline-update-buffer-encoding-h)
(add-hook 'after-save-hook #'+modeline-update-buffer-encoding-h)
(add-hook 'after-revert-hook #'+modeline-update-buffer-encoding-h)
(add-hook 'after-change-major-mode-hook #'+modeline-update-buffer-encoding-h)
(advice-add 'set-buffer-file-coding-system :after #'+modeline-update-buffer-encoding-h)

(+modeline-def-segment buffer-encoding
  "End-of-line style and coding system."
  (concat
   " "
   (propertize (or +modeline--buffer-encoding
                   (+modeline-update-buffer-encoding-h))
               'face (+modeline--face)
               'help-echo 'mode-line-mule-info-help-echo
               'mouse-face 'mode-line-highlight
               'local-map mode-line-coding-system-map)
   " "))

;; Major mode segment

(+modeline-def-segment major-mode
  "Major mode, including environment and text-scale info."
  (concat
   " "
   (propertize (+modeline--escape-propertized (format-mode-line mode-name))
               'face (+modeline--face '+modeline-buffer-major-mode)
               'mouse-face 'mode-line-highlight
               'help-echo "Major mode\n\
mouse-1: Display major mode menu\n\
mouse-2: Show help for major mode\n\
mouse-3: Toggle minor modes"
               'local-map mode-line-major-mode-keymap)
   (and (boundp 'text-scale-mode-amount)
        (/= text-scale-mode-amount 0)
        (propertize
         (format (if (> text-scale-mode-amount 0) " (%+d)" " (%d)")
                 text-scale-mode-amount)
         'face (+modeline--face '+modeline-buffer-major-mode)))
   " "))

;; Process info segment

(+modeline-def-segment process
  "Process info."
  (when-let* ((seg (string-trim (format-mode-line mode-line-process)))
              ((not (string-empty-p seg))))
    (concat " "
            (+modeline--display-text (+modeline--escape-propertized seg))
            " ")))

;; Version control segment

(defvar-local +modeline--vcs nil
  "Cached alist of vcs information with keys ICON, TEXT, HELP-ECHO, LOCAL-MAP and IN-GIT-WORKTREE.")

(defvar +modeline--git-worktree-cache (make-hash-table :test 'equal)
  "Cache mapping a \".git\" file name to whether it belongs to a linked worktree.
A checkout cannot stop being a worktree while it exists, so the answer is
read from disk once per repository instead of on every save.")

(defun +modeline--git-worktree-p (git-dir)
  "Return non-nil if the checkout rooted at GIT-DIR is a linked git worktree.
In a worktree \".git\" is a file rather than a directory, but the same
happens in a submodule, whose gitdir points into \".git/modules/\"
instead."
  (let* ((dot-git (expand-file-name ".git" git-dir))
         (cached (gethash dot-git +modeline--git-worktree-cache 'unknown)))
    (if (eq cached 'unknown)
        (puthash dot-git
                 (and (file-regular-p dot-git)
                      ;; Read defensively: this runs from `after-save-hook',
                      ;; where a signal would reach the user.
                      (ignore-errors
                        (with-temp-buffer
                          (insert-file-contents dot-git nil 0 512)
                          (goto-char (point-min))
                          (looking-at-p "gitdir:.*/worktrees/"))))
                 +modeline--git-worktree-cache)
      cached)))

(defun +modeline-update-vcs-h (&rest _)
  "Update vcs cached state for the mode-line."
  (setq +modeline--vcs
        (when (and vc-mode buffer-file-name)
          (let* ((backend (vc-backend buffer-file-name))
                 (state (vc-state buffer-file-name backend))
                 (icon (cond ((memq state '(edited added))
                              (propertize "*" 'face '+modeline-warning))
                             ((eq state 'needs-merge)
                              (propertize "?" 'face '+modeline-warning))
                             ((eq state 'needs-update)
                              (propertize "!" 'face '+modeline-warning))
                             ((memq state '(removed conflict unregistered))
                              (propertize "!" 'face '+modeline-urgent))
                             (t (propertize "@" 'face '+modeline-info))))
                 (str (or (and vc-display-status
                               vc-mode
                               (cadr (split-string (string-trim vc-mode) "^[A-Z]+[-:]+")))
                          ""))
                 (face (or (cdr (assq state
                                      '((edited . (+modeline-warning bold))
                                        (added . (+modeline-warning bold))
                                        (needs-merge . (+modeline-warning bold))
                                        (needs-update . (+modeline-warning bold))
                                        (removed . (+modeline-urgent bold))
                                        (conflict . (+modeline-urgent bold))
                                        (unregistered . (+modeline-urgent bold)))))
                           '(+modeline-info bold)))
                 ;; Escaped after truncating, so that a "%" in a branch name
                 ;; still counts as one character against the length limit.
                 (text (propertize (+modeline--escape
                                    (if (length> str +modeline-vcs-max-length)
                                        (concat
                                         (substring str 0 (- +modeline-vcs-max-length 3))
                                         (if (char-displayable-p ?…) "…" "..."))
                                      str))
                                   'face face))
                 (help-echo (get-text-property 1 'help-echo vc-mode))
                 (local-map (get-text-property 1 'local-map vc-mode))
                 (in-git-worktree (when-let* (((eq backend 'Git))
                                              (git-dir (and buffer-file-name
                                                            (not (file-remote-p buffer-file-name)) ; avoid tramp hangs
                                                            (locate-dominating-file buffer-file-name ".git"))))
                                    (+modeline--git-worktree-p git-dir))))
            `((icon . ,icon)
              (text . ,text)
              (help-echo . ,help-echo)
              (local-map . ,local-map)
              (in-git-worktree . ,in-git-worktree))))))
(add-hook 'after-save-hook #'+modeline-update-vcs-h)
(advice-add #'vc-refresh-state :after #'+modeline-update-vcs-h)

(+modeline-def-segment vcs
  "Current VC branch."
  (when +modeline--vcs
    (let-alist +modeline--vcs
      (let ((worktree-indicator (when .in-git-worktree
                                  (propertize "WT" 'face '+modeline-notice))))
        (concat
         " "
         (propertize (concat
                      (+modeline--display-text .icon)
                      (when worktree-indicator
                        (concat
                         +modeline--vspc
                         (+modeline--display-text worktree-indicator)))
                      (unless (+modeline--limited-window-width-p)
                        (concat
                         +modeline--vspc
                         (+modeline--display-text .text))))
                     'help-echo .help-echo
                     'mouse-face 'mode-line-highlight
                     'local-map .local-map)
         " ")))))

;; Flymake state segment

(defun +modeline--flymake-count-errors ()
  "Return an alist of diagnostic counts for the current buffer."
  (let ((warning-level (warning-numeric-level :warning))
        (note-level (warning-numeric-level :debug))
        (note 0) (warning 0) (error 0))
    (maphash (lambda (_b state)
               (dolist (diag (flymake--state-diags state))
                 (let ((severity (flymake--lookup-type-property
                                  (flymake--diag-type diag) 'severity
                                  (warning-numeric-level :error))))
                   (cond ((> severity warning-level) (incf error))
                         ((> severity note-level) (incf warning))
                         (t (incf note))))))
             flymake--state)
    `((note . ,note) (warning . ,warning) (error . ,error))))

(defvar +modeline--flymake-keymap nil
  "Keymap used by the flymake segment.")
(with-eval-after-load 'flymake
  (setq +modeline--flymake-keymap
        (let ((map (make-sparse-keymap)))
          (define-key map [mode-line down-mouse-1] flymake-menu)
          (define-key map [mode-line mouse-2]
                      (lambda () (interactive) (describe-function 'flymake-mode)))
          map)))

(defvar-local +modeline--flymake nil
  "Cached state of flymake errors count.
An alist with keys NOTE, WARNING, ERROR, KNOWN, RUNNING, ALL-DISABLED
and SOME-WAITING, or nil when flymake is inactive.")

(defun +modeline-update-flymake-h (&rest _)
  "Update flymake cached data for the mode-line."
  (setq +modeline--flymake
        (when (and (bound-and-true-p flymake-mode)
                   (bound-and-true-p flymake--state))
          (let ((running (flymake-running-backends))
                (disabled (flymake-disabled-backends))
                (reported (flymake-reporting-backends)))
            (append (+modeline--flymake-count-errors)
                    `((known . ,(hash-table-count flymake--state))
                      (running . ,(length running))
                      (all-disabled . ,(and disabled (null running) t))
                      (some-waiting . ,(and (seq-difference running reported) t))))))))
(advice-add #'flymake--handle-report :after #'+modeline-update-flymake-h)
(add-hook 'flymake-mode-hook #'+modeline-update-flymake-h)
(add-hook 'window-buffer-change-functions #'+modeline-update-flymake-h)

(+modeline-def-segment flymake
  "Flymake diagnostic counters, when `flymake-mode' is enabled."
  (when +modeline--flymake
    (let-alist +modeline--flymake
      (let* ((count (+ .error .warning .note))
             (seg (cond
                   ((zerop .known)
                    (propertize "!" 'face '+modeline-urgent))
                   (.all-disabled
                    (propertize "!" 'face '+modeline-warning))
                   (t
                    (concat
                     (when .some-waiting
                       (concat (propertize "*" 'face '+modeline-debug)
                               +modeline--vspc))
                     (if (+modeline--limited-window-width-p)
                         (let ((face (cond ((> .error 0) '+modeline-urgent)
                                           ((> .warning 0) '+modeline-warning)
                                           (t '+modeline-info))))
                           (concat
                            (propertize "!" 'face face)
                            +modeline--vspc
                            (propertize (number-to-string count) 'face face)))
                       (concat
                        (propertize "!" 'face '+modeline-urgent)
                        +modeline--vspc
                        (propertize (number-to-string .error) 'face '+modeline-urgent)
                        +modeline--vspc
                        (propertize "!" 'face '+modeline-warning)
                        +modeline--vspc
                        (propertize (number-to-string .warning) 'face '+modeline-warning)
                        +modeline--vspc
                        (propertize "!" 'face '+modeline-info)
                        +modeline--vspc
                        (propertize (number-to-string .note) 'face '+modeline-info))))))))
        (concat
         " "
         (propertize
          (+modeline--display-text seg)
          'help-echo (concat "Flymake\n"
                             (cond
                              (.some-waiting "Checking...")
                              ((zerop .known) "No Checker")
                              (.all-disabled "All Checkers Disabled")
                              (t (format "%d/%d backends running\nerror: %d, warning: %d, note: %d"
                                         .running .known .error .warning .note)))
                             "\nmouse-1: Display minor mode menu\nmouse-2: Show help for minor mode")
          'mouse-face 'mode-line-highlight
          'local-map +modeline--flymake-keymap)
         " ")))))


;;; Assembly of segments

(defun +modeline--prepare-segments (segments)
  "Turn SEGMENTS into a list of mode-line constructs."
  (mapcar
   (lambda (seg)
     (cond
      ((stringp seg) seg)
      ((symbolp seg)
       (if-let* ((fn (alist-get seg +modeline--fn-alist)))
           `(:eval (,fn))
         (or (alist-get seg +modeline--var-alist)
             (error "`%s' is not a defined mode-line segment" seg))))
      (t (error "`%S' is not a valid mode-line segment" seg))))
   segments))

(defun +modeline-refresh ()
  "Rebuild `mode-line-format' from the segment lists.

The segment lists are walked once, rather than on every redisplay.  This
command should be run after changing any of the two lists."
  (interactive)
  (setq-default mode-line-format
                (list "%e"
                      (+modeline--prepare-segments +modeline-lhs-segment-list)
                      'mode-line-format-right-align
                      (+modeline--prepare-segments +modeline-rhs-segment-list)))
  (force-mode-line-update t))


;;; Enable the mode-line

(+modeline-refresh)
