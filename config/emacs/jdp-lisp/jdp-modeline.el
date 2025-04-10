;;; jdp-modeline.el --- Code for my custom mode line -*- lexical-binding: t -*-

(defgroup jdp-modeline nil
  "Custom modeline that is stylistically close to the default."
  :group 'mode-line)

(defgroup jdp-modeline-faces nil
  "Faces for my custom modeline."
  :group 'jdp-modeline)

(defcustom jdp-modeline-string-truncate-length 15
  "String length after which truncation should be done in small windows."
  :type 'natnum)

;;;; Faces

(defface jdp-modeline-indicator-button nil
  "Generic face used for indicators that have a background.
Modify this face to, for example, add a :box attribute to all
relevant indicators (combines nicely with my `spacious-padding'
package).")

(defface jdp-modeline-indicator-red
  '((default :inherit bold)
    (((class color) (min-colors 88) (background light))
     :foreground "#880000")
    (((class color) (min-colors 88) (background dark))
     :foreground "#ff9f9f")
    (t :foreground "red"))
  "Face for modeline indicators."
  :group 'jdp-modeline-faces)

(defface jdp-modeline-indicator-red-bg
  '((default :inherit (bold jdp-modeline-indicator-button))
    (((class color) (min-colors 88) (background light))
     :background "#aa1111" :foreground "white")
    (((class color) (min-colors 88) (background dark))
     :background "#ff9090" :foreground "black")
    (t :background "red" :foreground "black"))
  "Face for modeline indicators with a background."
  :group 'jdp-modeline-faces)

(defface jdp-modeline-indicator-green
  '((default :inherit bold)
    (((class color) (min-colors 88) (background light))
     :foreground "#005f00")
    (((class color) (min-colors 88) (background dark))
     :foreground "#73fa7f")
    (t :foreground "green"))
  "Face for modeline indicators."
  :group 'jdp-modeline-faces)

(defface jdp-modeline-indicator-green-bg
  '((default :inherit (bold jdp-modeline-indicator-button))
    (((class color) (min-colors 88) (background light))
     :background "#207b20" :foreground "white")
    (((class color) (min-colors 88) (background dark))
     :background "#77d077" :foreground "black")
    (t :background "green" :foreground "black"))
  "Face for modeline indicators with a background."
  :group 'jdp-modeline-faces)

(defface jdp-modeline-indicator-yellow
  '((default :inherit bold)
    (((class color) (min-colors 88) (background light))
     :foreground "#6f4000")
    (((class color) (min-colors 88) (background dark))
     :foreground "#f0c526")
    (t :foreground "yellow"))
  "Face for modeline indicators."
  :group 'jdp-modeline-faces)

(defface jdp-modeline-indicator-yellow-bg
  '((default :inherit (bold jdp-modeline-indicator-button))
    (((class color) (min-colors 88) (background light))
     :background "#805000" :foreground "white")
    (((class color) (min-colors 88) (background dark))
     :background "#ffc800" :foreground "black")
    (t :background "yellow" :foreground "black"))
  "Face for modeline indicators with a background."
  :group 'jdp-modeline-faces)

(defface jdp-modeline-indicator-blue
  '((default :inherit bold)
    (((class color) (min-colors 88) (background light))
     :foreground "#00228a")
    (((class color) (min-colors 88) (background dark))
     :foreground "#88bfff")
    (t :foreground "blue"))
  "Face for modeline indicators."
  :group 'jdp-modeline-faces)

(defface jdp-modeline-indicator-blue-bg
  '((default :inherit (bold jdp-modeline-indicator-button))
    (((class color) (min-colors 88) (background light))
     :background "#0000aa" :foreground "white")
    (((class color) (min-colors 88) (background dark))
     :background "#77aaff" :foreground "black")
    (t :background "blue" :foreground "black"))
  "Face for modeline indicators with a background."
  :group 'jdp-modeline-faces)

(defface jdp-modeline-indicator-magenta
  '((default :inherit bold)
    (((class color) (min-colors 88) (background light))
     :foreground "#6a1aaf")
    (((class color) (min-colors 88) (background dark))
     :foreground "#e0a0ff")
    (t :foreground "magenta"))
  "Face for modeline indicators."
  :group 'jdp-modeline-faces)

(defface jdp-modeline-indicator-magenta-bg
  '((default :inherit (bold jdp-modeline-indicator-button))
    (((class color) (min-colors 88) (background light))
     :background "#6f0f9f" :foreground "white")
    (((class color) (min-colors 88) (background dark))
     :background "#e3a2ff" :foreground "black")
    (t :background "magenta" :foreground "black"))
  "Face for modeline indicators with a background."
  :group 'jdp-modeline-faces)

(defface jdp-modeline-indicator-cyan
  '((default :inherit bold)
    (((class color) (min-colors 88) (background light))
     :foreground "#004060")
    (((class color) (min-colors 88) (background dark))
     :foreground "#30b7cc")
    (t :foreground "cyan"))
  "Face for modeline indicators."
  :group 'jdp-modeline-faces)

(defface jdp-modeline-indicator-cyan-bg
  '((default :inherit (bold jdp-modeline-indicator-button))
    (((class color) (min-colors 88) (background light))
     :background "#006080" :foreground "white")
    (((class color) (min-colors 88) (background dark))
     :background "#40c0e0" :foreground "black")
    (t :background "cyan" :foreground "black"))
  "Face for modeline indicators with a background."
  :group 'jdp-modeline-faces)

(defface jdp-modeline-indicator-gray
  '((t :inherit shadow))
  "Face for modeline indicators."
  :group 'jdp-modeline-faces)

(defface jdp-modeline-indicator-gray-bg
  '((default :inherit (bold jdp-modeline-indicator-button))
    (((class color) (min-colors 88) (background light))
     :background "#808080" :foreground "white")
    (((class color) (min-colors 88) (background dark))
     :background "#a0a0a0" :foreground "black")
    (t :inverse-video t))
  "Face for modeline indicatovrs with a background."
  :group 'jdp-modeline-faces)

;;;; Common helper functions

(defun jdp-modeline--window-narrow-p ()
  "Return non-nil if window is narrow.
Check if the `window-width' is less than `split-width-threshold'."
  (and (numberp split-width-threshold)
       (< (window-total-width) split-width-threshold)))

(defun jdp-modeline--string-truncate-p (str)
  "Return non-nil if STR should be truncated."
  (cond
   ((or (not (stringp str))
        (string-empty-p str)
        (string-blank-p str))
    nil)
   ((and (jdp-modeline--window-narrow-p)
         (> (length str) jdp-modeline-string-truncate-length)
         (not (one-window-p :no-minibuffer))))))

(defun jdp-modeline--truncate-p ()
  "Return non-nil if truncation should happen.
This is a more general and less stringent variant of
`jdp-modeline--string-truncate-p'."
  (and (jdp-modeline--window-narrow-p)
       (not (one-window-p :no-minibuffer))))

(defun jdp-modeline-string-cut-end (str)
  "Return truncated STR, if appropriate, else return STR.
Cut off the end of STR by counting from its start up to
`jdp-modeline-string-truncate-length'."
  (if (jdp-modeline--string-truncate-p str)
      (concat (substring str 0 jdp-modeline-string-truncate-length) "...")
    str))

(defun jdp-modeline-string-cut-beginning (str)
  "Return truncated STR, if appropriate, else return STR.
Cut off the beginning of STR by counting from its end up to
`jdp-modeline-string-truncate-length'."
  (if (jdp-modeline--string-truncate-p str)
      (concat "..." (substring str (- jdp-modeline-string-truncate-length)))
    str))

(defun jdp-modeline-string-cut-middle (str)
  "Return truncated STR, if appropriate, else return STR.
Cut off the middle of STR by counting half of
`jdp-modeline-string-truncate-length' both from its beginning
and end."
  (let ((half (floor jdp-modeline-string-truncate-length 2)))
    (if (jdp-modeline--string-truncate-p str)
        (concat (substring str 0 half) "..." (substring str (- half)))
      str)))

(defun jdp-modeline--first-char (str)
  "Return first character from STR."
  (substring str 0 1))

(defun jdp-modeline-string-abbreviate (str)
  "Abbreviate STR individual hyphen or underscore separated words.
Also see `jdp-modeline-string-abbreviate-but-last'."
  (if (jdp-modeline--string-truncate-p str)
      (mapconcat #'jdp-modeline--first-char (split-string str "[_-]") "-")
    str))

(defun jdp-modeline-string-abbreviate-but-last (str nthlast)
  "Abbreviate STR, keeping NTHLAST words intact.
Also see `jdp-modeline-string-abbreviate'."
  (if (jdp-modeline--string-truncate-p str)
      (let* ((all-strings (split-string str "[_-]"))
             (nbutlast-strings (nbutlast (copy-sequence all-strings) nthlast))
             (last-strings (nreverse (ntake nthlast (nreverse (copy-sequence all-strings)))))
             (first-component (mapconcat #'jdp-modeline--first-char nbutlast-strings "-"))
             (last-component (mapconcat #'identity last-strings "-")))
        (if (string-empty-p first-component)
            last-component
          (concat first-component "-" last-component)))
    str))

;;;; Keyboard macro indicator

(defvar-local jdp-modeline-kbd-macro
    '(:eval
      (when (and (mode-line-window-selected-p) defining-kbd-macro)
        (propertize " MACRO " 'face 'jdp-modeline-indicator-blue-bg)))
  "Mode line construct displaying `mode-line-defining-kbd-macro'.
Specific to the current window's mode line.")

;;;; Narrow indicator

(defvar-local jdp-modeline-narrow
    '(:eval
      (when (and (mode-line-window-selected-p)
                 (buffer-narrowed-p)
                 (not (derived-mode-p 'Info-mode 'help-mode 'special-mode 'message-mode)))
        (propertize " NARROW " 'face 'jdp-modeline-indicator-cyan-bg)))
  "Mode line construct to report the narrowed state of the current buffer.")

;;;; Input method

(defvar-local jdp-modeline-input-method
    '(:eval
      (when current-input-method-title
        (propertize (format " %s " current-input-method-title)
                    'face 'jdp-modeline-indicator-green-bg
                    'mouse-face 'mode-line-highlight)))
  "Mode line construct to report the multilingual environment.")

;;;; Buffer status

;; TODO 2023-07-05: What else is there beside remote files?  If
;; nothing, this must be renamed accordingly.
(defvar-local jdp-modeline-buffer-status
    '(:eval
      (when (file-remote-p default-directory)
        (propertize " @ "
                    'face 'jdp-modeline-indicator-red-bg
                    'mouse-face 'mode-line-highlight)))
  "Mode line construct for showing remote file name.")

;;;; Dedicated window

(defvar-local jdp-modeline-window-dedicated-status
    '(:eval
      (when (window-dedicated-p)
        (propertize " = "
                    'face 'jdp-modeline-indicator-gray-bg
                    'mouse-face 'mode-line-highlight)))
  "Mode line construct for dedicated window indicator.")

;;;; Meow mode

(defvar-local jdp-modeline-meow-mode
    '(:eval
      (when (bound-and-true-p meow-mode)
        (list " " (meow-indicator))))
  "Mode line construct for showing the current Meow mode.")

;;;; Buffer name and modified status

(defun jdp-modeline--buffer-read-only ()
  "Return read only info of current buffer."
  (when buffer-read-only
    (propertize (char-to-string #xE0A2) 'face 'jdp-modeline-indicator-yellow)))

(defun jdp-modeline--buffer-status ()
  "Return the visited file status of current buffer."
  (let* ((file (buffer-file-name))
         (file-exists (and file (file-exists-p file)))
         (file-remote (and file (file-remote-p file))))
    (cond
     ;; For buffers which files are modified outside Emacs
     ((and file-exists (not file-remote) (not (verify-visited-file-modtime)))
      (propertize "!" 'face 'jdp-modeline-indicator-yellow))
     ((and file-exists (buffer-modified-p))
      (propertize "*" 'face 'jdp-modeline-indicator-green))
     ((and file (not file-exists) (not file-remote))
      (propertize "?" 'face 'jdp-modeline-indicator-red))
     (t " "))))

(defun jdp-modeline--buffer-name ()
  "Return `buffer-name', truncating it if necessary.
See `jdp-modeline-string-cut-middle'."
  (when-let* ((name (buffer-name)))
    (jdp-modeline-string-cut-middle name)))

(defun jdp-modeline-buffer-name ()
  "Return buffer name, with read-only indicator if relevant and visited
file status."
  (let ((name (jdp-modeline--buffer-name)))
    (concat
     " "
     (jdp-modeline--buffer-read-only)
     " "
     (if (mode-line-window-selected-p)
         (propertize name 'face 'mode-line-buffer-id)
       name)
     (jdp-modeline--buffer-status)
     " ")))

(defun jdp-modeline-buffer-name-help-echo ()
  "Return `help-echo' value for `jdp-modeline-buffer-identification'."
  (concat
   (propertize (buffer-name) 'face 'mode-line-buffer-id)
   "\n"
   (propertize
    (or (buffer-file-name)
        (format "No underlying file.\nDirectory is: %s" default-directory))
    'face 'font-lock-doc-face)))

(defvar-local jdp-modeline-buffer-identification
    '(:eval
      (propertize (jdp-modeline-buffer-name)
                  'mouse-face 'mode-line-highlight
                  'help-echo (jdp-modeline-buffer-name-help-echo)))
  "Mode line construct for identifying the buffer being displayed.
Propertize the current buffer with the `mode-line-buffer-id'
face.  Let other buffers have no face.")

;;;; Major mode

(defun jdp-modeline-major-mode-name ()
  "Return capitalized `major-mode' without the -mode suffix."
  (capitalize (string-replace "-mode" "" (symbol-name major-mode))))

(defun jdp-modeline-major-mode-help-echo ()
  "Return `help-echo' value for `jdp-modeline-major-mode'."
  (if-let* ((parent (get major-mode 'derived-mode-parent)))
      (format "Symbol: `%s'.  Derived from: `%s'" major-mode parent)
    (format "Symbol: `%s'." major-mode)))

(defvar-local jdp-modeline-major-mode
    (list
     " "
     (propertize "%[" 'face 'jdp-modeline-indicator-red)
     '(:eval
       (propertize
        (jdp-modeline-string-abbreviate-but-last
         (jdp-modeline-major-mode-name)
         2)
        'mouse-face 'mode-line-highlight
        'help-echo (jdp-modeline-major-mode-help-echo)))
     (propertize "%]" 'face 'jdp-modeline-indicator-red)
     " ")
  "Mode line construct for displaying major modes.")

(defvar-local jdp-modeline-process
    (list '("" mode-line-process))
  "Mode line construct for the running process indicator.")

;;;; Git branch and diffstat

(declare-function vc-git--symbolic-ref "vc-git" (file))

(defun jdp-modeline--vc-branch-name (file backend)
  "Return capitalized VC branch name for FILE with BACKEND."
  (when-let* ((rev (vc-working-revision file backend))
              (branch (or (vc-git--symbolic-ref file)
                          (substring rev 0 7))))
    (capitalize branch)))

;; NOTE 2023-07-27: This is a good idea, but it hardcodes Git, whereas
;; I want a generic VC method.  Granted, I only use Git but I still
;; want it to work as a VC extension.

;; (defun jdp-modeline-diffstat (file)
;;   "Return shortened Git diff numstat for FILE."
;;   (when-let* ((output (shell-command-to-string (format "git diff --numstat %s" file)))
;;               (stats (split-string output "[\s\t]" :omit-nulls "[\s\f\t\n\r\v]+"))
;;               (added (nth 0 stats))
;;               (deleted (nth 1 stats)))
;;     (cond
;;      ((and (equal added "0") (equal deleted "0"))
;;       "")
;;      ((and (not (equal added "0")) (equal deleted "0"))
;;       (propertize (format "+%s" added) 'face 'shadow))
;;      ((and (equal added "0") (not (equal deleted "0")))
;;       (propertize (format "-%s" deleted) 'face 'shadow))
;;      (t
;;       (propertize (format "+%s -%s" added deleted) 'face 'shadow)))))

(declare-function vc-git-working-revision "vc-git" (file))

(defvar jdp-modeline-vc-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line down-mouse-1] 'vc-diff)
    (define-key map [mode-line down-mouse-3] 'vc-root-diff)
    map)
  "Keymap to display on VC indicator.")

(defun jdp-modeline--vc-help-echo (file)
  "Return `help-echo' message for FILE tracked by VC."
  (format "Revision: %s\nmouse-1: `vc-diff'\nmouse-3: `vc-root-diff'"
          (vc-working-revision file)))

(defun jdp-modeline--vc-text (file branch &optional face)
  "Prepare text for Git controlled FILE, given BRANCH.
With optional FACE, use it to propertize the BRANCH."
  (concat
   (propertize (char-to-string #xE0A0) 'face 'shadow)
   " "
   (propertize branch
               'face face
               'mouse-face 'mode-line-highlight
               'help-echo (jdp-modeline--vc-help-echo file)
               'local-map jdp-modeline-vc-map)
   ;; " "
   ;; (jdp-modeline-diffstat file)
   ))

(defun jdp-modeline--vc-details (file branch &optional face)
  "Return Git BRANCH details for FILE, truncating it if necessary.
The string is truncated if the width of the window is smaller
than `split-width-threshold'."
  (jdp-modeline-string-cut-end
   (jdp-modeline--vc-text file branch face)))

(defvar jdp-modeline--vc-faces
  '((added . vc-locally-added-state)
    (edited . vc-edited-state)
    (removed . vc-removed-state)
    (missing . vc-missing-state)
    (conflict . vc-conflict-state)
    (locked . vc-locked-state)
    (up-to-date . vc-up-to-date-state))
  "VC state faces.")

(defun jdp-modeline--vc-get-face (key)
  "Get face from KEY in `jdp-modeline--vc-faces'."
  (alist-get key jdp-modeline--vc-faces 'up-to-date))

(defun jdp-modeline--vc-face (file backend)
  "Return VC state face for FILE with BACKEND."
  (jdp-modeline--vc-get-face (vc-state file backend)))

(defvar-local jdp-modeline-vc-branch
    '(:eval
      (when-let* (((mode-line-window-selected-p))
                  (file (or buffer-file-name default-directory))
                  (backend (or (vc-backend file) 'Git))
                  ;; ((vc-git-registered file))
                  (branch (jdp-modeline--vc-branch-name file backend))
                  (face (jdp-modeline--vc-face file backend)))
        (list " " (jdp-modeline--vc-details file branch face) " ")))
  "Mode line construct to return propertized VC branch.")

;;;; Eglot

(with-eval-after-load 'eglot
  (setq mode-line-misc-info
        (delete '(eglot--managed-mode (" [" eglot--mode-line-format "] ")) mode-line-misc-info)))

(defvar-local jdp-modeline-eglot
    '(:eval
      (when (and (featurep 'eglot) (mode-line-window-selected-p))
        '(" " (eglot--managed-mode eglot--mode-line-format) " ")))
  "Mode line construct displaying Eglot information.
Specific to the current window's mode line.")

;;;; Flymake errors, warnings, notes

(declare-function flymake--severity "flymake" (type))
(declare-function flymake-diagnostic-type "flymake" (diag))

;; Based on `flymake--mode-line-counter'.
(defun jdp-modeline-flymake-counter (type)
  "Compute number of diagnostics in buffer with TYPE's severity.
TYPE is usually keyword `:error', `:warning' or `:note'."
  (let ((count 0))
    (dolist (d (flymake-diagnostics))
      (when (= (flymake--severity type)
               (flymake--severity (flymake-diagnostic-type d)))
        (cl-incf count)))
    (when (cl-plusp count)
      (number-to-string count))))

(defvar jdp-modeline-flymake-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line down-mouse-1] 'flymake-show-buffer-diagnostics)
    (define-key map [mode-line down-mouse-3] 'flymake-show-project-diagnostics)
    map)
  "Keymap to display on Flymake indicator.")

(defmacro jdp-modeline-flymake-type (type indicator &optional face)
  "Return function that handles Flymake TYPE with stylistic INDICATOR and FACE."
  `(defun ,(intern (format "jdp-modeline-flymake-%s" type)) ()
     (when-let* ((count (jdp-modeline-flymake-counter
                         ,(intern (format ":%s" type)))))
       (concat
        (propertize count
                    'face ',(or face type)
                    'mouse-face 'mode-line-highlight
                    ;; FIXME 2023-07-03: Clicking on the text with
                    ;; this buffer and a single warning present, the
                    ;; diagnostics take up the entire frame.  Why?
                    'local-map jdp-modeline-flymake-map
                    'help-echo "mouse-1: buffer diagnostics\nmouse-3: project diagnostics")
        (propertize ,indicator 'face 'shadow)))))

(jdp-modeline-flymake-type error "e")
(jdp-modeline-flymake-type warning "w")
(jdp-modeline-flymake-type note "n" success)

(defvar-local jdp-modeline-flymake
    '(:eval
      (when (and (bound-and-true-p flymake-mode)
                 (mode-line-window-selected-p))
        (list
         " "
         ;; See the calls to the macro `jdp-modeline-flymake-type'
         '(:eval (jdp-modeline-flymake-error))
         '(:eval (jdp-modeline-flymake-warning))
         '(:eval (jdp-modeline-flymake-note))
         " ")))
  "Mode line construct displaying `flymake-mode-line-format'.
Specific to the current window's mode line.")

;;;; Envrc status

(defvar-local jdp-modeline-envrc-status
    '(:eval
      (when (and (bound-and-true-p envrc-mode)
                 (not (eq envrc--status 'none))
                 (mode-line-window-selected-p))
        (list envrc-lighter " ")))
  "Mode line construct displaying the status of `envrc-mode'.
Specific to the current window's mode line.")

;;;; Miscellaneous

(defvar-local jdp-modeline-misc-info
    '(:eval
      (when (mode-line-window-selected-p)
        mode-line-misc-info))
  "Mode line construct displaying `mode-line-misc-info'.
Specific to the current window's mode line.")

;;;; Risky local variables

;; NOTE 2025-04-10: The `risky-local-variable' is critical, as those variables
;; will not work without it.
(dolist (construct '(jdp-modeline-kbd-macro
                     jdp-modeline-narrow
                     jdp-modeline-input-method
                     jdp-modeline-meow-mode
                     jdp-modeline-buffer-status
                     jdp-modeline-window-dedicated-status
                     jdp-modeline-buffer-identification
                     jdp-modeline-major-mode
                     jdp-modeline-process
                     jdp-modeline-vc-branch
                     jdp-modeline-eglot
                     jdp-modeline-flymake
                     jdp-modeline-envrc-status
                     jdp-modeline-misc-info))
  (put construct 'risky-local-variable t))

(provide 'jdp-modeline)
;;; jdp-modeline.el ends here
