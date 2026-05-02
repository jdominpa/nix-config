;;; jdp-mode-line.el --- Code for my custom mode line -*- lexical-binding: t -*-

(defgroup jdp-mode-line nil
  "Custom mode line that is stylistically close to the default."
  :group 'mode-line)

(defgroup jdp-mode-line-faces nil
  "Faces for my custom mode line."
  :group 'jdp-mode-line)

(defcustom jdp-mode-line-string-truncate-length 33
  "String length after which truncation should be done in small windows."
  :type 'natnum)

;;;; Faces

(defface jdp-mode-line-indicator-button nil
  "Generic face used for indicators that have a background.
Modify this face to, for example, add a :box attribute to all
relevant indicators.")

(defface jdp-mode-line-indicator-red
  '((default :inherit bold)
    (((class color) (min-colors 88) (background light))
     :foreground "#880000")
    (((class color) (min-colors 88) (background dark))
     :foreground "#ff9f9f")
    (t :foreground "red"))
  "Face for mode line indicators."
  :group 'jdp-mode-line-faces)

(defface jdp-mode-line-indicator-red-bg
  '((default :inherit (bold jdp-mode-line-indicator-button))
    (((class color) (min-colors 88) (background light))
     :background "#aa1111" :foreground "white")
    (((class color) (min-colors 88) (background dark))
     :background "#ff9090" :foreground "black")
    (t :background "red" :foreground "black"))
  "Face for mode line indicators with a background."
  :group 'jdp-mode-line-faces)

(defface jdp-mode-line-indicator-green
  '((default :inherit bold)
    (((class color) (min-colors 88) (background light))
     :foreground "#005f00")
    (((class color) (min-colors 88) (background dark))
     :foreground "#73fa7f")
    (t :foreground "green"))
  "Face for mode line indicators."
  :group 'jdp-mode-line-faces)

(defface jdp-mode-line-indicator-green-bg
  '((default :inherit (bold jdp-mode-line-indicator-button))
    (((class color) (min-colors 88) (background light))
     :background "#207b20" :foreground "white")
    (((class color) (min-colors 88) (background dark))
     :background "#77d077" :foreground "black")
    (t :background "green" :foreground "black"))
  "Face for mode line indicators with a background."
  :group 'jdp-mode-line-faces)

(defface jdp-mode-line-indicator-yellow
  '((default :inherit bold)
    (((class color) (min-colors 88) (background light))
     :foreground "#6f4000")
    (((class color) (min-colors 88) (background dark))
     :foreground "#f0c526")
    (t :foreground "yellow"))
  "Face for mode line indicators."
  :group 'jdp-mode-line-faces)

(defface jdp-mode-line-indicator-yellow-bg
  '((default :inherit (bold jdp-mode-line-indicator-button))
    (((class color) (min-colors 88) (background light))
     :background "#805000" :foreground "white")
    (((class color) (min-colors 88) (background dark))
     :background "#ffc800" :foreground "black")
    (t :background "yellow" :foreground "black"))
  "Face for mode line indicators with a background."
  :group 'jdp-mode-line-faces)

(defface jdp-mode-line-indicator-blue
  '((default :inherit bold)
    (((class color) (min-colors 88) (background light))
     :foreground "#00228a")
    (((class color) (min-colors 88) (background dark))
     :foreground "#88bfff")
    (t :foreground "blue"))
  "Face for mode line indicators."
  :group 'jdp-mode-line-faces)

(defface jdp-mode-line-indicator-blue-bg
  '((default :inherit (bold jdp-mode-line-indicator-button))
    (((class color) (min-colors 88) (background light))
     :background "#0000aa" :foreground "white")
    (((class color) (min-colors 88) (background dark))
     :background "#77aaff" :foreground "black")
    (t :background "blue" :foreground "black"))
  "Face for mode line indicators with a background."
  :group 'jdp-mode-line-faces)

(defface jdp-mode-line-indicator-magenta
  '((default :inherit bold)
    (((class color) (min-colors 88) (background light))
     :foreground "#6a1aaf")
    (((class color) (min-colors 88) (background dark))
     :foreground "#e0a0ff")
    (t :foreground "magenta"))
  "Face for mode line indicators."
  :group 'jdp-mode-line-faces)

(defface jdp-mode-line-indicator-magenta-bg
  '((default :inherit (bold jdp-mode-line-indicator-button))
    (((class color) (min-colors 88) (background light))
     :background "#6f0f9f" :foreground "white")
    (((class color) (min-colors 88) (background dark))
     :background "#e3a2ff" :foreground "black")
    (t :background "magenta" :foreground "black"))
  "Face for mode line indicators with a background."
  :group 'jdp-mode-line-faces)

(defface jdp-mode-line-indicator-cyan
  '((default :inherit bold)
    (((class color) (min-colors 88) (background light))
     :foreground "#004060")
    (((class color) (min-colors 88) (background dark))
     :foreground "#30b7cc")
    (t :foreground "cyan"))
  "Face for mode line indicators."
  :group 'jdp-mode-line-faces)

(defface jdp-mode-line-indicator-cyan-bg
  '((default :inherit (bold jdp-mode-line-indicator-button))
    (((class color) (min-colors 88) (background light))
     :background "#006080" :foreground "white")
    (((class color) (min-colors 88) (background dark))
     :background "#40c0e0" :foreground "black")
    (t :background "cyan" :foreground "black"))
  "Face for mode line indicators with a background."
  :group 'jdp-mode-line-faces)

(defface jdp-mode-line-indicator-gray
  '((t :inherit shadow))
  "Face for mode line indicators."
  :group 'jdp-mode-line-faces)

(defface jdp-mode-line-indicator-gray-bg
  '((default :inherit (bold jdp-mode-line-indicator-button))
    (((class color) (min-colors 88) (background light))
     :background "#808080" :foreground "white")
    (((class color) (min-colors 88) (background dark))
     :background "#a0a0a0" :foreground "black")
    (t :inverse-video t))
  "Face for mode line indicatovrs with a background."
  :group 'jdp-mode-line-faces)

;;;; Common helper functions

(defun jdp-mode-line--string-truncate-p (str)
  "Return non-nil if STR should be truncated."
  (and (stringp str)
       (not (string-empty-p str))
       (not (string-blank-p str))
       (> (length str) jdp-mode-line-string-truncate-length)))

(defun jdp-mode-line-string-cut-end (str)
  "Return truncated STR, if appropriate, else return STR.
Cut off the end of STR by counting from its start up to
`jdp-mode-line-string-truncate-length'."
  (if (jdp-mode-line--string-truncate-p str)
      (concat (substring str 0 jdp-mode-line-string-truncate-length) "...")
    str))

(defun jdp-mode-line-string-cut-beginning (str)
  "Return truncated STR, if appropriate, else return STR.
Cut off the beginning of STR by counting from its end up to
`jdp-mode-line-string-truncate-length'."
  (if (jdp-mode-line--string-truncate-p str)
      (concat "..." (substring str (- jdp-mode-line-string-truncate-length)))
    str))

(defun jdp-mode-line-string-cut-middle (str)
  "Return truncated STR, if appropriate, else return STR.
Cut off the middle of STR by counting half of
`jdp-mode-line-string-truncate-length' both from its beginning
and end."
  (let ((half (floor (- jdp-mode-line-string-truncate-length 3) 2)))
    (if (jdp-mode-line--string-truncate-p str)
        (concat (substring str 0 half) "..." (substring str (- half)))
      str)))

;;;; Project name

(defvar-local jdp-mode-line-project-format
    '(:eval
      (when-let* ((project-name (format-mode-line project-mode-line-format))
                  ((and (buffer-file-name) ; check if buffer is a file buffer
                        (not (string-empty-p project-name)))))
        (propertize (concat (string-trim project-name) "/")
                    'face 'mode-line-buffer-id)))
  "Mode line construct to display the current project.  Meant to be used in
conjunction with `jdp-mode-line-buffer-name'.")

;;;; Buffer name

(defvar-local jdp-mode-line-buffer-identification
    '(:eval
      (propertize (jdp-mode-line-string-cut-middle (buffer-name))
                  'face 'mode-line-buffer-id
                  'help-echo "Buffer name
mouse-1: Previous buffer\nmouse-3: Next buffer"
                  'mouse-face 'mode-line-highlight
                  'local-map mode-line-buffer-identification-keymap))
  "Mode line construct for identifying the buffer being displayed.")

;;;; Risky local variables

;; NOTE 2025-04-10: The `risky-local-variable' is critical, as those variables
;; will not work without it.
(dolist (construct '(jdp-mode-line-project-format
                     jdp-mode-line-buffer-identification))
  (put construct 'risky-local-variable t))

(provide 'jdp-mode-line)
;;; jdp-mode-line.el ends here
