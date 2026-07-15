;;; mode-line.el --- Code for my custom mode line -*- lexical-binding: t -*-

(defcustom +mode-line-string-truncate-length 33
  "String length after which truncation should be done in small windows."
  :type 'natnum)

;;; Common helper functions

(defun +mode-line--string-truncate-p (str)
  "Return non-nil if STR should be truncated."
  (and (stringp str)
       (not (string-empty-p str))
       (not (string-blank-p str))
       (> (length str) +mode-line-string-truncate-length)))

(defun +mode-line-string-cut-middle (str)
  "Return truncated STR, if appropriate, else return STR.
Cut off the middle of STR by counting half of
`+mode-line-string-truncate-length' both from its beginning
and end."
  (let ((half (floor (- +mode-line-string-truncate-length 3) 2)))
    (if (+mode-line--string-truncate-p str)
        (concat (substring str 0 half) "..." (substring str (- half)))
      str)))

;;; Project name

(defvar-local +mode-line-project-format
    '(:eval
      (when-let* ((project-name (format-mode-line project-mode-line-format))
                  ((and (buffer-file-name) ; check if buffer is a file buffer
                        (not (string-empty-p project-name)))))
        (propertize (concat (string-trim project-name) "/")
                    'face 'mode-line-buffer-id)))
  "Mode line construct to display the current project.  Meant to be used in
conjunction with `+mode-line-buffer-name'.")

;;; Buffer name

(defvar-local +mode-line-buffer-identification
    '(:eval
      (propertize (+mode-line-string-cut-middle (buffer-name))
                  'face 'mode-line-buffer-id
                  'help-echo "Buffer name
mouse-1: Previous buffer\nmouse-3: Next buffer"
                  'mouse-face 'mode-line-highlight
                  'local-map mode-line-buffer-identification-keymap))
  "Mode line construct for identifying the buffer being displayed.")

;;; Risky local variables

;; NOTE 2025-04-10: The `risky-local-variable' is critical, as those variables
;; will not work without it.
(dolist (construct '(+mode-line-project-format
                     +mode-line-buffer-identification))
  (put construct 'risky-local-variable t))

(provide 'mode-line)
;;; mode-line.el ends here
