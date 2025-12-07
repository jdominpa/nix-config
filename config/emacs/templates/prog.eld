prog-mode

(bug (if (derived-mode-p 'emacs-lisp-mode) ";; " comment-start) "BUG ")
(fixme (if (derived-mode-p 'emacs-lisp-mode) ";; " comment-start) "FIXME ")
(note (if (derived-mode-p 'emacs-lisp-mode) ";; " comment-start) "NOTE ")
(todo (if (derived-mode-p 'emacs-lisp-mode) ";; " comment-start) "TODO ")
