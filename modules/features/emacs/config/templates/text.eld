text-mode

(box "┌─" (make-string (length str) ?─) "─┐" n
     "│ " (s str)                       " │" n
     "└─" (make-string (length str) ?─) "─┘" n :doc "UNICODE BOX")
(abox "+-" (make-string (length str) ?-) "-+" n
      "| " (s str)                       " |" n
      "+-" (make-string (length str) ?-) "-+" n :doc "ASCII BOX")
(rot13 (p "plain text" text) n "----" n (rot13 text))
