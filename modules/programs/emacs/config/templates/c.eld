c-mode c-ts-mode

(stdio "#include <stdio.h>")
(stdlib "#include <stdlib.h>")
(string "#include <string.h>")
(inc "#include <" (p (concat (file-name-base (or (buffer-file-name) (buffer-name))) ".h")) ">")
(incc "#include \"" (p (concat (file-name-base (or (buffer-file-name) (buffer-name))) ".h")) "\"")
(def "#define ")

;TODO fix the indentation of these templates
(for "for (" (p "i" index) " = " (p "0") "; " index " < " (p "n") "; ++" index ") {" n>
	 r> n>
	 "}")
(if "if (" (p "cond") ") {" n>
	r> n>
	"}")

(main "int main(" (p "int argc, char *argv[]") ")" n>
	  "{" n>
	  r> n
	  "}")

(malloc "malloc(" (p "n") " * sizeof(" p "))" q)
