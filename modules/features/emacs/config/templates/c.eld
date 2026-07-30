c-mode c-ts-mode

(stdio "#include <stdio.h>")
(stdlib "#include <stdlib.h>")
(string "#include <string.h>")

(for "for (" (p "size_t ") (p "i" index) " = " (p "0") "; " index " < " (p "n") "; ++" index ") {" n>
	 q n
	 "}" >)
(if "if (" (p "cond") ") {" n>
	q n
	"}" >)

(main "int main(" (p "int argc, char **argv") ")" n>
	  "{" n>
	  q n
	  "}" >)

(malloc "malloc(" (p "n") " * sizeof(" p "))" q)
