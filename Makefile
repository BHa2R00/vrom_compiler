SBCL=sbcl
all:sgk.lisp vrom_compile.lisp
	$(SBCL) --eval  '(progn (compile-file "sgk.lisp") (quit))'
	$(SBCL) --eval  '(progn (compile-file "vrom_compile.lisp") (quit))'
