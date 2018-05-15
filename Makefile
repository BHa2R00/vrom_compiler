ECL=ecl
all:compile.lisp sgk.lisp vrom_compile.lisp
	$(ECL) --eval '(progn (compile-file "sgk.lisp") (quit))'
	$(ECL) --c-stack 2097152 --frame-stack 4096000 --heap-size 512000000 --lisp-stack 64000 --shell compile.lisp
	@rm -v *.o
	@cp -v ./sgk.fas /srv/201803021201/201804250816/
	@cp -v ./vrom_compile /srv/201803021201/201804250816/
