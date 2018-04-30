ECL=ecl
all:compile.lisp sgk.lisp vrom_compile.lisp
	$(ECL) --c-stack 2097152  --shell compile.lisp
	@rm -v *.o
	@cp -v ./vrom_compile /srv/201803021201/201804250816/
