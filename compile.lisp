(require :cmp)
(in-package :cl-user)
(compile-file "sgk.lisp" :system-p t)
(compile-file "vrom_compile.lisp" :system-p t)
(c:build-program "vrom_compile" :lisp-files '(
											  "sgk.o"
											  "vrom_compile.o"
											  ))
