(load "./config.lisp")
;(load "./sgk.lisp")

(defvar srclib (sgk-rd-gds *leaf_cell*))

(defun xaddrs (wd cm)
  (let (wddp)
	(dotimes (n 13)
	  (if (and (>= wd (1+ (math-pow 2 n))) (<= wd (math-pow 2 (1+ n)))) (setf wddp (math-pow 2 (1+ n)))))
	(math-log 2 (/ wddp cm))))

(defun yaddrs (cm)
  (math-log 2 cm))

(defun ymuxx (yaddrs) (math-pow 2 yaddrs))

(defun ymuxsa2 (yaddrs)
  (cond
	((= yaddrs 3) "YMUX8SA2")
	((= yaddrs 4) "YMUX16SA2")
	((= yaddrs 5) "YMUX32SA2")))

(defun ymuxsa1left (yaddrs)
  (cond
	((= yaddrs 3) "YMUX8SA1L")
	((= yaddrs 4) "YMUX16SA1L")
	((= yaddrs 5) "YMUX32SA1L")))

(defun ymuxsa1right (yaddrs)
  (cond
	((= yaddrs 3) "YMUX8SA1R")
	((= yaddrs 4) "YMUX16SA1R")
	((= yaddrs 5) "YMUX32SA1R")))

(defun ypredeccell (yaddrs)
  (cond
	((= yaddrs 3) "YDECODE38")
	((= yaddrs 4) "YDECODE24_24")
	((= yaddrs 5) "YDECODE24_38")))

(defun itfctlcell (Yaddrs charmode)
  (cond
	((= yaddrs 3) (if (oddp charmode) "ITFCTL_CM8_char" "ITFCTL_CM8"))
	((= yaddrs 4) (if (oddp charmode) "ITFCTL_CM16_char" "ITFCTL_CM16"))
	((= yaddrs 5) (if (oddp charmode) "ITFCTL_CM32_char" "ITFCTL_CM32"))))

(defun dmy_colmux_odd (yaddrs)
  (cond
	((= yaddrs 3) "DMY_COLMUX8")
	((= yaddrs 4) "DMY_COLMUX16")
	((= yaddrs 5) "DMY_COLMUX32")))

(defun dmy_colmux_even (yaddrs) (concatenate 'string (dmy_colmux_odd yaddrs) "_W"))

(defun xpredec38 (cm)
  (cond
	((= cm 8) "XPREDEC38_CM8")
	((= cm 16) "XPREDEC38_CM16")
	((= cm 32) "XPREDEC38_CM32")))

(defun xdec_ydec_con (cm)
  (cond
	((= cm 8) "XDEC_YDEC_CON_CM8")
	((= cm 16) "XDEC_YDEC_CON_CM16")))

(defun col_dec_con (cm charmode)
  (cond
	((= cm 8) (if (oddp charmode) "COL_DEC_CON_CM8_char" "COL_DEC_CON_CM8"))
	((= cm 16) (if (oddp charmode) "COL_DEC_CON_CM16_char" "COL_DEC_CON_CM16"))
	((= cm 32) (if (oddp charmode) "COL_DEC_CON_CM32_char" "COL_DEC_CON_CM32"))))

(defun wl_tie (cm)
  (cond
	((= cm 8) "WL_TIE_CM8")
	((= cm 16) "WL_TIE_CM16")
	((= cm 32) "WL_TIE_CM32")))

(defun Xpredechigh (cm)
  (cond
	((= cm 8) "XPREDEC38_CM8")
	((= cm 16) "XPREDEC38_CM16")
	((= cm 32) "XPREDEC38_CM32")))

(defvar *xaddrsnh* (make-array '(8 3)
	:initial-contents '((3 0 0) (3 1 0) (3 2 0) (3 3 0) (3 3 1) (3 3 2) (3 3 3) (3 3 4))))

(defun num-of-xdecs (wd cps)
  (if (= 0 (floor (float (mod wd cps))))
	(floor (float (/ wd (* 8 cps))))
	(floor (1+ (float (/ wd (* 8 cps)))))))

;(sgk-ls-cell srclib)

(defvar *xaddrs* (floor (xaddrs *word-depth* *col-mux*)))
(defvar *yaddrs* (floor (yaddrs *col-mux*)))
(defvar *ymuxx* (floor (ymuxx *yaddrs*)))
(defvar *ymuxsa2* (ymuxsa2 *yaddrs*))
(defvar *ymuxsa1left* (ymuxsa1left *yaddrs*))
(defvar *ymuxsa1right* (ymuxsa1right *yaddrs*))
(defvar *ypredeccell* (ypredeccell *yaddrs*))
(defvar *itfctlcell* (itfctlcell *yaddrs* *charmode*))
(defvar *dmy_colmux_odd* (dmy_colmux_odd *yaddrs*))
(defvar *dmy_colmux_even* (dmy_colmux_even *yaddrs*))
(defvar *dmy_tkwl_corner_even* "DMY_TKWL_CORNER_W")
(defvar *dmy_tkwl_corner_odd* "DMY_TKWL_CORNER")
(defvar *dmy_wl_edgex4_even* "DMY_WL_EDGEX4_W")
(defvar *dmy_wl_edgex4_odd* "DMY_WL_EDGEX4")
(defvar *dmy_wl_corner_even* "DMY_WL_CORNER_W")
(defvar *dmy_wl_corner_odd* "DMY_WL_CORNER")
(defvar *xpredec38* (xpredec38 *col-mux*))
(defvar *xdec_ydec_con* (xdec_ydec_con *col-mux*))
(defvar *col_dec_con* (col_dec_con *col-mux* *charmode*))
(defvar *wl_tie* (wl_tie *col-mux*))
(defvar *Xpredechigh* (Xpredechigh *col-mux*))
(defvar *Xpredecmid* (Xpredechigh *col-mux*))
(defvar *Xpredeclow* (Xpredechigh *col-mux*))
(defvar *Xaddrslow* (aref *xaddrsnh* (- *yaddrs* 3) 0))
(defvar *Xaddrsmid* (aref *xaddrsnh* (- *yaddrs* 3) 1))
(defvar *Xaddrshigh* (aref *xaddrsnh* (- *yaddrs* 3) 2))
(defvar *mun-of-addr* (floor (math-log 2 *word-depth*)))
(defvar *col-per-sa* (floor (math-pow 2 *yaddrs*)))
(defvar *num-of-xdecs* (num-of-xdecs *word-depth* *col-per-sa*))
(defvar *outputs-left* (/ (1+ *word-length*) 2))
(defvar *outputs-right* (/ *word-length* 2))
(defvar *number-of-cols-l* (floor *outputs-left*))
(defvar *number-of-cols-r* (floor *outputs-right*))
(defvar *number-of-cols* (+ *number-of-cols-l* *number-of-cols-r*))
(defvar *compiler-args-check* (if(= (mod (- (math-pow 2 *mun-of-addr*) *word-depth*) (* 8 (math-pow 2 *yaddrs*))) 0) "ArgvOK ^_^" "InValidArgv X_X"))

(if (string/= *compiler-args-check* "ArgvOK ^_^") (format t "~a~%" *compiler-args-check*) (progn

(defun code (fi)
  (let ((code-map (make-array (list *word-depth* *word-length*))) (line))
	(with-open-file (str-cod fi :direction	:input)
	  (dotimes (n *word-depth*)
		(setf line (read-line str-cod nil 'eof))
		(setf line (subseq line (- (length line) *word-length*)))
		(dotimes (m *word-length*)
		  (setf (aref code-map n m) (aref line m)))))
	code-map))

(defvar *code-map* (code *code-file*))

(defvar *units* (list))

(format t
"compiler initial

CodeFile:~a
WordDepth: ~d	WordLength: ~d	ColMux: ~d	CharMode: ~d
XAddrs: ~d	YAddrs: ~d	YMuxX: ~d
YMuxSA2: ~a			YMuxSA1Left: ~a			YMuxSA1Left: ~a
YPreDecCell: ~a
ItfCtlCell: ~a
DmyColmuxOdd: ~a		DmyColmuxEven: ~a
DmyTkwlCornerEven: ~a	DmyTkwlCornerOdd: ~a
DmyWlEdgex4Even: ~a	DmyWlEdgex4Odd: ~a
DmyWlCornerEven: ~a	DmyWlCornerOdd: ~a
XPreDec38: ~a		XDecYDecCon: ~a
ColDecCon: ~a		WlTie: ~a
XPreDecHigh: ~a		XPreDecMid: ~a		XPreDecLow: ~a
XAddrsLow: ~d	XAddrsMid: ~d	XAddrsHigh: ~d
NumOfAddr: ~d
ColPerSA: ~d	NumOfXDecs: ~d
NumberOfColsL:~d	NumberOfColsR:~d	NumberOfCols:~d
CompilerArgvCheck: ~a

compiler start

"
		*code-file*
		*word-depth* *word-length* *col-mux* *charmode*
		*xaddrs* *yaddrs* *ymuxx*
		*ymuxsa2* *ymuxsa1left* *ymuxsa1right*
		*ypredeccell*
		*itfctlcell*
		*dmy_colmux_odd* *dmy_colmux_even*
		*dmy_tkwl_corner_even* *dmy_tkwl_corner_odd*
		*dmy_wl_edgex4_even* *dmy_wl_edgex4_odd*
		*dmy_wl_corner_even* *dmy_wl_corner_odd*
		*xpredec38* *xdec_ydec_con*
		*col_dec_con* *wl_tie*
		*Xpredechigh* *Xpredecmid* *Xpredeclow*
		*Xaddrslow* *Xaddrsmid* *Xaddrshigh*
		*mun-of-addr*
		*col-per-sa* *num-of-xdecs*
		*number-of-cols-l* *number-of-cols-r* *number-of-cols*
		*compiler-args-check*
		)
(format t "creating gds~%")
(defvar outunits (list))
(defvar srcunits (units-of srclib))

(<cell-add  outunits srclib "CORE00")
(<cell-add  outunits srclib "CORE01")
(<cell-add  outunits srclib "CORE10")
(<cell-add  outunits srclib "CORE11")

;;;;;;;;array_bit
(defun array_bit (num)
  (let ((cell-name (string+ "array_bit_" (itoa num)))
		(sref-map (list))
;		(x0 (+ 0 (str-width (get-str  outunits "CORE00")))) (y0 0)
		(x0 0) (y0 0)
		(x1 0) (y1 0)
		(addr0) (addr1)
		(sname "CORE")
		(CORE.w (str-width (get-str  outunits "CORE00")))
		(bv0) (bv1))
;	(setf x0 (- x0 CORE.w))
	(dotimes (dimx (floor (math-pow 2 *yaddrs*)))
;	  (if (evenp dimx) (setf x1 (+ x1 (* 2 (str-width (get-str  outunits "CORE00"))))) (setf x1 x1))
	  (setf y1 0)
	  (dotimes (dimy (floor (* 4 *num-of-xdecs*)))
		(setf addr0 (floor (+ dimx (* dimy 2 (math-pow 2 *yaddrs*)))))
		(setf addr1 (floor (+ dimx (* (1+ (* dimy 2)) (math-pow 2 *yaddrs*)))))
		(setf bv0 (aref *code-map* addr0 (- *word-length* 1 num)))
		(setf bv1 (aref *code-map* addr1 (- *word-length* 1 num)))
		(setf sname (string+ "CORE" (string bv1) (string bv0)))
		(if (evenp dimx)
		  (push (make-array '(5) :initial-contents (list sname "y" "a0" (+ x0 x1) (+ y0 y1))) sref-map)
		  (push (make-array '(5) :initial-contents (list sname "n" "a0" (+ x0 x1) (+ y0 y1))) sref-map))
		(setf y1 (+ y1 (str-high (get-str  outunits sname)))))
	  (setf x1 (+ x1 CORE.w))
	  )
	(new-cell-2 outunits cell-name sref-map)))

;;;;;;;;wl_edge_LL_col
(print "wl_edge_LL_col")
(let ((sref-map (list)) (x0 0) (y0 0))
  (dotimes (dimx (* 2 *num-of-xdecs*))
	(if (evenp *number-of-cols-l*)
	  (progn
		(<cell-add  outunits srclib *dmy_wl_edgex4_even*)
		(push (<sref *dmy_wl_edgex4_even* "n" "a0" x0 y0) sref-map)
		(setf y0 (+ y0 (str-high (get-str  outunits *dmy_wl_edgex4_even*)))))
	  (progn
		(<cell-add  outunits srclib *dmy_wl_edgex4_odd*)
		(push (<sref *dmy_wl_edgex4_odd* "n" "a0" x0 y0) sref-map)
		(setf y0 (+ y0 (str-high (get-str  outunits *dmy_wl_edgex4_odd*)))))))
  (new-cell-2 outunits "wl_edge_LL_col" sref-map))

;;;;;;;;wl_edge_RR_col
(print "wl_edge_RR_col")
(let ((sref-map (list)) (x0 0) (y0 0))
  (dotimes (dimx (* 2 *num-of-xdecs*))
	(if (evenp *number-of-cols-r*)
	  (progn
		(<cell-add  outunits srclib *dmy_wl_edgex4_even*)
;		(push (<sref *dmy_wl_edgex4_even* "y" "a0" (+ x0 (str-width (get-str  outunits *dmy_wl_edgex4_even*))) y0) sref-map)
		(push (<sref *dmy_wl_edgex4_even* "y" "a0" x0 y0) sref-map)
		(setf y0 (+ y0 (str-high (get-str  outunits *dmy_wl_edgex4_even*)))))
	  (progn
		(<cell-add  outunits srclib *dmy_wl_edgex4_odd*)
;		(push (<sref *dmy_wl_edgex4_odd* "y" "a0" (+ x0 (str-width (get-str  outunits *dmy_wl_edgex4_odd*))) y0) sref-map)
		(push (<sref *dmy_wl_edgex4_odd* "y" "a0" x0 y0) sref-map)
		(setf y0 (+ y0 (str-high (get-str  outunits *dmy_wl_edgex4_odd*)))))))
  (new-cell-2 outunits "wl_edge_RR_col" sref-map))

;;;;;;;;tkwl_edge_LL
(print "tkwl_edge_LL")
(if (evenp *number-of-cols-l*)
  (progn
	(<cell-add  outunits srclib *dmy_tkwl_corner_even*)
(<cell outunits "tkwl_edge_LL"
(<sref *dmy_tkwl_corner_even* "n" "a0" 0 0)))
  (progn
	(<cell-add  outunits srclib *dmy_tkwl_corner_odd*)
(<cell outunits "tkwl_edge_LL"
(<sref *dmy_tkwl_corner_odd* "n" "a0" 0 0))))

;;;;;;;;tkwl_edge_RR
(print "tkwl_edge_RR")
(if (evenp *number-of-cols-r*)
  (progn
	(<cell-add  outunits srclib *dmy_tkwl_corner_even*)
(<cell outunits "tkwl_edge_RR"
;(<sref *dmy_tkwl_corner_even* "y" "a0" (str-width (get-str  outunits *dmy_tkwl_corner_even*)) 0)))
(<sref *dmy_tkwl_corner_even* "y" "a0" 0 0)))
  (progn
	(<cell-add  outunits srclib *dmy_tkwl_corner_odd*)
(<cell outunits "tkwl_edge_RR"
;(<sref *dmy_tkwl_corner_odd* "y" "a0" (str-width (get-str  outunits *dmy_tkwl_corner_odd*)) 0))))
(<sref *dmy_tkwl_corner_odd* "y" "a0" 0 0))))

;;;;;;;;core_ref_array
(print "core_ref_array")
(<cell-add  outunits srclib "CORE_REF_R2C2")
(let ((sref-map (list)) (x0 0) (y0 0))
  (dotimes (dimx (* 4 *num-of-xdecs*))
	(push (<sref "CORE_REF_R2C2" "n" "a0" x0 y0) sref-map)
	(setf y0 (+ y0 (str-high (get-str  outunits "CORE_REF_R2C2")))))
  (new-cell-2 outunits "core_ref_array" sref-map))

;;;;;;;;core_dummy_array
(print "core_dummy_array")
(let ((sref-map (list)) (x0 0) (y0 0))
  (dotimes (dimx (* 4 *num-of-xdecs*))
	(push (<sref "CORE11" "n" "a0" x0 y0) sref-map)
	(setf y0 (+ y0 (str-high (get-str  outunits "CORE11")))))
  (new-cell-2 outunits "core_dummy_array" sref-map))

;;;;;;;;bulk

;;;;;;;;;;;;;;;;;core_tkwl_sub_1ymux
(print "core_tkwl_sub_1ymux")
(<cell-add  outunits srclib "CORE_TKWL_SUBX8")
(let ((sref-map (list)) (x0 0) (y0 0))
  (dotimes (dimy (/ *col-mux* 8))
	(push (<sref "CORE_TKWL_SUBX8" "n" "a0" x0 y0) sref-map)
	(setf y0 (+ y0 (str-width (get-str  outunits "CORE_TKWL_SUBX8")))))
  (new-cell-2 outunits "core_tkwl_sub_1ymux" sref-map))

;;;;;;;;;;;;;;;;;core_tkwl_sub_2ymux
(print "core_tkwl_sub_2ymux")
(<cell-add  outunits srclib "DUMY_CORE_REF_SUB")
(new-cell-2 outunits "core_tkwl_sub_2ymux" (list
(<sref "core_tkwl_sub_1ymux" "n" "a0" 0 0)
(<sref "DUMY_CORE_REF_SUB" "n" "a0" (str-width (get-str  outunits "core_tkwl_sub_1ymux")) 0)
(<sref "core_tkwl_sub_1ymux" "n" "a0" (+
	(str-width (get-str  outunits "core_tkwl_sub_1ymux"))
	(str-width (get-str  outunits "DUMY_CORE_REF_SUB"))) 0)))

;;;;;;;;;;;;;;;;;tkwl_sub_L
(defun tkwl_sub_L ()
(print "tkwl_sub_L")
(let ((sref-map (list)) (x0 0) (y0 0))
  (dotimes (dimx (div1 *number-of-cols-l* 2))
	(push (<sref "core_tkwl_sub_2ymux" "n" "a0" x0 y0) sref-map)
	(setf x0 (+ x0 (str-width (get-str  outunits "core_tkwl_sub_2ymux")))))
  (new-cell-2 outunits "tkwl_sub_L" sref-map)))

;;;;;;;;;;;;;;;;;tkwl_sub_left
(print "tkwl_sub_left")
(<cell-add  outunits srclib "CORE_TKWL_SUB")
(if (= 0 (mod *number-of-cols-l* 2))
  (progn
	(tkwl_sub_L)
(<cell outunits "tkwl_sub_left"
(<sref "tkwl_sub_L" "n" "a0" 0 0)
(<sref "CORE_TKWL_SUB" "n" "a0"
	   (- 0 (str-width (get-str  outunits "CORE_TKWL_SUB"))) 0)))
  (if (= 0 (/ *number-of-cols-l* 2))
	(progn
(<cell outunits "tkwl_sub_left"
(<sref "core_tkwl_sub_1ymux" "n" "a0" 0 0)
(<sref "DUMY_CORE_REF_SUB" "n" "a0" (- 0 (str-width (get-str  outunits "core_tkwl_sub_1ymux"))) 0)))
	(progn
	  (tkwl_sub_L)
(<cell outunits "tkwl_sub_left"
(<sref "tkwl_sub_L" "n" "a0" 0 0)
(<sref "core_tkwl_sub_1ymux" "n" "a0"
	   (- 0 (str-width (get-str  outunits "core_tkwl_sub_1ymux"))) 0)
(<sref "DUMY_CORE_REF_SUB" "n" "a0"
	   (- 0 (str-width (get-str  outunits "core_tkwl_sub_1ymux")) (str-width (get-str  outunits "DUMY_CORE_REF_SUB"))) 0)))))

;;;;;;;;;;;;;;;;;tkwl_sub_R
(defun tkwl_sub_R ()
(print "tkwl_sub_R")
(let ((sref-map (list)) (x0 0) (y0 0))
  (dotimes (dimx (div1 *number-of-cols-r* 2))
	(push (<sref "core_tkwl_sub_2ymux" "n" "a0" x0 y0) sref-map)
	(setf x0 (+ x0 (str-width (get-str  outunits "core_tkwl_sub_2ymux")))))
  (new-cell-2 outunits "tkwl_sub_R" sref-map)))

;;;;;;;;;;;;;;;;;tkwl_sub_right
(print "tkwl_sub_right")
(if (= 0 (mod *number-of-cols-r* 2))
  (progn
	(tkwl_sub_R)
(<cell outunits "tkwl_sub_right"
(<sref "tkwl_sub_R" "n" "a0" 0 0)
;(<sref "CORE_TKWL_SUB" "y" "a0" (+ 0 (str-width (get-str  outunits "tkwl_sub_R"))
;								   (str-width (get-str  outunits "CORE_TKWL_SUB"))) 0)))
(<sref "CORE_TKWL_SUB" "y" "a0" (+ 0 (str-width (get-str  outunits "tkwl_sub_R"))) 0)))
  (if (= 0 (/ *number-of-cols-r* 2))
	(progn
(<cell outunits "tkwl_sub_right"
(<sref "core_tkwl_sub_1ymux" "n" "a0" 0 0)
(<sref "DUMY_CORE_REF_SUB" "n" "a0" (+ 0 (str-width (get-str  outunits "DUMY_CORE_REF_SUB"))) 0)))
	(progn
	  (tkwl_sub_R)
(<cell outunits "tkwl_sub_right"
(<sref "tkwl_sub_R" "n" "a0" 0 0)
(<sref "core_tkwl_sub_1ymux" "n" "a0"
	   (+ 0 (str-width (get-str  outunits "tkwl_sub_R"))) 0)
(<sref "DUMY_CORE_REF_SUB" "n" "a0"
	   (+ 0 (str-width (get-str  outunits "tkwl_sub_R")) (str-width (get-str  outunits "core_tkwl_sub_1ymux"))) 0)))))

;;;;;;;;core_tkwl_array

;;;;;;;;;;;;;;;;;;core_tkwl_r4c4
(print "core_tkwl_r4c4")
(<cell-add  outunits srclib "CORE_TKWL_R4C1")
(let ((x0 (* 4 (str-width (get-str outunits "CORE_TKWL_R4C1")))))
(<cell outunits "core_tkwl_r4c4"
(<sref "CORE_TKWL_R4C1" "n" "a0" x0 0)
(<sref "CORE_TKWL_R4C1" "y" "a0" (- x0 (str-width (get-str outunits "CORE_TKWL_R4C1"))) 0)
(<sref "CORE_TKWL_R4C1" "n" "a0" (- x0 (* 2 (str-width (get-str outunits "CORE_TKWL_R4C1")))) 0)
(<sref "CORE_TKWL_R4C1" "y" "a0" (- x0 (* 3 (str-width (get-str outunits "CORE_TKWL_R4C1")))) 0)))

;;;;;;;;;;;;;;;;;;core_tkwl_r4c4_tie
(print "core_tkwl_r4c4_tie")
(<cell-add  outunits srclib "CORE_TKWL_R4C1_TIE")
;(let ((x0 (+ (* 2 (str-width (get-str outunits "CORE_TKWL_R4C1_TIE"))) (str-width (get-str outunits "CORE_TKWL_R4C1")))))
(let ((x0 0))
(<cell outunits "core_tkwl_r4c4_tie"
(<sref "CORE_TKWL_R4C1" "n" "a0" x0 0)
(<sref "CORE_TKWL_R4C1_TIE" "y" "a0" (- x0 (str-width (get-str outunits "CORE_TKWL_R4C1"))) 0)
(<sref "CORE_TKWL_R4C1_TIE" "n" "a0" (- x0 (str-width (get-str outunits "CORE_TKWL_R4C1"))
										 (str-width (get-str outunits "CORE_TKWL_R4C1_TIE"))) 0)
(<sref "CORE_TKWL_R4C1" "y" "a0" (- x0 (str-width (get-str outunits "CORE_TKWL_R4C1"))
										 (str-width (get-str outunits "CORE_TKWL_R4C1_TIE"))
										 (str-width (get-str outunits "CORE_TKWL_R4C1"))) 0)))

;;;;;;;;;;;;;;;;;core_tkwl_1ymux
(print "core_tkwl_1ymux")
(defun core_tkwl_1ymux ()
(let ((sref-map (list)) (x0 0) (y0 0))
  (dotimes (dimx (div1 (math-pow 2 *yaddrs*) 4))
	(push (<sref "core_tkwl_r4c4" "n" "a0" x0 y0) sref-map)
	(setf x0 (+ x0 (str-width (get-str  outunits "core_tkwl_r4c4")))))
  (new-cell-2 outunits "core_tkwl_1ymux" sref-map)))
(core_tkwl_1ymux)

;;;;;;;;;;;;;;;;;core_tkwl_1ymux_tie_mid
(print "core_tkwl_1ymux_tie_mid")
(defun core_tkwl_1ymux_tie_mid ()
(let ((sref-map (list)) (x0 0) (y0 0))
  (dotimes (dimx (div1 (math-pow 2 *yaddrs*) 4))
	(push (if (= dimx (div1 (math-pow 2 *yaddrs*) 8))
			(<sref "core_tkwl_r4c4_tie" "n" "a0" x0 y0)
			(<sref "core_tkwl_r4c4" "n" "a0" x0 y0)) sref-map)
	(setf x0 (+ x0 (str-width (get-str  outunits "core_tkwl_r4c4")))))
  (new-cell-2 outunits "core_tkwl_1ymux_tie_mid" sref-map)))
(core_tkwl_1ymux_tie_mid)

;;;;;;;;;;;;;;;;;core_tkwl_1ymux_tie_l
(print "core_tkwl_1ymux_tie_l")
(defun core_tkwl_1ymux_tie_l ()
(let ((sref-map (list)) (x0 0) (y0 0))
  (dotimes (dimx (div1 (math-pow 2 *yaddrs*) 4))
	(push (if (= dimx 0)
			(<sref "core_tkwl_r4c4_tie" "n" "a0" x0 y0)
			(<sref "core_tkwl_r4c4" "n" "a0" x0 y0)) sref-map)
	(setf x0 (+ x0 (str-width (get-str  outunits "core_tkwl_r4c4")))))
  (new-cell-2 outunits "core_tkwl_1ymux_tie_l" sref-map)))
(core_tkwl_1ymux_tie_l)

;;;;;;;;;;;;;;;;;core_tkwl_2ymux
(print "core_tkwl_2ymux")
(<cell-add  outunits srclib "DUMY_CORE_REF_R4C2")
(<cell outunits "core_tkwl_2ymux"
(<sref "core_tkwl_1ymux" "n" "a0" 0 0)
(<sref "DUMY_CORE_REF_R4C2" "n" "a0" (+ 0 (str-width (get-str outunits "core_tkwl_1ymux"))) 0)
(<sref "core_tkwl_1ymux" "n" "a0" (+ 0 (str-width (get-str outunits "core_tkwl_1ymux"))
									 (str-width (get-str outunits "DUMY_CORE_REF_R4C2"))) 0))

;;;;;;;;;;;;;;;;;core_tkwl_2ymux_r
(print "core_tkwl_2ymux_r")
(<cell-add  outunits srclib "DUMY_CORE_REF_R4C2_MID")
(<cell outunits "core_tkwl_2ymux_r"
(<sref "core_tkwl_1ymux" "n" "a0" 0 0)
(<sref "DUMY_CORE_REF_R4C2_MID" "n" "a0" (+ 0 (str-width (get-str outunits "core_tkwl_1ymux"))) 0)
(<sref "core_tkwl_1ymux" "n" "a0" (+ 0 (str-width (get-str outunits "core_tkwl_1ymux"))
									 (str-width (get-str outunits "DUMY_CORE_REF_R4C2_MID"))) 0))

;;;;;;;;;;;;;;;;;;core_tkwl_2ymux_tie_mid
(print "core_tkwl_2ymux_tie_mid")
(<cell-add  outunits srclib "DUMY_CORE_REF_R4C2")
(<cell outunits "core_tkwl_2ymux_tie_mid"
(<sref "core_tkwl_1ymux" "n" "a0" 0 0)
(<sref "DUMY_CORE_REF_R4C2" "n" "a0" (+ 0 (str-width (get-str outunits "core_tkwl_1ymux"))) 0)
(<sref "core_tkwl_1ymux_tie_l" "n" "a0" (+ 0 (str-width (get-str outunits "core_tkwl_1ymux"))
									 (str-width (get-str outunits "DUMY_CORE_REF_R4C2"))) 0))

;;;;;;;;;;;;;;;;;;core_tkwl_2ymux_tie_l
(print "core_tkwl_2ymux_tie_l")
(<cell-add  outunits srclib "DUMY_CORE_REF_R4C2_MID")
(<cell outunits "core_tkwl_2ymux_tie_l"
(<sref "core_tkwl_1ymux_tie_l" "n" "a0" 0 0)
(<sref "DUMY_CORE_REF_R4C2_MID" "n" "a0" (+ 0 (str-width (get-str outunits "core_tkwl_1ymux_tie_l"))) 0)
(<sref "core_tkwl_1ymux" "n" "a0" (+ 0 (str-width (get-str outunits "core_tkwl_1ymux_tie_l"))
									 (str-width (get-str outunits "DUMY_CORE_REF_R4C2_MID"))) 0))

;;;;;;;;;;;;;;;;;;core_tkwl_2ymux_tie_r_mid
(print "core_tkwl_2ymux_tie_r_mid")
(<cell-add  outunits srclib "DUMY_CORE_REF_R4C2")
(<cell outunits "core_tkwl_2ymux_tie_r_mid"
(<sref "core_tkwl_1ymux" "n" "a0" 0 0)
(<sref "DUMY_CORE_REF_R4C2" "n" "a0" (+ 0 (str-width (get-str outunits "core_tkwl_1ymux"))) 0)
(<sref "core_tkwl_1ymux_tie_mid" "n" "a0" (+ 0 (str-width (get-str outunits "core_tkwl_1ymux"))
									 (str-width (get-str outunits "DUMY_CORE_REF_R4C2"))) 0))

;;;;;;;;;;;;;;;;;;core_tkwl_2ymux_tie_l_mid
(print "core_tkwl_2ymux_tie_l_mid")
(<cell-add  outunits srclib "DUMY_CORE_REF_R4C2_MID")
(<cell outunits "core_tkwl_2ymux_tie_l_mid"
(<sref "core_tkwl_1ymux_tie_mid" "n" "a0" 0 0)
(<sref "DUMY_CORE_REF_R4C2_MID" "n" "a0" (+ 0 (str-width (get-str outunits "core_tkwl_1ymux_tie_mid"))) 0)
(<sref "core_tkwl_1ymux" "n" "a0" (+ 0 (str-width (get-str outunits "core_tkwl_1ymux_tie_mid"))
									 (str-width (get-str outunits "DUMY_CORE_REF_R4C2_MID"))) 0))

;;;;;;;;;;;;;;;;;;core_tkwl_dummy
(print "core_tkwl_dummy")
(let ((x0 (str-width (get-str outunits "CORE00"))))
(<cell outunits "core_tkwl_dummy"
(<sref "CORE00" "y" "a0" x0 0)
(<sref "CORE00" "y" "a0" x0 (str-high (get-str outunits "CORE00")))))

;;;;;;;;;;;;;;;;;;core_tkwl_L
(defun core_tkwl_L ()
(print "core_tkwl_L")
(let ((sref-map (list)) (x0 0) (y0 0))
  (dotimes (dimx (div1 *number-of-cols-l* 2))
	(push (<sref "core_tkwl_2ymux" "n" "a0" x0 y0) sref-map)
	(setf x0 (+ x0 (str-width (get-str  outunits "core_tkwl_2ymux")))))
  (new-cell-2 outunits "core_tkwl_L" sref-map)))

;;;;;;;;;;;;;;;;;;core_tkwl_left
(print "core_tkwl_left")
(<cell-add  outunits srclib "DUMY_CORE_REF_R4C2")
(let ((x0 0)
	  (y0 0)
	  (core_tkwl_L.w)
	  (core_tkwl_L.h)
	  (core_tkwl_1ymux.w (str-width (get-str  outunits "core_tkwl_1ymux")))
	  (core_tkwl_1ymux.h (str-high (get-str  outunits "core_tkwl_1ymux")))
	  (DUMY_CORE_REF_R4C2.w (str-width (get-str  outunits "DUMY_CORE_REF_R4C2")))
	  (tkwl_edge_LL.w (str-width (get-str  outunits "tkwl_edge_LL")))
	  (tkwl_sub_left.w (str-width (get-str  outunits "tkwl_sub_left"))))
(if (= 0 (mod *number-of-cols-l* 2))
  (progn
	(core_tkwl_L)
	(setf core_tkwl_L.w (str-width (get-str  outunits "core_tkwl_L")))
	(setf core_tkwl_L.h (str-high (get-str  outunits "core_tkwl_L")))
	(setf x0 (+ 0 core_tkwl_dummy.w tkwl_edge_LL.w))
(<cell outunits "core_tkwl_left"
(<sref "core_tkwl_L" "n" "a0" x0 y0)
(<sref "core_tkwl_dummy" "y" "a0" (- x0 core_tkwl_L.w) y0)
(<sref "tkwl_edge_LL" "n" "a0" (- x0 core_tkwl_dummy.w tkwl_edge_LL.w) y0)
(<sref "tkwl_sub_left" "n" "a0" ( + x0 core_tkwl_L.w (* -1 tkwl_sub_left.w)) (+ y0 core_tkwl_L.h))))
  (if (= 0 (div1 *number-of-cols-l* 2))
	(progn
	  (setf x0 (+ 0 DUMY_CORE_REF_R4C2.w tkwl_edge_LL.w))
(<cell outunits "core_tkwl_left"
(<sref "core_tkwl_1ymux" "n" "a0" x0 y0)
(<sref "DUMY_CORE_REF_R4C2" "n" "a0" (- x0 DUMY_CORE_REF_R4C2.w ) y0)
(<sref "tkwl_edge_LL" "n" "a0" (- x0 DUMY_CORE_REF_R4C2.w tkwl_edge_LL.w ) y0)
(<sref "tkwl_sub_left" "n" "a0" ( + x0 core_tkwl_L.w (* -1 tkwl_sub_left.w)) (+ y0 core_tkwl_1ymux.h))))
	(progn
	  (core_tkwl_L)
	  (setf core_tkwl_L.w (str-width (get-str  outunits "core_tkwl_L")))
	  (setf core_tkwl_L.h (str-high (get-str  outunits "core_tkwl_L")))
	  (setf x0 (+ 0 DUMY_CORE_REF_R4C2.w core_tkwl_1ymux.w tkwl_edge_LL.w))
(<cell outunits "core_tkwl_left"
(<sref "core_tkwl_L" "n" "a0" x0 y0)
(<sref "core_tkwl_1ymux" "n" "a0" (- x0 core_tkwl_1ymux.w) y0)
(<sref "DUMY_CORE_REF_R4C2" "n" "a0" (- x0 core_tkwl_1ymux.w DUMY_CORE_REF_R4C2.w) y0)
(<sref "tkwl_edge_LL" "n" "a0" (- x0 core_tkwl_1ymux.w DUMY_CORE_REF_R4C2.w tkwl_edge_LL.w) y0)
(<sref "tkwl_sub_left" "n" "a0" ( + x0 core_tkwl_L.w (* -1 tkwl_sub_left.w)) (+ y0 core_tkwl_L.h)))))))

;;;;;;;;;;;;;;;;;;core_tkwl_R
(defun core_tkwl_R (y1 y4)
(print "core_tkwl_R")
(let ((sref-map (list)) (x0 0) (y0 0) (sname))
  (dotimes (dimx (div1 *number-of-cols-l* 2))
	(setf sname
		  (if (= 1 (mod (div1 *number-of-cols-r* 2) 2))
			(if (= dimx (div1 *number-of-cols-r* 4))
			  (string+ "core_tkwl_2ymux" y1)
			  (if (> dimx (div1 *number-of-cols-r* 4))
				"core_tkwl_2ymux_r" "core_tkwl_2ymux"))
			(if (= dimx (div1 *number-of-cols-r* 4))
			  (string+ "core_tkwl_2ymux" y4)
			  (if (> dimx (div1 *number-of-cols-r* 4))
				"core_tkwl_2ymux_r" "core_tkwl_2ymux"))))
	(push (<sref sname "n" "a0" x0 y0) sref-map)
	(setf x0 (+ x0 (str-width (get-str  outunits sname)))))
  (new-cell-2 outunits "core_tkwl_R" sref-map)))

;;;;;;;;;;;;;;;;;;core_tkwl_right
(print "core_tkwl_right")
(let ((x0 0)
	  (y0 0)
	  (core_tkwl_R.w)
	  (core_tkwl_R.h)
	  (core_tkwl_dummy.w (str-width (get-str  outunits "core_tkwl_dummy")))
	  (core_tkwl_dummy.h (str-high (get-str  outunits "core_tkwl_dummy")))
	  (tkwl_edge_RR.w (str-width (get-str  outunits "tkwl_edge_RR")))
	  (tkwl_edge_RR.h (str-high (get-str  outunits "tkwl_edge_RR")))
	  (tkwl_sub_right.w (str-width (get-str  outunits "tkwl_sub_right")))
	  (tkwl_sub_right.h (str-high (get-str  outunits "tkwl_sub_right")))
	  (core_tkwl_1ymux_tie_l.w (str-width (get-str  outunits "core_tkwl_1ymux_tie_l")))
	  (core_tkwl_1ymux_tie_l.h (str-high (get-str  outunits "core_tkwl_1ymux_tie_l")))
	  (DUMY_CORE_REF_R4C2.w (str-width (get-str  outunits "DUMY_CORE_REF_R4C2")))
	  (DUMY_CORE_REF_R4C2.h (str-high (get-str  outunits "DUMY_CORE_REF_R4C2")))
	  (core_tkwl_2ymux.w (str-width (get-str  outunits "core_tkwl_2ymux")))
	  (core_tkwl_2ymux.h (str-high (get-str  outunits "core_tkwl_2ymux"))))
  (when (and (evenp *number-of-cols-r*) (evenp *number-of-cols-l*))
	(core_tkwl_R "_tie_mid" "_tie_l")
	(setf core_tkwl_R.w (str-width (get-str  outunits "core_tkwl_R")))
	(setf core_tkwl_R.h (str-high (get-str  outunits "core_tkwl_R")))
	(<cell outunits "core_tkwl_right"
		   (<sref "core_tkwl_R" "n" "a0" x0 y0)
		   (<sref "core_tkwl_dummy" "n" "a0" (+ x0 core_tkwl_R.w) y0)
		   (<sref "tkwl_edge_RR" "n" "a0" (+ x0 core_tkwl_R.w tkwl_edge_RR.w) y0)
		   (<sref "core_tkwl_R" "n" "a0" x0 (+ y0 core_tkwl_R.h))))
  (when (and (evenp *number-of-cols-r*) (oddp *number-of-cols-l*))
	(core_tkwl_R "_tie_r_mid" "_tie_l_mid")
	(setf core_tkwl_R.w (str-width (get-str  outunits "core_tkwl_R")))
	(setf core_tkwl_R.h (str-high (get-str  outunits "core_tkwl_R")))
	(<cell outunits "core_tkwl_right"
		   (<sref "core_tkwl_R" "n" "a0" x0 y0)
		   (<sref "core_tkwl_dummy" "n" "a0" (+ x0 core_tkwl_R.w) y0)
		   (<sref "tkwl_edge_RR" "n" "a0" (+ x0 core_tkwl_R.w core_tkwl_dummy.w) y0)
		   (<sref "tkwl_sub_right" "n" "a0" x0 (+ y0 core_tkwl_R.h))))
  (when (and (oddp *number-of-cols-r*) (evenp *number-of-cols-l*))
	(if (= 0 (div1 *number-of-cols-r* 2))
	  (progn
		(<cell outunits "core_tkwl_right"
			   (<sref "core_tkwl_1ymux_tie_l" "y" "a0" x0 y0)
			   (<sref "DUMY_CORE_REF_R4C2" "n" "a0" (+ x0 core_tkwl_1ymux_tie_l.w) y0)
			   (<sref "tkwl_edge_RR" "n" "a0" (+ x0 core_tkwl_1ymux_tie_l.w DUMY_CORE_REF_R4C2.w) y0)
			   (<sref "tkwl_sub_right" "n" "a0" x0 (+ y0 core_tkwl_1ymux_tie_l.h))))
	  (if (= 1 (div1 *number-of-cols-r* 2))
		(progn
		  (<cell outunits "core_tkwl_right"
				 (<sref "core_tkwl_2ymux" "n" "a0" x0 y0)
				 (<sref "core_tkwl_1ymux_tie_l" "n" "a0" (+ x0 core_tkwl_2ymux.w) y0)
				 (<sref "DUMY_CORE_REF_R4C2" "y" "a0" (+ x0 core_tkwl_2ymux.w core_tkwl_1ymux_tie_l.w) y0)
				 (<sref "tkwl_edge_RR" "n" "a0" (+ x0 core_tkwl_2ymux.w core_tkwl_1ymux_tie_l.w DUMY_CORE_REF_R4C2.w) y0)
				 (<sref "tkwl_sub_right" "n" "a0" x0 (+ y0 core_tkwl_2ymux.h))))
		(progn
		  (core_tkwl_R "_tie_l" "_tie_l_mid")
		  (setf core_tkwl_R.w (str-width (get-str  outunits "core_tkwl_R")))
		  (setf core_tkwl_R.h (str-high (get-str  outunits "core_tkwl_R")))
		  (<cell outunits "core_tkwl_right"
				 (<sref "core_tkwl_R" "n" "a0" x0 y0)
				 (<sref "core_tkwl_1ymux" "n" "a0" (+ x0 core_tkwl_R.w) y0)
				 (<sref "DUMY_CORE_REF_R4C2" "y" "a0" (+ x0 core_tkwl_R.w core_tkwl_1ymux.w) y0)
				 (<sref "tkwl_edge_RR" "n" "a0" (+ x0 core_tkwl_R.w core_tkwl_1ymux.w DUMY_CORE_REF_R4C2.w) y0)
				 (<sref "tkwl_sub_right" "n" "a0" x0 (+ y0 core_tkwl_R.h)))))))
  (when (and (oddp *number-of-cols-r*) (oddp *number-of-cols-l*))
	(if (= 0 (div1 *number-of-cols-r* 2))
	  (progn
		(<cell outunits "core_tkwl_right"
			   (<sref "core_tkwl_1ymux_tie_mid" "n" "a0" x0 y0)
			   (<sref "DUMY_CORE_REF_R4C2" "y" "a0" (+ x0 core_tkwl_1ymux_tie_mid.w) y0)
			   (<sref "tkwl_edge_RR" "n" "a0" (+ x0 core_tkwl_1ymux_tie_mid.w DUMY_CORE_REF_R4C2.w) y0)
			   (<sref "tkwl_sub_right" "n" "a0" x0 (+ y0 core_tkwl_1ymux_tie_mid.h))))
	  (progn
		(core_tkwl_R "_tie_r_mid" "_tie_l_mid")
		(setf core_tkwl_R.w (str-width (get-str  outunits "core_tkwl_R")))
		(setf core_tkwl_R.h (str-high (get-str  outunits "core_tkwl_R")))
		(<cell outunits "core_tkwl_right"
			   (<sref "core_tkwl_R" "n" "a0" x0 y0)
			   (<sref "core_tkwl_1ymux" "n" "a0" (+ x0 core_tkwl_R.w) y0))))))

;;;;;;;;bl_edge_per_2ymux

;;;;;;;;;;;;;;;;;;bl_edger4c1
(print "bl_edger4c1")
(<cell-add  outunits srclib "BL_EDGE")
(let ((BL_EDGE.w (str-width (get-str  outunits "BL_EDGE")))
	  (x0 0)
	  (y0 0))
  (setf x0 (* 3 BL_EDGE.w))
(<cell outunits "bl_edger4c1"
	   (<sref "BL_EDGE" "n" "a0" x0 y0)
	   (<sref "BL_EDGE" "y" "a0" (- x0 BL_EDGE.w) y0)
	   (<sref "BL_EDGE" "n" "a0" (- x0 BL_EDGE.w BL_EDGE.w) y0)
	   (<sref "BL_EDGE" "y" "a0" (- x0 BL_EDGE.w BL_EDGE.w BL_EDGE.w) y0)))

;;;;;;;;;;;;;;;;;bl_edge_per_1ymux
(print "bl_edge_per_1ymux")
(defun bl_edge_per_1ymux ()
(let ((sref-map (list)) (x0 0) (y0 0))
  (dotimes (dimx (div1 (math-pow 2 *yaddrs*) 4))
	(push (<sref "bl_edger4c1" "n" "a0" x0 y0) sref-map)
	(setf x0 (+ x0 (str-width (get-str  outunits "bl_edger4c1")))))
  (new-cell-2 outunits "bl_edge_per_1ymux" sref-map)))
(bl_edge_per_1ymux)

;;;;;;;;;;;;;;;;;bl_edge_per_2ymux
(print "bl_edge_per_2ymux")
(<cell-add  outunits srclib "CORE_REF_R2C2_EDGE")
(<cell outunits "bl_edge_per_2ymux"
	   (<sref "bl_edge_per_1ymux" "n" "a0" 0 0)
	   (<sref "CORE_REF_R2C2_EDGE" "n" "a0" (+ 0 (str-width (get-str  outunits "bl_edge_per_1ymux"))) 0)
	   (<sref "bl_edge_per_1ymux" "n" "a0" (+ 0 (str-width (get-str  outunits "bl_edge_per_1ymux"))
											  (str-width (get-str  outunits "CORE_REF_R2C2_EDGE"))) 0))

;;;;;;;;;;;;;;;;;bl_edge_L
(defun bl_edge_L ()
(print "bl_edge_L")
(let ((sref-map (list)) (x0 0) (y0 0))
  (dotimes (dimx (div1 *number-of-cols-l* 2))
	(push (<sref "bl_edge_per_2ymux" "n" "a0" x0 y0) sref-map)
	(setf x0 (+ x0 (str-width (get-str  outunits "bl_edge_per_2ymux")))))
  (new-cell-2 outunits "bl_edge_L" sref-map)))

;;;;;;;;;;;;;;;;;bl_edge_left
(print "bl_edge_left")
(<cell-add  outunits srclib *dmy_wl_corner_even*)
(<cell-add  outunits srclib *dmy_wl_corner_odd*)
(let ((x0 0)
	  (y0 0)
	  (bl_edge_L.w)
	  (BL_EDGE.w (str-width (get-str  outunits "BL_EDGE")))
	  (dmy_wl_corner.w (str-width (get-str  outunits (if (evenp *number-of-cols-l*) *dmy_wl_corner_even* *dmy_wl_corner_odd*))))
	  (bl_edge_per_1ymux.w (str-width (get-str  outunits "bl_edge_per_1ymux")))
	  (CORE_REF_R2C2_EDGE.w (str-width (get-str  outunits "CORE_REF_R2C2_EDGE"))))
  (if (evenp *number-of-cols-l*)
	(progn
	  (bl_edge_L)
	  (setf bl_edge_L.w (str-width (get-str  outunits "bl_edge_L")))
	  (setf x0 (+ 0 dmy_wl_corner.w BL_EDGE.w))
	  (<cell outunits "bl_edge_left"
			 (<sref "bl_edge_L" "n" "a0" x0 y0)
			 (<sref "BL_EDGE" "n" "a0" (- x0 BL_EDGE.w) y0)
			 (<sref *dmy_wl_corner_even* "n" "a0" (- x0 BL_EDGE.w dmy_wl_corner.w) y0)
			 )
	  )
	(if (= 0 (div1 *number-of-cols-l* 2))
	  (progn
		(setf x0 (+ 0 dmy_wl_corner.w CORE_REF_R2C2_EDGE.w))
		(<cell outunits "bl_edge_left"
			   (<sref "bl_edge_per_1ymux" "n" "a0" x0 y0)
			   (<sref "CORE_REF_R2C2_EDGE" "n" "a0" (- x0 CORE_REF_R2C2_EDGE.w) y0)
			   (<sref *dmy_wl_corner_odd* "n" "a0" (- x0 CORE_REF_R2C2_EDGE.w dmy_wl_corner.w) y0)
			   ))
	  (progn
		(bl_edge_L)
		(setf bl_edge_L.w (str-width (get-str  outunits "bl_edge_L")))
		(setf x0 (+ 0 dmy_wl_corner.w CORE_REF_R2C2_EDGE.w bl_edge_per_1ymux.w))
		(<cell outunits "bl_edge_left"
			   (<sref "bl_edge_L" "n" "a0" x0 y0)
			   (<sref "bl_edge_per_1ymux" "n" "a0" (- x0 bl_edge_per_1ymux.w) y0)
			   (<sref "CORE_REF_R2C2_EDGE" "n" "a0" (- x0 bl_edge_per_1ymux.w CORE_REF_R2C2_EDGE.w) y0)
			   (<sref *dmy_wl_corner_odd* "n" "a0" (- x0 bl_edge_per_1ymux.w CORE_REF_R2C2_EDGE.w dmy_wl_corner.w) y0)
			   )
		)))
  )


;;;;;;;;;;;;;;;;;bl_edge_R
(defun bl_edge_R ()
(print "bl_edge_R")
(let ((sref-map (list)) (x0 0) (y0 0))
  (dotimes (dimx (div1 *number-of-cols-r* 2))
	(push (<sref "bl_edge_per_2ymux" "n" "a0" x0 y0) sref-map)
	(setf x0 (+ x0 (str-width (get-str  outunits "bl_edge_per_2ymux")))))
  (new-cell-2 outunits "bl_edge_R" sref-map)))

;;;;;;;;;;;;;;;;;bl_edge_right
(print "bl_edge_right")
(let ((x0 0)
	  (y0 0)
	  (bl_edge_R.w)
	  (BL_EDGE.w (str-width (get-str  outunits "BL_EDGE")))
	  (dmy_wl_corner.w (str-width (get-str  outunits (if (evenp *number-of-cols-r*) *dmy_wl_corner_even* *dmy_wl_corner_odd*))))
	  (bl_edge_per_1ymux.w (str-width (get-str  outunits "bl_edge_per_1ymux")))
	  (CORE_REF_R2C2_EDGE.w (str-width (get-str  outunits "CORE_REF_R2C2_EDGE"))))
  (if (evenp *number-of-cols-r*)
	(progn
	  (bl_edge_R)
	  (setf bl_edge_R.w (str-width (get-str  outunits "bl_edge_R")))
	  (<cell outunits "bl_edge_right"
			 (<sref "bl_edge_R" "n" "a0" x0 y0)
			 (<sref "BL_EDGE" "y" "a0" (+ x0 bl_edge_R.w) y0)
			 (<sref *dmy_wl_corner_even* "y" "a0" (+ x0 bl_edge_R.w BL_EDGE.w) y0)))
	(if (= 0 (div1 *number-of-cols-r* 2))
	  (progn
		(<cell outunits "bl_edge_right"
			   (<sref "bl_edge_per_1ymux" "n" "a0" x0 y0)
			   (<sref "CORE_REF_R2C2_EDGE" "y" "a0" (+ x0 bl_edge_per_1ymux.w) y0)
			   (<sref *dmy_wl_corner_odd* "y" "a0" (+ x0 bl_edge_per_1ymux.w CORE_REF_R2C2_EDGE.w) y0)))
	  (progn
		(bl_edge_R)
		(setf bl_edge_R.w (str-width (get-str  outunits "bl_edge_R")))
		(<cell outunits "bl_edge_right"
			   (<sref "bl_edge_R" "n" "a0" x0 y0)
			   (<sref "bl_edge_per_1ymux" "n" "a0" (+ x0 bl_edge_R.w) y0)
			   (<sref "CORE_REF_R2C2_EDGE" "y" "a0" (+ x0 bl_edge_R.w) y0)
			   (<sref *dmy_wl_corner_odd* "y" "a0" (+ x0 bl_edge_R.w CORE_REF_R2C2_EDGE.w) y0))))))

;;;;;;;;left core

;;;;;;;;;;;;;;;;;core_left
(print "core_left")
(let ((x0 0)
	  (y0 0)
	  (array_bit.w)
	  (array_left.w)
	  (array_left.h)
	  (core_dummy_array.w (str-width (get-str  outunits "core_dummy_array")))
	  (core_dummy_array.h (str-high (get-str  outunits "core_dummy_array")))
	  (wl_edge_LL_col.w (str-width (get-str  outunits "wl_edge_LL_col")))
	  (wl_edge_LL_col.h (str-high (get-str  outunits "wl_edge_LL_col")))
	  (core_tkwl_left.w (str-width (get-str  outunits "core_tkwl_left")))
	  (core_tkwl_left.h (str-high (get-str  outunits "core_tkwl_left")))
	  (bl_edge_left.w (str-width (get-str  outunits "bl_edge_left")))
	  (bl_edge_left.h (str-high (get-str  outunits "bl_edge_left")))
	  (core_ref_array.w (str-width (get-str  outunits "core_ref_array")))
	  (core_ref_array.h (str-high (get-str  outunits "core_ref_array")))
	  (dimxn 0)
	  (sref-map (list)))
  (if (evenp *number-of-cols-l*)
	(progn
	  (when (= 0 *charmode*)
		(setf dimxn 0)
		(dotimes (dimx *number-of-cols-l*)
		  (array_bit dimx)
		  (setf array_bit.w (str-width (get-str  outunits (string+ "array_bit_" (itoa dimx)))))
		  (push (<sref (string+ "array_bit_" (itoa dimx)) "n" "a0" dimxn 0) sref-map)
		  (setf dimxn (+ dimxn array_bit.w))
		  (if (evenp dimx)
			(progn
			  (push (<sref "core_ref_array" "n" "a0" dimxn 0) sref-map)
			  (setf dimxn (+ dimxn core_ref_array.w))) nil))
		(new-cell-2 outunits "array_left" sref-map))
	  (when (= 1 *charmode*) nil)
	  (when (= 2 *charmode*) nil)
	  (setf array_left.w (str-width (get-str  outunits "array_left")))
	  (setf array_left.h (str-high (get-str  outunits "array_left")))
;	  (setf x0 (- 0 core_dummy_array.w wl_edge_LL_col.w))
;	  (setf y0 (+ 0 bl_edge_left.h))
	  (<cell outunits "core_left"
			 (<sref "array_left" "n" "a0" x0 y0)
			 (<sref "core_dummy_array" "n" "a0" (- x0 core_dummy_array.w) y0)
			 (<sref "wl_edge_LL_col" "n" "a0" (- x0 core_dummy_array.w wl_edge_LL_col.w) y0)
			 (<sref "core_tkwl_left" "n" "a0" (- x0 core_dummy_array.w wl_edge_LL_col.w) (+ y0 wl_edge_LL_col.h))
			 (<sref "bl_edge_left" "n" "a0" (- x0 core_dummy_array.w wl_edge_LL_col.w) (- y0 bl_edge_left.h))))
	(progn
	  (when (= 0 *charmode*)
		(setf dimxn 0)
		(dotimes (dimx *number-of-cols-l*)
		  (array_bit dimx)
		  (setf array_bit.w (str-width (get-str  outunits (string+ "array_bit_" (itoa dimx)))))
		  (push (<sref (string+ "array_bit_" (itoa dimx)) "n" "a0" dimxn 0) sref-map)
		  (setf dimxn (+ dimxn array_bit.w))
		  (if (oddp dimx)
			(progn
			  (push (<sref "core_ref_array" "n" "a0" dimxn 0) sref-map)
			  (setf dimxn (+ dimxn core_ref_array.w))) nil))
		(new-cell-2 outunits "array_left" sref-map))
	  (when (= 1 *charmode*) nil)
	  (when (= 2 *charmode*) nil)
	  (setf array_left.w (str-width (get-str  outunits "array_left")))
	  (setf array_left.h (str-high (get-str  outunits "array_left")))
;	  (setf x0 (- 0 core_ref_array.w wl_edge_LL_col.w))
;	  (setf y0 (+ 0 bl_edge_left.h))
	  (<cell outunits "core_left"
			 (<sref "array_left" "n" "a0" x0 y0)
			 (<sref "core_ref_array" "n" "a0" (- x0 core_ref_array.w) y0)
			 (<sref "wl_edge_LL_col" "n" "a0" (- x0 core_ref_array.w wl_edge_LL_col.w) y0)
			 (<sref "core_tkwl_left" "n" "a0" (- x0 core_ref_array.w wl_edge_LL_col.w) (+ y0 wl_edge_LL_col.h))
			 (<sref "bl_edge_left" "n" "a0" (- x0 core_ref_array.w wl_edge_LL_col.w) (- y0 bl_edge_left.h))))))

;;;;;;;;;;;;;;;;;core_right
(print "core_right")
(let ((x0 0)
	  (y0 0)
	  (array_bit.w)
	  (array_right.w)
	  (array_right.h)
	  (core_dummy_array.w (str-width (get-str  outunits "core_dummy_array")))
	  (core_dummy_array.h (str-high (get-str  outunits "core_dummy_array")))
	  (wl_edge_RR_col.w (str-width (get-str  outunits "wl_edge_RR_col")))
	  (wl_edge_RR_col.h (str-high (get-str  outunits "wl_edge_RR_col")))
	  (core_tkwl_right.w (str-width (get-str  outunits "core_tkwl_right")))
	  (core_tkwl_right.h (str-high (get-str  outunits "core_tkwl_right")))
	  (bl_edge_right.w (str-width (get-str  outunits "bl_edge_right")))
	  (bl_edge_right.h (str-high (get-str  outunits "bl_edge_right")))
	  (core_ref_array.w (str-width (get-str  outunits "core_ref_array")))
	  (core_ref_array.h (str-high (get-str  outunits "core_ref_array")))
	  (dimxn 0)
	  (sref-map (list))
	  (cal))
  (if (evenp *number-of-cols-r*)
	(progn
	  (when (= *charmode* 2) nil)
	  (when (= *charmode* 1) nil)
	  (when (= *charmode* 0)
		(setf dimxn 0)
		(dotimes (dimx *number-of-cols-r*)
		  (setf cal (+ *number-of-cols-l* dimx))
		  (array_bit cal)
		  (setf array_bit.w (str-width (get-str  outunits (string+ "array_bit_" (itoa cal)))))
		  (push (<sref (string+ "array_bit_" (itoa cal)) "n" "a0" dimxn 0) sref-map)
		  (setf dimxn (+ dimxn array_bit.w))
		  (when (and (evenp *number-of-cols-l*) (evenp cal))
			(push (<sref "core_ref_array" "n" "a0" dimxn 0) sref-map)
			(setf dimxn (+ dimxn core_ref_array.w)))
		  (when (and (oddp *number-of-cols-l*) (oddp cal))
			(push (<sref "core_ref_array" "n" "a0" dimxn 0) sref-map)
			(setf dimxn (+ dimxn core_ref_array.w))))
		(new-cell-2 outunits "array_right" sref-map))
	  (setf array_right.w (str-width (get-str  outunits "array_right")))
	  (setf array_right.h (str-high (get-str  outunits "array_right")))
;	  (setf y0 (+ 0 bl_edge_right.h))
	  (<cell outunits "core_right"
			 (<sref "array_right" "n" "a0" x0 y0)
			 (<sref "core_dummy_array" "y" "a0" (+ x0 array_right.w) y0)
			 (<sref "wl_edge_RR_col" "n" "a0" (+ x0 array_right.w core_dummy_array.w) y0)
			 (<sref "core_tkwl_right" "n" "a0" x0 (+ y0 array_right.h))
			 (<sref "bl_edge_right" "n" "a0" (+ x0 array_right.w core_dummy_array.w wl_edge_RR_col.w (* -1 bl_edge_right.w)) (- y0 bl_edge_right.h))))
	(progn
	  (when (= *charmode* 2) nil)
	  (when (= *charmode* 1) nil)
	  (when (= *charmode* 0)
		(setf dimxn 0)
		(dotimes (dimx *number-of-cols-r*)
		  (setf cal (+ *number-of-cols-l* dimx))
		  (array_bit cal)
		  (setf array_bit.w (str-width (get-str  outunits (string+ "array_bit_" (itoa cal)))))
		  (push (<sref (string+ "array_bit_" (itoa cal)) "n" "a0" dimxn 0) sref-map)
		  (setf dimxn (+ dimxn array_bit.w))
		  (when (and (evenp *number-of-cols-l*) (evenp cal))
			(if (= dimx (1- *number-of-cols-r*))
			  (progn
;			  	(setf dimxn (+ dimxn core_ref_array.w))
			  	(push (<sref "core_ref_array" "y" "a0" dimxn 0) sref-map))
			  (progn
			  	(push (<sref "core_ref_array" "n" "a0" dimxn 0) sref-map)
			  	(setf dimxn (+ dimxn core_ref_array.w)))))
		  (when (and (oddp *number-of-cols-l*) (oddp cal))
			(if (= dimx (1- *number-of-cols-r*))
			  (progn
;			  	(setf dimxn (+ dimxn core_ref_array.w))
			  	(push (<sref "core_ref_array" "y" "a0" dimxn 0) sref-map))
			  (progn
			  	(push (<sref "core_ref_array" "n" "a0" dimxn 0) sref-map)
			  	(setf dimxn (+ dimxn core_ref_array.w))))))
		(new-cell-2 outunits "array_right" sref-map))
	  (setf array_right.w (str-width (get-str  outunits "array_right")))
	  (setf array_right.h (str-high (get-str  outunits "array_right")))
;	  (setf y0 (+ 0 bl_edge_right.h))
	  (<cell outunits "core_right"
			 (<sref "array_right" "n" "a0" x0 y0)
			 (<sref "wl_edge_RR_col" "n" "a0" (+ x0 array_right.w) y0)
			 (<sref "core_tkwl_right" "n" "a0" x0 (+ y0 array_right.h))
			 (<sref "bl_edge_right" "n" "a0" (+ x0 array_right.w wl_edge_RR_col.w (* -1 bl_edge_right.w)) (- y0 bl_edge_right.h))))))

;;;;;;;;;;;;;;tkbl_array
(print "tkbl_array")
(<cell-add  outunits srclib "CORE_TKBL_DUMMY")
(let ((x0 0)
	  (y0 0)
	  (sref-map (list))
	  (CORE_TKBL_DUMMY.h (str-high (get-str  outunits "CORE_TKBL_DUMMY"))))
  (dotimes (dimy (* 4 *num-of-xdecs*))
	(push (<sref "CORE_TKBL_DUMMY" "n" "a0" x0 (+ y0 (* dimy CORE_TKBL_DUMMY.h))) sref-map))
  (new-cell-2 outunits "tkbl_array" sref-map))

;;;;;;;;tkbl_col
(print "tkbl_col")
(<cell-add  outunits srclib "CORE_TKBL")
(<cell-add  outunits srclib "CORE_TKBL_SUB")
(<cell-add  outunits srclib "CORE_TKBL_EDGE")
(let ((x0 0)
	  (y0 0)
	  (tkbl_array.w (str-width (get-str  outunits "tkbl_array")))
	  (tkbl_array.h (str-high (get-str  outunits "tkbl_array")))
	  (CORE_TKBL.w (str-width (get-str  outunits "CORE_TKBL")))
	  (CORE_TKBL.h (str-high (get-str  outunits "CORE_TKBL")))
	  (CORE_TKBL_SUB.w (str-width (get-str  outunits "CORE_TKBL_SUB")))
	  (CORE_TKBL_SUB.h (str-high (get-str  outunits "CORE_TKBL_SUB")))
	  (CORE_TKBL_EDGE.w (str-width (get-str  outunits "CORE_TKBL_EDGE")))
	  (CORE_TKBL_EDGE.h (str-high (get-str  outunits "CORE_TKBL_EDGE"))))
;  (setf y0 (+ 0 CORE_TKBL_EDGE.h))
;(print tkbl_array.h)
;(print CORE_TKBL.h)
;(print (+ tkbl_array.h CORE_TKBL.h))
  (<cell outunits "tkbl_col"
		 (<sref "tkbl_array" "n" "a0" x0 y0)
		 (<sref "CORE_TKBL" "n" "a0" (+ x0 tkbl_array.w (* -1 CORE_TKBL.w)) (+ y0 tkbl_array.h))
		 (<sref "CORE_TKBL_SUB" "n" "a0" (+ x0 tkbl_array.w (* -1 CORE_TKBL_SUB.w)) (+ y0 tkbl_array.h CORE_TKBL.h -285))
		 (<sref "CORE_TKBL_EDGE" "n" "a0" (+ x0 tkbl_array.w (* -1 CORE_TKBL_EDGE.w)) (- y0 CORE_TKBL_EDGE.h))))

;;;;;;;;wldrv

;;;;;;;;;;;;;;wldrv8
(print "wldrv8")
(<cell-add  outunits srclib "WLDRV_R8")
(<cell-add  outunits srclib "wldrv_r8_l")
(<cell-add  outunits srclib "wldrv_r8_cell")
(<cell-add  outunits srclib "wldrv_r8_cell1")
(<cell-add  outunits srclib "wldrv_r8_cell2")
(<cell-add  outunits srclib "wldrv_r8_inv2_4_1_6")
(<cell-add  outunits srclib "wldrv_r8_inv360_360")
(<cell-add  outunits srclib "wldrv_r8_inv9_6_4_8")
(<cell-add  outunits srclib "wldrv_r8_pg")
(<cell-add  outunits srclib "wldrv_r8_r")
(<cell-add  outunits srclib "wldry_r8_m4")
(<cell-add  outunits srclib "wldrv_r8_cell3")
(<cell-add  outunits srclib "wldrv_r8_cellx1")
(<cell-add  outunits srclib "wldrv_r8_cellx")
(let ((x0 0)
	  (y0 0))
  (<cell outunits "wldrv8"
		 (<sref "WLDRV_R8" "n" "a0" x0 y0)
		 )
  )

;;;;;;;;;;;;;;dmy_wldrv
(print "dmy_wldrv")
(<cell-add  outunits srclib "DMY_WLDRV")
(<cell-add  outunits srclib "dmy_wldrv_r")
(<cell-add  outunits srclib "dmy_wldrv_l")
(<cell-add  outunits srclib "dum_wldrv_m4")
(let ((x0 0)
	  (y0 0))
  (<cell outunits "dmy_wldrv"
		 (<sref "DMY_WLDRV" "n" "a0" x0 y0)
		 )
  )

;;;;;;;;;;;;;;wldrvrow
(print "wldrvrow")
(let ((x0 0)
	  (y0 0)
	  (sref-map (list))
	  (wldrv8.h (str-high (get-str  outunits "wldrv8"))))
  (dotimes (dimy *num-of-xdecs*)
	(push (<sref "wldrv8" "n" "a0" x0 (+ y0 (* dimy wldrv8.h))) sref-map))
  (new-cell-2 outunits "wldrvrow" sref-map))

;;;;;;;;;;;;;;wldrvall
(print "wldrvall")
(let ((x0 0)
	  (y0 0)
	  (sref-map (list))
	  (wldrvrow.w (str-width (get-str  outunits "wldrvrow")))
	  (wldrvrow.h (str-high (get-str  outunits "wldrvrow")))
	  (dmy_wldrv.w (str-width (get-str  outunits "dmy_wldrv")))
	  (dmy_wldrv.h (str-high (get-str  outunits "dmy_wldrv"))))
  (push (<sref "wldrvrow" "n" "a0" x0 y0) sref-map)
  (when (= *charmode* 0) nil)
  (when (<= *num-of-xdecs* 4)
	nil	;overlay
	)
  (when (and (<= *num-of-xdecs* 8) (> *num-of-xdecs* 1))
	nil	;overlay
	)
  (when (and (<= *num-of-xdecs* 128) (> *num-of-xdecs* 8))
	nil	;overlay
	)
  (push (<sref "dmy_wldrv" "n" "a0" (+ x0 wldrvrow.w (* -1 dmy_wldrv.w)) (+ y0 wldrvrow.h)) sref-map)
  (new-cell-2 outunits "wldrvall" sref-map))

;;;;;;;;ydecode

;;;;;;;;;;;;;;xypredec_itfctl
(print "xypredec_itfctl")
(<cell-add  outunits srclib *itfctlcell*)
(<cell-add  outunits srclib *ypredeccell*)
(<cell-add  outunits srclib *xdec_ydec_con*)
(<cell-add  outunits srclib *Xpredeclow*)
(<cell-add  outunits srclib *Xpredecmid*)
(<cell-add  outunits srclib *Xpredechigh*)
(<cell-add  outunits srclib *wl_tie*)
(<cell-add  outunits srclib *col_dec_con*)
(let ((x0 0)
	  (y0 0)
	  (sref-map (list))
	  (itfctlcell.w (str-width (get-str  outunits *itfctlcell*)))
	  (itfctlcell.h (str-high (get-str  outunits *itfctlcell*)))
	  (ypredeccell.w (str-width (get-str  outunits *ypredeccell*)))
	  (ypredeccell.h (str-high (get-str  outunits *ypredeccell*)))
	  (xdec_ydec_con.w (str-width (get-str  outunits *xdec_ydec_con*)))
	  (xdec_ydec_con.h (str-high (get-str  outunits *xdec_ydec_con*)))
	  (Xpredeclow.w (str-width (get-str  outunits *Xpredeclow*)))
	  (Xpredeclow.h (str-high (get-str  outunits *Xpredeclow*)))
	  (Xpredecmid.w (str-width (get-str  outunits *Xpredecmid*)))
	  (Xpredecmid.h (str-high (get-str  outunits *Xpredecmid*)))
	  (Xpredechigh.w (str-width (get-str  outunits *Xpredechigh*)))
	  (Xpredechigh.h (str-high (get-str  outunits *Xpredechigh*)))
	  (col_dec_con.w (str-width (get-str  outunits *col_dec_con*)))
	  (col_dec_con.h (str-high (get-str  outunits *col_dec_con*)))
	  (wl_tie.w (str-width (get-str  outunits *wl_tie*)))
	  (wl_tie.h (str-high (get-str  outunits *wl_tie*))))
  (if (< *col-mux* 32)
	(progn
;	  (setf x0 (+ 0 ypredeccell.w xdec_ydec_con.w Xpredeclow.w Xpredecmid.w Xpredechigh.w col_dec_con.w wl_tie.w))
	  (push (<sref *itfctlcell* "n" "a0" x0 y0) sref-map)
	  (push (<sref *ypredeccell* "n" "a0" (- x0 ypredeccell.w) y0) sref-map)
	  (push (<sref *xdec_ydec_con* "n" "a0" (- x0 ypredeccell.w xdec_ydec_con.w) y0) sref-map)
	  (push (<sref *Xpredeclow* "n" "a0" (- x0 ypredeccell.w xdec_ydec_con.w Xpredeclow.w) y0) sref-map)
  (push (<sref *Xpredecmid* "n" "a0" (- x0 ypredeccell.w xdec_ydec_con.w Xpredeclow.w Xpredecmid.w) y0) sref-map)
  (push (<sref *Xpredechigh* "n" "a0" (- x0 ypredeccell.w xdec_ydec_con.w Xpredeclow.w Xpredecmid.w Xpredechigh.w) y0) sref-map)
  (push (<sref *col_dec_con* "n" "a0" (- x0 ypredeccell.w xdec_ydec_con.w Xpredeclow.w Xpredecmid.w Xpredechigh.w col_dec_con.w) y0) sref-map)
  (push (<sref *wl_tie* "n" "a0" (- x0 ypredeccell.w xdec_ydec_con.w Xpredeclow.w Xpredecmid.w Xpredechigh.w col_dec_con.w) (+ y0 col_dec_con.h)) sref-map))
	(progn
;	  (setf x0 (+ 0 ypredeccell.w Xpredeclow.w Xpredecmid.w Xpredechigh.w col_dec_con.w wl_tie.w))
	  (push (<sref *itfctlcell* "n" "a0" x0 y0) sref-map)
	  (push (<sref *ypredeccell* "n" "a0" (- x0 ypredeccell.w) y0) sref-map)
	  (push (<sref *Xpredeclow* "n" "a0" (- x0 ypredeccell.w Xpredeclow.w) y0) sref-map)
  (push (<sref *Xpredecmid* "n" "a0" (- x0 ypredeccell.w Xpredeclow.w Xpredecmid.w) y0) sref-map)
  (push (<sref *Xpredechigh* "n" "a0" (- x0 ypredeccell.w Xpredeclow.w Xpredecmid.w Xpredechigh.w) y0) sref-map)
  (push (<sref *col_dec_con* "n" "a0" (- x0 ypredeccell.w Xpredeclow.w Xpredecmid.w Xpredechigh.w col_dec_con.w) y0) sref-map)
  (push (<sref *wl_tie* "n" "a0" (- x0 ypredeccell.w Xpredeclow.w Xpredecmid.w Xpredechigh.w col_dec_con.w) (+ y0 col_dec_con.h)) sref-map)))
  (new-cell-2 outunits "xypredec_itfctl" sref-map))

;;;;;;;;;;;;;;dmy_colmuxleft
(print "dmy_colmuxleft")
(<cell-add outunits srclib *dmy_colmux_even*)
(<cell-add outunits srclib *dmy_colmux_odd*)
(<cell outunits "dmy_colmuxleft"
	   (<sref (if (evenp *number-of-cols-l*) *dmy_colmux_even* *dmy_colmux_odd*) "n" "a0" 0 0))

;;;;;;;;;;;;;;dmy_colmuxright
(print "dmy_colmuxright")
(<cell-add outunits srclib *dmy_colmux_even*)
(<cell-add outunits srclib *dmy_colmux_odd*)
(<cell outunits "dmy_colmuxright"
	   (<sref (if (evenp *number-of-cols-r*) *dmy_colmux_even* *dmy_colmux_odd*) "y" "a0"
;			  (+ 0 (str-width (get-str outunits (if (evenp *number-of-cols-r*) *dmy_colmux_even* *dmy_colmux_odd*)))) 0))
			  0 0))

;;;;;;;;;;;;;;dio_ymux_left
(<cell-add outunits srclib *ymuxsa2*)
(defun dio_ymux_left ()
(print "dio_ymux_left")
  (let ((x0 0)
		(y0 0)
		(sref-map (list)))
	(dotimes (dimx (div1 *number-of-cols-l* 2))
	  (push (<sref *ymuxsa2* "n" "a0" (+ x0 (* dimx (str-width (get-str outunits *ymuxsa2*)))) y0) sref-map))
	(new-cell-2 outunits "dio_ymux_left" sref-map)))

;;;;;;;;;;;;;;dio_ymuxcol_left
(print "dio_ymuxcol_left")
(let ((x0 0)
	  (y0 0)
	  (dio_ymux_left.w)
	  (dio_ymux_left.h)
	  (ymuxsa1left.w)	; (str-width (get-str outunits *ymuxsa1left*))
	  (ymuxsa1left.h) 	;(str-high (get-str outunits *ymuxsa1left*))
	  (dmy_colmuxleft.w (str-width (get-str outunits "dmy_colmuxleft")))
	  (dmy_colmuxleft.h (str-high (get-str outunits "dmy_colmuxleft")))
	  )
  (if (evenp *number-of-cols-l*)
	(progn
	  (dio_ymux_left)
	  (setf dio_ymux_left.w (str-width (get-str outunits "dio_ymux_left")))
	  (setf dio_ymux_left.h (str-high (get-str outunits "dio_ymux_left")))
;	  (setf x0 (+ 0 dmy_colmuxright.w))
	  (<cell outunits "dio_ymuxcol_left"
			 (<sref "dio_ymux_left" "n" "a0" x0 y0)
			 (<sref "dmy_colmuxleft" "n" "a0" (- x0 dmy_colmuxright.w) y0)))
	(if (= 0 (div1 *number-of-cols-l* 2))
	  (progn
		(<cell-add outunits srclib *ymuxsa1left*)
		(setf ymuxsa1left.w (str-width (get-str outunits *ymuxsa1left*)))
		(setf ymuxsa1left.h (str-high (get-str outunits *ymuxsa1left*)))
;		(setf x0 (+ 0 dmy_colmuxleft.w))
		(<cell outunits "dio_ymuxcol_left"
			   (<sref *ymuxsa1left* "n" "a0" x0 y0)
			   (<sref "dmy_colmuxleft" "n" "a0" (- x0 dmy_colmuxleft.w) y0)
			   )
		)
	  (progn
		(dio_ymux_left)
		(<cell-add outunits srclib *ymuxsa1left*)
		(setf ymuxsa1left.w (str-width (get-str outunits *ymuxsa1left*)))
		(setf ymuxsa1left.h (str-high (get-str outunits *ymuxsa1left*)))
;		(setf x0 (+ 0 dmy_colmuxleft.w ymuxsa1left.w))
		(<cell outunits "dio_ymuxcol_left"
			   (<sref "dio_ymux_left" "n" "a0" x0 y0)
			   (<sref *ymuxsa1left* "n" "a0" (- x0 ymuxsa1left.w) y0)
			   (<sref "dmy_colmuxleft" "n" "a0" (- x0 ymuxsa1left.w dmy_colmuxleft.w) y0)
			   )
		))))

;;;;;;;;;;;;;;dio_ymux_right
(<cell-add outunits srclib *ymuxsa2*)
(defun dio_ymux_right ()
(print "dio_ymux_right")
  (let ((x0 0)
		(y0 0)
		(sref-map (list)))
	(dotimes (dimx (div1 *number-of-cols-r* 2))
	  (push (<sref *ymuxsa2* "n" "a0" (+ x0 (* dimx (str-width (get-str outunits *ymuxsa2*)))) y0) sref-map))
	(new-cell-2 outunits "dio_ymux_right" sref-map)))

;;;;;;;;;;;;;;dio_ymuxcol_right
(print "dio_ymuxcol_right")
(let ((x0 0)
	  (y0 0)
	  (dio_ymux_right.w)
	  (dio_ymux_right.h)
	  (ymuxsa1right.w)
	  (ymuxsa1right.h)
	  (dmy_colmuxright.w (str-width (get-str outunits "dmy_colmuxright")))
	  (dmy_colmuxright.h (str-high (get-str outunits "dmy_colmuxright")))
	  )
  (if (evenp *number-of-cols-r*)
	(progn
	  (dio_ymux_right)
	  (setf dio_ymux_right.w (str-width (get-str outunits "dio_ymux_right")))
	  (setf dio_ymux_right.h (str-high (get-str outunits "dio_ymux_right")))
	  (<cell outunits "dio_ymuxcol_right"
			 (<sref "dio_ymux_right" "n" "a0" x0 y0)
			 (<sref "dmy_colmuxright" "n" "a0" (+ x0 dio_ymux_right.w) y0)))
	(if (= 0 (div1 *number-of-cols-r* 2))
	  (progn
		(<cell-add outunits srclib *ymuxsa1right*)
		(setf ymuxsa1right.w (str-width (get-str outunits *ymuxsa1right*)))
		(setf ymuxsa1right.h (str-high (get-str outunits *ymuxsa1right*)))
		(<cell outunits "dio_ymuxcol_right"
			   (<sref *ymuxsa1right* "n" "a0" x0 y0)
			   (<sref "dmy_colmuxright" "n" "a0" (+ x0 ymuxsa1right.w) y0)
			   )
		)
	  (progn
		(dio_ymux_right)
		(<cell-add outunits srclib *ymuxsa1right*)
		(setf ymuxsa1right.w (str-width (get-str outunits *ymuxsa1right*)))
		(setf ymuxsa1right.h (str-high (get-str outunits *ymuxsa1right*)))
;		(setf x0 (+ 0 dmy_colmuxright.w ymuxsa1right.w))
		(<cell outunits "dio_ymuxcol_right"
			   (<sref "dio_ymux_right" "n" "a0" x0 y0)
			   (<sref *ymuxsa1right* "n" "a0" (+ x0 dio_ymux_right.w) y0)
			   (<sref "dmy_colmuxright" "n" "a0" (+ x0 dio_ymux_right.w ymuxsa1right.w) y0)
			   )
		))))

;;;;;;;;macro
(print "macro")
(let ((x0 0)
	  (y0 0)
	  (dio_ymuxcol_left.w (str-width (get-str outunits "dio_ymuxcol_left")))
	  (dio_ymuxcol_left.h (str-high (get-str outunits "dio_ymuxcol_left")))
	  (core_left.w (str-width (get-str outunits "core_left")))
	  (core_left.h (str-high (get-str outunits "core_left")))
	  (xypredec_itfctl.w (str-width (get-str outunits "xypredec_itfctl")))
	  (xypredec_itfctl.h (str-high (get-str outunits "xypredec_itfctl")))
	  (wldrvall.w (str-width (get-str outunits "wldrvall")))
	  (wldrvall.h (str-high (get-str outunits "wldrvall")))
	  (dio_ymuxcol_right.w (str-width (get-str outunits "dio_ymuxcol_right")))
	  (dio_ymuxcol_right.h (str-high (get-str outunits "dio_ymuxcol_right")))
	  (core_right.w (str-width (get-str outunits "core_right")))
	  (core_right.h (str-high (get-str outunits "core_right")))
	  (tkbl_col.w (str-width (get-str outunits "tkbl_col")))
	  (tkbl_col.h (str-high (get-str outunits "tkbl_col")))
	  )
  (if (or (= *charmode* 0) (= *charmode* 0))
	(progn
	  (<cell outunits "macro"
			 (<sref "dio_ymuxcol_left" "n" "a0" x0 y0)
			 (<sref "core_left" "n" "a0" x0 (+ y0 dio_ymuxcol_left.h))
			 (<sref "xypredec_itfctl" "n" "a0" (+ x0 dio_ymuxcol_left.w) y0)
			 (<sref "wldrvall" "n" "a0" (+ x0 dio_ymuxcol_left.w) (+ y0 xypredec_itfctl.h))
			 (<sref "dio_ymuxcol_right" "n" "a0" (+ x0 dio_ymuxcol_left.w xypredec_itfctl.w) y0)
			 (<sref "core_right" "n" "a0" (+ x0 dio_ymuxcol_left.w xypredec_itfctl.w dio_ymuxcol_right.w (* -1 core_right.w)) (+ y0 dio_ymuxcol_right.h))
			 (<sref "tkbl_col" "n" "a0" (+ x0 dio_ymuxcol_left.w xypredec_itfctl.w dio_ymuxcol_right.w (* -1 core_right.w) (* -1 tkbl_col.w)) (+ y0 dio_ymuxcol_right.h))
			 )
	  ) nil))

;(format t "
;>>>>>>>>>>
;")
;(format t "~a~%" (depend (units-of srclib) *ymuxsa1left*))
;(format t "~a~%" (have-sref-p (units-of srclib) *ymuxsa1left*))

;;;;;;;;dump gds
(format t "
dump gds
")
(defvar outlib (sgk-lib *outlib-name* outunits))
(sgk-wt-gds *gds-out* outlib)
))
(quit)
