(load "./sgk.fasl")

(declaim (optimize (speed 3)
				   (compilation-speed 0)
				   (safety 0)
				   (debug 0)))

(defun vrom-compile (&key leaf_cell code-file word-depth word-length col-mux charmode hpinlayer vpinlayer anchor_layer_number outlib-name top-name gds-out)

(setf *anchor_layer_number* anchor_layer_number)

(defvar srclib (sgk-rd-gds leaf_cell))

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

(defvar *xaddrs* (floor (xaddrs word-depth col-mux)))
(defvar *yaddrs* (floor (yaddrs col-mux)))
(defvar *ymuxx* (floor (ymuxx *yaddrs*)))
(defvar *ymuxsa2* (ymuxsa2 *yaddrs*))
(defvar *ymuxsa1left* (ymuxsa1left *yaddrs*))
(defvar *ymuxsa1right* (ymuxsa1right *yaddrs*))
(defvar *ypredeccell* (ypredeccell *yaddrs*))
(defvar *itfctlcell* (itfctlcell *yaddrs* charmode))
(defvar *dmy_colmux_odd* (dmy_colmux_odd *yaddrs*))
(defvar *dmy_colmux_even* (dmy_colmux_even *yaddrs*))
(defvar *dmy_tkwl_corner_even* "DMY_TKWL_CORNER_W")
(defvar *dmy_tkwl_corner_odd* "DMY_TKWL_CORNER")
(defvar *dmy_wl_edgex4_even* "DMY_WL_EDGEX4_W")
(defvar *dmy_wl_edgex4_odd* "DMY_WL_EDGEX4")
(defvar *dmy_wl_corner_even* "DMY_WL_CORNER_W")
(defvar *dmy_wl_corner_odd* "DMY_WL_CORNER")
(defvar *xpredec38* (xpredec38 col-mux))
(defvar *xdec_ydec_con* (xdec_ydec_con col-mux))
(defvar *col_dec_con* (col_dec_con col-mux charmode))
(defvar *wl_tie* (wl_tie col-mux))
(defvar *Xpredechigh* (Xpredechigh col-mux))
(defvar *Xpredecmid* (Xpredechigh col-mux))
(defvar *Xpredeclow* (Xpredechigh col-mux))
(defvar *Xaddrslow* (aref *xaddrsnh* (- *xaddrs* 3) 0))
(defvar *Xaddrsmid* (aref *xaddrsnh* (- *xaddrs* 3) 1))
(defvar *Xaddrshigh* (aref *xaddrsnh* (- *xaddrs* 3) 2))
(defvar *mun-of-addr* (floor (math-log 2 word-depth)))
(defvar *col-per-sa* (floor (math-pow 2 *yaddrs*)))
(defvar *num-of-xdecs* (num-of-xdecs word-depth *col-per-sa*))
(defvar *outputs-left* (/ (1+ word-length) 2))
(defvar *outputs-right* (/ word-length 2))
(defvar *number-of-cols-l* (floor *outputs-left*))
(defvar *number-of-cols-r* (floor *outputs-right*))
(defvar *number-of-cols* (+ *number-of-cols-l* *number-of-cols-r*))
(defvar *compiler-args-check* (if(= (mod (- (math-pow 2 *mun-of-addr*) word-depth) (* 8 (math-pow 2 *yaddrs*))) 0) "ArgvOK ^_^" "InValidArgv X_X"))

(if (string/= *compiler-args-check* "ArgvOK ^_^") (format t "~a~%" *compiler-args-check*) (progn

(defun code (fi)
  (let ((code-map (make-array (list word-depth word-length))) (line))
	(with-open-file (str-cod fi :direction	:input)
	  (dotimes (n word-depth)
		(setf line (read-line str-cod nil 'eof))
		(setf line (subseq line (- (length line) word-length)))
		(dotimes (m word-length)
		  (setf (aref code-map n m) (aref line m)))))
	code-map))

(defvar *code-map* (code code-file))

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
		code-file
		word-depth word-length col-mux charmode
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
(defvar *srcunits* (get-units srclib))

(format t "
>>>>>>>>>>
")

;;;;;;;;wl_edge_array
(print "wl_edge_array")
(let ((insts (list)) (p0 (vector 0 0)) (i0))
  (dotimes (dimx (* 2 *num-of-xdecs*))
	(if (evenp *number-of-cols-l*)
	  (progn
		(setf i0 (vector *dmy_wl_edgex4_even* #x0000 0.0d0 p0
						 (list "GND_m1" "GND_m2" "GND_m3" "GND_m4" "WL0" "WL1" "WL2" "WL3")
						 (list "GND_m1" "GND_m2" "GND_m3" "GND_m4" "WL0" "WL1" "WL2" "WL3")))
		(push (inst i0) insts)
		(setf p0 (vector+ p0 (vector 0 (vinst-high i0)))))
	  (progn
		(setf i0 (vector *dmy_wl_edgex4_odd* #x0000 0.0d0 p0
						 (list "GND_m1" "GND_m2" "GND_m3" "GND_m4" "WL0" "WL1" "WL2" "WL3")
						 (list "GND_m1" "GND_m2" "GND_m3" "GND_m4" "WL0" "WL1" "WL2" "WL3")))
		(push (inst i0) insts)
		(setf p0 (vector+ p0 (vector 0 (vinst-high i0)))))))
  (push (cell-3 "wl_edge_array" insts) *srcunits*))

;;;;;;;;wl_edge_LL_col
(print "wl_edge_LL_col")
(let ((s0) (GND_ (concatenate 'string "GND_" hpinlayer)))
  (setf s0 (vector "wl_edge_array" #x0000 0.0d0 (vector 0 0)
				   (list GND_  "WL0" "WL3")
				   (list "GND" "AC_WL0_2" "AC_WL1_2")))
  (push (cell-3 "wl_edge_LL_col" (list (inst s0))) *srcunits*))

;;;;;;;;wl_edge_RR_col
(print "wl_edge_RR_col")
(let ((insts (list)) (p0 (vector 0 0)) (i0) (GND_ (concatenate 'string "GND_" hpinlayer)))
  (dotimes (dimx (* 2 *num-of-xdecs*))
	(if (evenp *number-of-cols-r*)
	  (progn
		(setf i0 (vector *dmy_wl_edgex4_even* #x4000 0.0d0 p0
						 (list GND_)
						 (list "GND")))
		(push (inst i0) insts)
		(setf p0 (vector+ p0 (vector 0 (vinst-high i0)))))
	  (progn
		(setf i0 (vector *dmy_wl_edgex4_odd* #x4000 0.0d0 p0
						 (list GND_)
						 (list "GND")))
		(push (inst i0) insts)
		(setf p0 (vector+ p0 (vector 0 (vinst-high i0)))))))
  (push (cell-3 "wl_edge_RR_col" insts) *srcunits*))


;;;;;;;;tkwl_edge_LL
(print "tkwl_edge_LL")
(let ((i0) (p0 (vector 0 0)) (GND_ (concatenate 'string "GND_" hpinlayer)))
  (if (evenp *number-of-cols-l*)
	(setf i0 (vector *dmy_tkwl_corner_even* #x0000 0.0d0 p0
					 (list GND_
						   (if (= 1 charmode) "MWL" nil))
					 (list "GND"
						   (if (= 1 charmode) "AC_MWL_2" nil))))
	(setf i0 (vector *dmy_tkwl_corner_odd* #x0000 0.0d0 p0
					 (list GND_
						   (if (= 1 charmode) "MWL" nil))
					 (list "GND"
						   (if (= 1 charmode) "AC_MWL_2" nil)))))
  (push (cell-3 "tkwl_edge_LL" (list (inst i0))) *srcunits*))

;;;;;;;;tkwl_edge_RR
(print "tkwl_edge_RR")
(let ((i0) (p0 (vector 0 0)) (GND_ (concatenate 'string "GND_" hpinlayer)))
  (if (evenp *number-of-cols-r*)
	(setf i0 (vector *dmy_tkwl_corner_even* #x4000 0.0d0 p0
					 (list GND_)
					 (list "GND")))
	(setf i0 (vector *dmy_tkwl_corner_odd* #x4000 0.0d0 p0
					 (list GND_)
					 (list "GND"))))
  (push (cell-3 "tkwl_edge_RR" (list (inst i0))) *srcunits*))

;;;;;;;;core_ref_array
(print "core_ref_array")
(let ((i1) (p0 (vector 0 0)) (insts (list)))
  (dotimes (dimx (* 4 *num-of-xdecs*))
	(setf i1 (vector "CORE_REF_R2C2" #x0000 0.0d0 p0
					 (list "gnd"
						   "BLREFL" "FIX_GND" "BLREFR"
						   "WL0" "WL1")
					 (list "gnd"
						   "BLREFL" "FIX_GND" "BLREFR"
						   "WL0" "WL1")))
	(push (inst i1) insts)
	(setf p0 (vector+ p0 (vector 0 (vinst-high i1)))))
  (push (cell-3 "core_ref_array" insts) *srcunits*))

;;;;;;;;core_dummy_array
(print "core_dummy_array")
(let ((i1) (p0 (vector 0 0)) (insts (list)))
  (dotimes (dimx (* 4 *num-of-xdecs*))
	(setf i1 (vector "CORE11" #x0000 0.0d0 p0
					 (list "gnd" "BL"
						   "WL0" "WL1")
					 (list "gnd" "BL"
						   "WL0" "WL1")))
	(push (inst i1) insts)
	(setf p0 (vector+ p0 (vector 0 (vinst-high i1)))))
  (push (cell-3 "core_dummy_array" insts) *srcunits*))

;;;;;;;;core_tkwl_sub_1ymux
(print "core_tkwl_sub_1ymux")
(let ((insts (list)) (p0 (vector 0 0)) (i1))
  (dotimes (dimy (div1 col-mux 8))
	(setf i1 (vector "CORE_TKWL_SUBX8" #x0000 0.0d0 p0
					 (list "GND" "GND_m1" "GND_m2" "GND_m3" "GND_m4")
					 (list "GND" "GND_m1" "GND_m2" "GND_m3" "GND_m4")))
	(push (inst i1) insts)
	(setf p0 (vector+ p0 (vector (vinst-width i1) 0))))
  (push (cell-3 "core_tkwl_sub_1ymux" insts) *srcunits*))

;;;;;;;;core_tkwl_sub_2ymux
(print "core_tkwl_sub_2ymux")
(let ((p0 (vector 0 0)) (s0) (s1) (s2)
						(GND_ (concatenate 'string "GND_" vpinlayer))
						(VDD_ (concatenate 'string "VDD_" vpinlayer)))
  (setf s0 (inst (vector "core_tkwl_sub_1ymux" #x0000 0.0d0 p0
						 (list GND_)
						 (list "GND"))))
  (setf s1 (inst-right-down s0 (vector "DUMY_CORE_REF_SUB" #x0000 0.0d0 p0
						 (list GND_ VDD_)
						 (list "GND" "VDD"))))
  (setf s2 (inst-right-down s1 (vector "core_tkwl_sub_1ymux" #x0000 0.0d0 p0
						 (list GND_)
						 (list "GND"))))
  (push (cell-3 "core_tkwl_sub_2ymux" (list s0 s1 s2)) *srcunits*))

;;;;;;;;tkwl_sub_L
(print "tkwl_sub_L")
(let ((insts (list)) (p0 (vector 0 0)) (i1)
					 (GND_ (concatenate 'string "GND_" vpinlayer))
					 (VDD_ (concatenate 'string "VDD_" vpinlayer)))
  (dotimes (dimx (div1 *number-of-cols-l* 2))
	(setf i1 (vector "core_tkwl_sub_2ymux" #x0000 0.0d0 p0
					 (list "GND" GND_)
					 (list "GND" GND_)))
	(push (inst i1) insts)
	(setf p0 (vector+ p0 (vector (vinst-width i1) 0))))
  (push (cell-3 "tkwl_sub_L" insts) *srcunits*))

;;;;;;;;tkwl_sub_left
(print "tkwl_sub_left")
(if (evenp *number-of-cols-l*)
(let ((p0 (vector 0 0)) (s0) (s1)
						(GND_ (concatenate 'string "GND_" vpinlayer))
						(VDD_ (concatenate 'string "VDD_" vpinlayer)))
  (setf s0 (inst (vector "tkwl_sub_L" #x0000 0.0d0 p0
						 (list "GND" GND_)
						 (list "GND" "GND"))))
  (setf s1 (inst-left-down s0 (vector "CORE_TKWL_SUB" #x0000 0.0d0 p0
						 (list "GND" GND_)
						 (list "GND" "GND"))))
  (push (cell-3 "tkwl_sub_left" (list s0 s1)) *srcunits*))
(if (= (div1 *number-of-cols-l* 2) 0)
(let ((p0 (vector 0 0)) (s0) (s1)
						(GND_ (concatenate 'string "GND_" vpinlayer))
						(VDD_ (concatenate 'string "VDD_" vpinlayer)))
  (setf s0 (inst (vector "core_tkwl_sub_1ymux" #x0000 0.0d0 p0
						 (list "GND" GND_)
						 (list "GND" "GND"))))
  (setf s1 (inst-left-down s0 (vector "DUMY_CORE_REF_SUB" #x0000 0.0d0 p0
						 (list "GND" GND_ VDD_)
						 (list "GND" "GND" "VDD"))))
  (push (cell-3 "tkwl_sub_left" (list s0 s1)) *srcunits*))
(let ((p0 (vector 0 0)) (s0) (s1) (s2)
						(GND_ (concatenate 'string "GND_" vpinlayer))
						(VDD_ (concatenate 'string "VDD_" vpinlayer)))
  (setf s0 (inst (vector "tkwl_sub_L" #x0000 0.0d0 p0
						 (list "GND")
						 (list "GND"))))
  (setf s1 (inst-left-down s0 (vector "core_tkwl_sub_1ymux" #x0000 0.0d0 p0
						 (list "GND" GND_)
						 (list "GND" "GND"))))
  (setf s2 (inst-left-down s1 (vector "DUMY_CORE_REF_SUB" #x0000 0.0d0 p0
						 (list "GND" GND_ VDD_)
						 (list "GND" "GND" "VDD"))))
  (push (cell-3 "tkwl_sub_left" (list s0 s1 s2)) *srcunits*))))

;;;;;;;;tkwl_sub_R
(print "tkwl_sub_R")
(let ((insts (list)) (p0 (vector 0 0)) (i1)
					 (GND_ (concatenate 'string "GND_" vpinlayer))
					 (VDD_ (concatenate 'string "VDD_" vpinlayer)))
  (dotimes (dimx (div1 *number-of-cols-r* 2))
	(setf i1 (vector "core_tkwl_sub_2ymux" #x0000 0.0d0 p0
					 (list "GND" GND_ VDD_)
					 (list "GND" GND_ VDD_)))
	(push (inst i1) insts)
	(setf p0 (vector+ p0 (vector (vinst-width i1) 0))))
  (push (cell-3 "tkwl_sub_R" insts) *srcunits*))

;;;;;;;;tkwl_sub_right
(print "tkwl_sub_right")
(if (evenp *number-of-cols-r*)
(let ((p0 (vector 0 0)) (s0) (s1)
						(GND_ (concatenate 'string "GND_" vpinlayer))
						(VDD_ (concatenate 'string "VDD_" vpinlayer)))
  (setf s0 (inst (vector "tkwl_sub_R" #x0000 0.0d0 p0
						 (list "GND" GND_)
						 (list "GND" "GND"))))
  (setf s1 (inst-right-down s0 (vector "CORE_TKWL_SUB" #x4000 0.0d0 p0
						 (list "GND")
						 (list "GND"))))
  (push (cell-3 "tkwl_sub_right" (list s0 s1)) *srcunits*))
(if (= (div1 *number-of-cols-r* 2) 0)
(let ((p0 (vector 0 0)) (s0) (s1)
						(GND_ (concatenate 'string "GND_" vpinlayer))
						(VDD_ (concatenate 'string "VDD_" vpinlayer)))
  (setf s0 (inst (vector "core_tkwl_sub_1ymux" #x0000 0.0d0 p0
						 (list "GND" GND_)
						 (list "GND" "GND"))))
  (setf s1 (inst-right-down s0 (vector "DUMY_CORE_REF_SUB" #x0000 0.0d0 p0
						 (list "GND" VDD_)
						 (list "GND" "VDD"))))
  (push (cell-3 "tkwl_sub_right" (list s0 s1)) *srcunits*))
(let ((p0 (vector 0 0)) (s0) (s1) (s2)
						(GND_ (concatenate 'string "GND_" vpinlayer))
						(VDD_ (concatenate 'string "VDD_" vpinlayer)))
  (setf s0 (inst (vector "tkwl_sub_R" #x0000 0.0d0 p0
						 (list "GND" GND_)
						 (list "GND" "GND"))))
  (setf s1 (inst-right-down s0 (vector "core_tkwl_sub_1ymux" #x0000 0.0d0 p0
						 (list "GND" GND_)
						 (list "GND" "GND"))))
  (setf s2 (inst-right-down s1 (vector "DUMY_CORE_REF_SUB" #x0000 0.0d0 p0
						 (list "GND" VDD_)
						 (list "GND" "VDD"))))
  (push (cell-3 "tkwl_sub_right" (list s0 s1 s2)) *srcunits*))))

;;;;;;;;core_tkwl_r4c4
(print "core_tkwl_r4c4")
(let ((p0 (vector 0 0)) (i0) (i1) (i2) (i3))
  (setf i0 (inst (vector "CORE_TKWL_R4C1" #x0000 0.0d0 p0
						 (list "gnd" "BL"
							   "MWL_TK" "MWL" "FIX_GND")
						 (list "gnd" "BL"
							   "MWL_TK" "MWL" "FIX_GND"))))
  (setf i1 (inst-left-down i0 (vector "CORE_TKWL_R4C1" #x4000 0.0d0 p0
						 (list "gnd" "BL"
							   "MWL_TK" "MWL" "FIX_GND")
						 (list "gnd" "BL"
							   "MWL_TK" "MWL" "FIX_GND"))))
  (setf i2 (inst-left-down i1 (vector "CORE_TKWL_R4C1" #x0000 0.0d0 p0
						 (list "gnd" "BL"
							   "MWL_TK" "MWL" "FIX_GND")
						 (list "gnd" "BL"
							   "MWL_TK" "MWL" "FIX_GND"))))
  (setf i3 (inst-left-down i2 (vector "CORE_TKWL_R4C1" #x4000 0.0d0 p0
						 (list "gnd" "BL"
							   "MWL_TK" "MWL" "FIX_GND")
						 (list "gnd" "BL"
							   "MWL_TK" "MWL" "FIX_GND"))))
  (push (cell-3 "core_tkwl_r4c4" (list i0 i1 i2 i3)) *srcunits*))

;;;;;;;;core_tkwl_r4c4_tie
(print "core_tkwl_r4c4_tie")
(let ((p0 (vector 0 0)) (i0) (i1) (i2) (i3))
  (setf i0 (inst (vector "CORE_TKWL_R4C1" #x0000 0.0d0 p0
						 (list "gnd" "BL"
							   "MWL_TK" "MWL" "FIX_GND")
						 (list "gnd" "BL"
							   "MWL_TK" "MWL" "FIX_GND"))))
  (setf i1 (inst-left-down i0 (vector "CORE_TKWL_R4C1_TIE" #x4000 0.0d0 p0
						 (list "gnd" "BL"
							   "MWL_TK" "MWL" "FIX_GND")
						 (list "gnd" "BL"
							   "MWL_TK" "MWL" "FIX_GND"))))
  (setf i2 (inst-left-down i1 (vector "CORE_TKWL_R4C1_TIE" #x0000 0.0d0 p0
						 (list "gnd" "BL"
							   "MWL_TK" "MWL" "FIX_GND")
						 (list "gnd" "BL"
							   "MWL_TK" "MWL" "FIX_GND"))))
  (setf i3 (inst-left-down i2 (vector "CORE_TKWL_R4C1" #x4000 0.0d0 p0
						 (list "gnd" "BL"
							   "MWL_TK" "MWL" "FIX_GND")
						 (list "gnd" "BL"
							   "MWL_TK" "MWL" "FIX_GND"))))
  (push (cell-3 "core_tkwl_r4c4_tie" (list i0 i1 i2 i3)) *srcunits*))

;;;;;;;;core_tkwl_1ymux
(print "core_tkwl_1ymux")
(let ((p0 (vector 0 0)) (i1) (insts (list)))
  (dotimes (dimx (div1 (math-pow 2 *yaddrs*) 4))
	(setf i1 (vector "core_tkwl_r4c4" #x0000 0.0d0 p0
					 (list "gnd" "BL" "MWL_TK" "MWL" "FIX_GND")
					 (list "gnd" "BL" "MWL_TK" "MWL" "FIX_GND")))
	(push (inst i1) insts)
	(setf p0 (vector+ p0 (vector (vinst-width i1) 0))))
  (push (cell-3 "core_tkwl_1ymux" insts) *srcunits*))

;;;;;;;;core_tkwl_1ymux_tie_mid
(print "core_tkwl_1ymux_tie_mid")
(let ((p0 (vector 0 0)) (i1) (insts (list)))
  (dotimes (dimx (div1 (math-pow 2 *yaddrs*) 4))
	(setf i1 (vector (if (= dimx (div1 (math-pow 2 *yaddrs*) 8))
					   "core_tkwl_r4c4_tie" "core_tkwl_r4c4")
					 #x0000 0.0d0 p0
					 (list "gnd" "BL" "MWL_TK" "MWL" "FIX_GND")
					 (list "gnd" "BL" "MWL_TK" "MWL" "FIX_GND")))
	(push (inst i1) insts)
	(setf p0 (vector+ p0 (vector (vinst-width i1) 0))))
  (push (cell-3 "core_tkwl_1ymux_tie_mid" insts) *srcunits*))

;;;;;;;;core_tkwl_1ymux_tie_l
(print "core_tkwl_1ymux_tie_l")
(let ((p0 (vector 0 0)) (i1) (insts (list)))
  (dotimes (dimx (div1 (math-pow 2 *yaddrs*) 4))
	(setf i1 (vector (if (= dimx 0)
					   "core_tkwl_r4c4_tie" "core_tkwl_r4c4")
					 #x0000 0.0d0 p0
					 (list "gnd" "BL" "MWL_TK" "MWL" "FIX_GND")
					 (list "gnd" "BL" "MWL_TK" "MWL" "FIX_GND")))
	(push (inst i1) insts)
	(setf p0 (vector+ p0 (vector (vinst-width i1) 0))))
  (push (cell-3 "core_tkwl_1ymux_tie_l" insts) *srcunits*))

;;;;;;;;core_tkwl_2ymux
(print "core_tkwl_2ymux")
(let ((p0 (vector 0 0)) (s0) (s1) (s2))
  (setf s0 (inst (vector "core_tkwl_1ymux" #x0000 0.0d0 p0
						 (list "gnd" "BL" "MWL_TK" "MWL" "FIX_GND")
						 (list "gnd" "BL" "MWL_TK" "MWL" "FIX_GND"))))
  (setf s1 (inst-right-down s0 (vector "DUMY_CORE_REF_R4C2" #x0000 0.0d0 p0
						 (list "gnd" "BLREFL" "FIX_GND" "BLREFR")
						 (list "gnd" "BLREFL" "FIX_GND" "BLREFR"))))
  (setf s2 (inst-right-down s1 (vector "core_tkwl_1ymux" #x0000 0.0d0 p0
						 (list "gnd" "BL" "MWL_TK" "MWL" "FIX_GND")
						 (list "gnd" "BL" "MWL_TK" "MWL" "FIX_GND"))))
  (push (cell-3 "core_tkwl_2ymux" (list s0 s1 s2)) *srcunits*))

;;;;;;;;core_tkwl_2ymux_r
(print "core_tkwl_2ymux_r")
(let ((p0 (vector 0 0)) (s0) (s1) (s2))
  (setf s0 (inst (vector "core_tkwl_1ymux" #x0000 0.0d0 p0
						 (list "gnd" "BL" "MWL_TK" "MWL" "FIX_GND")
						 (list "gnd" "BL" "MWL_TK" "MWL" "FIX_GND"))))
  (setf s1 (inst-right-down s0 (vector "DUMY_CORE_REF_R4C2_MID" #x0000 0.0d0 p0
						 (list "gnd" "BLREFL" "FIX_GND" "BLREFR")
						 (list "gnd" "BLREFL" "FIX_GND" "BLREFR"))))
  (setf s2 (inst-right-down s1 (vector "core_tkwl_1ymux" #x0000 0.0d0 p0
						 (list "gnd" "BL" "MWL_TK" "MWL" "FIX_GND")
						 (list "gnd" "BL" "MWL_TK" "MWL" "FIX_GND"))))
  (push (cell-3 "core_tkwl_2ymux_r" (list s0 s1 s2)) *srcunits*))

;;;;;;;;core_tkwl_2ymux_tie_mid
(print "core_tkwl_2ymux_tie_mid")
(let ((p0 (vector 0 0)) (s0) (s1) (s2))
  (setf s0 (inst (vector "core_tkwl_1ymux" #x0000 0.0d0 p0
						 (list "gnd" "BL" "MWL_TK" "MWL" "FIX_GND")
						 (list "gnd" "BL" "MWL_TK" "MWL" "FIX_GND"))))
  (setf s1 (inst-right-down s0 (vector "DUMY_CORE_REF_R4C2" #x0000 0.0d0 p0
						 (list "gnd" "BLREFL" "FIX_GND" "BLREFR")
						 (list "gnd" "BLREFL" "FIX_GND" "BLREFR"))))
  (setf s2 (inst-right-down s1 (vector "core_tkwl_1ymux_tie_l" #x0000 0.0d0 p0
						 (list "gnd" "BL" "MWL_TK" "MWL" "FIX_GND")
						 (list "gnd" "BL" "MWL_TK" "MWL" "FIX_GND"))))
  (push (cell-3 "core_tkwl_2ymux_tie_mid" (list s0 s1 s2)) *srcunits*))

;;;;;;;;core_tkwl_2ymux_tie_l
(print "core_tkwl_2ymux_tie_l")
(let ((p0 (vector 0 0)) (s0) (s1) (s2))
  (setf s0 (inst (vector "core_tkwl_1ymux_tie_l" #x0000 0.0d0 p0
						 (list "gnd" "BL" "MWL_TK" "MWL" "FIX_GND")
						 (list "gnd" "BL" "MWL_TK" "MWL" "FIX_GND"))))
  (setf s1 (inst-right-down s0 (vector "DUMY_CORE_REF_R4C2_MID" #x0000 0.0d0 p0
						 (list "gnd" "BLREFL" "FIX_GND" "BLREFR")
						 (list "gnd" "BLREFL" "FIX_GND" "BLREFR"))))
  (setf s2 (inst-right-down s1 (vector "core_tkwl_1ymux" #x0000 0.0d0 p0
						 (list "gnd" "BL" "MWL_TK" "MWL" "FIX_GND")
						 (list "gnd" "BL" "MWL_TK" "MWL" "FIX_GND"))))
  (push (cell-3 "core_tkwl_2ymux_tie_l" (list s0 s1 s2)) *srcunits*))

;;;;;;;;core_tkwl_2ymux_tie_r_mid
(print "core_tkwl_2ymux_tie_r_mid")
(let ((p0 (vector 0 0)) (s0) (s1) (s2))
  (setf s0 (inst (vector "core_tkwl_1ymux" #x0000 0.0d0 p0
						 (list "gnd" "BL" "MWL_TK" "MWL" "FIX_GND")
						 (list "gnd" "BL" "MWL_TK" "MWL" "FIX_GND"))))
  (setf s1 (inst-right-down s0 (vector "DUMY_CORE_REF_R4C2" #x0000 0.0d0 p0
						 (list "gnd" "BLREFL" "FIX_GND" "BLREFR")
						 (list "gnd" "BLREFL" "FIX_GND" "BLREFR"))))
  (setf s2 (inst-right-down s1 (vector "core_tkwl_1ymux_tie_mid" #x0000 0.0d0 p0
						 (list "gnd" "BL" "MWL_TK" "MWL" "FIX_GND")
						 (list "gnd" "BL" "MWL_TK" "MWL" "FIX_GND"))))
  (push (cell-3 "core_tkwl_2ymux_tie_r_mid" (list s0 s1 s2)) *srcunits*))

;;;;;;;;core_tkwl_2ymux_tie_l_mid
(print "core_tkwl_2ymux_tie_l_mid")
(let ((p0 (vector 0 0)) (s0) (s1) (s2))
  (setf s0 (inst (vector "core_tkwl_1ymux_tie_mid" #x0000 0.0d0 p0
						 (list "gnd" "BL" "MWL_TK" "MWL" "FIX_GND")
						 (list "gnd" "BL" "MWL_TK" "MWL" "FIX_GND"))))
  (setf s1 (inst-right-down s0 (vector "DUMY_CORE_REF_R4C2_MID" #x0000 0.0d0 p0
						 (list "gnd" "BLREFL" "FIX_GND" "BLREFR")
						 (list "gnd" "BLREFL" "FIX_GND" "BLREFR"))))
  (setf s2 (inst-right-down s1 (vector "core_tkwl_1ymux" #x0000 0.0d0 p0
						 (list "gnd" "BL" "MWL_TK" "MWL" "FIX_GND")
						 (list "gnd" "BL" "MWL_TK" "MWL" "FIX_GND"))))
  (push (cell-3 "core_tkwl_2ymux_tie_l_mid" (list s0 s1 s2)) *srcunits*))

;;;;;;;;core_tkwl_dummy
(print "core_tkwl_dummy")
(let ((p0 (vector 0 0)) (i1) (insts (list)))
  (dotimes (dimx 2)
	(setf i1 (vector "CORE00" #x4000 0.0d0 p0
					 (list "gnd" "BL" "WL0" "WL1")
					 (list "gnd" "BL" "WL0" "WL1")))
	(push (inst i1) insts)
	(setf p0 (vector+ p0 (vector 0 (vinst-high i1)))))
  (push (cell-3 "core_tkwl_dummy" insts) *srcunits*))

;;;;;;;;core_tkwl_L
(print "core_tkwl_L")
(defun core_tkwl_L (sname)
  (let ((p0 (vector 0 0)) (insts (list)))
	(dotimes (dimx (div1 *number-of-cols-l* 2))
	  (setf i1 (vector sname #x0000 0.0d0 p0
					   (list "gnd" "BL" "MWL_TK" "MWL" "FIX_GND" "BLREFL" "BLREFR")
					   (list "gnd" "BL" "MWL_TK" "MWL" "FIX_GND" "BLREFL" "BLREFR")))
	  (push (inst i1) insts)
	  (setf p0 (vector+ p0 (vector (vinst-width i1) 0))))
	(push (cell-3 "core_tkwl_L" insts) *srcunits*)))

;;;;;;;;core_tkwl_left
(print "core_tkwl_left")
(if (evenp *number-of-cols-l*)
(let ((p0 (vector 0 0)) (s0) (s1) (s2) (s3))
  (core_tkwl_L "core_tkwl_2ymux")
  (setf s0 (inst (vector "core_tkwl_L" #x0000 0.0d0 p0
						 (list "BL" "BLREFL" "BLREFR")
						 (list "BL" "BLREFL" "BLREFR"))))
  (setf s1 (inst-left-down s0 (vector "core_tkwl_dummy" #x4000 0.0d0 p0
						 (list)
						 (list))))
  (setf s2 (inst-left-down s1 (vector "tkwl_edge_LL" #x0000 0.0d0 p0
						 (list "VDD" "GND")
						 (list "VDD" "GND"))))
  (setf s3 (inst-up-right s0 (vector "tkwl_sub_left" #x0000 0.0d0 p0
						 (list "VDD" "GND")
						 (list "VDD" "GND"))))
  (push (cell-3 "core_tkwl_left" (list s0 s1 s2 s3)) *srcunits*))
  (if (= (div1 *number-of-cols-l* 2) 0)
(let ((p0 (vector 0 0)) (s0) (s1) (s2) (s3))
  (setf s0 (inst (vector "core_tkwl_1ymux" #x0000 0.0d0 p0
						 (list "BL")
						 (list "BL"))))
  (setf s1 (inst-left-down s0 (vector "DUMY_CORE_REF_R4C2" #x0000 0.0d0 p0
						 (list)
						 (list))))
  (setf s2 (inst-left-down s1 (vector "tkwl_edge_LL" #x0000 0.0d0 p0
						 (list "VDD" "GND")
						 (list "VDD" "GND"))))
  (setf s3 (inst-up-right s0 (vector "tkwl_sub_left" #x0000 0.0d0 p0
						 (list "VDD" "GND")
						 (list "VDD" "GND"))))
  (push (cell-3 "core_tkwl_left" (list s0 s1 s2 s3)) *srcunits*))
(let ((p0 (vector 0 0)) (s0) (s1) (s2) (s3) (s4))
  (core_tkwl_L "core_tkwl_2ymux")
  (setf s0 (inst (vector "core_tkwl_L" #x0000 0.0d0 p0
						 (list "BL" "BLREFL" "BLREFR")
						 (list "BL" "BLREFL" "BLREFR"))))
  (setf s1 (inst-left-down s0 (vector "core_tkwl_1ymux" #x0000 0.0d0 p0
						 (list "BL")
						 (list "BL"))))
  (setf s2 (inst-left-down s1 (vector "DUMY_CORE_REF_R4C2" #x0000 0.0d0 p0
						 (list)
						 (list))))
  (setf s3 (inst-left-down s2 (vector "tkwl_edge_LL" #x0000 0.0d0 p0
						 (list "VDD" "GND")
						 (list "VDD" "GND"))))
  (setf s4 (inst-up-right s0 (vector "tkwl_sub_left" #x0000 0.0d0 p0
						 (list "VDD" "GND")
						 (list "VDD" "GND"))))
  (push (cell-3 "core_tkwl_left" (list s0 s1 s2 s3 s4)) *srcunits*))))

;;;;;;;;core_tkwl_R
(print "core_tkwl_R")
(defun core_tkwl_R (sname1 sname2 sname3 sname4 sname5 sname6 sel)
  (let ((p0 (vector 0 0)) (insts (list)))
	(dotimes (dimx (div1 *number-of-cols-r* 2))
	  (setf i1 (vector (if (case sel
							 (1 (oddp (div1 *number-of-cols-r* 2)))
							 (2 (oddp (div1 *number-of-cols-l* 2))))
						 (if (= (div1 *number-of-cols-l* 4) dimx)
						   sname1
						   (if (> dimx (div1 *number-of-cols-r* 4))
							sname2 sname3 ))
						 (if (= (div1 *number-of-cols-l* 4) dimx)
						   sname4
						   (if (> dimx (div1 *number-of-cols-r* 4))
							sname5 sname6 ))) #x0000 0.0d0 p0
					   (list "gnd" "BL" "MWL_TK" "MWL" "FIX_GND" "BLREFL" "BLREFR")
					   (list "gnd" "BL" "MWL_TK" "MWL" "FIX_GND" "BLREFL" "BLREFR")))
	  (push (inst i1) insts)
	  (setf p0 (vector+ p0 (vector (vinst-width i1) 0))))
	(push (cell-3 "core_tkwl_R" insts) *srcunits*)))

;;;;;;;;core_tkwl_right
(print "core_tkwl_right")
(cond
  ((and (evenp *number-of-cols-r*) (evenp *number-of-cols-l*))
   (let ((s0) (s1) (s2) (s3) (p0 (vector 0 0)))
	 (core_tkwl_R "core_tkwl_2ymux_tie_mid" "core_tkwl_2ymux_r" "core_tkwl_2ymux"
				  "core_tkwl_2ymux_tie_l" "core_tkwl_2ymux_r" "core_tkwl_2ymux" 1)
	 (setf s0 (inst (vector "core_tkwl_R" #x0000 0.0d0 p0
							(list)
							(list))))
	 (setf s1 (inst-right-down s0 (vector "core_tkwl_dummy" #x0000 0.0d0 p0
							(list)
							(list))))
	 (setf s2 (inst-right-down s1 (vector "tkwl_edge_RR" #x0000 0.0d0 p0
							(list "GND" "VDD")
							(list "GND" "VDD"))))
	 (setf s3 (inst-up-left s0 (vector "tkwl_sub_right" #x0000 0.0d0 p0
							(list "GND" "VDD")
							(list "GND" "VDD"))))
	 (push (cell-3 "core_tkwl_right" (list  s0 s1 s2 s3)) *srcunits*)))
  ((and (evenp *number-of-cols-r*) (oddp *number-of-cols-l*))
   (let ((s0) (s1) (s2) (s3) (p0 (vector 0 0)))
	 (core_tkwl_R "core_tkwl_2ymux_tie_r_mid" "core_tkwl_2ymux_r" "core_tkwl_2ymux"
				  "core_tkwl_2ymux_tie_l_mid" "core_tkwl_2ymux_r" "core_tkwl_2ymux" 2)
	 (setf s0 (inst (vector "core_tkwl_R" #x0000 0.0d0 p0
							(list)
							(list))))
	 (setf s1 (inst-right-down s0 (vector "core_tkwl_dummy" #x0000 0.0d0 p0
							(list)
							(list))))
	 (setf s2 (inst-right-down s1 (vector "tkwl_edge_RR" #x0000 0.0d0 p0
							(list "GND" "VDD")
							(list "GND" "VDD"))))
	 (setf s3 (inst-up-left s0 (vector "tkwl_sub_right" #x0000 0.0d0 p0
							(list "GND" "VDD")
							(list "GND" "VDD"))))
	 (push (cell-3 "core_tkwl_right" (list s0 s1 s2 s3)) *srcunits*)))
  ((and (oddp *number-of-cols-r*) (evenp *number-of-cols-l*))
   (cond
	 ((= 0 (div1 *number-of-cols-r* 2))
	  (let ((s0) (s1) (s2) (s3) (p0 (vector 0 0)))
		(setf s0 (inst (vector "core_tkwl_1ymux_tie_l" #x4000 0.0d0 p0
							(list)
							(list))))
		(setf s1 (inst-right-down s0 (vector "DUMY_CORE_REF_R4C2" #x0000 0.0d0 p0
							(list)
							(list))))
		(setf s2 (inst-right-down s1 (vector "tkwl_edge_RR" #x0000 0.0d0 p0
							(list "GND" "VDD")
							(list "GND" "VDD"))))
		(setf s3 (inst-up-left s0 (vector "tkwl_sub_right" #x0000 0.0d0 p0
							(list "GND" "VDD")
							(list "GND" "VDD"))))
		(push (cell-3 "core_tkwl_right" (list s0 s1 s2 s3)) *srcunits*)))
	 ((= 1 (div1 *number-of-cols-r* 2))
	  (let ((s0) (s1) (s2) (s3) (s4) (p0 (vector 0 0)))
		(setf s0 (inst (vector "core_tkwl_2ymux" #x4000 0.0d0 p0
							(list)
							(list))))
		(setf s1 (inst-right-down s0 (vector "core_tkwl_1ymux_tie_l" #x0000 0.0d0 p0
							(list)
							(list))))
		(setf s2 (inst-right-down s1 (vector "DUMY_CORE_REF_R4C2" #x4000 0.0d0 p0
							(list)
							(list))))
		(setf s3 (inst-right-down s2 (vector "tkwl_edge_RR" #x0000 0.0d0 p0
							(list "GND" "VDD")
							(list "GND" "VDD"))))
		(setf s4 (inst-up-left s0 (vector "tkwl_sub_right" #x0000 0.0d0 p0
							(list "GND" "VDD")
							(list "GND" "VDD"))))
		(push (cell-3 "core_tkwl_right" (list s0 s1 s2 s3 s4)) *srcunits*)))
	 (t (let ((s0) (s1) (s2) (s4) (s5) (p0 (vector 0 0)))
	   (core_tkwl_R "core_tkwl_2ymux_tie_l" "core_tkwl_2ymux_r" "core_tkwl_2ymux"
					"core_tkwl_2ymux_tie_mid" "core_tkwl_2ymux_r" "core_tkwl_2ymux" 1)
	   (setf s0 (inst (vector "core_tkwl_R" #x0000 0.0d0 p0
							(list)
							(list))))
	   (setf s1 (inst-right-down s0 (vector "core_tkwl_1ymux" #x0000 0.0d0 p0
							(list)
							(list))))
		(setf s2 (inst-right-down s1 (vector "DUMY_CORE_REF_R4C2" #x4000 0.0d0 p0
							(list)
							(list))))
		(setf s4 (inst-right-down s2 (vector "tkwl_edge_RR" #x0000 0.0d0 p0
							(list "GND" "VDD")
							(list "GND" "VDD"))))
		(setf s5 (inst-up-left s0 (vector "tkwl_sub_right" #x0000 0.0d0 p0
							(list "GND" "VDD")
							(list "GND" "VDD"))))
	   (push (cell-3 "core_tkwl_right" (list s0 s1 s2 s4 s5)) *srcunits*)))))
  ((and (oddp *number-of-cols-r*) (oddp *number-of-cols-l*))
   (if (= 0 (div1 *number-of-cols-r* 2))
	 (let ((s0) (s1) (s2) (s3) (p0 (vector 0 0)))
	   (setf s0 (inst (vector "core_tkwl_1ymux_tie_mid" #x4000 0.0d0 p0
							(list)
							(list))))
		(setf s1 (inst-right-down s0 (vector "DUMY_CORE_REF_R4C2" #x4000 0.0d0 p0
							(list)
							(list))))
		(setf s2 (inst-right-down s1 (vector "tkwl_edge_RR" #x0000 0.0d0 p0
							(list "GND" "VDD")
							(list "GND" "VDD"))))
		(setf s3 (inst-up-left s0 (vector "tkwl_sub_right" #x0000 0.0d0 p0
							(list "GND" "VDD")
							(list "GND" "VDD"))))
	   (push (cell-3 "core_tkwl_right" (list s0 s1 s2 s3)) *srcunits*))
	 (let ((s0) (s1) (s2) (s4) (s5) (p0 (vector 0 0)))
	   (core_tkwl_R "core_tkwl_2ymux_tie_r_mid" "core_tkwl_2ymux_r" "core_tkwl_2ymux"
					"core_tkwl_2ymux_tie_l_mid" "core_tkwl_2ymux_r" "core_tkwl_2ymux")
	   (setf s0 (inst (vector "core_tkwl_R" #x0000 0.0d0 p0
							(list)
							(list))))
	   (setf s1 (inst-right-down s0 (vector "core_tkwl_1ymux" #x0000 0.0d0 p0
							(list)
							(list))))
	   (setf s2 (inst-right-down s1 (if (evenp *number-of-cols-r*)
									  (vector "DUMY_CORE_REF_R4C2" #x4000 0.0d0 p0 (list) (list))
									  (vector "DUMY_CORE_REF_R4C2" #x4000 0.0d0 p0 (list) (list)))))
		(setf s4 (inst-right-down s2 (vector "tkwl_edge_RR" #x0000 0.0d0 p0
							(list "GND" "VDD")
							(list "GND" "VDD"))))
		(setf s5 (inst-up-left s0 (vector "tkwl_sub_right" #x0000 0.0d0 p0
							(list "GND" "VDD")
							(list "GND" "VDD"))))
	   (push (cell-3 "core_tkwl_right" (list s0 s1 s2 s4 s5)) *srcunits*)))))

;;;;;;;;bl_edger4c1
(print "bl_edger4c1")
(let ((i0) (i1) (i2) (i3) (p0 (vector 0 0)))
  (setf i0 (inst (vector "BL_EDGE" #x0000 0.0d0 p0
						 (list "gnd" "BL")
						 (list "gnd" "BL"))))
  (setf i1 (inst-left-down i0 (vector "BL_EDGE" #x4000 0.0d0 p0
						 (list "gnd" "BL")
						 (list "gnd" "BL"))))
  (setf i2 (inst-left-down i1 (vector "BL_EDGE" #x0000 0.0d0 p0
						 (list "gnd" "BL")
						 (list "gnd" "BL"))))
  (setf i3 (inst-left-down i2 (vector "BL_EDGE" #x4000 0.0d0 p0
						 (list "gnd" "BL")
						 (list "gnd" "BL"))))
  (push (cell-3 "bl_edger4c1" (list i0 i1 i2 i3)) *srcunits*))

;;;;;;;;bl_edge_per_1ymux
(print "bl_edge_per_1ymux")
(let ((i1) (p0 (vector 0 0)) (insts (list)))
  (dotimes (dimx (div1 (math-pow 2 *yaddrs*) 4))
	(setf i1 (vector "bl_edger4c1" #x0000 0.0d0 p0
					 (list "gnd" "BL")
					 (list "gnd" "BL")))
	(push (inst i1) insts)
	(setf p0 (vector+ p0 (vector (vinst-width i1) 0))))
  (push (cell-3 "bl_edge_per_1ymux" insts) *srcunits*))

;;;;;;;;bl_edge_per_2ymux
(print "bl_edge_per_2ymux")
(let ((s0) (s1) (s2) (p0 (vector 0 0)))
  (setf s0 (inst (vector "bl_edge_per_1ymux" #x0000 0.0d0 p0
						 (list "gnd" "BL" "FIX_GND")
						 (list "gnd" "BL" "FIX_GND"))))
  (setf s1 (inst-right-down s0 (vector "CORE_REF_R2C2_EDGE" #x0000 0.0d0 p0
						 (list "gnd" "BLREFL" "BLREFR" "FIX_GND")
						 (list "gnd" "BLREFL" "BLREFR" "FIX_GND"))))
  (setf s2 (inst-right-down s1 (vector "bl_edge_per_1ymux" #x0000 0.0d0 p0
						 (list "gnd" "BL" "FIX_GND")
						 (list "gnd" "BL" "FIX_GND"))))
  (push (cell-3 "bl_edge_per_2ymux" (list s0 s1 s2)) *srcunits*))

;;;;;;;;bl_edge_L
(print "bl_edge_L")
(let ((i1) (p0 (vector 0 0)) (insts (list)))
  (dotimes (dimx (div1 *number-of-cols-l* 2))
	(setf i1 (vector "bl_edge_per_2ymux" #x0000 0.0d0 p0
					 (list "gnd" "BL" "BLREFL" "BLREFR" "FIX_GND")
					 (list "gnd" "BL" "BLREFL" "BLREFR" "FIX_GND")))
	(push (inst i1) insts)
	(setf p0 (vector+ p0 (vector (vinst-width i1) 0))))
  (push (cell-3 "bl_edge_L" insts) *srcunits*))

;;;;;;;;bl_edge_left
(print "bl_edge_left")
(if (evenp *number-of-cols-l*)
  (let ((s0) (s1) (s2) (p0 (vector 0 0)))
	(setf s0 (inst (vector "bl_edge_L" #x0000 0.0d0 p0
						   (list "BL" "BLREFL" "BLREFR")
						   (list "BL" "BLREFL" "BLREFR"))))
	(setf s1 (inst-left-down s0 (vector "BL_EDGE" #x0000 0.0d0 p0
						   (list)
						   (list))))
	(setf s2 (inst-left-down s1 (vector *dmy_wl_corner_even* #x0000 0.0d0 p0
						   (list)
						   (list))))
	(push (cell-3 "bl_edge_left" (list s0 s1 s2)) *srcunits*))
  (if (= (div1 *number-of-cols-l* 2) 0)
	(let ((s0) (s1) (s2) (p0 (vector 0 0)))
	  (setf s0 (inst (vector "bl_edge_per_1ymux" #x0000 0.0d0 p0
							 (list "BL")
							 (list "BL"))))
	  (setf s1 (inst-left-down s0 (vector "CORE_REF_R2C2_EDGE" #x0000 0.0d0 p0
							 (list)
							 (list))))
	  (setf s2 (inst-left-down s1 (vector *dmy_wl_corner_odd* #x0000 0.0d0 p0
							 (list)
							 (list))))
	  (push (cell-3 "bl_edge_left" (list s0 s1 s2)) *srcunits*))
	(let ((s0) (s1) (s2) (s3) (p0 (vector 0 0)))
	  (setf s0 (inst (vector "bl_edge_L" #x0000 0.0d0 p0
							 (list "BL" "BLREFL" "BLREFR")
							 (list "BL" "BLREFL" "BLREFR"))))
	  (setf s1 (inst-left-down s0 (vector "bl_edge_per_1ymux" #x0000 0.0d0 p0
							 (list "BL")
							 (list "BL"))))
	  (setf s2 (inst-left-down s1 (vector "CORE_REF_R2C2_EDGE" #x0000 0.0d0 p0
							 (list)
							 (list))))
	  (setf s3 (inst-left-down s2 (vector *dmy_wl_corner_odd* #x0000 0.0d0 p0
							 (list)
							 (list))))
	  (push (cell-3 "bl_edge_left" (list s0 s1 s2 s3)) *srcunits*))))

;;;;;;;;bl_edge_R
(print "bl_edge_R")
(let ((i1) (p0 (vector 0 0)) (insts (list)))
  (dotimes (dimx (div1 *number-of-cols-r* 2))
	(setf i1 (vector "bl_edge_per_2ymux" #x0000 0.0d0 p0
					 (list "gnd" "BL" "BLREFL" "BLREFR" "FIX_GND")
					 (list "gnd" "BL" "BLREFL" "BLREFR" "FIX_GND")))
	(push (inst i1) insts)
	(setf p0 (vector+ p0 (vector (vinst-width i1) 0))))
  (push (cell-3 "bl_edge_R" insts) *srcunits*))

;;;;;;;;bl_edge_right
(print "bl_edge_right")
(if (evenp *number-of-cols-r*)
  (let ((s0) (s1) (s2) (p0 (vector 0 0)))
	(setf s0 (inst (vector "bl_edge_R" #x0000 0.0d0 p0 (list) (list))))
	(setf s1 (inst-right-down s0 (vector "BL_EDGE" #x4000 0.0d0 p0 (list) (list))))
	(setf s2 (inst-right-down s1 (vector *dmy_wl_corner_even* #x4000 0.0d0 p0 (list) (list))))
	(push (cell-3 "bl_edge_right" (list s0 s1 s2)) *srcunits*))
  (if (= (div1 *number-of-cols-r* 2) 0)
	(let ((s0) (s1) (s2) (p0 (vector 0 0)))
	  (setf s0 (inst (vector "bl_edge_per_1ymux" #x0000 0.0d0 p0 (list) (list))))
	  (setf s1 (inst-right-down s0 (vector "CORE_REF_R2C2_EDGE" #x4000 0.0d0 p0 (list) (list))))
	  (setf s2 (inst-right-down s1 (vector *dmy_wl_corner_odd* #x4000 0.0d0 p0 (list) (list))))
	  (push (cell-3 "bl_edge_right" (list s0 s1 s2)) *srcunits*))
	(let ((s0) (s1) (s2) (s4) (p0 (vector 0 0)))
	  (setf s0 (inst (vector "bl_edge_R" #x0000 0.0d0 p0 (list) (list))))
	  (setf s1 (inst-right-down s0 (vector "bl_edge_per_1ymux" #x0000 0.0d0 p0 (list) (list))))
	  (setf s2 (inst-right-down s0 (vector "CORE_REF_R2C2_EDGE" #x4000 0.0d0 p0 (list) (list))))
	  (setf s4 (inst-right-down s1 (vector *dmy_wl_corner_odd* #x4000 0.0d0 p0 (list) (list))))
	  (push (cell-3 "bl_edge_right" (list s0 s1 s2 s4)) *srcunits*))))

;;;;;;;;array_bit
(defun array_bit (num)
(print (concatenate 'string "array_bit_" (itoa num)))
(let ((cellname (concatenate 'string "array_bit_" (itoa num)))
	  (p0 (vector 0 0))
	  (addr0) (addr1)
	  (sname "CORE")
	  (bv0) (bv1)
	  (i1) (insts (list)))
  (dotimes (dimx (floor (math-pow 2 *yaddrs*)))
	(setf (aref p0 1) 0)
	(dotimes (dimy (floor (* 4 *num-of-xdecs*)))
	  (setf addr0 (floor (+ dimx (* dimy 2 (math-pow 2 *yaddrs*)))))
	  (setf addr1 (floor (+ dimx (* (1+ (* dimy 2)) (math-pow 2 *yaddrs*)))))
	  (setf bv0 (aref *code-map* addr0 (- word-length 1 num)))
	  (setf bv1 (aref *code-map* addr1 (- word-length 1 num)))
	  (setf sname (concatenate 'string "CORE" (string bv1) (string bv0)))
	  (if (evenp dimx)
		(setf i1 (vector sname #x4000 0.0d0 p0 (list) (list)))
		(setf i1 (vector sname #x0000 0.0d0 p0 (list) (list))))
	  (push (inst i1) insts)
	  (setf p0 (vector+ p0 (vector 0 (vinst-high i1)))))
	(setf p0 (vector+ p0 (vector (vinst-width i1) 0))))
  (push (cell-3 cellname insts) *srcunits*)))

;;;;;;;;array_bit_char
;(print "array_bit_char")

;;;;;;;;array_bit_char1
;(print "array_bit_char1")

;;;;;;;;array_bit_char2
;(print "array_bit_char2")

;;;;;;;;array_left
(defun array_left_even ()
(print "array_left")
(let ((s0) (s1) (p0 (vector 0 0)) (insts (list)))
(cond
  ((= charmode 0)
   (dotimes (dimx *number-of-cols-l*)
	 (array_bit dimx)
	 (setf s0 (vector (concatenate 'string "array_bit_" (itoa dimx)) #x0000 0.0d0 p0 (list) (list)))
	 (push (inst s0) insts)
	 (if (evenp dimx)
	   (progn
		 (setf s1 (vector "core_ref_array" #x0000 0.0d0 (vector+ p0 (vector (vinst-width s0) 0)) (list) (list)))
		 (push (inst s1) insts))
	   nil)
	 (setf p0 (vector+ p0 (vector (if (evenp dimx)
									(+ (vinst-width s0) (vinst-width s1)) (vinst-width s0)) 0))))))
(push (cell-3 "array_left" insts) *srcunits*)))

(defun array_left_odd ()
(print "array_left")
(let ((s0) (s1) (p0 (vector 0 0)) (insts (list)))
(cond
  ((= charmode 0)
   (dotimes (dimx *number-of-cols-l*)
	 (array_bit dimx)
	 (setf s0 (vector (concatenate 'string "array_bit_" (itoa dimx)) #x0000 0.0d0 p0 (list) (list)))
	 (push (inst s0) insts)
	 (if (oddp dimx)
	   (progn
		 (setf s1 (vector "core_ref_array" #x0000 0.0d0 (vector+ p0 (vector (vinst-width s0) 0)) (list) (list)))
		 (push (inst s1) insts))
	   nil)
	 (setf p0 (vector+ p0 (vector (if (oddp dimx)
									(+ (vinst-width s0) (vinst-width s1)) (vinst-width s0)) 0))))))
(push (cell-3 "array_left" insts) *srcunits*)))

;;;;;;;;core_left
(print "core_left")
(if (evenp *number-of-cols-l*)
  (let ((p0 (vector 0 0)) (s2) (s3) (s4) (s5) (s6))
	(array_left_even)
	(setf s2 (inst (vector "array_left" #x0000 0.0d0 p0 (list) (list))))
	(setf s3 (inst-left-down s2 (vector "core_dummy_array" #x0000 0.0d0 p0 (list) (list))))
	(setf s4 (inst-left-down s3 (vector "wl_edge_LL_col" #x0000 0.0d0 p0 (list "GND") (list "GND"))))
	(setf s5 (inst-up-left s4 (vector "core_tkwl_left" #x0000 0.0d0 p0 (list "GND" "VDD") (list "GND" "VDD"))))
	(setf s6 (inst-down-left s4 (vector "bl_edge_left" #x0000 0.0d0 p0 (list) (list))))
	(push (cell-3 "core_left" (list s2 s3 s4 s5 s6)) *srcunits*))
  (let ((p0 (vector 0 0)) (s2) (s3) (s4) (s5) (s6))
	(array_left_odd)
	(setf s2 (inst (vector "array_left" #x0000 0.0d0 p0 (list) (list))))
	(setf s3 (inst-left-down s2 (vector "core_ref_array" #x0000 0.0d0 p0 (list) (list))))
	(setf s4 (inst-left-down s3 (vector "wl_edge_LL_col" #x0000 0.0d0 p0 (list "GND") (list "GND"))))
	(setf s5 (inst-up-left s4 (vector "core_tkwl_left" #x0000 0.0d0 p0 (list "GND" "VDD") (list "GND" "VDD"))))
	(setf s6 (inst-down-left s4 (vector "bl_edge_left" #x0000 0.0d0 p0 (list) (list))))
	(push (cell-3 "core_left" (list s2 s3 s4 s5 s6)) *srcunits*)))

;;;;;;;;array_right
(defun array_right_1 ()
(print "array_right")
(let ((s0) (s1) (cal) (p0 (vector 0 0)) (insts (list)))
  (dotimes (dimx *number-of-cols-r*)
	(setf cal (+ *number-of-cols-l* dimx))
	(array_bit cal)
	(setf s0 (vector (concatenate 'string "array_bit_" (itoa cal)) #x0000 0.0d0 p0 (list) (list)))
	(push (inst s0) insts)
	(cond
	  ((and (evenp *number-of-cols-l*) (evenp cal))
	   (progn
		 (setf s1 (vector "core_ref_array" #x0000 0.0d0 (vector+ p0 (vector (vinst-width s0) 0)) (list) (list)))
		 (push (inst s1) insts)
		 (setf p0 (vector+ p0 (vector (+ (vinst-width s0) (vinst-width s1)) 0)))))
	  ((and (oddp *number-of-cols-l*) (oddp cal))
	   (progn
		 (setf s1 (vector "core_ref_array" #x0000 0.0d0 (vector+ p0 (vector (vinst-width s0) 0)) (list) (list)))
		 (push (inst s1) insts)
		 (setf p0 (vector+ p0 (vector (+ (vinst-width s0) (vinst-width s1)) 0)))))
	  (t (setf p0 (vector+ p0 (vector (vinst-width s0) 0))))))
  (push (cell-3 "array_right" insts) *srcunits*)))

(defun array_right_2 ()
(print "array_right")
(let ((s0) (s1) (cal) (p0 (vector 0 0)) (insts (list)))
  (dotimes (dimx *number-of-cols-r*)
	(setf cal (+ *number-of-cols-l* dimx))
	(array_bit cal)
	(setf s0 (vector (concatenate 'string "array_bit_" (itoa cal)) #x0000 0.0d0 p0 (list) (list)))
	(push (inst s0) insts)
	(cond
	  ((and (evenp *number-of-cols-l*) (evenp cal))
	   (progn
		 (setf s1 (if (= (- *number-of-cols-r* 1) dimx)
					(vector "core_ref_array" #x4000 0.0d0 (vector+ p0 (vector (vinst-width s0) 0)) (list) (list))
					(vector "core_ref_array" #x0000 0.0d0 (vector+ p0 (vector (vinst-width s0) 0)) (list) (list))))
		 (push (inst s1) insts)
		 (setf p0 (vector+ p0 (vector (+ (vinst-width s0) (vinst-width s1)) 0)))))
	  ((and (oddp *number-of-cols-l*) (oddp cal))
	   (progn
		 (setf s1 (if (= (- *number-of-cols-r* 1) dimx)
					(vector "core_ref_array" #x4000 0.0d0 (vector+ p0 (vector (vinst-width s0) 0)) (list) (list))
					(vector "core_ref_array" #x0000 0.0d0 (vector+ p0 (vector (vinst-width s0) 0)) (list) (list))))
		 (push (inst s1) insts)
		 (setf p0 (vector+ p0 (vector (+ (vinst-width s0) (vinst-width s1)) 0)))))
	  (t (setf p0 (vector+ p0 (vector (vinst-width s0) 0))))))
  (push (cell-3 "array_right" insts) *srcunits*)))

;;;;;;;;core_right
(print "core_right")
(if (evenp *number-of-cols-r*)
  (let ((s2) (s3) (s4) (s5) (s6) (p0 (vector 0 0)))
	(case charmode
	  (0 (array_right_1)))
	(setf s2 (inst (vector "array_right" #x0000 0.0d0 p0 (list) (list))))
	(setf s3 (inst-right-down s2 (vector "core_dummy_array" #x4000 0.0d0 p0 (list) (list))))
	(setf s4 (inst-right-down s3 (vector "wl_edge_RR_col" #x0000 0.0d0 p0 (list "GND") (list "GND"))))
	(setf s5 (inst-up-left s2 (vector "core_tkwl_right" #x0000 0.0d0 p0 (list "GND" "VDD") (list "GND" "VDD"))))
	(setf s6 (inst-down-right s4 (vector "bl_edge_right" #x0000 0.0d0 p0 (list) (list))))
	(push (cell-3 "core_right" (list s2 s3 s4 s5 s6)) *srcunits*))
  (let ((s2) (s3) (s4) (s5) (p0 (vector 0 0)))
	(case charmode
	  (0 (array_right_2)))
	(setf s2 (inst (vector "array_right" #x0000 0.0d0 p0 (list) (list))))
	(setf s3 (inst-right-down s2 (vector "wl_edge_RR_col" #x0000 0.0d0 p0 (list "GND") (list "GND"))))
	(setf s4 (inst-up-left s2 (vector "core_tkwl_right" #x0000 0.0d0 p0 (list "GND" "VDD") (list "GND" "VDD"))))
	(setf s5 (inst-down-right s3 (vector "bl_edge_right" #x0000 0.0d0 p0 (list) (list))))
	(push (cell-3 "core_right" (list s2 s3 s4 s5)) *srcunits*)))

;;;;;;;;tkbl_array
(print "tkbl_array")
(let ((insts (list)) (p0 (vector 0 0)) (i0))
  (dotimes (dimx (* *num-of-xdecs* 4))
	(setf i0 (vector "CORE_TKBL_DUMMY" #x0000 0.0d0 p0
					 (list "gnd" "WL0" "WL1" "MBL" "FIX_GND")
					 (list "gnd" "WL0" "WL1" "MBL" "FIX_GND")))
	(push (inst i0) insts)
	(setf p0 (vector+ p0 (vector 0 (vinst-high i0)))))
  (push (cell-3 "tkbl_array" insts) *srcunits*))

;;;;;;;;tkbl_col
(print "tkbl_col")
(let ((s0) (p0 (vector 0 0)) (GND_ (concatenate 'string "GND_" vpinlayer)))
  (setf s0 (inst (vector "CORE_TKBL_SUB" #x0000 0.0d0 p0
						 (list GND_)
						 (list GND_))))
  (push (cell-3 "core_tkbl_sub" (list s0)) *srcunits*))
(let ((s0) (s1) (s2) (s3) (p0 (vector 0 0)) (GND_ (concatenate 'string "GND_" vpinlayer)))
  (setf s0 (inst (vector "tkbl_array" #x0000 0.0d0 p0 (list) (list))))
  (setf s1 (inst-up-right s0 (vector "CORE_TKBL" #x0000 0.0d0 p0 (list) (list))))
  (setf s2 (inst-up-right s1 (vector "core_tkbl_sub" #x0000 0.0d0 p0
									 (list GND_)
									 (list "GND"))))
  (setf s3 (inst-down-right s0 (vector "CORE_TKBL_EDGE" #x0000 0.0d0 p0 (list) (list))))
  (push (cell-3 "tkbl_col" (list s0 s1 s2 s3)) *srcunits*))

;;;;;;;;wldrv8
(print "wldrv8")
(let ((i1) (p0 (vector 0 0)))
  (setf i1 (inst (vector "WLDRV_R8" #x0000 0.0d0 p0
						 (list "WLL0" "WLL1" "WLL2" "WLL3" "WLL4" "WLL5" "WLL6" "WLL7"
							   "WLR0" "WLR1" "WLR2" "WLR3" "WLR4" "WLR5" "WLR6" "WLR7"
							   "GTP" "FIX_VDD_B" "FIX_VDD_C" "vdd" "gnd"
							   "XPA0" "XPA1" "XPA2" "XPA3" "XPA4" "XPA5" "XPA6" "XPA7"
							   "XPB0" "XPB1" "XPB2" "XPB3" "XPB4" "XPB5" "XPB6" "XPB7"
							   "XPC0" "XPC1" "XPC2" "XPC3" "XPC4" "XPC5" "XPC6" "XPC7" "XPC8")
						 (list "WLL0" "WLL1" "WLL2" "WLL3" "WLL4" "WLL5" "WLL6" "WLL7"
							   "WLR0" "WLR1" "WLR2" "WLR3" "WLR4" "WLR5" "WLR6" "WLR7"
							   "GTP" "FIX_VDD_B" "FIX_VDD_C" "VDD" "GND"
							   "XPA0" "XPA1" "XPA2" "XPA3" "XPA4" "XPA5" "XPA6" "XPA7"
							   "XPB0" "XPB1" "XPB2" "XPB3" "XPB4" "XPB5" "XPB6" "XPB7"
							   "XPC0" "XPC1" "XPC2" "XPC3" "XPC4" "XPC5" "XPC6" "XPC7" "XPC8"))))
  (push (cell-3 "wldrv8" (list i1)) *srcunits*))

;;;;;;;;dmy_wldrv
(print "dmy_wldrv")
(let ((i1) (p0 (vector 0 0)))
  (setf i1 (inst (vector "DMY_WLDRV" #x0000 0.0d0 p0
						 (list "GND_m1" "GND_m2" "GND_m3" "GND_m4"
							   "VDD_m1" "VDD_m2" "VDD_m3" "VDD_m4"
							   "MWLL" "MWLR" "MBL" "FIX_GND" "FIX_VDD")
						 (list "GND_m1" "GND_m2" "GND_m3" "GND_m4"
							   "VDD_m1" "VDD_m2" "VDD_m3" "VDD_m4"
							   "MWLL" "MWLR" "MBL" "FIX_GND" "FIX_VDD"))))
  (push (cell-3 "dmy_wldrv" (list i1)) *srcunits*))

;;;;;;;;wldrvrow
(print "wldrvrow")
(let ((insts (list)) (p0 (vector 0 0)) (wldrv))
  (dotimes (dimx *num-of-xdecs*)
	(setf wldrv (vector "wldrv8" #x0000 0.0d0 p0
						(list "WLL0" "WLL1" "WLL2" "WLL3" "WLL4" "WLL5" "WLL6" "WLL7"
							  "WLR0" "WLR1" "WLR2" "WLR3" "WLR4" "WLR5" "WLR6" "WLR7"
							  "GTP" "FIX_VDD_B" "FIX_VDD_C" "VDD" "GND"
							  "XPA0" "XPA1" "XPA2" "XPA3" "XPA4" "XPA5" "XPA6" "XPA7"
							  "XPB0" "XPB1" "XPB2" "XPB3" "XPB4" "XPB5" "XPB6" "XPB7"
							  "XPC0" "XPC1" "XPC2" "XPC3" "XPC4" "XPC5" "XPC6" "XPC7" "XPC8")
						(list (concatenate 'string "r" (itoa dimx) "WLL0")
							  (concatenate 'string "r" (itoa dimx) "WLL1")
							  (concatenate 'string "r" (itoa dimx) "WLL2")
							  (concatenate 'string "r" (itoa dimx) "WLL3")
							  (concatenate 'string "r" (itoa dimx) "WLL4")
							  (concatenate 'string "r" (itoa dimx) "WLL5")
							  (concatenate 'string "r" (itoa dimx) "WLL6")
							  (concatenate 'string "r" (itoa dimx) "WLL7")
							  (concatenate 'string "r" (itoa dimx) "WLR0")
							  (concatenate 'string "r" (itoa dimx) "WLR1")
							  (concatenate 'string "r" (itoa dimx) "WLR2")
							  (concatenate 'string "r" (itoa dimx) "WLR3")
							  (concatenate 'string "r" (itoa dimx) "WLR4")
							  (concatenate 'string "r" (itoa dimx) "WLR5")
							  (concatenate 'string "r" (itoa dimx) "WLR6")
							  (concatenate 'string "r" (itoa dimx) "WLR7")
							  (concatenate 'string "r" (itoa dimx) "GTP")
							  (concatenate 'string "r" (itoa dimx) "FIX_VDD_B")
							  (concatenate 'string "r" (itoa dimx) "FIX_VDD_C")
							  "VDD" "GND"
							  (concatenate 'string "r" (itoa dimx) "XPA0")
							  (concatenate 'string "r" (itoa dimx) "XPA1")
							  (concatenate 'string "r" (itoa dimx) "XPA2")
							  (concatenate 'string "r" (itoa dimx) "XPA3")
							  (concatenate 'string "r" (itoa dimx) "XPA4")
							  (concatenate 'string "r" (itoa dimx) "XPA5")
							  (concatenate 'string "r" (itoa dimx) "XPA6")
							  (concatenate 'string "r" (itoa dimx) "XPA7")
							  (concatenate 'string "r" (itoa dimx) "XPB0")
							  (concatenate 'string "r" (itoa dimx) "XPB1")
							  (concatenate 'string "r" (itoa dimx) "XPB2")
							  (concatenate 'string "r" (itoa dimx) "XPB3")
							  (concatenate 'string "r" (itoa dimx) "XPB4")
							  (concatenate 'string "r" (itoa dimx) "XPB5")
							  (concatenate 'string "r" (itoa dimx) "XPB6")
							  (concatenate 'string "r" (itoa dimx) "XPB7")
							  (concatenate 'string "r" (itoa dimx) "XPC0")
							  (concatenate 'string "r" (itoa dimx) "XPC1")
							  (concatenate 'string "r" (itoa dimx) "XPC2")
							  (concatenate 'string "r" (itoa dimx) "XPC3")
							  (concatenate 'string "r" (itoa dimx) "XPC4")
							  (concatenate 'string "r" (itoa dimx) "XPC5")
							  (concatenate 'string "r" (itoa dimx) "XPC6")
							  (concatenate 'string "r" (itoa dimx) "XPC7")
							  (concatenate 'string "r" (itoa dimx) "XPC8"))))
	(push (inst wldrv) insts)
	(setf p0 (vector+ p0 (vector 0 (vinst-high wldrv)))))
  (push (cell-3 "wldrvrow" insts) *srcunits*))

;;;;;;;;wldrvall
(print "wldrvall")
(let ((s0) (s3) (p0 (vector 0 0)) (indexxpb) (indexxpc)
		   (GND_ (concatenate 'string "GND_" vpinlayer))
		   (VDD_ (concatenate 'string "VDD_" vpinlayer))
		   (lst1 (list)) (lst2 (list)))
  (cond
	((= 1 charmode) nil)
	((<= *num-of-xdecs* 1)
	 (progn
	   (dotimes (dimx *num-of-xdecs*)
		 (push (overlay-1 "DECODE_VIA23" #x0000 90.0d0 (get-cell *srcunits* "wldrvrow") "C0" (concatenate 'string "r" (itoa dimx) "FIX_VDD_B"))
			   (cdddr (get-cell *srcunits* "wldrvrow")))
		 (push (overlay-1 "DECODE_VIA23" #x0000 90.0d0 (get-cell *srcunits* "wldrvrow") "C0" (concatenate 'string "r" (itoa dimx) "FIX_VDD_C"))
			   (cdddr (get-cell *srcunits* "wldrvrow"))))))
	((and (<= *num-of-xdecs* 8) (> *num-of-xdecs* 1))
	 (progn
	   (dotimes (dimy *num-of-xdecs*)
		 (setf indexxpb (mod dimy 8))
		 (push (overlay-1 "WLDRV_VIA12" #x0000 0.0d0 (get-cell *srcunits* "wldrvrow") "C0" (concatenate 'string "r" (itoa dimy) "XPB" (itoa indexxpb)))
			   (cdddr (get-cell *srcunits* "wldrvrow")))
		 (push (overlay-1 "DECODE_VIA23" #x0000 90.0d0 (get-cell *srcunits* "wldrvrow") "C0" (concatenate 'string "r" (itoa dimy) "FIX_VDD_C"))
			   (cdddr (get-cell *srcunits* "wldrvrow"))))))
	((and (<= *num-of-xdecs* 128) (> *num-of-xdecs* 8))
	 (progn
	   (dotimes (dimy *num-of-xdecs*)
		 (setf indexxpb (mod dimy 8))
		 (setf indexxpc (div1 dimy 8))
		 (push (overlay-1 "WLDRV_VIA12" #x0000 0.0d0 (get-cell *srcunits* "wldrvrow") "C0" (concatenate 'string "r" (itoa dimy) "XPB" (itoa indexxpb)))
			   (cdddr (get-cell *srcunits* "wldrvrow")))
		 (push (overlay-1 "WLDRV_VIA12" #x0000 0.0d0 (get-cell *srcunits* "wldrvrow") "C0" (concatenate 'string "r" (itoa dimy) "XPC" (itoa indexxpc)))
			   (cdddr (get-cell *srcunits* "wldrvrow")))))))
  (setf s0 (inst (vector "wldrvrow" #x0000 0.0d0 p0 lst1 lst2)))
  (setf s3 (inst-up-right s0 (vector "dmy_wldrv" #x0000 0.0d0 p0 (list GND_ VDD_) (list "GND" "VDD"))))
  (push (cell-3 "wldrvall" (list s0 s3)) *srcunits*))

;;;;;;;;xypredec_itfctl
(print "xypredec_itfctl")
(defvar dec_pins (list))
(let ((p0 (vector 0 0)) (ictl) (iypre) (ixyprecon) (ixprelow) (ixpremid) (ixprehigh) (icoldec) (iwl_tie)
						(GND_ (concatenate 'string "GND_" vpinlayer))
						(VDD_ (concatenate 'string "VDD_" vpinlayer))
						(CLK_ (concatenate 'string "CLK_" vpinlayer))
						(CEB_ (concatenate 'string "CEB_" vpinlayer))
						(AD0_ (concatenate 'string "AD0_" vpinlayer))
						(AD1_ (concatenate 'string "AD1_" vpinlayer))
						(AD2_ (concatenate 'string "AD2_" vpinlayer))
						(AD3_ (concatenate 'string "AD3_" vpinlayer))
						(AD4_ (concatenate 'string "AD4_" vpinlayer))
						(A0_ (concatenate 'string "A0_" vpinlayer))
						(A1_ (concatenate 'string "A1_" vpinlayer))
						(A2_ (concatenate 'string "A2_" vpinlayer))
						(ictl.l1) (ictl.l2)
						(iypre.l1) (iypre.l2)
						(ixyprecon.l1) (ixyprecon.l2)
						(ixprelow.l1) (ixprelow.l2)
						(ixpremid.l1) (ixpremid.l2)
						(ixprehigh.l1) (ixprehigh.l2)
						(icoldec.l1) (icoldec.l2)
						(iwl_tie.l1) (iwl_tie.l2))
  (setf ictl.l1 (list GND_ VDD_ CLK_ CEB_))
  (setf ictl.l2 (list "GND" "VDD" "CLK" "CEB"))
  (setf iypre.l1 (list GND_ VDD_))
  (setf iypre.l2 (list "GND" "VDD"))
  (setf ixyprecon.l1 (list GND_ VDD_))
  (setf ixyprecon.l2 (list "GND" "VDD"))
  (setf ixprelow.l1 (list GND_ VDD_))
  (setf ixprelow.l2 (list "GND" "VDD"))
  (setf ixpremid.l1 (list GND_ VDD_))
  (setf ixpremid.l2 (list "GND" "VDD"))
  (setf ixprehigh.l1 (list GND_ VDD_))
  (setf ixprehigh.l2 (list "GND" "VDD"))
  (case col-mux
	(8 (progn
		 (push AD0_ iypre.l1) (push "AD[0]" iypre.l2)
		 (push AD1_ iypre.l1) (push "AD[1]" iypre.l2)
		 (push AD2_ iypre.l1) (push "AD[2]" iypre.l2)
		 (push A0_ ixprelow.l1) (push "AD[3]" ixprelow.l2)
		 (push A1_ ixprelow.l1) (push "AD[4]" ixprelow.l2)
		 (push A2_ ixprelow.l1) (push "AD[5]" ixprelow.l2)
		 (case *Xaddrsmid*
		   (0 (progn
				(push "FIX_GND0" ixpremid.l1) (push "ixpremid.FIX_GND0" ixpremid.l2) (push "ixpremid.FIX_GND0" dec_pins)
				(push "FIX_GND1" ixpremid.l1) (push "ixpremid.FIX_GND1" ixpremid.l2) (push "ixpremid.FIX_GND1" dec_pins)
				(push "FIX_GND2" ixpremid.l1) (push "ixpremid.FIX_GND2" ixpremid.l2) (push "ixpremid.FIX_GND2" dec_pins)))
		   (1 (progn
				(push A0_ ixpremid.l1) (push "AD[6]" ixpremid.l2)
				(push "FIX_GND1" ixpremid.l1) (push "ixpremid.FIX_GND1" ixpremid.l2) (push "ixpremid.FIX_GND1" dec_pins)
				(push "FIX_GND2" ixpremid.l1) (push "ixpremid.FIX_GND2" ixpremid.l2) (push "ixpremid.FIX_GND2" dec_pins)))
		   (2 (progn
				(push A0_ ixpremid.l1) (push "AD[6]" ixpremid.l2)
				(push A1_ ixpremid.l1) (push "AD[7]" ixpremid.l2)
				(push "FIX_GND2" ixpremid.l1) (push "ixpremid.FIX_GND2" ixpremid.l2) (push "ixpremid.FIX_GND2" dec_pins)))
		   (3 (progn
				(push A0_ ixpremid.l1) (push "AD[6]" ixpremid.l2)
				(push A1_ ixpremid.l1) (push "AD[7]" ixpremid.l2)
				(push A2_ ixpremid.l1) (push "AD[8]" ixpremid.l2))))
		 (case *Xaddrshigh*
		   (0 (progn
				(push "FIX_GND0" ixprehigh.l1) (push "ixprehigh.FIX_GND0" ixprehigh.l2) (push "ixprehigh.FIX_GND0" dec_pins)
				(push "FIX_GND1" ixprehigh.l1) (push "ixprehigh.FIX_GND1" ixprehigh.l2) (push "ixprehigh.FIX_GND1" dec_pins)
				(push "FIX_GND2" ixprehigh.l1) (push "ixprehigh.FIX_GND2" ixprehigh.l2) (push "ixprehigh.FIX_GND2" dec_pins)))
		   (1 (progn
				(push A0_ ixprehigh.l1) (push "AD[9]" ixprehigh.l2)
				(push "FIX_GND1" ixprehigh.l1) (push "ixprehigh.FIX_GND1" ixprehigh.l2) (push "ixprehigh.FIX_GND1" dec_pins)
				(push "FIX_GND2" ixprehigh.l1) (push "ixprehigh.FIX_GND2" ixprehigh.l2) (push "ixprehigh.FIX_GND2" dec_pins)))
		   (2 (progn
				(push A0_ ixprehigh.l1) (push "AD[9]" ixprehigh.l2)
				(push A1_ ixprehigh.l1) (push "AD[10]" ixprehigh.l2)
				(push "FIX_GND2" ixprehigh.l1) (push "ixprehigh.FIX_GND2" ixprehigh.l2) (push "ixprehigh.FIX_GND2" dec_pins)))
		   (3 (progn
				(push A0_ ixprehigh.l1) (push "AD[9]" ixprehigh.l2)
				(push A1_ ixprehigh.l1) (push "AD[10]" ixprehigh.l2)
				(push A2_ ixprehigh.l1) (push "AD[11]" ixprehigh.l2)))
		   (4 (progn
				(push A0_ ixprehigh.l1) (push "AD[9]" ixprehigh.l2)
				(push A1_ ixprehigh.l1) (push "AD[10]" ixprehigh.l2)
				(push A2_ ixprehigh.l1) (push "AD[11]" ixprehigh.l2))))))
	(16 (progn
		  (push AD0_ iypre.l1) (push "AD[0]" iypre.l2)
		  (push AD1_ iypre.l1) (push "AD[1]" iypre.l2)
		  (push AD2_ iypre.l1) (push "AD[2]" iypre.l2)
		  (push AD3_ iypre.l1) (push "AD[3]" iypre.l2)
		  (push A0_ ixprelow.l1) (push "AD[4]" ixprelow.l2)
		  (push A1_ ixprelow.l1) (push "AD[5]" ixprelow.l2)
		  (push A2_ ixprelow.l1) (push "AD[6]" ixprelow.l2)
		  (case *Xaddrsmid*
			(0 (progn
				(push "FIX_GND0" ixpremid.l1) (push "ixpremid.FIX_GND0" ixpremid.l2) (push "ixpremid.FIX_GND0" dec_pins)
				(push "FIX_GND1" ixpremid.l1) (push "ixpremid.FIX_GND1" ixpremid.l2) (push "ixpremid.FIX_GND1" dec_pins)
				(push "FIX_GND2" ixpremid.l1) (push "ixpremid.FIX_GND2" ixpremid.l2) (push "ixpremid.FIX_GND2" dec_pins)))
		   (1 (progn
				(push A0_ ixpremid.l1) (push "AD[7]" ixpremid.l2)
				(push "FIX_GND1" ixpremid.l1) (push "ixpremid.FIX_GND1" ixpremid.l2) (push "ixpremid.FIX_GND1" dec_pins)
				(push "FIX_GND2" ixpremid.l1) (push "ixpremid.FIX_GND2" ixpremid.l2) (push "ixpremid.FIX_GND2" dec_pins)))
		   (2 (progn
				(push A0_ ixpremid.l1) (push "AD[7]" ixpremid.l2)
				(push A1_ ixpremid.l1) (push "AD[8]" ixpremid.l2)
				(push "FIX_GND2" ixpremid.l1) (push "ixpremid.FIX_GND2" ixpremid.l2) (push "ixpremid.FIX_GND2" dec_pins)))
		   (3 (progn
				(push A0_ ixpremid.l1) (push "AD[7]" ixpremid.l2)
				(push A1_ ixpremid.l1) (push "AD[8]" ixpremid.l2)
				(push A2_ ixpremid.l1) (push "AD[9]" ixpremid.l2))))
		  (case *Xaddrshigh*
		   (0 (progn
				(push "FIX_GND0" ixprehigh.l1) (push "ixprehigh.FIX_GND0" ixprehigh.l2) (push "ixprehigh.FIX_GND0" dec_pins)
				(push "FIX_GND1" ixprehigh.l1) (push "ixprehigh.FIX_GND1" ixprehigh.l2) (push "ixprehigh.FIX_GND1" dec_pins)
				(push "FIX_GND2" ixprehigh.l1) (push "ixprehigh.FIX_GND2" ixprehigh.l2) (push "ixprehigh.FIX_GND2" dec_pins)))
		   (1 (progn
				(push A0_ ixprehigh.l1) (push "AD[10]" ixprehigh.l2)
				(push "FIX_GND1" ixprehigh.l1) (push "ixprehigh.FIX_GND1" ixprehigh.l2) (push "ixprehigh.FIX_GND1" dec_pins)
				(push "FIX_GND2" ixprehigh.l1) (push "ixprehigh.FIX_GND2" ixprehigh.l2) (push "ixprehigh.FIX_GND2" dec_pins)))
		   (2 (progn
				(push A0_ ixprehigh.l1) (push "AD[10]" ixprehigh.l2)
				(push A1_ ixprehigh.l1) (push "AD[11]" ixprehigh.l2)
				(push "FIX_GND2" ixprehigh.l1) (push "ixprehigh.FIX_GND2" ixprehigh.l2) (push "ixprehigh.FIX_GND2" dec_pins)))
		   (3 (progn
				(push A0_ ixprehigh.l1) (push "AD[10]" ixprehigh.l2)
				(push A1_ ixprehigh.l1) (push "AD[11]" ixprehigh.l2)
				(push A2_ ixprehigh.l1) (push "AD[12]" ixprehigh.l2)))
		   (4 (progn
				(push A0_ ixprehigh.l1) (push "AD[10]" ixprehigh.l2)
				(push A1_ ixprehigh.l1) (push "AD[11]" ixprehigh.l2)
				(push A2_ ixprehigh.l1) (push "AD[12]" ixprehigh.l2))))))
	(32 (progn
		  (push AD0_ iypre.l1) (push "AD[0]" iypre.l2)
		  (push AD1_ iypre.l1) (push "AD[1]" iypre.l2)
		  (push AD2_ iypre.l1) (push "AD[2]" iypre.l2)
		  (push AD3_ iypre.l1) (push "AD[3]" iypre.l2)
		  (push AD4_ iypre.l1) (push "AD[4]" iypre.l2)
		  (push A0_ ixprelow.l1) (push "AD[5]" ixprelow.l2)
		  (push A1_ ixprelow.l1) (push "AD[6]" ixprelow.l2)
		  (push A2_ ixprelow.l1) (push "AD[7]" ixprelow.l2)
		  (case *Xaddrsmid*
			(0 (progn
				(push "FIX_GND0" ixpremid.l1) (push "ixpremid.FIX_GND0" ixpremid.l2) (push "ixpremid.FIX_GND0" dec_pins)
				(push "FIX_GND1" ixpremid.l1) (push "ixpremid.FIX_GND1" ixpremid.l2) (push "ixpremid.FIX_GND1" dec_pins)
				(push "FIX_GND2" ixpremid.l1) (push "ixpremid.FIX_GND2" ixpremid.l2) (push "ixpremid.FIX_GND2" dec_pins)))
		   (1 (progn
				(push A0_ ixpremid.l1) (push "AD[8]" ixpremid.l2)
				(push "FIX_GND1" ixpremid.l1) (push "ixpremid.FIX_GND1" ixpremid.l2) (push "ixpremid.FIX_GND1" dec_pins)
				(push "FIX_GND2" ixpremid.l1) (push "ixpremid.FIX_GND2" ixpremid.l2) (push "ixpremid.FIX_GND2" dec_pins)))
		   (2 (progn
				(push A0_ ixpremid.l1) (push "AD[8]" ixpremid.l2)
				(push A1_ ixpremid.l1) (push "AD[9]" ixpremid.l2)
				(push "FIX_GND2" ixpremid.l1) (push "ixpremid.FIX_GND2" ixpremid.l2) (push "ixpremid.FIX_GND2" dec_pins)))
		   (3 (progn
				(push A0_ ixpremid.l1) (push "AD[8]" ixpremid.l2)
				(push A1_ ixpremid.l1) (push "AD[9]" ixpremid.l2)
				(push A2_ ixpremid.l1) (push "AD[10]" ixpremid.l2))))
		  (case *Xaddrshigh*
		   (0 (progn
				(push "FIX_GND0" ixprehigh.l1) (push "ixprehigh.FIX_GND0" ixprehigh.l2) (push "ixprehigh.FIX_GND0" dec_pins)
				(push "FIX_GND1" ixprehigh.l1) (push "ixprehigh.FIX_GND1" ixprehigh.l2) (push "ixprehigh.FIX_GND1" dec_pins)
				(push "FIX_GND2" ixprehigh.l1) (push "ixprehigh.FIX_GND2" ixprehigh.l2) (push "ixprehigh.FIX_GND2" dec_pins)))
		   (1 (progn
				(push A0_ ixprehigh.l1) (push "AD[11]" ixprehigh.l2)
				(push "FIX_GND1" ixprehigh.l1) (push "ixprehigh.FIX_GND1" ixprehigh.l2) (push "ixprehigh.FIX_GND1" dec_pins)
				(push "FIX_GND2" ixprehigh.l1) (push "ixprehigh.FIX_GND2" ixprehigh.l2) (push "ixprehigh.FIX_GND2" dec_pins)))
		   (2 (progn
				(push A0_ ixprehigh.l1) (push "AD[11]" ixprehigh.l2)
				(push A1_ ixprehigh.l1) (push "AD[12]" ixprehigh.l2)
				(push "FIX_GND2" ixprehigh.l1) (push "ixprehigh.FIX_GND2" ixprehigh.l2) (push "ixprehigh.FIX_GND2" dec_pins)))
		   (3 (progn
				(push A0_ ixprehigh.l1) (push "AD[11]" ixprehigh.l2)
				(push A1_ ixprehigh.l1) (push "AD[12]" ixprehigh.l2)
				(push A2_ ixprehigh.l1) (push "AD[13]" ixprehigh.l2)))
		   (4 (progn
				(push A0_ ixprehigh.l1) (push "AD[11]" ixprehigh.l2)
				(push A1_ ixprehigh.l1) (push "AD[12]" ixprehigh.l2)
				(push A2_ ixprehigh.l1) (push "AD[13]" ixprehigh.l2)))))))
  (setf ictl (inst (vector *itfctlcell* #x0000 0.0d0 p0 ictl.l1 ictl.l2)))
  (setf iypre (inst-left-down ictl (vector *ypredeccell* #x0000 0.0d0 p0 iypre.l1 iypre.l2)))
  (if (< col-mux 32)
	(progn
	  (setf ixyprecon (inst-left-down iypre (vector *xdec_ydec_con* #x0000 0.0d0 p0 ixyprecon.l1 ixyprecon.l2)))
	  (setf ixprelow (inst-left-down ixyprecon (vector *Xpredeclow* #x0000 0.0d0 p0 ixprelow.l1 ixprelow.l2))))
	(setf ixprelow (inst-left-down iypre (vector *Xpredeclow* #x0000 0.0d0 p0 ixprelow.l1 ixprelow.l2))))
  (setf ixpremid (inst-left-down ixprelow (vector *Xpredecmid* #x0000 0.0d0 p0 ixpremid.l1 ixpremid.l2)))
  (setf ixprehigh (inst-left-down ixpremid (vector *Xpredechigh* #x0000 0.0d0 p0 ixprehigh.l1 ixprehigh.l2)))
  (setf icoldec (inst-left-down ixprehigh (vector *col_dec_con* #x0000 0.0d0 p0 (list GND_ VDD_) (list "GND" "VDD"))))
  (setf iwl_tie (inst-up-left icoldec (vector *wl_tie* #x0000 0.0d0 p0 (list GND_ VDD_) (list "GND" "VDD"))))
  (push (cell-3 "xypredec_itfctl" (if (< col-mux 32)
									(list ictl iypre ixyprecon ixprelow ixpremid ixprehigh icoldec iwl_tie)
									(list ictl iypre ixprelow ixpremid ixprehigh icoldec iwl_tie))) *srcunits*))

(dolist (pin dec_pins)
  (push (overlay-1 "DECODE_VIA23" #x0000 0.0d0 (get-cell *srcunits* "xypredec_itfctl") "C0" pin) (cdddr (get-cell *srcunits* "xypredec_itfctl"))))

;;;;;;;;dmy_colmuxleft
(print "dmy_colmuxleft")
(let ((i1) (p0 (vector 0 0))
		   (GND_ (concatenate 'string "GND_" hpinlayer))
		   (VDD_ (concatenate 'string "VDD_" hpinlayer)))
  (if (evenp *number-of-cols-l*)
	(setf i1 (vector *dmy_colmux_even* #x0000 0.0d0 p0 (list GND_ VDD_) (list "GND" "VDD")))
	(setf i1 (vector *dmy_colmux_odd* #x0000 0.0d0 p0 (list GND_ VDD_) (list "GND" "VDD"))))
  (push (cell-3 "dmy_colmuxleft" (list (inst i1))) *srcunits*))

;;;;;;;;dmy_colmuxright
(print "dmy_colmuxright")
(let ((i1) (p0 (vector 0 0))
		   (GND_ (concatenate 'string "GND_" hpinlayer))
		   (VDD_ (concatenate 'string "VDD_" hpinlayer)))
  (if (evenp *number-of-cols-r*)
	(setf i1 (vector *dmy_colmux_even* #x4000 0.0d0 p0 (list GND_ VDD_) (list "GND" "VDD")))
	(setf i1 (vector *dmy_colmux_odd* #x4000 0.0d0 p0 (list GND_ VDD_) (list "GND" "VDD"))))
  (push (cell-3 "dmy_colmuxright" (list (inst i1))) *srcunits*))

;;;;;;;;dio_ymux_left
(print "dio_ymux_left")
(let ((i1) (p0 (vector 0 0))
		   (GND_ (concatenate 'string "GND_" hpinlayer))
		   (VDD_ (concatenate 'string "VDD_" hpinlayer))
		   (insts (list)))
  (dotimes (dimx (div1 *number-of-cols-l* 2))
	(setf i1 (vector *ymuxsa2* #x0000 0.0d0 p0
					 (list "VDD" "VDD_m1" "VDD_m2" "VDD_m3" "VDD_m4"
						   "GND" "GND_m1" "GND_m3" "GND_m3" "GND_m4"
						   "DO[0]_m1"
						   "DO[0]_m2"
						   "DO[0]_m3"
						   "DO[0]_m4"
						   "DO[1]_m1"
						   "DO[1]_m2"
						   "DO[1]_m3"
						   "DO[1]_m4"
						   "YPA[0]" "YPA[1]" "YPA[2]" "YPA[3]" "YPA[4]" "YPA[5]" "YPA[6]" "YPA[7]"
						   "YPB[0]" "YPB[1]" "YPB[2]" "YPB[3]" "YPB[4]" "YPB[5]" "YPB[6]" "YPB[7]"
						   "GTP" "SAE")
					 (list "VDD" "VDD_m1" "VDD_m2" "VDD_m3" "VDD_m4"
						   "GND" "GND_m1" "GND_m3" "GND_m3" "GND_m4"
						   (concatenate 'string "DO[0]_m1" "_r" (itoa dimx))
						   (concatenate 'string "DO[0]_m2" "_r" (itoa dimx))
						   (concatenate 'string "DO[0]_m3" "_r" (itoa dimx))
						   (concatenate 'string "DO[0]_m4" "_r" (itoa dimx))
						   (concatenate 'string "DO[1]_m1" "_r" (itoa dimx))
						   (concatenate 'string "DO[1]_m2" "_r" (itoa dimx))
						   (concatenate 'string "DO[1]_m3" "_r" (itoa dimx))
						   (concatenate 'string "DO[1]_m4" "_r" (itoa dimx))
						   "YPA[0]" "YPA[1]" "YPA[2]" "YPA[3]" "YPA[4]" "YPA[5]" "YPA[6]" "YPA[7]"
						   "YPB[0]" "YPB[1]" "YPB[2]" "YPB[3]" "YPB[4]" "YPB[5]" "YPB[6]" "YPB[7]"
						   "GTP" "SAE")))
	(push (inst i1) insts)gds-out
	(setf p0 (vector+ p0 (vector (vinst-width i1) 0))))
  (push (cell-3 "dio_ymux_left" insts) *srcunits*))

;;;;;;;;dio_ymuxcol_left
(print "dio_ymuxcol_left")
(if (evenp *number-of-cols-l*)
  (let ((s0) (s1) (p0 (vector 0 0))
			 (GND_ (concatenate 'string "GND_" vpinlayer))
			 (VDD_ (concatenate 'string "VDD_" vpinlayer))
			 (DO[0]_ (concatenate 'string "DO[0]_" vpinlayer))
			 (DO[1]_ (concatenate 'string "DO[1]_" vpinlayer))
			 (s0.sl (list)) (s0.tl (list))
			 (index1))
	(setf s0.sl (list GND_ VDD_))
	(setf s0.tl (list "GND" "VDD"))
	(dotimes (dimx *outputs-left*)
	  (setf index1 (div1 dimx 2))
	  (if (evenp dimx)
		(push (concatenate 'string DO[0]_ "_r" (itoa index1)) s0.sl)
		(push (concatenate 'string DO[1]_ "_r" (itoa index1)) s0.sl))
	  (push (concatenate 'string "DO[" (itoa dimx) "]") s0.tl))
	(setf s0 (inst (vector "dio_ymux_left" #x0000 0.0d0 p0 s0.sl s0.tl)))
	(setf s1 (inst-left-down s0 (vector "dmy_colmuxleft" #x0000 0.0d0 p0 (list "GND" "VDD") (list "GND" "VDD"))))
	(push (cell-3 "dio_ymuxcol_left" (list s0 s1)) *srcunits*))
  (if (= (div1 *number-of-cols-l* 2) 0)
	(let ((i1) (i2) (p0 (vector 0 0))
			   (GND_ (concatenate 'string "GND_" vpinlayer))
			   (VDD_ (concatenate 'string "VDD_" vpinlayer))
			   (DO_ (concatenate 'string "DO_" vpinlayer))
			   (DO[0]_ (concatenate 'string "DO[0]_" vpinlayer))
			   (DO[1]_ (concatenate 'string "DO[1]_" vpinlayer)))
	  (setf i1 (inst (vector *ymuxsa1left* #x0000 0.0d0 p0
							 (list GND_ VDD_ DO_)
							 (list "GND" "VDD" "DO[0]"))))
	  (setf i2 (inst-left-down i1 (vector "dmy_colmuxleft" #x0000 0.0d0 p0 (list "GND" "VDD") (list "GND" "VDD"))))
	  (push (cell-3 "dio_ymuxcol_left" (list i1 i2)) *srcunits*))
	(let ((s0) (s1) (s2) (p0 (vector 0 0))
			   (GND_ (concatenate 'string "GND_" vpinlayer))
			   (VDD_ (concatenate 'string "VDD_" vpinlayer))
			   (DO_ (concatenate 'string "DO_" vpinlayer))
			   (DO[0]_ (concatenate 'string "DO[0]_" vpinlayer))
			   (DO[1]_ (concatenate 'string "DO[1]_" vpinlayer))
			   (s0.sl (list)) (s0.tl (list))
			   (index1) (cal))
	  (setf s0.sl (list GND_ VDD_))
	  (setf s0.tl (list "GND" "VDD"))
	  (dotimes (dimx (1- *outputs-left*))
		(setf index1 (div1 dimx 2))
		(setf cal (+ dimx 1))
		(if (evenp dimx)
		  (push (concatenate 'string DO[0]_ "_r" (itoa index1)) s0.sl)
		  (push (concatenate 'string DO[1]_ "_r" (itoa index1)) s0.sl))
		(push (concatenate 'string "DO[" (itoa cal) "]") s0.tl))
	  (setf s0 (inst (vector "dio_ymux_left" #x0000 0.0d0 p0 s0.sl s0.tl)))
	  (setf s1 (inst-left-down s0 (vector *ymuxsa1left* #x0000 0.0d0 p0
										  (list GND_ VDD_ DO_)
										  (list "GND" "VDD" "DO[0]"))))
	  (setf s2 (inst-left-down s1 (vector "dmy_colmuxleft" #x0000 0.0d0 p0 (list "GND" "VDD") (list "GND" "VDD"))))
	  (push (cell-3 "dio_ymuxcol_left" (list s0 s1 s2)) *srcunits*))))

;;;;;;;;dio_ymux_right
(print "dio_ymux_right")
(let ((i1) (p0 (vector 0 0))
		   (GND_ (concatenate 'string "GND_" vpinlayer))
		   (VDD_ (concatenate 'string "VDD_" vpinlayer))
		   (insts (list)))
  (dotimes (dimx (div1 *number-of-cols-r* 2))
	(setf i1 (vector *ymuxsa2* #x0000 0.0d0 p0
					 (list "VDD" "VDD_m1" "VDD_m2" "VDD_m3" "VDD_m4"
						   "GND" "GND_m1" "GND_m3" "GND_m3" "GND_m4"
						   "DO[0]_m1" "DO[0]_m2" "DO[0]_m3" "DO[0]_m4"
						   "DO[1]_m1" "DO[1]_m2" "DO[1]_m3" "DO[1]_m4"
						   "YPA[0]" "YPA[1]" "YPA[2]" "YPA[3]" "YPA[4]" "YPA[5]" "YPA[6]" "YPA[7]"
						   "YPB[0]" "YPB[1]" "YPB[2]" "YPB[3]" "YPB[4]" "YPB[5]" "YPB[6]" "YPB[7]"
						   "GTP" "SAE")
					 (list "VDD" "VDD_m1" "VDD_m2" "VDD_m3" "VDD_m4"
						   "GND" "GND_m1" "GND_m3" "GND_m3" "GND_m4"
						   (concatenate 'string "DO[0]_m1" "_r" (itoa dimx))
						   (concatenate 'string "DO[0]_m2" "_r" (itoa dimx))
						   (concatenate 'string "DO[0]_m3" "_r" (itoa dimx))
						   (concatenate 'string "DO[0]_m4" "_r" (itoa dimx))
						   (concatenate 'string "DO[1]_m1" "_r" (itoa dimx))
						   (concatenate 'string "DO[1]_m2" "_r" (itoa dimx))
						   (concatenate 'string "DO[1]_m3" "_r" (itoa dimx))
						   (concatenate 'string "DO[1]_m4" "_r" (itoa dimx))
						   "YPA[0]" "YPA[1]" "YPA[2]" "YPA[3]" "YPA[4]" "YPA[5]" "YPA[6]" "YPA[7]"
						   "YPB[0]" "YPB[1]" "YPB[2]" "YPB[3]" "YPB[4]" "YPB[5]" "YPB[6]" "YPB[7]"
						   "GTP" "SAE")))
	(push (inst i1) insts)
	(setf p0 (vector+ p0 (vector (vinst-width i1) 0))))
  (push (cell-3 "dio_ymux_right" insts) *srcunits*))

;;;;;;;;dio_ymuxcol_right
(print "dio_ymuxcol_right")
(if (evenp *number-of-cols-r*)
  (let ((s0) (s1) (p0 (vector 0 0))
			 (GND_ (concatenate 'string "GND_" vpinlayer))
			 (VDD_ (concatenate 'string "VDD_" vpinlayer))
			 (DO[0]_ (concatenate 'string "DO[0]_" vpinlayer))
			 (DO[1]_ (concatenate 'string "DO[1]_" vpinlayer))
			 (s0.sl (list)) (s0.tl (list))
			 (index1) (cal))
	(setf s0.sl (list GND_ VDD_))
	(setf s0.tl (list "GND" "VDD"))
	(dotimes (dimx *outputs-right*)
	  (setf index1 (div1 dimx 2))
	  (setf cal (+ dimx *outputs-left*))
	  (if (evenp dimx)
		(push (concatenate 'string DO[0]_ "_r" (itoa index1)) s0.sl)
		(push (concatenate 'string DO[1]_ "_r" (itoa index1)) s0.sl))
	  (push (concatenate 'string "DO[" (itoa cal) "]") s0.tl))
	(setf s0 (inst (vector "dio_ymux_right" #x0000 0.0d0 p0 s0.sl s0.tl)))
	(setf s1 (inst-right-down s0 (vector "dmy_colmuxright" #x0000 0.0d0 p0 (list "GND" "VDD") (list "GND" "VDD"))))
	(push (cell-3 "dio_ymuxcol_right" (list s0 s1)) *srcunits*))
  (if (= (div1 *number-of-cols-r* 2) 0)
	(let ((i1) (i2) (p0 (vector 0 0))
			   (GND_ (concatenate 'string "GND_" vpinlayer))
			   (VDD_ (concatenate 'string "VDD_" vpinlayer))
			   (DO_ (concatenate 'string "DO_" vpinlayer)))
	  (setf i1 (inst (vector *ymuxsa1right* #x0000 0.0d0 p0
							 (list GND_ VDD_ DO_)
							 (list "GND" "VDD" "DO[0]"))))
	  (setf i2 (inst-right-down i1 (vector "dmy_colmuxright" #x0000 0.0d0 p0 (list "GND" "VDD") (list "GND" "VDD"))))
	  (push (cell-3 "dio_ymuxcol_right" (list i1 i2)) *srcunits*))
	(let ((s0) (s1) (s2) (p0 (vector 0 0))
			   (GND_ (concatenate 'string "GND_" vpinlayer))
			   (VDD_ (concatenate 'string "VDD_" vpinlayer))
			   (DO_ (concatenate 'string "DO_" vpinlayer))
			   (DO[0]_ (concatenate 'string "DO[0]_" vpinlayer))
			   (DO[1]_ (concatenate 'string "DO[1]_" vpinlayer))
			   (s0.sl (list)) (s0.tl (list))
			   (index1) (cal))
	  (setf s0.sl (list GND_ VDD_))
	  (setf s0.tl (list "GND" "VDD"))
	  (dotimes (dimx (1- *outputs-right*))
		(setf index1 (div1 dimx 2))
		(setf cal (+ dimx *outputs-left*))
		(if (evenp dimx)
		  (push (concatenate 'string DO[0]_ "_r" (itoa index1)) s0.sl)
		  (push (concatenate 'string DO[1]_ "_r" (itoa index1)) s0.sl))
		(push (concatenate 'string "DO[" (itoa cal) "]") s0.tl))
	  (setf s0 (inst (vector "dio_ymux_right" #x0000 0.0d0 p0 s0.sl s0.tl)))
	  (setf s1 (inst-right-down s0 (vector *ymuxsa1right* #x0000 0.0d0 p0
										  (list GND_ VDD_)
										  (list "GND" "VDD"))))
	  (setf s2 (inst-right-down s1 (vector "dmy_colmuxright" #x0000 0.0d0 p0 (list "GND" "VDD") (list "GND" "VDD"))))
	  (push (cell-3 "dio_ymuxcol_right" (list s0 s1 s2)) *srcunits*))))

;;;;;;;;macro
(print "macro")
(let ((i0) (i1) (i2) (i3) (i4) (i5) (i6) (p0 (vector 0 0)))
  (setf i0 (inst (vector "dio_ymuxcol_left" #x0000 0.0d0 p0
						 (list "GND" "VDD" "DO[0]" "DO[1]" "DO[2]" "DO[3]" "DO[4]" "DO[5]" "DO[6]" "DO[7]" "DO[8]" "DO[9]"
							   "DO[10]" "DO[11]" "DO[12]" "DO[13]" "DO[14]" "DO[15]" "DO[16]" "DO[17]" "DO[18]" "DO[19]"
							   "DO[20]" "DO[21]" "DO[22]" "DO[23]" "DO[24]" "DO[25]" "DO[26]" "DO[27]" "DO[28]" "DO[29]")
						 (list "GND" "VDD" "DO[0]" "DO[1]" "DO[2]" "DO[3]" "DO[4]" "DO[5]" "DO[6]" "DO[7]" "DO[8]" "DO[9]"
							   "DO[10]" "DO[11]" "DO[12]" "DO[13]" "DO[14]" "DO[15]" "DO[16]" "DO[17]" "DO[18]" "DO[19]"
							   "DO[20]" "DO[21]" "DO[22]" "DO[23]" "DO[24]" "DO[25]" "DO[26]" "DO[27]" "DO[28]" "DO[29]"))))
  (setf i1 (inst-up-right i0 (vector "core_left" #x0000 0.0d0 p0
						 (list "GND")
						 (list "GND"))))
  (setf i2 (inst-right-down i0 (vector "xypredec_itfctl" #x0000 0.0d0 p0
						 (list "GND" "VDD" "CEB" "CLK"
							   "AD[0]" "AD[1]" "AD[2]" "AD[3]" "AD[4]" "AD[5]" "AD[6]"
							   "AD[7]" "AD[8]" "AD[9]" "AD[10]" "AD[11]" "AD[12]""AD[13]")
						 (list "GND" "VDD" "CEB" "CLK"
							   "AD[0]" "AD[1]" "AD[2]" "AD[3]" "AD[4]" "AD[5]" "AD[6]"
							   "AD[7]" "AD[8]" "AD[9]" "AD[10]" "AD[11]" "AD[12]""AD[13]"))))
  (setf i3 (inst-up-left i2 (vector "wldrvall" #x0000 0.0d0 p0
						 (list "GND" "VDD")
						 (list "GND" "VDD"))))
  (setf i4 (inst-right-down i2 (vector "dio_ymuxcol_right" #x0000 0.0d0 p0
						 (list "GND" "VDD" "DO[0]" "DO[1]" "DO[2]" "DO[3]" "DO[4]" "DO[5]" "DO[6]" "DO[7]" "DO[8]" "DO[9]"
							   "DO[10]" "DO[11]" "DO[12]" "DO[13]" "DO[14]" "DO[15]" "DO[16]" "DO[17]" "DO[18]" "DO[19]"
							   "DO[20]" "DO[21]" "DO[22]" "DO[23]" "DO[24]" "DO[25]" "DO[26]" "DO[27]" "DO[28]" "DO[29]")
						 (list "GND" "VDD" "DO[0]" "DO[1]" "DO[2]" "DO[3]" "DO[4]" "DO[5]" "DO[6]" "DO[7]" "DO[8]" "DO[9]"
							   "DO[10]" "DO[11]" "DO[12]" "DO[13]" "DO[14]" "DO[15]" "DO[16]" "DO[17]" "DO[18]" "DO[19]"
							   "DO[20]" "DO[21]" "DO[22]" "DO[23]" "DO[24]" "DO[25]" "DO[26]" "DO[27]" "DO[28]" "DO[29]"))))
  (setf i5 (inst-up-left i4 (vector "core_right" #x0000 0.0d0 p0
						 (list "GND")
						 (list "GND"))))
  (setf i6 (inst-left-down i5 (vector "tkbl_col" #x0000 0.0d0 p0
						 (list "GND")
						 (list "GND"))))
  (push (cell-3 "macro" (list i0 i1 i2 i3 i4 i5 i6)) *srcunits*))

;;;;;;;;withdraw top
(format t "
withdraw top : ~a
" top-name)
(defvar outlib (lib outlib-name (withdraw top-name)))
;;;;;;;;dump gds
(format t "
dump gds ~a
" gds-out)
(sgk-wt-gds gds-out outlib)
))
)
