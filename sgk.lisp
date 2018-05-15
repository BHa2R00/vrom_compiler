(defvar *RT_BGNLIB*			#x0102)
(defvar *RT_HEADER*			#x0002)
(defvar *RT_LIBNAME*		#x0206)
(defvar *RT_UNITS*			#x0305)
(defvar *RT_ENDLIB*			#x0400)
(defvar *RT_BGNSTR*			#x0502)
(defvar *RT_STRNAME*		#x0606)
(defvar *RT_ENDSTR*			#x0700)
(defvar *RT_BOUNDARY*		#x0800)
(defvar *RT_PATH*			#x0900)
(defvar *RT_SREF*			#x0A00)
(defvar *RT_AREF*			#x0B00)
(defvar *RT_TEXT*			#x0C00)
(defvar *RT_LAYER*			#x0D02)
(defvar *RT_DATATYPE*		#x0E02)
(defvar *RT_WIDTH*			#x0F03)
(defvar *RT_XY*				#x1003)
(defvar *RT_ENDEL*			#x1100)
(defvar	*RT_SNAME*			#x1206)
(defvar	*RT_COLROW*			#x1302)
(defvar	*RT_TEXTNODE*		#x1400)
(defvar	*RT_NODE*			#x1500)
(defvar	*RT_TEXTTYPE*		#x1602)
(defvar	*RT_PRESENTATION*	#x1701)
(defvar	*RT_SPACING*		#x1800)
(defvar *RT_STRING* 		#x1906)
(defvar *RT_STRANS*			#x1A01)
(defvar *RT_MAG*			#x1B05)
(defvar *RT_ANGLE*			#x1C05)
(defvar *RT_UINTEGER*		#x1D00)
(defvar *RT_USTRING*		#x1E00)
(defvar *RT_REFLIB*			#x1F06)
(defvar *RT_FONTS*			#x2006)
(defvar *RT_PATHTYPE*		#x2102)
(defvar *RT_GENERATIONS*	#x2202)
(defvar *RT_ATTRTABLE*		#x2306)
(defvar *RT_STYPTABLE*		#x2406)
(defvar *RT_STRTYPE*		#x2502)
(defvar *RT_ELFLAGS*		#x2601)
(defvar *RT_ELKEY*			#x2703)
(defvar *RT_LINKTYPE*		#x2800)
(defvar *RT_LINKKEYS*		#x2900)
(defvar *RT_NODETYPE*		#x2A02)
(defvar *RT_PROPATTR*		#x2B02)
(defvar *RT_PROPVALUE*		#x2C06)
(defvar *RT_BOX*			#x2D00)
(defvar *RT_BOXTYPE*		#x2E02)
(defvar *RT_PLEX*			#x2F03)
(defvar *RT_BGNEXTN*		#x3003)
(defvar *RT_ENDEXTN*		#x3103)
(defvar *RT_TAPENUM*		#x3202)
(defvar *RT_TAPECODE*		#x3302)
(defvar *RT_STRCLASS*		#x3401)
(defvar *RT_RESERVED*		#x3503)
(defvar *RT_FORMAT*			#x3602)
(defvar *RT_MASK*			#x3706)
(defvar *RT_ENDMASK*		#x3800)
(defvar *RT_LIBDIRSIZE*		#x3902)
(defvar *RT_SRFNAME*		#x3A06)
(defvar	*RT_LIBSECUR*		#x3B02)
(defvar	*DT_NODATA*			#x00)
(defvar	*DT_BITARRAY*		#x01)
(defvar	*DT_SIGNED16*		#x02)
(defvar	*DT_SIGNED32*		#x03)
(defvar	*DT_REAL32*			#x04)
(defvar	*DT_REAL64*			#x05)
(defvar	*DT_STRING*			#x06)
(defvar	*PRES_MASK_H*		#x0003)
(defvar	*PRES_MASK_V*		#x000C)
(defvar	*PRES_MASK_FONT*	#x0030)
(defvar	*PRES_TOP*			#x0)
(defvar	*PRES_MIDDLE*		#x1)
(defvar	*PRES_BOTTOM*		#x2)
(defvar	*PRES_LEFT*			#x0)
(defvar	*PRES_CENTER*		#x1)
(defvar	*PRES_RIGHT*		#x2)
(defvar	*STRANS_ABSMAG*		#x0002)
(defvar	*STRANS_ABSANGLE*	#x0004)
(defvar	*STRANS_REFLECTION*	#x8000)
(defvar	*PATHTYPE_FLUSH*	#x0)
(defvar	*PATHTYPE_ROUND*	#x1)
(defvar	*PATHTYPE_SQUARE*	#x2)
(defvar	*ELFLAG_TEMPLATE*	#x0001)
(defvar	*ELFLAG_EXTERNAL*	#x0002)
(defvar	*PLEX_HEAD*			#x01000000)
(defvar	*PLEX_VALID_BITS*	#x00FFFFFF)

(defun rd-rt-libname (str-in oldlist)
  (let ((bt (rd-u2 str-in)))
	(if (= bt *RT_LIBNAME*)
	  (progn
		(setq oldlist (push (reverse (list (rd-dt-string str-in "") "RT_LIBNAME")) oldlist))
		(sgk-f003 str-in oldlist))
	  (progn
		(rd-rt-libname str-in oldlist)))))

(defun rd-dt-bitarray (str-in len num)
  (let ((lst '()))
	(dotimes (n num)
	  (cond
		((= len 2) (push (rd-u2 str-in) lst))
		((= len 4) (push (rd-u4 str-in) lst))))
	(make-array (length lst) :initial-contents (reverse lst))
	))

(defun d2r642 (d)
  (cond
	((= d 0.02d0) #x3F51EB851EB851EC)
	((= d 0.05d0) #x3FCCCCCCCCCCCCD0)
	((= d 0.1d0) #x401999999999999A)
	((= d 0.12d0) #x401EB851EB851EB8)
	((= d 0.15d0) #x4026666666666666)
	((= d 0.16d0) #x4028F5C28F5C28F6)
	((= d 0.2d0) #x4033333333333334)
	((= d 0.3d0) #x404CCCCCCCCCCCCC)
	((= d 0.5d0) #x4080000000000000)
	((= d -0.02d0) #xBF51EB851EB851EC)
	((= d -0.05d0) #xBFCCCCCCCCCCCCD0)
	((= d -0.1d0) #xC01999999999999A)
	((= d -0.12d0) #xC01EB851EB851EB8)
	((= d -0.15d0) #xC026666666666666)
	((= d -0.16d0) #xC028F5C28F5C28F6)
	((= d -0.2d0) #xC033333333333334)
	((= d -0.3d0) #xC04CCCCCCCCCCCCC)
	((= d -0.5d0) #xC080000000000000)))

(defun r642d2 (r)
  (let ((rh (ldb (byte 63 60) r))
		(rb (ldb (byte 60 0) r)))
	(if (or (= rh #xb) (= rh #xc))
	  (* -1 (r642d3 rb))
	  (r642d3 rb))))
(defun r642d3 (rb)
  (cond
	((= rb #xF51EB851EB851EC) 0.02d0)
	((= rb #xFCCCCCCCCCCCCD0) 0.05d0)
	((= rb #x01999999999999A) 0.1d0)
	((= rb #x01EB851EB851EB8) 0.12d0)
	((= rb #x026666666666666) 0.15d0)
	((= rb #x028F5C28F5C28F6) 0.16d0)
	((= rb #x033333333333334) 0.2d0)
	((= rb #x04CCCCCCCCCCCCC) 0.3d0)
	((= rb #x080000000000000) 0.5d0)))

(defun >_< (a x b) (if (and (> x a) (<= x b)) t nil))

(defun d2r64 (d)
  (cond
	((> d 360.0d0) (d2r64 (- d 360.0d0)))
	((< d -360.0d0) (d2r64 (+ d 360.0d0)))
	((>_< -45.0d0 d 45.0d0) #x0000000000000000)
	((>_< 45.0d0 d 135.0d0) #x425A000000000000)
	((>_< 135.0d0 d 225.0d0) #x42B4000000000000)
	((>_< 225.0d0 d 315.0d0) #x4310E00000000000)
	((>_< 315.0d0 d 360.0d0) #x4316800000000000)
	((>_< -45.0d0 d -135.0d0) #xC25A000000000000)
	((>_< -135.0d0 d -225.0d0) #xC2B4000000000000)
	((>_< -225.0d0 d -315.0d0) #xC310E00000000000)
	((>_< -315.0d0 d -360.0d0) #xC316800000000000)))

(defun r642d (r)
  (let ((rh (ldb (byte 63 60) r))
		(rb (ldb (byte 60 0) r)))
	(if (= rh #xc)
	  (* -1 (r642d1 rb))
	  (r642d1 rb))))
(defun r642d1 (rb)
  (cond
	((>_< #x000000000000000 rb #x22D000000000000) 0.0d0)
	((>_< #x22D000000000000 rb #x287000000000000) 90.0d0)
	((>_< #x287000000000000 rb #x2E1000000000000) 180.0d0)
	((>_< #x2E1000000000000 rb #x313B00000000000) 270.0d0)
	((>_< #x313B00000000000 rb #x316800000000000) 360.0d0)))

(defun is-name? (asii)
  (let ((c (code-char asii)))
  (if 	(or
		(and (char>= c #\A) (char<= c #\Z))
		(and (char>= c #\a) (char<= c #\z))
		(and (char>= c #\0) (char<= c #\9))
		(char= c #\_)
		(char= c #\!)
		(char= c #\$)
;		(char= c #\()
;		(char= c #\))
		(char= c #\[)
		(char= c #\])
		(char= c #\<)
		(char= c #\>)
		)
	t
	nil)))

(defun is-name-head? (asii)
  (let ((c (code-char asii)))
  (if 	(or
		(and (char>= c #\A) (char<= c #\Z))
		(and (char>= c #\a) (char<= c #\z))
		)
	t
	nil)))

(defun push-char-to-string (bt stri)
  (concatenate 'string stri (string (code-char bt))))

(defun rd-dt-string (str-in stri)
  (let ((bt (read-byte str-in)))
	(if (is-name? bt)
	  (progn
		(setq stri (push-char-to-string bt stri))
		(rd-dt-string str-in stri))
	  (string stri))))

(defun rd-dt-string41 (str-in)
  (let ((bt) (stri ""))
	(loop
	  (setf bt (rd-u2 str-in))
	  (if (is-name? (ldb (byte 8 8) bt))
		 (setq stri (push-char-to-string (ldb (byte 8 8) bt) stri))
		 nil)
	  (if (is-name? (ldb (byte 8 0) bt))
		 (setq stri (push-char-to-string (ldb (byte 8 0) bt) stri))
		 nil)
	  (when (= bt #x0004)
		(return (string stri))))))

(defun rd-dt-string4 (str-in)
  (let ((stri (rd-dt-string41 str-in)))
	(if (is-name-head? (char-code (char stri 0)))
	  stri
	  (subseq stri 1))
	))

(defun rd-dt-string61 (str-in)
  (let ((bt) (stri ""))
	(loop
	  (setf bt (rd-u2 str-in))
	  (if (is-name? (ldb (byte 8 8) bt))
		 (setq stri (push-char-to-string (ldb (byte 8 8) bt) stri))
		 nil)
	  (if (is-name? (ldb (byte 8 0) bt))
		 (setq stri (push-char-to-string (ldb (byte 8 0) bt) stri))
		 nil)
	  (when (= bt #x0006)
		(return (string stri))))))

(defun rd-dt-string6 (str-in)
  (let ((stri (rd-dt-string61 str-in)))
	(if (is-name-head? (char-code (char stri 0)))
	  stri
	  (subseq stri 1))))

(defun rd-dt-string3 (str-in stri)
  (let ((bt (rd-u2 str-in)))
	(if (or
		  (is-name? (ldb (byte 8 0) bt))
		  (is-name? (ldb (byte 8 8) bt))
		  )
	  (progn
		(if (is-name? (ldb (byte 8 8) bt))
		 (setq stri (push-char-to-string (ldb (byte 8 8) bt) stri))
		 nil)
		(if (is-name? (ldb (byte 8 0) bt))
		  (setq stri (push-char-to-string (ldb (byte 8 0) bt) stri))
		  nil)
		(rd-dt-string3 str-in stri))
	  (if (or
			(= bt #x0000)
			(= bt #x0004)
			(= bt #x0006)
			)
		(string stri)
		(rd-dt-string3 str-in stri)))))

(defun rd-u2 (str-in)
  (let ((u2 0) (eof 0))
	(setf (ldb (byte 8 8) u2) (read-byte str-in nil eof))
	(setf (ldb (byte 8 0) u2) (read-byte str-in nil eof))
	u2)
  )

(defun rd-u4 (str-in)
  (let ((u4 0))
	(setf (ldb (byte 16 16) u4) (rd-u2 str-in))
	(setf (ldb (byte 16 0) u4) (rd-u2 str-in))
	u4)
  )

(defun rd-u8 (str-in)
  (let ((u8 0))
	(setf (ldb (byte 32 32) u8) (rd-u4 str-in))
	(setf (ldb (byte 32 0) u8) (rd-u4 str-in))
	u8)
  )

(defun rd-u16 (str-in)
  (let ((u16 0))
	(setf (ldb (byte 64 64) u16) (rd-u8 str-in))
	(setf (ldb (byte 64 0) u16) (rd-u8 str-in))
	u16)
  )

(defun wt-u2 (str-out var)
  (write-byte (ldb (byte 8 8) var) str-out)
  (write-byte (ldb (byte 8 0) var) str-out)
  )

(defun wt-u4 (str-out var)
  (wt-u2 str-out (ldb (byte 16 16) var))
  (wt-u2 str-out (ldb (byte 16 0) var))
  )

(defun wt-u8 (str-out var)
  (wt-u4 str-out (ldb (byte 32 32) var))
  (wt-u4 str-out (ldb (byte 32 0) var))
  )

(defun wt-u16 (str-out var)
  (wt-u8 str-out (ldb (byte 64 64) var))
  (wt-u8 str-out (ldb (byte 64 0) var))
  )

(defun f-return-srctree (fi)
  (let ((srctree '()))
	(with-open-file (str-in fi :direction	:input
							:element-type	'(unsigned-byte 8))
	  (setq srctree (sgk-f003 str-in srctree))
	  (reverse (push "RT_ENDLIB" srctree)))))

(defun sgk-f003 (str-in oldlist)
  (let ((bt (rd-u2 str-in)) (newlist '()))
	(cond
	  ((= bt *RT_BGNLIB*) (progn
							(push (list "RT_BGNLIB" (rd-dt-bitarray str-in 2 12))
								  oldlist)
							(rd-rt-libname str-in oldlist)))
	  ((= bt *RT_HEADER*) (progn
							(push (list "RT_HEADER" (rd-u2 str-in)) oldlist)
							(sgk-f003 str-in oldlist)))
	  ((= bt *RT_UNITS*) (progn
						   (push "RT_UNITS" newlist)
						   (push 999999999999999d-10 newlist)
						   (push 999999999999998d-4 newlist)
						   (push (reverse (car (sgk-f004 str-in newlist))) oldlist)))
	  (t (sgk-f003 str-in oldlist)))))

(defun sgk-f001 (str-in oldlist)
  (let ((bt (read-byte str-in nil 'eof)))
	(if (eql bt 'eof)
	  (list oldlist)
	  (sgk-f001 str-in oldlist))))

(defun sgk-f004 (str-in oldlist)
  (let ((bt (rd-u2 str-in)) (newlist '()))
	(if (eql bt *RT_ENDLIB*)
	  (progn
		(sgk-f001 str-in oldlist))
	  (cond
		((= bt *RT_BGNSTR*) (progn
							  (push (reverse (sgk-f005 str-in)) oldlist)
							  (sgk-f004 str-in oldlist)))
		(t (sgk-f004 str-in oldlist))))))

(defvar strlist '())
(defvar elm '())

(defun sgk-f005 (str-in)
  (let ((bt) (rtl))
  (setq strlist (reverse (list 
				  (list "RT_BGNSTR" (rd-dt-bitarray str-in 2 12)
						)
				  (list "RT_STRNAME" (rd-dt-string4 str-in)
						))))
  (loop
	(setq bt (rd-u2 str-in))
	(setq elm '())
	(cond
	  ((= bt *RT_BOUNDARY*) (progn
							  (sgk-f006 str-in)
							  (push (reverse elm) strlist)))
	  ((= bt *RT_PATH*) (progn
						  (sgk-f016 str-in)
						  (push (reverse elm) strlist)))
	  ((= bt *RT_SREF*) (progn
						  (sgk-f026 str-in)
						  (push (reverse elm) strlist)))
	  ((= bt *RT_AREF*) (progn
						  (sgk-f036 str-in)
						  (push (reverse elm) strlist)))
	  ((= bt *RT_TEXT*) (progn
						  (sgk-f046 str-in)
						  (push (reverse elm) strlist)))
	  ((= bt *RT_NODE*) (progn
						  (sgk-f056 str-in)
						  (push (reverse elm) strlist)))
	  ((= bt *RT_BOX*) (progn
						 (sgk-f066 str-in)
						 (push (reverse elm) strlist)))
	  )
	(when (= bt *RT_ENDSTR*)
	  (return (push "RT_ENDSTR" strlist))))))

(defun sgk-f006 (str-in)
  (let ((bt 0) (xy) (rtl))
	(setq elm '("RT_BOUNDARY"))
	(loop
	  (setf rtl (- (logior bt) 4))
	  (setq bt (rd-u2 str-in))
	  (cond
		((= bt *RT_LAYER*) (progn
							 (push (list "RT_LAYER" (rd-u2 str-in)) elm)))
		((= bt *RT_DATATYPE*) (progn
								(push (list "RT_DATATYPE" (rd-u2 str-in)) elm)))
		((= bt *RT_XY*) (push (list "RT_XY"
									(rd-dt-bitarray str-in 4 (/ rtl 4))
									) elm)))
	  (when (= bt *RT_ENDEL*)
		(push "RT_ENDEL" elm)
		(return elm)))))

(defun sgk-f016 (str-in)
  (let ((bt 0) (xy) (rtl))
	(setq elm '("RT_PATH"))
	(loop
	  (setf rtl (- (logior bt) 4))
	  (setq bt (rd-u2 str-in))
	  (cond
		((= bt *RT_LAYER*) (progn
							 (push (list "RT_LAYER" (rd-u2 str-in)) elm)
							 ))
		((= bt *RT_DATATYPE*) (progn
								(push (list "RT_DATATYPE" (rd-u2 str-in)) elm)
								))
		((= bt *RT_PATHTYPE*) (progn
								(push (list "RT_PATHTYPE" (rd-u2 str-in)) elm)
								))
		((= bt *RT_WIDTH*) (progn
							 (push (list "RT_WIDTH" (logior (rd-u4 str-in))) elm)
							 ))
		((= bt *RT_XY*) (push (list "RT_XY"
									(rd-dt-bitarray str-in 4 (/ rtl 4))
									) elm))
		)
	  (when (= bt *RT_ENDEL*)
		(push "RT_ENDEL" elm)
		(return elm)))))

(defun sgk-f026 (str-in)
  (let ((bt 0) (xy) (rtl))
	(setq elm '( "RT_SREF"))
	(loop
	  (setf rtl (- (logior bt) 4))
	  (setq bt (rd-u2 str-in))
	  (cond
		((= bt *RT_SNAME*) (progn
							 (push (list "RT_SNAME" (rd-dt-string6 str-in)) elm)
							 ))
		((= bt *RT_STRANS*) (progn
							  (push (list "RT_STRANS" (rd-u2 str-in)) elm)
							  ))
		((= bt *RT_PATHTYPE*) (progn
								(push (list "RT_PATHTYPE" (rd-u2 str-in)) elm)
								))
		((= bt *RT_ANGLE*) (progn
							 (push (list "RT_ANGLE" (r642d (rd-u8 str-in))) elm)
							 ))
		((= bt *RT_XY*) (push (list "RT_XY"
									(rd-dt-bitarray str-in 4 (/ rtl 4))
									) elm))
		)
	  (when (= bt *RT_ENDEL*)
		(push "RT_ENDEL" elm)
		(return elm)))))

(defun sgk-f036 (str-in)
  (let ((bt 0) (rtl))
	(setq elm '("RT_AREF"))
	(loop
	  (setf rtl (- (logior bt) 4))
	  (setq bt (rd-u2 str-in))
	  (cond
		((= bt *RT_SNAME*) (progn
							 (push (list "RT_SNAME" (rd-dt-string6 str-in)) elm)
							 ))
		((= bt *RT_STRANS*) (progn
							  (push (list "RT_STRANS" (rd-u2 str-in)) elm)
							  ))
		((= bt *RT_ANGLE*) (progn
							 (push (list "RT_ANGLE" (r642d (rd-u8 str-in))) elm)
							 ))
		((= bt *RT_COLROW*) (progn
							  (push (list "RT_COLROW" (rd-dt-bitarray str-in 2 2)) elm)
							  ))
		((= bt *RT_XY*) (push (list "RT_XY"
									(rd-dt-bitarray str-in 4 (/ rtl 4))
									) elm))
		)
	  (when (= bt *RT_ENDEL*)
		(push "RT_ENDEL" elm)
		(return elm)))))

(defun sgk-f046 (str-in)
  (let ((bt 0) (rtl 0))
	(setq elm '("RT_TEXT"))
	(loop
	  (setf rtl (- (logior bt) 4))
	  (setq bt (rd-u2 str-in))
	  (cond
		((= bt *RT_LAYER*) (progn
							 (push (list "RT_LAYER" (rd-u2 str-in)) elm)
							 ))
		((= bt *RT_TEXTTYPE*) (progn
								(push (list "RT_TEXTTYPE" (rd-u2 str-in)) elm)
								))
		((= bt *RT_PRESENTATION*) (progn
									(push (list "RT_PRESENTATION" (rd-u2 str-in)) elm)
									))
		((= bt *RT_STRANS*) (progn
							  (push (list "RT_STRANS" (rd-u2 str-in)) elm)
							  ))
		((= bt *RT_MAG*) (progn
							 (push (list "RT_MAG" (r642d2 (rd-u8 str-in))) elm)
							 ))
		((= bt *RT_ANGLE*) (progn
							 (push (list "RT_ANGLE" (r642d (rd-u8 str-in))) elm)
							 ))
		((= bt *RT_XY*) (push (list "RT_XY"
									(rd-dt-bitarray str-in 4 (/ rtl 4))
									) elm))
		((= bt *RT_STRING*) (progn
							  (push (list "RT_STRING" (rd-dt-string4 str-in)) elm)
							  ))
	  )
	  (when (= bt *RT_ENDEL*)
		(push "RT_ENDEL" elm)
		(return elm)))))

(defun sgk-f056 (str-in)
  (let ((bt) (rtl))
	(setq elm '("RT_NODE"))
	(loop
	  (setq bt (rd-u2 str-in))
	  (when (= bt *RT_ENDEL*)
		(push "RT_ENDEL" elm)
		(push (rd-u2 str-in) elm)
		(return elm)))))

(defun sgk-f066 (str-in)
  (let ((bt) (rtl))
	(setq elm '("RT_BOX"))
	(loop
	  (setq bt (rd-u2 str-in))
	  (when (= bt *RT_ENDEL*)
		(push "RT_ENDEL" elm)
		(push (rd-u2 str-in) elm)
		(return elm)))))

(defun flatten-tree (tree)
  (cond
	((null tree) nil)
	((atom tree) (list tree))
	(t (mapcan #'flatten-tree tree))))

(defun wt-dt-string5 (str-out stri)
  (let ((u2 0) (len (length stri)))
	(dotimes (n (floor (float (/ len 2))))
	  (setf (ldb (byte 8 8) u2) (char-code (aref stri (* 2 n))))
	  (setf (ldb (byte 8 0) u2) (char-code (aref stri (1+ (* 2 n)))))
	  (wt-u2 str-out u2)
	  )
	(if (oddp len)
	  (progn
		(setf (ldb (byte 8 8) u2) (char-code (aref stri (1- len))))
		(setf (ldb (byte 8 0) u2) #x00)
		(wt-u2 str-out u2))
	  nil)
	))

(defun wt-dt-string6 (str-out stri)
  (wt-dt-string7 str-out stri)
  (if (oddp (length stri))
	(write-byte #x2e str-out)
	nil))

(defun wt-dt-string7 (str-out stri)
  (loop for n from 0 to (1- (length stri)) do
		(write-byte (char-code (aref stri n)) str-out)))

(defun wt-dt-bitarray (str-out ary sel)
  (loop for n from 0 to (1- (array-total-size ary)) do
		(cond
		  ((= 2 sel) (wt-u2 str-out (aref ary n)))
		  ((= 4 sel) (wt-u4 str-out (aref ary n)))
		  ((= 8 sel) (wt-u8 str-out (aref ary n)))
		  )))

(defun wt-dt-real64 (str-out d)
  (wt-u8 str-out (d2r64 d)))

(defun wt-dt-real642 (str-out d)
  (wt-u8 str-out (d2r642 d)))

(defun sgk-f100 (fo tree)
  (let ((lst (flatten-tree tree)) (sel 0))
	(setq lst (make-array (length lst) :initial-contents lst))
	(with-open-file (str-out fo :direction	:output
							 :if-does-not-exist :create
							 :if-exists		:supersede
							 :element-type	'(unsigned-byte 8))
	  (loop for n from 0 to (1- (array-total-size lst)) do
			(cond
			  ((stringp (aref lst n)) (sgk-f101 str-out lst n))
			  ))
)))

(defun length1 (stri)
  (if (oddp (length stri))
	(1+ (length stri))
	(length stri)))

(defun length2 (ary))

(defun sgk-f101 (str-out lst n)
  (cond
	((string= "RT_HEADER" (aref lst n)) (progn
										  (wt-u2 str-out #x0006)
										  (wt-u2 str-out *RT_HEADER*)
										  (wt-u2 str-out (aref lst (1+ n)))
										  (wt-u2 str-out #x001c)
										  ))
	((string= "RT_BGNLIB" (aref lst n)) (progn
										  (wt-u2 str-out *RT_BGNLIB*)
										  (wt-dt-bitarray str-out (aref lst (1+ n)) 2)
										  ))
	((string= "RT_LIBNAME" (aref lst n)) (progn
										  (wt-u2 str-out (+ 6 (* 1 (length1 (aref lst (1+ n))))))
										  (wt-u2 str-out *RT_LIBNAME*)
										  (wt-dt-string6 str-out (aref lst (1+ n)))
										  (wt-u2 str-out #x4442)
										  (wt-u2 str-out #x0014)
										  ))
	((string= "RT_UNITS" (aref lst n)) (progn
										  (wt-u2 str-out *RT_UNITS*)
										  (wt-u8 str-out #x3E4189374BC6A7EC)
										  (wt-u8 str-out #x3944B82FA09B5A50)
										  ))
	((string= "RT_ENDLIB" (aref lst n)) (progn
										  (wt-u2 str-out #x0004)
										  (wt-u2 str-out *RT_ENDLIB*)
										  ))
	((string= "RT_BGNSTR" (aref lst n)) (progn
										  (wt-u2 str-out #x001c)
										  (wt-u2 str-out *RT_BGNSTR*)
										  (wt-dt-bitarray str-out (aref lst (1+ n)) 2)
										  ))
	((string= "RT_STRNAME" (aref lst n)) (progn
										  (wt-u2 str-out (+ 4 (* 1 (length1 (aref lst (1+ n))))))
										  (wt-u2 str-out *RT_STRNAME*)
										  (wt-dt-string5 str-out (aref lst (1+ n)))
										  ))
	((string= "RT_BOUNDARY" (aref lst n)) (progn
										  (wt-u2 str-out #x0004)
										  (wt-u2 str-out *RT_BOUNDARY*)
										  ))
	((string= "RT_PATH" (aref lst n)) (progn
										  (wt-u2 str-out #x0004)
										  (wt-u2 str-out *RT_PATH*)
										  ))
	((string= "RT_SREF" (aref lst n)) (progn
										  (wt-u2 str-out #x0004)
										  (wt-u2 str-out *RT_SREF*)
										  ))
	((string= "RT_AREF" (aref lst n)) (progn
										  (wt-u2 str-out #x0004)
										  (wt-u2 str-out *RT_AREF*)
										  ))
	((string= "RT_TEXT" (aref lst n)) (progn
										  (wt-u2 str-out #x0004)
										  (wt-u2 str-out *RT_TEXT*)
										  ))
	((string= "RT_NODE" (aref lst n)) (progn
										  (wt-u2 str-out #x0004)
										  (wt-u2 str-out *RT_NODE*)
										  ))
	((string= "RT_BOX" (aref lst n)) (progn
										  (wt-u2 str-out #x0004)
										  (wt-u2 str-out *RT_BOX*)
										  ))
	((string= "RT_ENDSTR" (aref lst n)) (progn
										  (wt-u2 str-out #x0004)
										  (wt-u2 str-out *RT_ENDSTR*)
										  ))
	((string= "RT_LAYER" (aref lst n)) (progn
										  (wt-u2 str-out #x0006)
										  (wt-u2 str-out *RT_LAYER*)
										  (wt-u2 str-out (aref lst (1+ n)))
										  ))
	((string= "RT_DATATYPE" (aref lst n)) (progn
										  (wt-u2 str-out #x0006)
										  (wt-u2 str-out *RT_DATATYPE*)
										  (wt-u2 str-out (aref lst (1+ n)))
										  ))
	((string= "RT_PATHTYPE" (aref lst n)) (progn
										  (wt-u2 str-out #x0006)
										  (wt-u2 str-out *RT_PATHTYPE*)
										  (wt-u2 str-out (aref lst (1+ n)))
										  ))
	((string= "RT_XY" (aref lst n)) (progn
										  (wt-u2 str-out (+ 4 (* 4 (length (aref lst (1+ n))))))
										  (wt-u2 str-out *RT_XY*)
										  (wt-dt-bitarray str-out (aref lst (1+ n)) 4)
										  ))
	((string= "RT_ENDEL" (aref lst n)) (progn
										  (wt-u2 str-out #x0004)
										  (wt-u2 str-out *RT_ENDEL*)
										  ))
	((string= "RT_SNAME" (aref lst n)) (progn
										  (wt-u2 str-out (+ 4 (* 1 (length1 (aref lst (1+ n))))))
										  (wt-u2 str-out *RT_SNAME*)
										  (wt-dt-string5 str-out (aref lst (1+ n)))
										  ))
	((string= "RT_STRANS" (aref lst n)) (progn
										  (wt-u2 str-out #x0006)
										  (wt-u2 str-out *RT_STRANS*)
										  (wt-u2 str-out (aref lst (1+ n)))
										  ))
	((string= "RT_ANGLE" (aref lst n)) (progn
										  (wt-u2 str-out #x000c)
										  (wt-u2 str-out *RT_ANGLE*)
										  (wt-dt-real64 str-out (aref lst (1+ n)))
										  ))
	((string= "RT_COLROW" (aref lst n)) (progn
										  (wt-u2 str-out #x0008)
										  (wt-u2 str-out *RT_COLROW*)
										  (wt-dt-bitarray str-out (aref lst (1+ n)) 2)
										  ))
	((string= "RT_TEXTTYPE" (aref lst n)) (progn
										  (wt-u2 str-out #x0006)
										  (wt-u2 str-out *RT_TEXTTYPE*)
										  (wt-u2 str-out (aref lst (1+ n)))
										  ))
	((string= "RT_PRESENTATION" (aref lst n)) (progn
										  (wt-u2 str-out #x0006)
										  (wt-u2 str-out *RT_PRESENTATION*)
										  (wt-u2 str-out (aref lst (1+ n)))
										  ))
	((string= "RT_MAG" (aref lst n)) (progn
										  (wt-u2 str-out #x000c)
										  (wt-u2 str-out *RT_MAG*)
										  (wt-dt-real642 str-out (aref lst (1+ n)))
										  ;(wt-u8 str-out (aref lst (1+ n)))
										  ))
	((string= "RT_STRING" (aref lst n)) (progn
										  (wt-u2 str-out (+ 4 (* 1 (length1 (aref lst (1+ n))))))
										  (wt-u2 str-out *RT_STRING*)
										  (wt-dt-string5 str-out (aref lst (1+ n)))
										  ))
	((string= "RT_WIDTH" (aref lst n)) (progn
										  (wt-u2 str-out #x0008)
										  (wt-u2 str-out *RT_WIDTH*)
										  (wt-u4 str-out (aref lst (1+ n)))
										  ))
	)
  )

(defun sgk-rd-gds (fi) (f-return-srctree fi))

(defun sgk-wt-gds (fo tree) (sgk-f100 fo (flatten-tree tree)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *srcunits*)

(defun by-car-stri (lst stri)
  (if (cdr lst)
	(if (listp (car lst))
	  (if (stringp (car lst))
		(if (string= stri (car lst))
		  (car lst)
		  (by-car-stri (cdr lst) stri))
		(if (string= stri (car (car lst)))
		  (car lst)
		  (by-car-stri (cdr lst) stri)))
	  (if (string= stri (car lst))
		(car lst)
		(by-car-stri (cdr lst) stri)))
	(if (stringp (car (cdr lst)))
	  (if (string= stri (car (cdr lst)))
		(cdr lst) nil) nil)))

(defun itoa (i)
  (let ((lst1 (list)) (r1 (floor (float (mod i 10)))) (s ""))
	(loop
	  (when (< i 10)
		(progn
		  (push (floor (float (mod i 10))) lst1)
		  (dolist (i2 lst1)
			(setf s (concatenate 'string s (string (code-char (+ i2 48))))))
		  (return s)))
	  (push (floor (float (mod i 10))) lst1)
	  (setf i (floor (float (/ i 10)))))))

(defmacro string+ (&rest body)
  `(concatenate 'string ,@body))

(defun math-pow (a x)
  (if (= a 0)
	0
	(if (= x 0)
	  1
	  (exp (* x (log a))))))

(defun math-log (a x)
  (/ (log x) (log  a)))

(defmacro div1 (&rest body)
  `(floor (float (/ ,@body))))

(defun max-of (lst)
  (let ((cmd '(max)))
	(dolist (n lst)
	  (push n cmd))
	(eval (reverse cmd))))

(defun min-of (lst)
  (let ((cmd '(min)))
	(dolist (n lst)
	  (push n cmd))
	(eval (reverse cmd))))

(defun sta-angle (angle)
  (if (>= angle 360.0d0)
	(mod angle 360.0d0)
	angle))

(defun v1-v2 (v1)
  (let ((v2 (list)))
	(dotimes (n (length v1))
	  (if (oddp n)
		(push (vector (aref v1 (1- n)) (aref v1 n)) v2)))
	(make-array (list (length v2)) :initial-contents (reverse v2))))

(defun v2-v1 (v2)
  (let ((v1 (list)))
	(dotimes (n (length v2))
	  (push (aref (aref v2 n) 0) v1)
	  (push (aref (aref v2 n) 1) v1))
	(make-array (list (length v1)) :initial-contents (reverse v1))))

(defun v1s-v2s (v1s)
  (let ((v2s (list)))
	(dolist (v1 v1s)
	  (push (v1-v2 v1) v2s))
	v2s))

(defun v2s-v1s (v2s)
  (let ((v1s (list)))
	(dolist (v2 v2s)
	  (push (v2-v1 v2) v1s))
	v1s))

(defun xmin (v2)
  (let (xs (list))
	(dotimes (n (length v2))
	  (push (aref (aref v2 n) 0) xs))
	(min-of xs)))

(defun ymin (v2)
  (let (ys (list))
	(dotimes (n (length v2))
	  (push (aref (aref v2 n) 1) ys))
	(min-of ys)))

(defun xmax (v2)
  (let (xs (list))
	(dotimes (n (length v2))
	  (push (aref (aref v2 n) 0) xs))
	(max-of xs)))

(defun ymax (v2)
  (let (ys (list))
	(dotimes (n (length v2))
	  (push (aref (aref v2 n) 1) ys))
	(max-of ys)))

(defun xsmin (v2s)
  (let ((mins (list)))
	(dolist (v2 v2s)
	  (push (xmin v2) mins))
	(min-of mins)))

(defun ysmin (v2s)
  (let ((mins (list)))
	(dolist (v2 v2s)
	  (push (ymin v2) mins))
	(min-of mins)))

(defun xsmax (v2s)
  (let ((maxs (list)))
	(dolist (v2 v2s)
	  (push (xmax v2) maxs))
	(max-of maxs)))

(defun ysmax (v2s)
  (let ((maxs (list)))
	(dolist (v2 v2s)
	  (push (ymax v2) maxs))
	(max-of maxs)))

(defun vector+ (v1 v2)
  (vector (+ (aref v1 0) (aref v2 0)) (+ (aref v1 1) (aref v2 1))))

(defun vector- (v1 v2)
  (vector (- (aref v1 0) (aref v2 0)) (- (aref v1 1) (aref v2 1))))

(defun vector*num (v1 num)
  (vector (* (aref v1 0) num) (* (aref v1 1) num)))

(defun vector/num (v1 num)
  (vector (div (aref v1 0) num) (* (aref v1 1) num)))

(defun vector-dot (v1 v2)
  (vector (* (aref v1 0) (aref v2 0)) (* (aref v1 1) (aref v2 1))))

(defun div (a b)
  (if (= 0 b)
	nil
	(/ a b)))

(defun div1 (a b)
  (floor (float (div a b))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro rt-lib (name units)
  `(list (list "RT_HEADER" 5)
		(list "RT_BGNLIB" #(117 1 11 16 17 41 117 1 11 16 21 29))
		(list "RT_LIBNAME" ,name)
		(concatenate 'list (list "RT_UNITS" 9.999999999999998d-4 9.999999999999999d-10) (reverse ,units))
		"RT_ENDLIB"))

(defmacro rt-cell (name &rest elms)
  `(concatenate 'list
	 (list
	   (list "RT_BGNSTR" #(70 1 1 8 0 0 118 2 2 14 29 35))
	   (list "RT_STRNAME" ,name))
	 ,@elms
	 (list "RT_ENDSTR")))

(defmacro rt-sref (vsref)
  `(let ((sname (aref ,vsref 0))
		 (strans (aref ,vsref 1))
		 (angle (aref ,vsref 2))
		 (xy (aref ,vsref 3))
		 )
	 (cond
	 ((eql strans #x4000)
	  (list "RT_SREF" (list "RT_SNAME" sname)
		   (list "RT_STRANS" #x8000)
		   (list "RT_ANGLE" (sta-angle (+ 180.0d0 angle)))
		   (list "RT_XY" xy)
		   "RT_ENDEL"))
	 ((eql strans #x0000)
	  (list "RT_SREF" (list "RT_SNAME" sname)
		   (list "RT_STRANS" strans)
		   (list "RT_ANGLE" angle)
		   (list "RT_XY" xy)
		   "RT_ENDEL"))
	 ((eql strans #x8000)
	  (list "RT_SREF" (list "RT_SNAME" sname)
		   (list "RT_STRANS" strans)
		   (list "RT_ANGLE" angle)
		   (list "RT_XY" xy)
		   "RT_ENDEL")))))

(defmacro rt-boundary (layer v2)
  `(list "RT_BOUNDARY"
		(list "RT_LAYER" ,layer)
		(list "RT_DATATYPE" 0)
		(list "RT_XY" (v2-v1 ,v2))
		"RT_ENDEL"))

(defmacro rt-text (vtext)
  `(let (
		 (layer (aref ,vtext 0))
		 (xy (aref ,vtext 1))
		 (stri (aref ,vtext 2))
		 )
	 (list "RT_TEXT"
	   (list "RT_LAYER" layer)
	   (list "RT_TEXTTYPE" 0)
	   (list "RT_PRESENTATION" 5)
	   (list "RT_STRANS" 0)
	   (list "RT_MAG" 0.2d0)
	   (list "RT_XY" xy)
	   (list "RT_STRING" stri)
	   "RT_ENDEL")))

(defmacro rt-box (layer xmin ymin xmax ymax)
  `(rt-boundary ,layer (vector
						 (vector ,xmin ,ymin)
						 (vector ,xmax ,ymin)
						 (vector ,xmax ,ymax)
						 (vector ,xmin ,ymax)
						 (vector ,xmin ,ymin)
						 )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun lib (name units)
  (rt-lib name units))

(defun get-units (lib) (cdddr (cadddr lib)))

(defun get-cell (units name)
  (dolist (str units)
	(when (string= name (get-cellname str))
	  (return str))))

(defun get-cellname (str)
  (cadr (by-car-stri str "RT_STRNAME")))

(defun srefp (elm)
  (if (string= (car elm) "RT_SREF") t nil))

(defun get-snames (str)
  (let ((lst (list)))
	 (dolist (elm str)
	   (if (listp elm)
		 (if (srefp elm)
		   (push (cadr (cadr elm)) lst)
		   nil) nil))
	 lst))

(defun get-cellnames (units)
  (let ((lst (list)))
	 (dolist (str units)
	   (push (get-cellname str) lst))
	 lst))

(defun in-units-p (units cell-name)
  (let ((names (get-cellnames units)) (rt nil))
	 (dolist (name names)
	   (if (string= name cell-name)
		 (setf rt t)
		 nil))
	 rt))

(defun have-sref-p (strname)
  (if (eql nil (get-snames (get-cell *srcunits* strname)))
	nil t))

(defun depend-2 (strname)
(if (have-sref-p strname)
  (let ((lst0 (get-snames (get-cell *srcunits* strname)))
		(lst1 (list)))
	(dolist (sname lst0)
	  (push sname lst1)
	  (push (depend-2 sname)
		lst1))
	(flatten-tree lst1))
  (concatenate 'list (list strname) (get-snames (get-cell *srcunits* strname)))))

(defun withdraw (name)
  (let ((outunits) (names (concatenate 'list (list name) (depend-2 name))))
	(dolist (name1 names)
	  (push (if (in-units-p outunits name1) nil (get-cell *srcunits* name1)) outunits))
	outunits))

(defun sref-sname (sref)
  (cadr (by-car-stri sref "RT_SNAME")))

(defun sref-strans (sref)
  (cadr (by-car-stri sref "RT_STRANS")))

(defun sref-angle (sref)
  (cadr (by-car-stri sref "RT_ANGLE")))

(defun sref-xy (sref)
  (cadr (by-car-stri sref "RT_XY")))

(defun cell-strname (cell)
  (cadr (by-car-stri cell "RT_STRNAME")))

(defun vsref-real-xy (vsref pt)
	(let ((sref1 (rt-sref vsref)) (mir) (ang) (x (aref pt 0)) (y (aref pt 1)) (x0) (y0))
	  (setf mir (sref-strans sref1))
	  (setf ang (sref-angle sref1))
	  (setf x0 (aref (sref-xy sref1) 0))
	  (setf y0 (aref (sref-xy sref1) 1))
	  (cond
		((and (eql #x0000 mir) (eql 0.0d0 ang)) (vector (+ x0 x)			(+ y0 y)))
		((and (eql #x0000 mir) (eql 90.0d0 ang)) (vector (+ x0 (* -1 y))			(+ y0 x)))
		((and (eql #x0000 mir) (eql 180.0d0 ang)) (vector (+ x0 (* -1 x))	(+ y0 (* -1 y))))
		((and (eql #x0000 mir) (eql 270.0d0 ang)) (vector (+ x0 y)			(+ y0 (* -1 x))))
		((and (eql #x8000 mir) (eql 0.0d0 ang)) (vector (+ x0 x)			(+ y0 (* -1 y))))
		((and (eql #x8000 mir) (eql 90.0d0 ang)) (vector (+ x0 y) 			(+ y0 x)))
		((and (eql #x8000 mir) (eql 180.0d0 ang)) (vector (+ x0 (* -1 x))	(+ y0 y)))
		((and (eql #x8000 mir) (eql 270.0d0 ang)) (vector (+ x0 (* -1 y))	(+ y0 (* -1 x)))))))

(defun vsref-real-v2 (vsref v2)
  (let ((v2-1 (make-array (list (length v2)))))
	(dotimes (n (length v2))
	  (setf (aref v2-1 n) (vsref-real-xy vsref (aref v2 n))))
	v2-1))

(defun vsref-real-v2s (vsref v2s)
  (let ((v2s-1 (list)))
	(dolist (v2 v2s)
	  (push (vsref-real-v2 vsref v2) v2s-1))
	v2s-1))

(defun vsref-real-xys (vsref pts)
  (let ((xys (list)))
	(dolist (pt pts)
	  (push (vsref-real-xy vsref pt) xys))
	xys))

(defun boundaryp (elm)
  (if (string= (car elm) "RT_BOUNDARY") t nil))

(defun get-boundarys (cell layer)
  (let ((lst1 (list)))
	(dolist (elm cell)
	  (if (listp elm)
		(if (boundaryp elm)
		  (if (= layer (cadr (cadr elm)))
			(push (cadr (cadddr elm)) lst1) nil)
		  nil) nil))
	(v1s-v2s lst1)))

(defun cell-xmin (cell)
  (xsmin (get-boundarys cell *anchor_layer_number*)))

(defun cell-ymin (cell)
  (ysmin (get-boundarys cell *anchor_layer_number*)))

(defun cell-xmax (cell)
  (xsmax (get-boundarys cell *anchor_layer_number*)))

(defun cell-ymax (cell)
  (ysmax (get-boundarys cell *anchor_layer_number*)))

(defun cell-width (cell)
  (let ((boundarys (get-boundarys cell *anchor_layer_number*)))
	(abs (- (xsmax boundarys) (xsmin boundarys)))))

(defun cell-high (cell)
  (let ((boundarys (get-boundarys cell *anchor_layer_number*)))
	(abs (- (ysmax boundarys) (ysmin boundarys)))))

(defun vsref-xmin (vsref)
  (xsmin (vsref-real-v2s vsref (get-boundarys (get-cell *srcunits* (aref vsref 0)) *anchor_layer_number*))))

(defun vsref-ymin (vsref)
  (ysmin (vsref-real-v2s vsref (get-boundarys (get-cell *srcunits* (aref vsref 0)) *anchor_layer_number*))))

(defun vsref-xmax (vsref)
  (xsmax (vsref-real-v2s vsref (get-boundarys (get-cell *srcunits* (aref vsref 0)) *anchor_layer_number*))))

(defun vsref-ymax (vsref)
  (ysmax (vsref-real-v2s vsref (get-boundarys (get-cell *srcunits* (aref vsref 0)) *anchor_layer_number*))))

(defun vsrefs-xmin (vsrefs)
  (let ((mins (list)))
	(dolist (vsref vsrefs)
	  (push (vsref-xmin vsref) mins))
	(min-of mins)))

(defun vsrefs-ymin (vsrefs)
  (let ((mins (list)))
	(dolist (vsref vsrefs)
	  (push (vsref-ymin vsref) mins))
	(min-of mins)))

(defun vsrefs-xmax (vsrefs)
  (let ((maxs (list)))
	(dolist (vsref vsrefs)
	  (push (vsref-xmax vsref) maxs))
	(max-of maxs)))

(defun vsrefs-ymax (vsrefs)
  (let ((maxs (list)))
	(dolist (vsref vsrefs)
	  (push (vsref-ymax vsref) maxs))
	(max-of maxs)))

(defun textp (elm)
  (if (string= (car elm) "RT_TEXT") t nil))

(defun get-texts (cell layer stri)
  (let ((lst1 (list)))
	(dolist (elm cell)
	  (if (listp elm)
		(if (textp elm)
		  (if (and
				(= layer (cadr (by-car-stri elm "RT_LAYER")))
				(string= stri (cadr (by-car-stri elm "RT_STRING"))))
			(push elm lst1) nil)
		  nil) nil))
	lst1))

(defun get-pins (cell stri)
  (let ((lst1 (list)))
	(dolist (elm cell)
	  (if (listp elm)
		(if (textp elm)
		  (if (string= stri (cadr (by-car-stri elm "RT_STRING")))
			(push elm lst1) nil)
		  nil) nil))
	lst1))

(defun get-texts-xy (cell layer stri)
  (let ((xys (list)) (texts (get-texts cell layer stri)))
	(dolist (text texts)
	  (push (cadr (by-car-stri text "RT_XY")) xys))
	xys))

(defun get-pins-xy (cell stri)
  (let ((xys (list)) (texts (get-pins cell stri)))
	(dolist (text texts)
	  (push (cadr (by-car-stri text "RT_XY")) xys))
	xys))

(defun get-pins-layer (cell stri)
  (let ((xys (list)) (texts (get-pins cell stri)))
	(dolist (text texts)
	  (push (cadr (by-car-stri text "RT_LAYER")) xys))
	xys))

(defun get-pinlists (cell stris)
  (let ((pinlists (list)))
	(dolist (stri stris)
	  (push (get-pins cell stri) pinlists))
	pinlists))

(defun get-pinlists-xys (cell stris)
  (let ((pinlists (list)))
	(dolist (stri stris)
	  (push (get-pins-xy cell stri) pinlists))
	pinlists))

(defun get-pinlists-layers (cell stris)
  (let ((pinlists (list)))
	(dolist (stri stris)
	  (push (get-pins-layer cell stri) pinlists))
	pinlists))

(defun rt-srefs (vsrefs)
  (let ((srefs (list)))
	(dolist (vsref vsrefs)
	  (push (rt-sref vsref) srefs))
	srefs))

(defun rt-texts (vtexts)
  (let ((texts (list)))
	(dolist (vtext vtexts)
	  (push (rt-text vtext) texts))
	texts))

(defun cell-1 (name vsrefs vtexts)
  (rt-cell
	name
	(concatenate 'list
	(list
	  (rt-box *anchor_layer_number*
			  (vsrefs-xmin vsrefs)
			  (vsrefs-ymin vsrefs)
			  (vsrefs-xmax vsrefs)
			  (vsrefs-ymax vsrefs)))
	(rt-srefs vsrefs)
	(rt-texts vtexts))))

(defun combine-list-layers-list-xys (list-layers list-xys)
  (let ((vector-layers (make-array (list (length list-layers)) :initial-contents list-layers))
		(vector-xys (make-array (list (length list-xys)) :initial-contents list-xys))
		(lst (list)))
	(if (= (length vector-layers) (length vector-xys))
	  (dotimes (n (length vector-xys))
		(push (concatenate 'list (list (aref vector-layers n)) (list (aref vector-xys n))) lst))
	  nil)
	lst))

(defun get-vsref-vtexts (vsref pinnames mapnames)
  (let ((combine-list (combine-list-layers-list-xys
						(get-pinlists-layers (get-cell *srcunits* (aref vsref 0)) pinnames)
						(get-pinlists-xys (get-cell *srcunits* (aref vsref 0)) pinnames)))
		(vtexts (list))
		(vector-mapnames (make-array (list (length mapnames)) :initial-contents mapnames))
		(n 0)
		(mapname)
		(vlayers)
		(vxys))
	(dolist (combine combine-list)
	  (setf mapname (aref vector-mapnames n))
	  (setf vlayers (make-array (list (length (car combine))) :initial-contents (car combine)))
	  (setf vxys (make-array (list (length (cadr combine))) :initial-contents (cadr combine)))
	  (dotimes (n (length vxys))
		(push (vector (aref vlayers n) (vsref-real-xy vsref (aref vxys n)) mapname) vtexts))
	  (incf n))
	vtexts))

(defun get-rt-paths (cell layer)
  (let ((paths (list)))
	(dolist (elm cell)
	  (if (listp elm)
		(if (string= (car elm) "RT_PATH") 
		  (if (= (cadr (by-car-stri elm "RT_LAYER")) layer)
			(push elm paths) nil) nil) nil))
	paths))

(defun get-paths (cell layer)
  (let ((rt-paths (get-rt-paths cell layer))
		(paths (list)))
	(dolist (rt-path rt-paths)
	  (push (vector (cadr (by-car-stri rt-path "RT_WIDTH"))
					(v1-v2 (cadr (by-car-stri rt-path "RT_XY")))) paths))
	paths))

(defun get-metals (cell layer)
  (concatenate 'list (get-boundarys cell layer)
			   (get-paths cell layer)))

;;	instance:	(vector vsref (sub-pinlist "XPB0" "XPB1" ... ) (top-pinlist "shit0" "shit1" ... ))  !! NOTICE !!

(defun cell-2 (name instances)
  (let ((vsrefs (list))
		(vtexts (list)))
	(dolist (instance instances)
	  (push (aref instance 0) vsrefs)
	  (setf vtexts (concatenate 'list vtexts
								(get-vsref-vtexts (aref instance 0)
												  (aref instance 1)
												  (aref instance 2)))))
	(cell-1 name vsrefs vtexts)))

(defun relocate-1 (instance1s)
  (let ((instances (list))
		(vsrefs (list))
		(xymin)
		(n 0)
		(vvsrefs (list)))
	(dolist (instance1 instance1s)
	  (push (aref instance1 0) vsrefs))
	(setf vsrefs (reverse vsrefs))
	(setf xymin (vector (vsrefs-xmin vsrefs) (vsrefs-ymin vsrefs)))
	(dolist (vsref vsrefs)
	  (setf (aref vsref 3) (vector- (aref vsref 3) xymin))
	  (push vsref vvsrefs))
	(setf vvsrefs (make-array (list (length vvsrefs)) :initial-contents (reverse vvsrefs)))
	(dolist (instance1 instance1s)
	  (setf (aref instance1 0) (aref vvsrefs n))
	  (push instance1 instances)
	  (incf n))
	(reverse instances)))

(defun cell-3 (name instance1s)
  (cell-2 name (relocate-1 (relocate-2 instance1s))))

(defun relocate-2 (instance1s)
  (let ((instances (list))
		(vsrefs (list))
		(xymin)
		(n 0)
		(vvsrefs (list))
		(vsref.h)
		(vsref.w))
	(dolist (instance1 instance1s)
	  (push (aref instance1 0) vsrefs))
	(setf vsrefs (reverse vsrefs))
	(setf xymin (vector (vsrefs-xmin vsrefs) (vsrefs-ymin vsrefs)))
	(dolist (vsref vsrefs)
	  (setf vsref.h (cell-high (get-cell *srcunits* (aref vsref 0))))
	  (setf vsref.w (cell-width (get-cell *srcunits* (aref vsref 0))))
	  (case (aref vsref 1)
		(#x0000 (case (aref vsref 2)
				  (0.0d0 (setf (aref vsref 3) (vector+ (aref vsref 3) (vector 0 0))))
				  (90.0d0 (setf (aref vsref 3) (vector+ (aref vsref 3) (vector vsref.h 0))))
				  (180.0d0 (setf (aref vsref 3) (vector+ (aref vsref 3) (vector vsref.w vsref.h))))
				  (270.0d0 (setf (aref vsref 3) (vector+ (aref vsref 3) (vector 0 vsref.w))))
				  ))
		(#x4000 (case (aref vsref 2)
				  (0.0d0 (setf (aref vsref 3) (vector+ (aref vsref 3) (vector vsref.w 0))))
				  (90.0d0 (setf (aref vsref 3) (vector+ (aref vsref 3) (vector vsref.h vsref.w))))
				  (180.0d0 (setf (aref vsref 3) (vector+ (aref vsref 3) (vector 0 vsref.h))))
				  (270.0d0 (setf (aref vsref 3) (vector+ (aref vsref 3) (vector 0 0))))
				  ))
		(#x8000 (case (aref vsref 2)
				  (180.0d0 (setf (aref vsref 3) (vector+ (aref vsref 3) (vector vsref.w 0))))
				  (270.0d0 (setf (aref vsref 3) (vector+ (aref vsref 3) (vector vsref.h vsref.w))))
				  (0.0d0 (setf (aref vsref 3) (vector+ (aref vsref 3) (vector 0 vsref.h))))
				  (90.0d0 (setf (aref vsref 3) (vector+ (aref vsref 3) (vector 0 0))))
				  ))
		)
	  (push vsref vvsrefs))
	(setf vvsrefs (make-array (list (length vvsrefs)) :initial-contents (reverse vvsrefs)))
	(dolist (instance1 instance1s)
	  (setf (aref instance1 0) (aref vvsrefs n))
	  (push instance1 instances)
	  (incf n))
	(reverse instances)))

(defun sname-width (sname)
  (cell-width (get-cell *srcunits* sname)))

(defun sname-high (sname)
  (cell-high (get-cell *srcunits* sname)))

(defun inst (vinst)
  (let ((sname (aref vinst 0))
		(strans (aref vinst 1))
		(angle (aref vinst 2))
		(xy (aref vinst 3))
		(inst-pin-list (aref vinst 4))
		(top-pin-list (aref vinst 5)))
	(vector
	  (vector sname strans angle xy)
	  inst-pin-list
	  top-pin-list)))

(defun vinst-width (vinst)
  (let ((cw (sname-width (aref vinst 0)))
		(ch (sname-high (aref vinst 0))))
	(case (aref vinst 2)
	  (0.0d0 cw)
	  (180.0d0 cw)
	  (90.0d0 ch)
	  (270.0d0 ch))))

(defun vinst-high (vinst)
  (let ((cw (sname-width (aref vinst 0)))
		(ch (sname-high (aref vinst 0))))
	(case (aref vinst 2)
	  (0.0d0 ch)
	  (180.0d0 ch)
	  (90.0d0 cw)
	  (270.0d0 cw))))

(defun inst-up-left (inst1 vinst)
  (let ((inst2.w (vinst-width vinst))
		(inst2.h (vinst-high vinst))
		(inst1.w (vinst-width (aref inst1 0)))
		(inst1.h (vinst-high (aref inst1 0)))
		(p0 (aref (aref inst1 0) 3)))
	(setf (aref vinst 3) (vector+ p0 (vector 0 inst1.h)))
	(inst vinst)))

(defun inst-up-right (inst1 vinst)
  (let ((inst2.w (vinst-width vinst))
		(inst2.h (vinst-high vinst))
		(inst1.w (vinst-width (aref inst1 0)))
		(inst1.h (vinst-high (aref inst1 0)))
		(p0 (aref (aref inst1 0) 3)))
	(setf (aref vinst 3) (vector+ p0 (vector (- inst1.w inst2.w) inst1.h)))
	(inst vinst)))

(defun inst-right-down (inst1 vinst)
  (let ((inst2.w (vinst-width vinst))
		(inst2.h (vinst-high vinst))
		(inst1.w (vinst-width (aref inst1 0)))
		(inst1.h (vinst-high (aref inst1 0)))
		(p0 (aref (aref inst1 0) 3)))
	(setf (aref vinst 3) (vector+ p0 (vector inst1.w 0)))
	(inst vinst)))

(defun inst-right-up (inst1 vinst)
  (let ((inst2.w (vinst-width vinst))
		(inst2.h (vinst-high vinst))
		(inst1.w (vinst-width (aref inst1 0)))
		(inst1.h (vinst-high (aref inst1 0)))
		(p0 (aref (aref inst1 0) 3)))
	(setf (aref vinst 3) (vector+ p0 (vector inst1.w (- inst1.h inst2.h))))
	(inst vinst)))

(defun inst-left-down (inst1 vinst)
  (let ((inst2.w (vinst-width vinst))
		(inst2.h (vinst-high vinst))
		(inst1.w (vinst-width (aref inst1 0)))
		(inst1.h (vinst-high (aref inst1 0)))
		(p0 (aref (aref inst1 0) 3)))
	(setf (aref vinst 3) (vector+ p0 (vector (* -1 inst2.w) 0)))
	(inst vinst)))

(defun inst-left-up (inst1 vinst)
  (let ((inst2.w (vinst-width vinst))
		(inst2.h (vinst-high vinst))
		(inst1.w (vinst-width (aref inst1 0)))
		(inst1.h (vinst-high (aref inst1 0)))
		(p0 (aref (aref inst1 0) 3)))
	(setf (aref vinst 3) (vector+ p0 (vector (* -1 inst2.w) (- inst1.h inst2.h))))
	(inst vinst)))

(defun inst-down-left (inst1 vinst)
  (let ((inst2.w (vinst-width vinst))
		(inst2.h (vinst-high vinst))
		(inst1.w (vinst-width (aref inst1 0)))
		(inst1.h (vinst-high (aref inst1 0)))
		(p0 (aref (aref inst1 0) 3)))
	(setf (aref vinst 3) (vector+ p0 (vector 0 (* -1 inst2.h))))
	(inst vinst)))

(defun inst-down-right (inst1 vinst)
  (let ((inst2.w (vinst-width vinst))
		(inst2.h (vinst-high vinst))
		(inst1.w (vinst-width (aref inst1 0)))
		(inst1.h (vinst-high (aref inst1 0)))
		(p0 (aref (aref inst1 0) 3)))
	(setf (aref vinst 3) (vector+ p0 (vector (- inst1.w inst2.w) (* -1 inst2.h))))
	(inst vinst)))

(defun p-on-stright (p p0 k)
  (let ((dp (vector- p p0)))
	(if (eql (div (aref dp 1) (aref dp 0)) nil)
	  (p-on-stright p (vector (+ (aref p0 0) 1) (+ (aref p0 1) k)) k)
	  (if (eql (div (aref dp 1) (aref dp 0)) k)
		t nil))))

(defun p-on-line (p p1 p2)
  (let ((k)
		(dp (vector- p2 p1)))
	(setf k (div (aref dp 1) (aref dp 0)))
	(if (p-on-stright p p1 k)
	  (if (and (and (>= (aref p 0) (min (aref p1 0) (aref p2 0))) (<= (aref p 0) (max (aref p1 0) (aref p2 0))))
			   (and (>= (aref p 1) (min (aref p1 1) (aref p2 1))) (<= (aref p 1) (max (aref p1 1) (aref p2 1)))))
		t nil)
	  nil)))

(defun overlay (vsref stri)
  (let ((text-xy (get-pins-xy (get-cell *srcunits* (aref vsref 0)) stri)))
	(if (= 1 (length text-xy))
	  (setf text-xy (car text-xy)) nil)
;	(setf (aref vsref 3) (vector- (aref vsref 3) text-xy))
	(case (aref vsref 2)
	  (0.0d0 (setf (aref vsref 3) (vector- (aref vsref 3) text-xy)))
	  (90.0d0 (setf (aref vsref 3) (vector- (aref vsref 3) (reverse (vector-dot text-xy (vector 1 -1))))))
	  (270.0d0 (setf (aref vsref 3) (vector- (aref vsref 3) (reverse (vector-dot text-xy (vector 1 -1))))))
	  (180.0d0 (setf (aref vsref 3) (vector- (aref vsref 3) (vector-dot text-xy (vector -1 -1)))))
	  )
	(rt-sref vsref)))

(defun overlay-1 (sname strans angle cell stri stri0)
  (overlay
	(vector sname strans angle
			(car (get-pins-xy cell stri0)))
	stri))

;(defun overlay-2 (sname strans angle cellname stri stri0)
;  (push (overlay-1 sname strans angle (get-cell *srcunits* cellname) stri stri0) (cdddr (get-cell *srcunits* cellname))))

