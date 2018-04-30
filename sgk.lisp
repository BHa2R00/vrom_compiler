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

(defun get-units (lib) (cdddr (cadddr lib)))

(defmacro units-of (lib) `(cdddr (cadddr ,lib)))

(defun is-str? (lst)
  (if (string= (caar lst) "RT_BGNSTR") t nil))

(defun get-str-name (str) (cadr (cadr str)))

(defun get-str (units name)
  (dolist (str units)
	(when (string= name (get-str-name str))
	  (return str))))

(defun is-boundary? (elm)
  (if (string= (car elm) "RT_BOUNDARY") t nil))

(defun get-boundary (str layer)
  (let ((lst1 (list)))
	(dolist (elm str)
	  (if (listp elm)
		(if (is-boundary? elm)
		  (if (= layer (cadr (cadr elm)))
			(push (cadr (cadddr elm)) lst1) nil)
		  nil) nil))
	lst1))

(defun is-rectangle? (b-xy) (if (= 8 (- (length b-xy) 2)) t nil))

(defun size-of-rectangle (b-xy)
  (let ((size))
	(if (is-rectangle? b-xy)
	  (setf size (list
				   (max (aref b-xy 0) (aref b-xy 2) (aref b-xy 4) (aref b-xy 6))
				   (max (aref b-xy 1) (aref b-xy 3) (aref b-xy 5) (aref b-xy 7))
				   )) nil)
	(make-array '(2) :initial-contents size)))

(defmacro srefp (elm)
  `(if (string= (car ,elm) "RT_SREF") t nil))

(defun textp (elm)
  (if (string= (car elm) "RT_TEXT") t nil))

(defun pathp (elm)
  (if (string= (car elm) "RT_PATH") t nil))

(defmacro get-srefs-of (str)
  `(let ((lst (list)))
	 (dolist (elm ,str)
	   (if (listp elm)
		 (if (srefp elm)
		   (push elm lst)
		   nil) nil))
	 lst))

(defun get-texts (str)
  (let ((lst (list)))
	(dolist (elm str)
	  (if (listp elm)
		(if (textp elm)
		  (push elm lst)
		  nil) nil))
	lst))

(defun get-paths (str layer)
  (let ((lst (list)))
	(dolist (elm str)
	  (if (listp elm)
		(if (pathp elm)
		  (if (= layer (cadr (cadr elm)))
			(push elm lst) nil)
		  nil) nil))
	lst))

(defmacro get-snames-of (str)
  `(let ((lst (list)))
	 (dolist (elm ,str)
	   (if (listp elm)
		 (if (srefp elm)
		   (push (cadr (cadr elm)) lst)
		   nil) nil))
	 lst))

(defun get-strnames-of (lib)
  (let ((lst1 (get-units lib)) (lst (list)))
	 (dolist (elm lst1)
	   (push (get-str-name elm) lst))
	 lst))

(defun get-units-names (units)
  (let ((lst (list)))
	 (dolist (str units)
	   (push (get-str-name str) lst))
	 lst))

(defmacro in-lib-p (lib cell-name)
  `(let ((lst1 (get-strnames-of ,lib)) (rt nil))
	 (dolist (name lst1)
	   (if (string= name ,cell-name)
		 (setf rt t)
		 nil))
	 rt))

(defun in-units-p (units cell-name)
  (let ((names (get-units-names units)) (rt nil))
	 (dolist (name names)
	   (if (string= name cell-name)
		 (setf rt t)
		 nil))
	 rt))

(defun math-pow (a x)
  (if (= a 0)
	0
	(if (= x 0)
	  1
	  (exp (* x (log a))))))

(defun math-log (a x)
  (/ (log x) (log  a)))

(defun +< (hd bd)
  (let ((rhd (reverse hd)))
	(dolist (n rhd)
	  (push n bd))
	bd))

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

(defun <f> (f)
  (if (> f 0)
	(floor f)
	(ceiling f)))

(defun dot-product (v1 v2)
  (if (eql (length v1) (length v2))
	(let ((r 0))
	  (dotimes (n (length v1))
		(setf r (+ (* (aref v1 n) (aref v2 n)) r)))
	  r)
	nil))

(defun sgk-rd-gds (fi) (f-return-srctree fi))

(defun sgk-wt-gds (fo tree) (sgk-f100 fo (flatten-tree tree)))

(defun sgk-lib (name units)
  (list (list "RT_HEADER" 5)
		(list "RT_BGNLIB" #(117 1 11 16 17 41 117 1 11 16 21 29))
		(list "RT_LIBNAME" name)
		(+< (list "RT_UNITS" 9.999999999999998d-4 9.999999999999999d-10) (reverse units))
		"RT_ENDLIB"))

(defun sgk-cell (name elms)
  (let ((lst (list (list "RT_STRNAME" name) (list "RT_BGNSTR" #(70 1 1 8 0 0 118 2 2 14 29 35)))))
	(dolist (elm elms)
	  (push elm lst))
	(push "RT_ENDSTR" lst)
	(reverse lst)))

(defun sgk-sref (sname strans angle xy)
  (if (= strans #x4000)
	(sgk-sref1 sname #x8000 (+ 180.0d0 angle) xy)
	(sgk-sref1 sname strans angle xy)))

(defun sgk-sref1 (sname strans angle xy)
  (list "RT_SREF" (list "RT_SNAME" sname)
	(list "RT_STRANS" strans)
	(list "RT_ANGLE" angle)
	(list "RT_XY" (make-array '(2) :initial-contents xy))
	"RT_ENDEL"))

(defun sgk-text (layer typ pre strans mag xy text)
  (list "RT_TEXT"
	(list "RT_LAYER" layer)
	(list "RT_TEXTTYPE" typ)
	(list "RT_PRESENTATION" pre)
	(list "RT_STRANS" strans)
	(list "RT_MAG" mag)
	(list "RT_XY" (make-array '(2) :initial-contents xy))
	(list "RT_STRING" text)
	"RT_ENDEL"))

(defun sgk-boundary (layer datatype xy)
  (list "RT_BOUNDARY"
		(list "RT_LAYER" layer)
		(list "RT_DATATYPE" datatype)
		(list "RT_XY" (make-array (list (length xy)) :initial-contents xy))
		"RT_ENDEL"))

(defun sgk-path (layer datatype pathtype width xy)
  (list "RT_PATH"
		(list "RT_LAYER" layer)
		(list "RT_DATATYPE" datatype)
		(list "RT_PATHTYPE" pathtype)
		(list "RT_WIDTH" width)
		(list "RT_XY" (make-array (list (length xy)) :initial-contents xy))
		"RT_ENDEL"))

(defun sgk-boundary-rectangle (layer strans x0 y0 w h)
  (cond
	((= strans #x4000) (sgk-boundary-rectangle layer #x0000 x0 y0 (* -1 w) h))
	((= strans #x8000) (sgk-boundary-rectangle layer #x0000 x0 y0 w (* -1 h)))
	(t (sgk-boundary layer 0 (list x0 y0
								   (+ x0 w) y0
								   (+ x0 w) (+ y0 h)
								   x0 (+ y0 h)
								   x0 y0)))))

(defun sgk-ls-cell (lib)
  (dolist (n (get-units lib))
  (format t "~a~%" (get-str-name n))))

(defun sgk-xy2d (x y)
  (make-array '(2) :initial-contents (list x y)))

(defmacro add-cell-to-units (units ilib name)
  `(if (in-units-p ,units ,name)
	 nil
	 (push (get-str (get-units ,ilib) ,name) ,units)))

(defmacro add-cells-to-units (units ilib names)
  `(dolist (name ,names)
	 (add-cell-to-units ,units ,ilib name)))

(defmacro add1-cell (units ilib name)
  `(add-cells-to-units ,units ,ilib (get-snames-of (get-str (get-units ,ilib) ,name))))

(defmacro cell-add (units ilib name)
  `(progn
	(add1-cell ,units ,ilib ,name)
	(add-cell-to-units ,units ,ilib ,name)))

(defun map2sref (sref-map) 		;; #(NAME	MIR:'0-no,'x-xaix,'y-yaix	ANG:'0-0d0,'1-90d0,'2-180d0,'3-270d0	X	Y)
  (sgk-sref (aref sref-map 0)
			 (cond
			   ((string= "n" (aref sref-map 1)) #x0000)
			   ((string= "x" (aref sref-map 1)) #x8000)
			   ((string= "y" (aref sref-map 1)) #x4000))
			 (cond
			   ((string= "a0" (aref sref-map 2)) 0.0d0)
			   ((string= "a1" (aref sref-map 2)) 90.0d0)
			   ((string= "a2" (aref sref-map 2)) 180.0d0)
			   ((string= "a3" (aref sref-map 2)) 270.0d0))
			 (list (aref sref-map 3) (aref sref-map 4))))

(defun new-cell-1 (units name sref-maps)
  (let ((elms (list)))
	 (dolist (sref-map sref-maps)
	   (push (map2sref sref-map) elms))
	 (push (sref-maps-boundary units sref-maps) elms)
	 (sgk-cell name elms)))

(defun relocate (sref-map)
  (let ((xs (list))
		(ys (list))
		(xmin)
		(ymin))
	(dolist (sref sref-map)
	  (push (aref sref 3) xs)
	  (push (aref sref 4) ys))
	(setf xmin (min-of xs))
	(setf ymin (min-of ys))
;	(dolist (sref sref-map) (setf (aref sref 3) (- (aref sref 3) xmin)))
;	(dolist (sref sref-map) (setf (aref sref 4) (- (aref sref 4) ymin)))
	(dolist (sref sref-map)
	  (setf (aref sref 3) (- (aref sref 3) xmin))
	  (setf (aref sref 4) (- (aref sref 4) ymin)))
	sref-map))

(defun relocate-2 (sref-map)
  (let ((xs (list))
		(ys (list))
		(xmin)
		(ymin))
	(dolist (sref sref-map)
	  (push (aref sref 3) xs)
	  (push (aref sref 4) ys))
	(setf xmin (min-of xs))
	(setf ymin (min-of ys))
	(if (< xmin 0)
	  (dolist (sref sref-map)
		(setf (aref sref 3) (- (aref sref 3) (abs xmin)))) nil)
	(if (< ymin 0)
	  (dolist (sref sref-map)
		(setf (aref sref 4) (- (aref sref 4) (abs ymin)))) nil)
	sref-map))

(defun relocate1 (units sref-map)
  (dolist (sref sref-map)
	(when (string= "y" (aref sref 1))
	  (setf (aref sref 3) (+ (aref sref 3) (str-width (get-str units (aref sref 0))))))
	(when (string= "x" (aref sref 1))
	  (setf (aref sref 4) (+ (aref sref 4) (str-high (get-str units (aref sref 0)))))))
  sref-map)

;(defmacro new-cell-2 (units name sref-maps)
;  `(push (new-cell-1 ,units ,name (relocate-2 (relocate (relocate1 ,units ,sref-maps)))) ,units))

(defmacro new-cell-2 (units name sref-maps)
  `(push (new-cell-1 ,units ,name (relocate1 ,units (relocate ,sref-maps))) ,units))

;(defmacro new-cell-3 (units name sref-maps)
;  `(push (new-cell-1 ,units ,name ,sref-maps) ,units))

(defun get-boundary-1 (str)
  (let ((xy0 (list)) (xy1 (car (get-boundary str *anchor_layer_number*))) (n1))
	(dotimes (n (length xy1))
	  (if (oddp n)
		(push (make-array '(2) :initial-contents (list (aref xy1 (1- n)) (aref xy1 n))) xy0)
		(setf n1 n)))
	(reverse xy0)))

(defun get-boundary-3 (str layer)
  (let ((xy0 (list)) (xy1 (car (get-boundary str layer))) (n1))
	(dotimes (n (length xy1))
	  (if (oddp n)
		(push (make-array '(2) :initial-contents (list (aref xy1 (1- n)) (aref xy1 n))) xy0)
		(setf n1 n)))
	xy0))

(defun get-boundary-4 (str layer)
  (let ((xy0) (xy0s (list)) (xy1s (get-boundary str layer)))
	(dolist (xy1 xy1s)
	  (setf xy0 (list))
	  (dotimes (n (length xy1))
		(if (oddp n)
		(push (make-array '(2) :initial-contents (list (aref xy1 (1- n)) (aref xy1 n))) xy0) nil))
	  (push xy0 xy0s))
	xy0s))

(defun str-width (str)
  (let ((bl0 (get-boundary-4 str *anchor_layer_number*)) (xs (list)) (widths (list)))
	(dolist (bl1 bl0)
	  (dolist (xy bl1)
		(push (aref xy 0) xs))
	  (push (- (max-of xs) (min-of xs)) widths))
	(max-of widths)))

(defun str-high (str)
  (let ((bl0 (get-boundary-4 str *anchor_layer_number*)) (ys (list)) (highs (list)))
	(dolist (bl1 bl0)
	  (dolist (xy bl1)
		(push (aref xy 1) ys))
	  (push (- (max-of ys) (min-of ys)) highs))
	(max-of highs)))

(defmacro real-xy-1 (sref-map pt)
  `(let ((x (aref ,sref-map 3)) (y (aref ,sref-map 4)) (ang (aref ,sref-map 2)) (ang1))
	 (cond
	   ((string= ang "a0") (setf ang1 0.0d0))
	   ((string= ang "a1") (setf ang1 1.5708d0))
	   ((string= ang "a2") (setf ang1 3.1416d0))
	   ((string= ang "a3") (setf ang1 4.7124d0)))
	 (list
	   (+ (<f> (+ (* x (cos ang1)) (* -1 y (sin ang1)))) (aref ,pt 0))
	   (+ (<f> (+ (* x (sin ang1)) (* y (cos ang1)))) (aref ,pt 1)))))

(defun real-xy (sref-map pt)
	(let ((mir (aref sref-map 1)) (ang (aref sref-map 2)) (x (aref pt 0)) (y (aref pt 1)) (x0 (aref sref-map 3)) (y0 (aref sref-map 4)))
	  (cond
		((and (string= "n" mir) (string= "a0" ang)) (list (+ x0 x)			(+ y0 y)))
		((and (string= "n" mir) (string= "a1" ang)) (list (+ x0 (* -1 y))			(+ y0 x)))
		((and (string= "n" mir) (string= "a2" ang)) (list (+ x0 (* -1 x))	(+ y0 (* -1 y))))
		((and (string= "n" mir) (string= "a3" ang)) (list (+ x0 y)			(+ y0 (* -1 x))))
		((and (string= "x" mir) (string= "a0" ang)) (list (+ x0 x)			(+ y0 (* -1 y))))
		((and (string= "x" mir) (string= "a1" ang)) (list (+ x0 y) 			(+ y0 x)))
		((and (string= "x" mir) (string= "a2" ang)) (list (+ x0 (* -1 x))	(+ y0 y)))
		((and (string= "x" mir) (string= "a3" ang)) (list (+ x0 (* -1 y))	(+ y0 (* -1 x))))
		((and (string= "y" mir) (string= "a0" ang)) (list (+ x0 (* -1 x))	(+ y0 y)))
		((and (string= "y" mir) (string= "a1" ang)) (list (+ x0 (* -1 y))	(+ y0 (* -1 x))))
		((and (string= "y" mir) (string= "a2" ang)) (list (+ x0 x)			(+ y0 (* -1 y))))
		((and (string= "y" mir) (string= "a3" ang)) (list (+ x0 y) 			(+ y0 x)))
		)))

(defun get-boundary-2 (units sref-maps)
  (let ((xy1 (list)))
	(dolist (sref-map sref-maps)
	  (dolist (pt (get-boundary-1 (get-str units (aref sref-map 0))))
		(push (make-array '(2) :initial-contents
						  ;(list (+ (aref pt 0) (aref sref-map 3)) (+ (aref pt 1) (aref sref-map 4)))
						  (real-xy sref-map pt)
						  )
			  xy1)))
	(reverse xy1)))

(defun x-of (xy)
  (let ((x (list)))
	(dolist (xyi xy)
	  (push (aref xyi 0) x))
	(reverse x)))

(defun y-of (xy)
  (let ((y (list)))
	(dolist (xyi xy)
	  (push (aref xyi 1) y))
	(reverse y)))

(defun xymax (units sref-maps)
  (let ((xy0 (get-boundary-2 units sref-maps)))
	(list (max-of (x-of xy0)) (max-of (y-of xy0)))))

(defun xymin (units sref-maps)
  (let ((xy0 (get-boundary-2 units sref-maps)))
	(list (min-of (x-of xy0)) (min-of (y-of xy0)))))

(defun sref-maps-boundary (units sref-maps)
  (let ((x0 (car (xymin units sref-maps)))
		(y0 (cadr (xymin units sref-maps)))
		(x1 (car (xymax units sref-maps)))
		(y1 (cadr (xymax units sref-maps))))
	(sgk-boundary *anchor_layer_number* 0 (list x0 y0 x1 y0 x1 y1 x0 y1 x0 y0))))

(defun itoa (i)
  (let ((lst1 (list)) (r1 (floor (float (mod i 10)))) (s ""))
	(loop
	  (when (<= i 10)
		(progn
		  (push (floor (float (mod i 10))) lst1)
		  (dolist (i2 lst1)
			(setf s (concatenate 'string s (string (code-char (+ i2 48))))))
		  (return s)))
	  (push (floor (float (mod i 10))) lst1)
	  (setf i (floor (float (/ i 10)))))))

(defmacro string+ (&rest body)
  `(concatenate 'string ,@body))

(defun 0>_<0 (a x b) (if (and (> x a) (< x b)) t nil))
(defun 1>_<1 (a x b) (if (and (>= x a) (<= x b)) t nil))

(defun half-line-segment-cross-p-1 (x0 y0 ang x1 y1 x2 y2)
  (cond
	((= ang 0) (if (= x1 x2)
				 (if (and (<= x0 x1)
						  (1>_<1 (min y1 y2) y0 (max y1 y2))) t nil) nil))
	((= ang 1) (if (= y1 y2)
				 (if (and (<= y0 y1)
						  (1>_<1 (min x1 x2) x0 (max x1 x2))) t nil) nil))
	((= ang 2) (if (= x1 x2)
				 (if (and (>= x0 x1)
						  (1>_<1 (min y1 y2) y0 (max y1 y2))) t nil) nil))
	((= ang 3) (if (= y1 y2)
				 (if (and (>= y0 y1)
						  (1>_<1 (min x1 x2) x0 (max x1 x2))) t nil) nil))))

(defun half-line-segment-cross-p-2 (p0 ang p1 p2)
  (half-line-segment-cross-p-1 (aref p0 0) (aref p0 1) ang
							   (aref p1 0) (aref p1 1)
							   (aref p2 0) (aref p2 1)))

(defun point-in-boundary-p (p0 b1)
  (let ((cnt 0) (rt '(and)) (b0 (make-array (list (length b1)) :initial-contents b1)))
	(dotimes (ang 4)
	  (setf cnt 0)
	  (dotimes (n (1- (length b0)))
		(if (half-line-segment-cross-p-2 p0 ang (aref b0 n) (aref b0 (1+ n)))
		  (setf cnt (1+ cnt))
		  nil))
	  (if (oddp cnt)
		(push t rt)
		(push nil rt)))
	(eval (reverse rt))))

(defmacro caddddr (a)
  `(caddr (cddr ,a)))

(defun get-path-width (path)
  (cadr (caddddr path)))

(defmacro <sref (&rest body)
  `(make-array '(5) :initial-contents (list ,@body)))

(defmacro <cell (units name &rest body)
  `(new-cell-2 ,units ,name (list ,@body)))

;(defmacro <cell-1 (units name &rest body)
;  `(new-cell-3 ,units ,name (list ,@body)))

(defmacro div1 (&rest body)
  `(floor (float (/ ,@body))))

(defun have-sref-p (srcunits strname)
  (if (eql nil (get-snames-of (get-str srcunits strname)))
	nil t))

(defun in-list-p (lst elms)
  (let ((r nil))
	(dolist (n lst)
	  (dolist (elm elms)
		(if (and (stringp n) (stringp elm))
		  (if (string= n elm) (setf r t) nil)
		  (if (and (listp n) (listp elm))
			(if (eql n elm) (setf r t) nil)
			nil)))) r))

(defun depend-2 (srcunits strname)
(if (have-sref-p srcunits strname)
  (let ((lst0 (get-snames-of (get-str srcunits strname)))
		(lst1 (list)))
	(dolist (sname lst0)
	  (push sname lst1)
	  (push (depend-2 srcunits sname)
		lst1))
	(flatten-tree lst1))
  (concatenate 'list (list strname) (get-snames-of (get-str srcunits strname)))))

(defun depend-1 (srcunits strname)
(if (have-sref-p srcunits strname)
  (let ((lst0 (get-snames-of (get-str srcunits strname)))
		(lst1 (list)))
	(dolist (sname lst0)
	  (push sname lst1)
	  (push ;(depend-1 srcunits sname)
		(if (not (in-list-p lst1 (depend-1 srcunits sname)))
		  (depend-1 srcunits sname) sname)
		lst1))
	lst1)
  (concatenate 'list (list strname) (get-snames-of (get-str srcunits strname)))))

(defun depend (srcunits strname)
  (flatten-tree (depend-1 srcunits strname)))

(defmacro cell-add-1 (outunits srclib names)
  `(dolist (name ,names)
	(cell-add ,outunits ,srclib name)))

(defmacro <cell-add (outunits srclib name)
  `(progn
	(cell-add ,outunits ,srclib ,name)
	(cell-add-1 ,outunits ,srclib (depend-2 (units-of ,srclib) ,name))))

;(defun cell-array-list-1 (outunits srclib op sref1s)
;  (dolist (sref1 sref1s)
;	(<cell-add outunits srclib (aref sref1 0)))
;  (let ((sref-map (list)) (x0 0) (y0 0) (x1 0) (y1 0))
;	(dolist (sref1 sref1s)
;	  (cond
;	  ((string= )))
;	  ))
