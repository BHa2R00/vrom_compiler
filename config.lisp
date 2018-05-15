(defvar *leaf_cell* "./lc1.gds")

;(defvar *code-file* "./VROM128X8_CM16.cod")
(defvar *code-file* "./VROM320X9_CM8.cod")
;(defvar *code-file* "./VROM4KX21_CM8.cod")
;(defvar *code-file* "./VROM8KX23_CM32.cod")

;(defvar *word-depth* 128)
(defvar *word-depth* 320)
;(defvar *word-depth* 4096)
;(defvar *word-depth* 8192)

;(defvar *word-length* 8)
(defvar *word-length* 9)
;(defvar *word-length* 21)
;(defvar *word-length* 23)

;(defvar *col-mux* 16)
(defvar *col-mux* 8)
;(defvar *col-mux* 8)
;(defvar *col-mux* 32)

(defvar *charmode* 0)

(defvar *vpinlayer* "m2")
(defvar *hpinlayer* "m3")

(defvar *anchor_layer_number* 60)
(defvar *outlib-name* "shit")
(defvar *gds-out* "./out.gds")
;(defvar *gds-out* (string+ *outlib-name* (itoa *word-depth*) "X" (itoa *word-length*) "CM" (itoa *col-mux*) ".gds"))
