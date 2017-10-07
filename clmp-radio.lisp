;; CLMP
;; clmp-radio.lisp
;; Developer: Branitskiy Alexander <schurshik@yahoo.com>

(in-package :clmp)

(defstruct radio-station name addr)

(defclass clmp-radio ()
  ((window :initarg :window
	   :initform nil
	   :reader get-window
	   :writer set-window)
   (radarr :initarg :radarr
	   :initform (make-array 0 :initial-contents '())
	   :reader get-radarr
	   :writer set-radarr)
   (curind :initarg :curind
	   :initform 0
	   :reader get-curind
	   :writer set-curind)
   (currow :initarg :currow
	   :initform +startoffset-row+
	   :reader get-currow
	   :writer set-currow)
   (curcol :initarg :curcol
	   :initform +startoffset-column+
	   :reader get-curcol
	   :writer set-curcol)))

(defmethod create-window ((self clmp-radio) &key ((:height h) 0 h?) ((:width w) 0 w?) ((:row r) 0 r?) ((:column c) 0 c?))
  (if (and h? w? r? c?)
      (set-window (cl-ncurses:newwin h w r c) self))
  (colorize-window (get-window self))
  ;(render-window self :height h :width w :row r :column c)
  (loop for key being the hash-keys of radio-stations do
	(let ((radio-name key)
	      (radio-addr (gethash key radio-stations))
	      (s (make-radio-station)))
	  (setf (radio-station-name s) radio-name)
	  (setf (radio-station-addr s) radio-addr)
	  (set-radarr (concatenate 'vector (get-radarr self) (make-array 1 :initial-contents (list s))) self))))

(defmethod destroy-window ((self clmp-radio))
  (cl-ncurses:delwin (get-window self)))

(defmethod create ((self clmp-radio) &key ((:height h) 0 h?) ((:width w) 0 w?) ((:row r) 0 r?) ((:column c) 0 c?))
  (if (and h? w? r? c?)
      (create-window self :height h :width w :row r :column c)))

(defmethod destroy ((self clmp-radio))
  (destroy-window self))

(defmethod render-window ((self clmp-radio) &key ((:height h) 0 h?) ((:width w) 0 w?) ((:row r) 0 r?) ((:column c) 0 c?))
  (cl-ncurses:wclear (get-window self))
  (when (and h? w? r? c?)
    (cl-ncurses:wresize (get-window self) h w)
    (cl-ncurses:mvwin (get-window self) r c))
  (loop for i from 0 to (- (length (get-radarr self)) 1) do
	(cl-ncurses:wmove (get-window self) (+ i 1) 1)
	(let* ((s (aref (get-radarr self) i))
	       (name (radio-station-name s))
	       (addr (radio-station-addr s)))
	  (cl-ncurses:wprintw (get-window self) (format nil "~a ~a" name addr))))
  (cl-ncurses:wmove (get-window self) 1 1)
  (draw-frame (get-window self))
  (mark-line self)
  (cl-ncurses:wrefresh (get-window self)))

(defmethod render ((self clmp-radio) &key ((:height h) 0 h?) ((:width w) 0 w?) ((:row r) 0 r?) ((:column c) 0 c?))
  (if (and h? w? r? c?)
      (render-window self :height h :width w :row r :column c)
    (render-window self)))

(defmethod mark-line ((self clmp-radio))
;  (cl-ncurses:init-pair 2 cl-ncurses:color_black cursor-color)
  #+(or sbcl clisp)
  (progn
    (cl-ncurses:init-pair 2 cl-ncurses:color_black cursor-color)
    (let ((window (get-window self)))
      (cl-ncurses:mvwchgat window (get-currow self) (get-curcol self) (- (cl-ncurses:getmaxx window) 2) cl-ncurses:a_normal 2 nil)))
  #+cmu
  (let ((window (get-window self)))
    (cl-ncurses:init-pair 2 cursor-color background)
    (cl-ncurses:wattron window (cl-ncurses:color-pair 2))
    (cl-ncurses:mvwprintw (get-window self) (get-currow self) 0 "*")
    (cl-ncurses:wattron window (cl-ncurses:color-pair 1))))

(defmethod unmark-line ((self clmp-radio))
  (cl-ncurses:init-pair 3 cursor-color cl-ncurses:color_black)
  #+(or sbcl clisp)  
  (let ((window (get-window self)))
    (cl-ncurses:mvwchgat window (get-currow self) (get-curcol self) (- (cl-ncurses:getmaxx window) 2) cl-ncurses:a_normal 3 nil)))

(defmethod press-down ((self clmp-radio))
  (let ((curind (get-curind self)))
    (when (< curind (- (length (get-radarr self)) 1))
      (set-curind (+ curind 1) self)
      (let ((currow (get-currow self)))
	(if (< currow (- (cl-ncurses:getmaxy (get-window self)) +endoffset-row+ 1))
	    (progn (unmark-line self)
		   (set-currow (+ currow 1) self)
		   (render-window self))
	  (render-window self))))))

(defmethod press-up ((self clmp-radio))
  (let ((curind (get-curind self)))
    (when (> curind 0)
      (set-curind (- curind 1) self)
      (let ((currow (get-currow self)))
	(if (> currow +startoffset-row+)
	    (progn (unmark-line self)
		   (set-currow (- currow 1) self)
		   (render-window self))
	  (render-window self))))))

(defmethod press-pagedown ((self clmp-radio))
  (let ((curind (get-curind self)))
    (when (< curind (- (length (get-radarr self)) 1))
      (if (< curind (- (length (get-radarr self)) (- (cl-ncurses:getmaxy (get-window self)) +endoffset-row+ 1)))
	  (set-curind (+ curind (- (cl-ncurses:getmaxy (get-window self)) +endoffset-row+ 1)) self)
	(let ((offset (- (length (get-radarr self)) curind 1)))
	  (set-curind (+ curind offset) self)
	  (let ((currow (get-currow self)))
	    (unmark-line self)
	    (set-currow (+ currow offset) self))))
      (render-window self))))

(defmethod press-pageup ((self clmp-radio))
  (let ((curind (get-curind self)))
    (when (> curind 0)
      (if (>= curind (- (cl-ncurses:getmaxy (get-window self)) +endoffset-row+ 1))
	  (set-curind (- curind (- (cl-ncurses:getmaxy (get-window self)) +endoffset-row+ 1)) self)
	(let ((offset curind))
	  (set-curind (- curind offset) self)
	  (let ((currow (get-currow self)))
	    (unmark-line self)
	    (set-currow (- currow offset) self))))
      (render-window self))))
