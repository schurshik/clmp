;; CLMP
;; clmp-iface.lisp
;; Developer: Branitskiy Alexander <schurshick@yahoo.com>

(in-package :clmp)

(defconstant +key-resize+ 410)

(defconstant +key-down+ 258)

(defconstant +key-up+ 259)

(defconstant +key-left+ 260)

(defconstant +key-right+ 261)

(defconstant +key-pagedown+ 338)

(defconstant +key-pageup+ 339)

(defconstant +timeout-press-msec+ 1000)

(defvar *g-clmp-iface* nil)

(defstruct size-pos
  h ; the window heigth
  w ; the window width
  y ; the most upper row
  x ; the most left column
  )

(defun window-size-fmanager ()
  (setq s (make-size-pos))
  (setf (size-pos-h s) (- (cl-ncurses:getmaxy cl-ncurses:*stdscr*) 9)
	(size-pos-w s) (- (cl-ncurses:getmaxx cl-ncurses:*stdscr*) 2)
	(size-pos-y s) (+ (cl-ncurses:getbegy cl-ncurses:*stdscr*) 1)
	(size-pos-x s) (+ (cl-ncurses:getbegx cl-ncurses:*stdscr*) 1))
  (return-from window-size-fmanager s))

(defun window-size-player ()
  (setq s (make-size-pos))
  (setf (size-pos-h s) 7
	(size-pos-w s) (- (cl-ncurses:getmaxx cl-ncurses:*stdscr*) 2)
	(size-pos-y s) (- (cl-ncurses:getmaxy cl-ncurses:*stdscr*) 8)
	(size-pos-x s) (+ (cl-ncurses:getbegx cl-ncurses:*stdscr*) 1))
  (return-from window-size-player s))

(defclass clmp-iface ()
  ((window :initarg :window
	   :initform nil
	   :reader get-window
	   :writer set-window)
   (fmanager :initarg :fmanager
	     :initform nil
	     :reader get-fmanager
	     :writer set-fmanager)
   (player :initarg :player
	   :initform nil
	   :reader get-player
	   :writer set-player)
   (isabort :initarg :isabort
	    :initform nil
	    :reader get-isabort
	    :writer set-isabort)))

(defmethod create-window ((self clmp-iface) &key ((:height h) 0 h?) ((:width w) 0 w?) ((:row r) 0 r?) ((:column c) 0 c?))
  (set-window (cl-ncurses:initscr) self)
  (cl-ncurses:cbreak)
  (cl-ncurses:raw)
  (cl-ncurses:noecho)
  (cl-ncurses:curs-set 0)
  (cl-ncurses:keypad (get-window self) 1)
  (when (not (cl-ncurses:has-colors))
    (cl-ncurses:endwin)
    (error "the terminal does not support color"))
  (cl-ncurses:start-color)
  (colorize-window (get-window self))
  (if (and h? w? r? c?)
      (render-window self h w r c)
    (render-window self)))

(defmethod destroy-window ((self clmp-iface))
  (cl-ncurses:attroff (cl-ncurses:color-pair 1))
;  (cl-ncurses:delwin (get-window self))
  (cl-ncurses:endwin))

(defmethod create-fmanager ((self clmp-iface))
  (set-fmanager (make-instance 'clmp-fmanager) self)
  (let* ((s (window-size-fmanager))
	 (h (size-pos-h s))
	 (w (size-pos-w s))
	 (r (size-pos-y s))
	 (c (size-pos-x s)))
    (create (get-fmanager self) :height h :width w :row r :column c)))

(defmethod destroy-fmanager ((self clmp-iface))
  (destroy (get-fmanager self)))

(defmethod create-player ((self clmp-iface))
  (set-player (make-instance 'clmp-player) self)
  (let* ((s (window-size-player))
	 (h (size-pos-h s))
	 (w (size-pos-w s))
	 (r (size-pos-y s))
	 (c (size-pos-x s)))
    (create (get-player self) :height h :width w :row r :column c)))

(defmethod destroy-player ((self clmp-iface))
  (destroy (get-player self)))

(defmethod print-title ((self clmp-iface))
  (let* ((title "CLMP")
	 (y (cl-ncurses:getbegy (get-window self)))
	 (x (floor (/ (- (cl-ncurses:getmaxx (get-window self)) (length title)) 2))))
    (cl-ncurses:mvprintw y x title)))

(defmethod render-window ((self clmp-iface) &key ((:height h) 0 h?) ((:width w) 0 w?) ((:row r) 0 r?) ((:column c) 0 c?))
  (unless (and h? w? r? c?)
    (cl-ncurses:clear)
    (draw-frame (get-window self))
    (print-title self)
    (cl-ncurses:refresh)))

(defmethod render-fmanager ((self clmp-iface))
  (let* ((s (window-size-fmanager))
	 (h (size-pos-h s))
	 (w (size-pos-w s))
	 (r (size-pos-y s))
	 (c (size-pos-x s)))
    (render (get-fmanager self) :height h :width w :row r :column c)))

(defmethod render-player ((self clmp-iface))
  (let* ((s (window-size-player))
	 (h (size-pos-h s))
	 (w (size-pos-w s))
	 (r (size-pos-y s))
	 (c (size-pos-x s)))
    (render (get-player self) :height h :width w :row r :column c)))

(defmethod render ((self clmp-iface) &key ((:height h) 0 h?) ((:width w) 0 w?) ((:row r) 0 r?) ((:column c) 0 c?))
  (unless (and h? w? r? c?)
    (render-window self)
    (render-fmanager self)
    (render-player self)))

(defmethod resize-term ((self clmp-iface))
  (render self))

(defmethod press-down ((self clmp-iface))
  (press-down (get-fmanager self)))

(defmethod press-up ((self clmp-iface))
  (press-up (get-fmanager self)))

(defmethod press-enter ((self clmp-iface))
  (let* ((fmanager (get-fmanager self))
	 (dir (namestring (truename (nth (get-curind fmanager) (get-lsdir fmanager))))))
    (if (is-dir? dir)
	(gointo-curdir fmanager)
      (let ((file dir))
	(when (is-audio-file fmanager file t)
	    (start-playback (get-player self) file))))))

(defmethod up-dir ((self clmp-iface))
  (let ((fmanager (get-fmanager self)))
    (gointo-pardir fmanager)))

(defmethod press-pagedown ((self clmp-iface))
  (press-pagedown (get-fmanager self)))

(defmethod press-pageup ((self clmp-iface))
  (press-pageup (get-fmanager self)))

(defmethod play-pause ((self clmp-iface))
  (play-pause-file (get-player self)))

(defmethod stop ((self clmp-iface))
  (stop-file (get-player self)))

(defmethod change-volume ((self clmp-iface) param)
  (when (eq param '+)
    (inc-volume (get-player self))
    (return-from change-volume))
  (when (eq param '-)
    (dec-volume (get-player self))))

(defmethod change-timepos ((self clmp-iface) param)
  (when (eq param '>)
    (inc-timepos (get-player self))
    (return-from change-timepos))
  (when (eq param '<)
    (dec-timepos (get-player self))))

(defmethod prev-press-enter ((self clmp-iface))
  (press-up self)
  (press-enter self))

(defmethod next-press-enter ((self clmp-iface))
  (press-down self)
  (press-enter self))

(defmethod play-file ((self clmp-iface))
  (let* ((player (get-player self))
	 (cur-filename (get-play-filename player)))
    (cond ((eq play-mode 'play-once)
	   (return-from play-file))
	  ((eq play-mode 'play-repeatedly)
	   (let ((play-file cur-filename))
	     (start-playback player play-file)))
	  ((eq play-mode 'play-around)
	   (let* ((fmanager (get-fmanager self))
		  (play-file (find-next-prev-rand-audio-file fmanager cur-filename 'next)))
	     (start-playback player play-file)))
	  ((eq play-mode 'play-reverse)
	   (let* ((fmanager (get-fmanager self))
		  (play-file (find-next-prev-rand-audio-file fmanager cur-filename 'prev)))
	     (start-playback player play-file)))
	  ((eq play-mode 'play-random)
	   (let* ((fmanager (get-fmanager self))
		  (play-file (find-next-prev-rand-audio-file fmanager cur-filename 'rand)))
	     (start-playback player play-file))))))

(defmethod process-keys ((self clmp-iface))
  #+sbcl
  (sb-sys:enable-interrupt sb-posix:sigint #'(lambda (sig info context) (declare (ignore sig info context)) (set-isabort t *g-clmp-iface*)))
  (loop (not nil)
	(cl-ncurses:wtimeout (get-window self) +timeout-press-msec+)
	(let ((key (cl-ncurses:getch)))
	  (cond ((= key (char-int #\h))
		 (up-dir self))
		((= key (char-int #\k))
		 (press-up self))
		((= key (char-int #\j))
		 (press-down self))
		((or (= key (char-int #\linefeed)) (= key (char-int #\l)))
		 (press-enter self))
		((= key +key-pagedown+)
		 (press-pagedown self))
		((= key +key-pageup+)
		 (press-pageup self))
		((= key (char-int #\p))
		 (play-pause self))
		((= key (char-int #\s))
		 (stop self))
		((= key (char-int #\`))
		 (prev-press-enter self))
		((= key (char-int #\tab))
		 (next-press-enter self))
		((= key +key-up+)
		 (change-volume self '+))
		((= key +key-down+)
		 (change-volume self '-))
		((= key +key-right+)
		 (change-timepos self '>))
		((= key +key-left+)
		 (change-timepos self '<))
		((= key +key-resize+)
		 (resize-term self))
		((or (= key (char-int #\q)) (and (= key cl-ncurses:ERR) (get-isabort self)))
		 (return))
		((get-is-eof (get-player self))
		 (play-file self))))))

(defmethod create ((self clmp-iface) &key ((:height h) 0 h?) ((:width w) 0 w?) ((:row r) 0 r?) ((:column c) 0 c?))
  (setq *g-clmp-iface* self)
  (if (and h? w? r? c?)
      (create-window self :height h :width w :row r :column c)
    (create-window self))
  (create-fmanager self)
  (create-player self))

(defmethod destroy ((self clmp-iface))
  (destroy-player self)
  (destroy-fmanager self)
  (destroy-window self))
