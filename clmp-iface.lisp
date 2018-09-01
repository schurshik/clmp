;; CLMP
;; clmp-iface.lisp
;; Developer: Branitskiy Alexander <schurshik@yahoo.com>

(in-package :clmp)

(defconstant +key-resize+ 410)

(defconstant +key-down+ 258)

(defconstant +key-up+ 259)

(defconstant +key-left+ 260)

(defconstant +key-right+ 261)

(defconstant +key-pagedown+ 338)

(defconstant +key-pageup+ 339)

(defconstant +key-escape+ 27)

(defconstant +timeout-press-msec+ 1000)

(defvar *g-clmp-iface* nil)

(defstruct size-pos
  h ; the window heigth
  w ; the window width
  y ; the most upper row
  x ; the most left column
  )

(defun window-size-fmanager (&key ((:pos p) 'central p?))
  (setq s (make-size-pos))
  (cond ((or (not p?) (eq p 'central))
         (setf (size-pos-h s) (- (cl-ncurses:getmaxy cl-ncurses:*stdscr*) 9)
               (size-pos-w s) (- (cl-ncurses:getmaxx cl-ncurses:*stdscr*) 2)
               (size-pos-y s) (+ (cl-ncurses:getbegy cl-ncurses:*stdscr*) 1)
               (size-pos-x s) (+ (cl-ncurses:getbegx cl-ncurses:*stdscr*) 1)))
        ((eq p 'left)
         (setf (size-pos-h s) (- (cl-ncurses:getmaxy cl-ncurses:*stdscr*) 9)
               (size-pos-w s) (floor (/ (- (cl-ncurses:getmaxx cl-ncurses:*stdscr*) 2) 2))
               (size-pos-y s) (+ (cl-ncurses:getbegy cl-ncurses:*stdscr*) 1)
               (size-pos-x s) (+ (cl-ncurses:getbegx cl-ncurses:*stdscr*) 1)))
        ((eq p 'right)
         (setf (size-pos-h s) (- (cl-ncurses:getmaxy cl-ncurses:*stdscr*) 9)
               (size-pos-w s) (- (- (cl-ncurses:getmaxx cl-ncurses:*stdscr*) 2)
                                 (floor (/ (- (cl-ncurses:getmaxx cl-ncurses:*stdscr*) 2) 2)) 1)
               (size-pos-y s) (+ (cl-ncurses:getbegy cl-ncurses:*stdscr*) 1)
               (size-pos-x s) (+ (cl-ncurses:getbegx cl-ncurses:*stdscr*) 1
                                 (floor (/ (- (cl-ncurses:getmaxx cl-ncurses:*stdscr*) 2) 2)) 1)))
        ((eq p 'top)
         (setf (size-pos-h s) (floor (- (cl-ncurses:getmaxy cl-ncurses:*stdscr*) 9) 2)
               (size-pos-w s) (- (cl-ncurses:getmaxx cl-ncurses:*stdscr*) 2)
               (size-pos-y s) (+ (cl-ncurses:getbegy cl-ncurses:*stdscr*) 1)
               (size-pos-x s) (+ (cl-ncurses:getbegx cl-ncurses:*stdscr*) 1)))
        ((eq p 'bottom)
         (setf (size-pos-h s) (- (cl-ncurses:getmaxy cl-ncurses:*stdscr*) 9 (floor (- (cl-ncurses:getmaxy cl-ncurses:*stdscr*) 9) 2) 1)
               (size-pos-w s) (- (cl-ncurses:getmaxx cl-ncurses:*stdscr*) 2)
               (size-pos-y s) (+ (+ (cl-ncurses:getbegy cl-ncurses:*stdscr*) 1) (floor (- (cl-ncurses:getmaxy cl-ncurses:*stdscr*) 9) 2) 1)
               (size-pos-x s) (+ (cl-ncurses:getbegx cl-ncurses:*stdscr*) 1))))
  (return-from window-size-fmanager s))

(defun window-size-radio ()
  (setq s (make-size-pos))
  (setf (size-pos-h s) (- (cl-ncurses:getmaxy cl-ncurses:*stdscr*) 9)
	(size-pos-w s) (- (cl-ncurses:getmaxx cl-ncurses:*stdscr*) 2)
	(size-pos-y s) (+ (cl-ncurses:getbegy cl-ncurses:*stdscr*) 1)
	(size-pos-x s) (+ (cl-ncurses:getbegx cl-ncurses:*stdscr*) 1))
  (return-from window-size-radio s))

(defun window-size-player ()
  (setq s (make-size-pos))
  (setf (size-pos-h s) 7
	(size-pos-w s) (- (cl-ncurses:getmaxx cl-ncurses:*stdscr*) 2)
	(size-pos-y s) (- (cl-ncurses:getmaxy cl-ncurses:*stdscr*) 8)
	(size-pos-x s) (+ (cl-ncurses:getbegx cl-ncurses:*stdscr*) 1))
  (return-from window-size-player s))

(defun window-size-whelp ()
  (setq s (make-size-pos))
  (setf (size-pos-h s) (- (cl-ncurses:getmaxy cl-ncurses:*stdscr*) 2)
	(size-pos-w s) (- (cl-ncurses:getmaxx cl-ncurses:*stdscr*) 2)
	(size-pos-y s) (+ (cl-ncurses:getbegy cl-ncurses:*stdscr*) 1)
	(size-pos-x s) (+ (cl-ncurses:getbegx cl-ncurses:*stdscr*) 1))
  (return-from window-size-whelp s))

(defclass clmp-iface ()
  ((window :initarg :window
	   :initform nil
	   :reader get-window
	   :writer set-window)
   (fmanagers :initarg :fmanagers
	     :initform nil
	     :reader get-fmanagers
	     :writer set-fmanagers)
   (fmind :initarg :fmind
          :initform 0
          :reader get-fmind
          :writer set-fmind)
   (radio :initarg :radio
	  :initform nil
	  :reader get-radio
	  :writer set-radio)
   (player :initarg :player
	   :initform nil
	   :reader get-player
	   :writer set-player)
   (whelp :initarg :whelp
          :initform nil
          :reader get-whelp
          :writer set-whelp)
   (onwhelp :initarg :onwhelp
            :initform nil
            :reader get-onwhelp
            :writer set-onwhelp)
   (mode :initarg :mode
	 :initform 'fmanager
	 :reader get-mode
	 :writer set-mode)
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

(defmethod create-fmanagers ((self clmp-iface))
  (let* ((pos-lst
          (cond ((eq file-manager 'one-panel)
                 (list 'central))
                ((eq file-manager 'two-vertical-panels)
                 (list 'left 'right))
                ((eq file-manager 'two-horizontal-panels)
                 (list 'top 'bottom))
                (t (error "incorrect value of file-manager"))))
         (pos-lst-len (list-length pos-lst)))
    (set-fmanagers (make-array pos-lst-len :initial-contents (make-list pos-lst-len :initial-element nil)) self)
    (let ((i 0))
      (dolist (p pos-lst)
        (setf (aref (get-fmanagers self) i) (make-instance 'clmp-fmanager))
        (let* ((s (window-size-fmanager :pos p))
               (h (size-pos-h s))
               (w (size-pos-w s))
               (r (size-pos-y s))
               (c (size-pos-x s)))
          (create (aref (get-fmanagers self) i) :height h :width w :row r :column c :line (= i (get-fmind self))))
        (incf i)))))

(defmethod destroy-fmanagers ((self clmp-iface))
  (loop for i from 0 to (- (length (get-fmanagers self)) 1) do
       (destroy (aref (get-fmanagers self) i))))

(defmethod create-radio ((self clmp-iface))
  (set-radio (make-instance 'clmp-radio) self)
  (let* ((s (window-size-radio))
	 (h (size-pos-h s))
	 (w (size-pos-w s))
	 (r (size-pos-y s))
	 (c (size-pos-x s)))
    (create (get-radio self) :height h :width w :row r :column c)))

(defmethod destroy-radio ((self clmp-iface))
  (destroy (get-radio self)))

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

(defmethod create-whelp ((self clmp-iface))
  (set-whelp (make-instance 'clmp-whelp) self)
  (let* ((s (window-size-whelp))
	 (h (size-pos-h s))
	 (w (size-pos-w s))
	 (r (size-pos-y s))
	 (c (size-pos-x s)))
    (create (get-whelp self) :height h :width w :row r :column c)))

(defmethod destroy-whelp ((self clmp-iface))
  (destroy (get-whelp self)))

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

(defmethod render-fmanagers ((self clmp-iface))
  (let ((pos-lst
         (cond ((eq file-manager 'one-panel)
                (list 'central))
               ((eq file-manager 'two-vertical-panels)
                (list 'left 'right))
               ((eq file-manager 'two-horizontal-panels)
                (list 'top 'bottom))
               (t (error "incorrect value of file-manager")))))
    (let ((i 0))
      (dolist (p pos-lst)
        (let* ((s (window-size-fmanager :pos p))
               (h (size-pos-h s))
               (w (size-pos-w s))
               (r (size-pos-y s))
               (c (size-pos-x s)))
          (render (aref (get-fmanagers self) i) :height h :width w :row r :column c :line (= i (get-fmind self))))
        (incf i)))))

(defmethod render-radio ((self clmp-iface))
  (let* ((s (window-size-radio))
	 (h (size-pos-h s))
	 (w (size-pos-w s))
	 (r (size-pos-y s))
	 (c (size-pos-x s)))
    (render (get-radio self) :height h :width w :row r :column c)))

(defmethod render-player ((self clmp-iface))
  (let* ((s (window-size-player))
	 (h (size-pos-h s))
	 (w (size-pos-w s))
	 (r (size-pos-y s))
	 (c (size-pos-x s)))
    (render (get-player self) :height h :width w :row r :column c)))

(defmethod render-whelp ((self clmp-iface))
  (let* ((s (window-size-whelp))
	 (h (size-pos-h s))
	 (w (size-pos-w s))
	 (r (size-pos-y s))
	 (c (size-pos-x s)))
    (render (get-whelp self) :height h :width w :row r :column c)))

(defmethod render ((self clmp-iface) &key ((:height h) 0 h?) ((:width w) 0 w?) ((:row r) 0 r?) ((:column c) 0 c?))
  (unless (and h? w? r? c?)
    (render-window self)
    (if (get-onwhelp self)
        (render-whelp self)
      (progn
        (cond ((eq (get-mode self) 'fmanager)
               (render-fmanagers self))
              ((eq (get-mode self) 'radio)
               (render-radio self)))
        (render-player self)))))

(defmethod resize-term ((self clmp-iface))
  (render self))

(defmethod press-down ((self clmp-iface))
  (cond ((eq (get-mode self) 'fmanager)
	 (press-down (aref (get-fmanagers self) (get-fmind self))))
	((eq (get-mode self) 'radio)
	 (press-down (get-radio self)))))

(defmethod press-up ((self clmp-iface))
  (cond ((eq (get-mode self) 'fmanager)
	 (press-up (aref (get-fmanagers self) (get-fmind self))))
	((eq (get-mode self) 'radio)
	 (press-up (get-radio self)))))

(defmethod press-enter ((self clmp-iface))
  (cond ((eq (get-mode self) 'fmanager)
	 (let* ((fmanager (aref (get-fmanagers self) (get-fmind self)))
		(dir (namestring (truename (nth (get-curind fmanager) (get-lsdir fmanager))))))
	   (if (is-dir? dir)
	       (gointo-curdir fmanager)
	     (let ((file dir))
	       (when (is-audio-video-file fmanager file t)
		 (start-playback (get-player self) file))))))
	((eq (get-mode self) 'radio)
	 (start-stream (get-player self) (radio-station-addr (aref (get-radarr (get-radio self)) (get-curind (get-radio self))))))))

(defmethod up-dir ((self clmp-iface))
  (when (eq (get-mode self) 'fmanager)
    (let ((fmanager (aref (get-fmanagers self) (get-fmind self))))
      (gointo-pardir fmanager))))

(defmethod press-pagedown ((self clmp-iface))
  (cond ((eq (get-mode self) 'fmanager)
	 (press-pagedown (aref (get-fmanagers self) (get-fmind self))))
	((eq (get-mode self) 'radio)
	 (press-pagedown (get-radio self)))))

(defmethod press-pageup ((self clmp-iface))
  (cond ((eq (get-mode self) 'fmanager)
	 (press-pageup (aref (get-fmanagers self) (get-fmind self))))
	((eq (get-mode self) 'radio)
	 (press-pageup (get-radio self)))))

(defmethod play-pause ((self clmp-iface))
  (when (eq (get-mode self) 'fmanager)
    (play-pause-file (get-player self))))

(defmethod stop ((self clmp-iface))
  (when (eq (get-mode self) 'fmanager)
    (stop-file (get-player self))))

(defmethod change-volume ((self clmp-iface) param)
  (when (eq param '+)
    (inc-volume (get-player self))
    (return-from change-volume))
  (when (eq param '-)
    (dec-volume (get-player self))))

(defmethod change-timepos ((self clmp-iface) param)
  (when (eq (get-mode self) 'fmanager)
    (when (eq param '>)
      (inc-timepos (get-player self))
      (return-from change-timepos))
    (when (eq param '<)
      (dec-timepos (get-player self)))))

(defmethod prev-press-enter ((self clmp-iface))
  (press-up self)
  (press-enter self))

(defmethod next-press-enter ((self clmp-iface))
  (press-down self)
  (press-enter self))

(defmethod switch-panel ((self clmp-iface))
  (when (or (eq file-manager 'two-vertical-panels) (eq file-manager 'two-horizontal-panels))
    (set-fmind (mod (+ (get-fmind self) 1) 2) self)
    (render-fmanagers self)))

(defmethod play-file ((self clmp-iface))
  (when (eq (get-mode self) 'fmanager)
    (let* ((player (get-player self))
	   (cur-filename (get-play-filename player)))
      (cond ((eq play-mode 'play-once)
	     (return-from play-file))
	    ((eq play-mode 'play-repeatedly)
	     (let ((play-file cur-filename))
	       (start-playback player play-file)))
	    ((eq play-mode 'play-around)
	     (let* ((fmanager (aref (get-fmanagers self) (get-fmind self)))
		    (play-file (find-next-prev-rand-audio-video-file fmanager cur-filename 'next)))
	       (start-playback player play-file)))
	    ((eq play-mode 'play-reverse)
	     (let* ((fmanager (aref (get-fmanagers self) (get-fmind self)))
		    (play-file (find-next-prev-rand-audio-video-file fmanager cur-filename 'prev)))
	       (start-playback player play-file)))
	    ((eq play-mode 'play-random)
	     (let* ((fmanager (aref (get-fmanagers self) (get-fmind self)))
		    (play-file (find-next-prev-rand-audio-video-file fmanager cur-filename 'rand)))
	       (start-playback player play-file)))))))

(defmethod show-fmanagers ((self clmp-iface))
  (when (and (eq (get-mode self) 'fmanager) (not (get-onwhelp self)))
    (return-from show-fmanagers))
  (set-mode 'fmanager self)
  (render-fmanagers self)
  (when (get-onwhelp self)
    (set-onwhelp nil self)
    (render-player self)))

(defmethod show-radio ((self clmp-iface))
  (when (and (eq (get-mode self) 'radio) (not (get-onwhelp self)))
    (return-from show-radio))
  (set-mode 'radio self)
  (render-radio self)
  (when (get-onwhelp self)
    (set-onwhelp nil self)
    (render-player self)))

(defmethod show-whelp ((self clmp-iface))
  (when (get-onwhelp self)
    (return-from show-whelp))
  (set-onwhelp t self)
  (render-whelp self))

(defmethod hide-whelp ((self clmp-iface))
  (when (not (get-onwhelp self))
    (return-from hide-whelp))
  (set-onwhelp nil self)
  (cond ((eq (get-mode self) 'fmanager)
	 (render-fmanagers self))
	((eq (get-mode self) 'radio)
	 (render-radio self)))
  (render-player self))

(defmethod process-keys ((self clmp-iface))
  #+sbcl
  (sb-sys:enable-interrupt sb-posix:sigint #'(lambda (sig info context) (declare (ignore sig info context)) (set-isabort t *g-clmp-iface*)))
  (loop (not nil)
	#+(or sbcl clisp)
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
		((= key (char-int #\1))
		 (prev-press-enter self))
        ((= key (char-int #\2))
		 (next-press-enter self))
		((= key (char-int #\tab))
		 (switch-panel self))
		((= key +key-up+)
		 (change-volume self '+))
		((= key +key-down+)
		 (change-volume self '-))
		((= key +key-right+)
		 (change-timepos self '>))
		((= key +key-left+)
		 (change-timepos self '<))
		((= key (char-int #\F))
		 (show-fmanagers self))
		((= key (char-int #\R))
		 (show-radio self))
		((= key (char-int #\H))
		 (show-whelp self))
		((= key +key-escape+)
		 (hide-whelp self))
		((= key +key-resize+)
		 (resize-term self))
		((or (= key (char-int #\q)) (and (= key cl-ncurses:ERR) (get-isabort self)))
		 (return))
		((track-info-iseof (get-track (get-player self))) ;;(get-is-eof (get-player self))
		 (play-file self))))))

(defmethod create ((self clmp-iface) &key ((:height h) 0 h?) ((:width w) 0 w?) ((:row r) 0 r?) ((:column c) 0 c?))
  (setq *g-clmp-iface* self)
  (if (and h? w? r? c?)
      (create-window self :height h :width w :row r :column c)
    (create-window self))
  (create-fmanagers self)
  (create-radio self)
  (create-player self)
  (create-whelp self))

(defmethod destroy ((self clmp-iface))
  (destroy-player self)
  (destroy-radio self)
  (destroy-fmanagers self)
  (destroy-whelp self)
  (destroy-window self))
