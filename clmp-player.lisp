;; CLMP
;; clmp-player.lisp
;; Developer: Branitskiy Alexander <schurshick@yahoo.com>

(in-package :clmp)

(defconstant +in-fifo-path+ "/tmp/clmp-in.fifo")

(defconstant +out-fifo-path+ "/tmp/clmp-out.fifo")

(defconstant +alarm-time+ 1)

(defvar *g-clmp-player* nil)

#+clisp
(defvar *g-sleep-thread* nil)

(defstruct track-info
  (artist "Unknown")
  (title "Unknown")
  (bitrate 0)
  (genre "Unknown")
  (year "Unknown")
  (timepos 0)
  (percpos 0)
  (timelen 0)
  (filename "Unknown")
  (volume 0)
  (iseof nil))

(defclass clmp-player ()
  ((left-window :initarg left-window
		:initform nil
		:reader get-left-window
		:writer set-left-window)
   (right-window :initarg :rigth-window
		 :initform nil
		 :reader get-right-window
		 :writer set-right-window)
   (in-fifo :initarg :in-fifo
	    :initform nil
	    :reader get-in-fifo
	    :writer set-in-fifo)
   (out-fifo :initarg :out-fifo
	     :initform nil
	     :reader get-out-fifo
	     :writer set-out-fifo)
   #+clisp
   (out-fd :initarg :out-fd
	   :initform nil
	   :reader get-out-fd
	   :writer set-out-fd)
   #+(or sbcl cmu)
   (mprocess :initarg :mprocess
	     :initform 0
	     :reader get-mprocess
	     :writer set-mprocess)
   #+clisp
   (mthread :initarg :mthread
	    :initform nil
	    :reader get-mthread
	    :writer set-mthread)
   #+(or sbcl clisp)
   (thread :initarg :thread
	   :initform nil
	   :reader get-thread
	   :writer set-thread)
   (state :initarg :state
	  :initform 'stop
	  :reader get-state
	  :writer set-state)
   (track :initarg :track
	  :initform (make-track-info)
	  :reader get-track
	  :writer set-track)
   (play-filename :initarg :play-filename
		  :initform nil
		  :reader get-play-filename
		  :writer set-play-filename)
   #+cmu
   (proc-handler :initarg :proc-handler
                 :initform (make-instance 'clmp-libs:clmp-processhandler)
                 :reader get-proc-handler
                 :writer set-proc-handler)))

(defmethod create-windows ((self clmp-player) &key ((:height h) 0 h?) ((:width w) 0 w?) ((:row r) 0 r?) ((:column c) 0 c?))
  (when (and h? w? r? c?)
    (create-left-window self :height h :width (floor (/ w 2)) :row r :column c)
    (create-right-window self :height h :width (floor (/ w 2)) :row r :column (+ c (- w (floor (/ w 2)))))))

(defmethod destroy-windows ((self clmp-player))
  (destroy-left-window self)
  (destroy-right-window self))

(defmethod render-windows ((self clmp-player) &key ((:height h) 0 h?) ((:width w) 0 w?) ((:row r) 0 r?) ((:column c) 0 c?))
  (when (and h? w? r? c?)
    (render-left-window self :height h :width (floor (/ w 2)) :row r :column c)
    (render-right-window self :height h :width (floor (/ w 2)) :row r :column (+ c (- w (floor (/ w 2)))))))

(defmethod render ((self clmp-player) &key ((:height h) 0 h?) ((:width w) 0 w?) ((:row r) 0 r?) ((:column c) 0 c?))
  (if (and h? w? r? c?)
      (render-windows self :height h :width w :row r :column c)
    (render-windows self)))

(defmethod create-left-window ((self clmp-player) &key ((:height h) 0 h?) ((:width w) 0 w?) ((:row r) 0 r?) ((:column c) 0 c?))
  (when (and h? w? r? c?)
    (set-left-window (cl-ncurses:newwin h w r c) self))
  (colorize-window (get-left-window self))
  (render-left-window self))

(defmethod destroy-left-window ((self clmp-player))
  (cl-ncurses:delwin (get-left-window self)))

(defmethod draw-volume-dial ((self clmp-player))
  (let* ((left-window (get-left-window self))
	 (first-x 2)
	 (last-x (- (cl-ncurses:getmaxx left-window) 15))
	 (x-len (- last-x first-x 2))
	 (x-pos (+ first-x 1 (floor (* x-len (/ (track-info-volume (get-track self)) 100)))))
	 (y 2))
    (cl-ncurses:wmove left-window y first-x)
    (loop for i from first-x to last-x do
	  (cond ((= i first-x) (cl-ncurses:wprintw left-window "("))
		((= i last-x) (cl-ncurses:wprintw left-window ")"))
		((= i x-pos) (cl-ncurses:wprintw left-window "#"))
		(t (cl-ncurses:wprintw left-window ":"))))
    (if (eq (get-state self) 'stop)
	(cl-ncurses:mvwprintw left-window y (+ last-x 2) (format nil "0/0"))
      (let ((volume (track-info-volume (get-track self))))
	(cl-ncurses:mvwprintw left-window y (+ last-x 2) (format nil "~3,1f/100" volume))))))

(defmethod draw-play-position ((self clmp-player))
  (let* ((left-window (get-left-window self))
	 (first-x 2)
	 (last-x (- (cl-ncurses:getmaxx left-window) 15))
	 (x-len (- last-x first-x 2))
	 (x-pos (+ first-x 1 (floor (* x-len (/ (track-info-percpos (get-track self)) 100)))))
	 (y (- (cl-ncurses:getmaxy left-window) 3)))
    (cl-ncurses:wmove left-window y first-x)
    (loop for i from first-x to last-x do
	  (cond ((= i first-x) (cl-ncurses:wprintw left-window "["))
		((= i last-x) (cl-ncurses:wprintw left-window "]"))
		((= i x-pos) (cl-ncurses:wprintw left-window "$"))
		(t (cl-ncurses:wprintw left-window "="))))
    (if (eq (get-state self) 'stop)
	(cl-ncurses:mvwprintw left-window y (+ last-x 2) "0/0")
      (let ((timepos (track-info-timepos (get-track self)))
	    (timelen (track-info-timelen (get-track self))))
	(cl-ncurses:mvwprintw left-window y (+ last-x 2) (format nil "~3,1f/~3,1f" timepos timelen))))))

(defmethod render-left-window ((self clmp-player) &key ((:height h) 0 h?) ((:width w) 0 w?) ((:row r) 0 r?) ((:column c) 0 c?))
  (let ((left-window (get-left-window self)))
    (cl-ncurses:wclear (get-left-window self))
    (when (and h? w? r? c?)
      (cl-ncurses:wresize left-window h w)
      (cl-ncurses:mvwin left-window r c))
    (draw-frame left-window)
    (draw-volume-dial self)
    (draw-play-position self)
    (cl-ncurses:wrefresh left-window)))

(defmethod create-right-window ((self clmp-player) &key ((:height h) 0 h?) ((:width w) 0 w?) ((:row r) 0 r?) ((:column c) 0 c?))
  (when (and h? w? r? c?)
    (set-right-window (cl-ncurses:newwin h w r c) self))
  (colorize-window (get-right-window self))
  (render-right-window self))

(defmethod destroy-right-window ((self clmp-player))
  (cl-ncurses:delwin (get-right-window self)))

(defmethod render-right-window ((self clmp-player) &key ((:height h) 0 h?) ((:width w) 0 w?) ((:row r) 0 r?) ((:column c) 0 c?))
  (let ((right-window (get-right-window self)))
    (cl-ncurses:wclear right-window)
    (when (and h? w? r? c?)
      (cl-ncurses:wresize right-window h w)
      (cl-ncurses:mvwin right-window r c))
    (draw-frame right-window)
    (if (not (eq (get-state self) 'stop))
	(let* ((track (get-track self))
	       (artist (track-info-artist track))
	       (title (track-info-title track))
	       (bitrate (track-info-bitrate track))
	       (genre (track-info-genre track))
	       (year (track-info-year track))
	       (filename (track-info-filename track)))
	  (cl-ncurses:mvwprintw right-window 1 1 (format nil "TRACK: ~a - ~a (~a kbps)" artist title bitrate))
	  (cl-ncurses:mvwprintw right-window 3 1 (format nil "INFO:  ~a <~a>" genre year))
	  (cl-ncurses:mvwprintw right-window 5 1 (format nil "FILE:  ~a" filename))))
    (cl-ncurses:wrefresh right-window)))
  
(defmethod create-in-fifo ((self clmp-player))
  (if (probe-file 
       #+sbcl
       (make-pathname :directory +in-fifo-path+)
       #+(or clisp cmu)
       (pathname +in-fifo-path+))
      (delete-file +in-fifo-path+))
  #+sbcl
  (let ((mode (logior sb-posix:s-iwusr sb-posix:s-irusr)))
    (sb-posix:mkfifo +in-fifo-path+ mode))
  #+clisp
  (ext:run-program "mkfifo" :arguments (list +in-fifo-path+ "--mode=0600"))
  #+cmu
  (ext:run-program "mkfifo" (list +in-fifo-path+ "--mode=0600")))

(defmethod destroy-in-fifo ((self clmp-player))
  (if (probe-file
       #+sbcl
       (make-pathname :directory +in-fifo-path+)
       #+(or clisp cmu)
       (pathname +in-fifo-path+))
      (delete-file +in-fifo-path+)))

(defmethod create-out-fifo ((self clmp-player))
  (if (probe-file
       #+sbcl
       (make-pathname :directory +out-fifo-path+)
       #+(or clisp cmu)
       (pathname +out-fifo-path+))
      (delete-file +out-fifo-path+))
  #+sbcl
  (let ((mode (logior sb-posix:s-iwusr sb-posix:s-irusr)))
    (sb-posix:mkfifo +out-fifo-path+ mode)
    (let ((out (open +out-fifo-path+ :direction :io :if-exists :supersede)))
      (when (null out)
	(error "can not open file"))
      (let ((flags (sb-posix:fcntl out sb-posix:f-getfl)))
	(when (< flags 0)
	  (error "can not call fcntl"))
	(let ((flags (logior flags sb-posix:o-nonblock)))
	  (when (< (sb-posix:fcntl out sb-posix:f-setfl flags) 0)
	    (error "can not call fcntl"))
	  (set-out-fifo out self)))))
  #+clisp
  (progn
    (ext:run-program "mkfifo" :arguments (list +out-fifo-path+ "--mode=0600"))
    (let* ((fd (posix:fopen +out-fifo-path+ "r+"))
	   (f-getfl 3)
	   (f-setfl 4)
	   (o-nonblock 2048)
	   (flags (cffi:foreign-funcall "fcntl" :int (posix:fileno fd) :int f-getfl :pointer (cffi:null-pointer) :int)))
      ;(cffi:foreign-funcall "fcntl" :int (posix:fileno fd) :int f-setfl :int (logior flags o-nonblock) :int)
      (set-out-fifo (ext:make-stream (posix:fileno fd) :direction :io) self)
      (set-out-fd fd self)))
  #+cmu
  (progn
    (ext:run-program "mkfifo" (list +out-fifo-path+ "--mode=0600"))
    (set-out-fifo (open +out-fifo-path+ :direction :io :if-exists :supersede) self)))

(defmethod destroy-out-fifo ((self clmp-player))
  (close (get-out-fifo self))
  #+clisp
  (posix:fclose (get-out-fd self))
  (if (probe-file
       #+sbcl
       (make-pathname :directory +out-fifo-path+)
       #+(or clisp cmu)
       (pathname +out-fifo-path+))
      (delete-file +out-fifo-path+)))

(defmethod exec-player ((self clmp-player))
  #+sbcl
  (cond ((eq player 'mplayer)
	 (sb-ext:run-program +mplayer-bin+ (list "-idle" "-slave" "-msglevel" "all=1:statusline=5:global=6" "-input" (format nil "file=~a" +in-fifo-path+) "2>/dev/null") :output (get-out-fifo self)))
	((eq player 'mpv)
	 (sb-ext:run-program +mpv-bin+ (list "--idle=yes" "--no-input-terminal" (format nil "--input-file=~a" +in-fifo-path+) "2>/dev/null") :output (get-out-fifo self)))
	(t (error (format nil "non valid player"))))
  #+clisp
  (let ((cmd (cond ((eq player 'mplayer)
		    (concatenate 'string +mplayer-bin+ " -idle" " -slave" " -msglevel" " all=1:statusline=5:global=6" " -input" (format nil " file=~a" +in-fifo-path+) " 2>/dev/null"))
		   ((eq player 'mpv)
		    (concatenate 'string +mpv-bin+ " --idle=yes" " --keep-open=yes" " --no-input-terminal" (format nil " --input-file=~a" +in-fifo-path+) " 2>/dev/null"))
		   (t (error (format nil "non valid player"))))))
    (with-open-stream (istream (ext:make-pipe-input-stream cmd))
    		      (loop (not nil) (loop for line = (read-line istream nil nil) while line do (write-line line (get-out-fifo self))))))
  #+cmu
  (cond ((eq player 'mplayer)
	 (setf ret-val (ext:run-program +mplayer-bin+ (list "-idle" "-slave" "-msglevel" "all=1:statusline=5:global=6" "-input" (format nil "file=~a" +in-fifo-path+) "2>/dev/null") :output (get-out-fifo self) :wait nil)))
	((eq player 'mpv)
	 (setf ret-val (ext:run-program +mpv-bin+ (list "--idle=yes" "--no-input-terminal" (format nil "--input-file=~a" +in-fifo-path+) "2>/dev/null") :output (get-out-fifo self) :wait nil)))
	(t (error (format nil "non valid player"))))
  #+sbcl
  (sb-ext:quit :unix-status 0)
  #+clisp
  (ext:quit 0)
  #+cmu
  (return-from exec-player ret-val))

#+sbcl
(defun signal-handle (sig info context)
  (declare (ignore info context))
  (when (= sig sb-posix:sigalrm)
    (when (eq (get-state *g-clmp-player*) 'play)
      (get-timepos *g-clmp-player*)
      (get-percpos *g-clmp-player*)
      (get-iseof *g-clmp-player*)
      (sb-posix:alarm +alarm-time+))))

#+clisp
(defun create-sleep-thread ()
  (setq *g-sleep-thread* (bordeaux-threads:make-thread (lambda () (catch 'break-sleep-thread
								    (loop (not nil)
									  (when (eq (get-state *g-clmp-player*) 'play)
									    (get-timepos *g-clmp-player*)
									    (get-percpos *g-clmp-player*)
									    (get-iseof *g-clmp-player*)
									    (sleep +alarm-time+))))))))

#+(or sbcl cmu)
(defmethod create-mprocess ((self clmp-player))
  #+sbcl
  (let ((pid (sb-posix:fork)))
    (cond ((zerop pid) (exec-player self))
	  ((plusp pid) (set-mprocess pid self))
	  (t (error "can not create process")))
    (sb-sys:enable-interrupt sb-posix:sigalrm #'signal-handle))
  #+cmu
  (set-mprocess (exec-player self) self))

#+clisp
(defmethod create-mthread ((self clmp-player))
  (set-mthread (bordeaux-threads:make-thread (lambda () (catch 'break-mthread (progn (exec-player self))))) self)
  (create-sleep-thread))

#+(or sbcl cmu)
(defmethod destroy-mprocess ((self clmp-player))
  (quit-player self)
  #+sbcl
  (sb-posix:waitpid (get-mprocess self) 0)
  #+cmu
  (ext:process-kill (get-mprocess self) 2))

#+clisp
(defmethod destroy-mthread ((self clmp-player))
  (quit-player self)
  (bordeaux-threads:interrupt-thread (get-mthread self) (lambda () (throw 'break-mthread 'interrupted-mthread)))
  ;#+cmu (green-threads:destroy-thread (get-mthread self))
  (bordeaux-threads:join-thread (get-mthread self))
  (bordeaux-threads:interrupt-thread *g-sleep-thread* (lambda () (throw 'break-sleep-thread 'interrupted-sleep-thread)))
  ;#+cmu (green-threads:destroy-thread *g-sleep-thread*)
  (bordeaux-threads:join-thread *g-sleep-thread*))

(defmethod #+(or sbcl clisp) create-thread #+cmu parse-output ((self clmp-player))
  ; http://www.mplayerhq.hu/DOCS/tech/slave.txt
  ; mpv.io/manual/master/
  (let ((func (lambda ()
		(let ((read-fifo (get-out-fifo self))
		      (get-value #'(lambda (string regex)
				     (let ((value (nth-value 1 (cl-ppcre:scan-to-strings regex string))))
				       (if (not (null value))
					   (aref value 0)
					 nil)))))
                  (block loop-block
		    (loop (not nil)
			  (block iter
			    (let ((string (read-line read-fifo)))
			      (let* ((regex (cond ((eq player 'mplayer) "^ANS_META_ARTIST='(.*)'")
						  ((eq player 'mpv) "ANS_metadata/artist=(.*)$")
						  (t (error "non valid player"))))
				     (artist (funcall get-value string regex)))
				;(let ((out_ (open "/tmp/test.out" :direction :io :if-exists :append)))
				;  (format out_ "in loop: ~a~%" string)
				;  (close out_))
				(when (not (null artist))
				  (set-artist self artist)
				  (return-from iter)))
			      (let* ((regex (cond ((eq player 'mplayer) "^ANS_META_TITLE='(.*)'")
						  ((eq player 'mpv) "ANS_metadata/title=(.*)$")
						  (t (error "non valid player"))))
				     (title (funcall get-value string regex)))
				(when (not (null title))
				  (set-title self title)
				  (return-from iter)))
			      (let* ((regex (cond ((eq player 'mplayer) "^ANS_AUDIO_BITRATE='(.*) kbps'")
						  ((eq player 'mpv) "ANS_packet-audio-bitrate=(.*)$")
						  (t (error "non valid player"))))
				     (bitrate (funcall get-value string regex)))
				(when (not (null bitrate))
				  (set-bitrate self bitrate)
				  (return-from iter)))
			      (let* ((regex (cond ((eq player 'mplayer) "^ANS_META_GENRE='(.*)'")
						  ((eq player 'mpv) "ANS_metadata/genre=(.*)$")
						  (t (error "non valid player"))))
				     (genre (funcall get-value string regex)))
				(when (not (null genre))
				  (set-genre self genre)
				  (return-from iter)))
			      (let* ((regex (cond ((eq player 'mplayer) "^ANS_META_YEAR='(.*)'")
						  ((eq player 'mpv) "ANS_metadata/year=(.*)$")
						  (t (error "non valid player"))))
				     (year (funcall get-value string regex)))
				(when (not (null year))
				  (set-year self year)
				  (return-from iter)))
			      (let* ((regex (cond ((eq player 'mplayer) "^ANS_TIME_POSITION=(.*)$")
						  ((eq player 'mpv) "ANS_time-pos=(.*)$")
						  (t (error "non valid player"))))
				     (timepos (funcall get-value string regex)))
				(when (not (null timepos))
				  (set-timepos self timepos)
				  (render-left-window *g-clmp-player*)
				  (return-from iter)))
			      (let* ((regex (cond ((eq player 'mplayer) "^ANS_PERCENT_POSITION=(.*)$")
						  ((eq player 'mpv) "ANS_percent-pos=(.*)$")
						  (t (error "non valid player"))))
				     (percpos (funcall get-value string regex)))
				(when (not (null percpos))
				  (set-percpos self percpos)
				  (render-left-window *g-clmp-player*)
				  #+(or sbcl clisp)
				  (return-from iter)
				  #+cmu
				  (return-from loop-block)))
			      (let* ((regex (cond ((eq player 'mplayer) "^ANS_LENGTH=(.*)$")
						  ((eq player 'mpv) "ANS_length=(.*)$")
						  (t (error "non valid player"))))
				     (timelen (funcall get-value string regex)))
				(when (not (null timelen))
				  (set-timelen self timelen)
				  (return-from iter)))
			      (let* ((regex (cond ((eq player 'mplayer) "^ANS_FILENAME='(.*)'")
						  ((eq player 'mpv) "ANS_filename=(.*)$")
						  (t (error "non valid player"))))
				     (filename (funcall get-value string regex)))
				(when (not (null filename))
				  (set-filename self filename)
				  (render-right-window *g-clmp-player*)
				  (return-from iter)))
			      (let* ((regex (cond ((eq player 'mplayer) "^ANS_volume=(.*)$")
						  ((eq player 'mpv) "ANS_volume=(.*)$")
						  (t (error "non valid player"))))
				     (volume (funcall get-value string regex)))
				(when (not (null volume))
				  (set-volume self volume)
				  (render-left-window *g-clmp-player*)
				  (return-from iter)))
			      (let* ((regex (cond ((eq player 'mplayer) "^EOF code: ([0-9]+)")
						  ((eq player 'mpv) "ANS_eof-reached=(.*)$")
						  (t (error "non valid player"))))
				     (eof-code (funcall get-value string regex)))
				(when (and (not (null eof-code))
					   (or (and (eq player 'mplayer) (string= eof-code "1"))
					       (and (eq player 'mpv) (string= eof-code "yes"))))
				  (set-iseof self t)
				  (return-from iter)))
			      #+cmu
			      (let* ((regex (cond ((eq player 'mplayer) "^(quit)$")
						  ((eq player 'mpv) "^(quit)$")
						  (t (error "non valid player"))))
				     (q (funcall get-value string regex)))
				(when (not (null q))
				  (return-from loop-block)))))))))))
    #+sbcl
    (set-thread (sb-thread:make-thread func) self)
    #+clisp
    (set-thread (bordeaux-threads:make-thread (lambda () (catch 'break-thread (funcall func)))) self)
    ;(set-thread (mp:make-process func :name "mp") self)
    ;(set-thread (with-green-thread (funcall func)) self)
    ;(set-thread (green-threads:make-thread func) self)
    #+cmu
    (funcall func)))

#+(or sbcl clisp)
(defmethod destroy-thread ((self clmp-player))
  #+sbcl
  (progn
    (sb-thread:terminate-thread (get-thread self))
    (sb-thread:join-thread (get-thread self) :timeout 10 :default 1))
  #+clisp
  (progn
    (bordeaux-threads:interrupt-thread (get-thread self) (lambda () (throw 'break-thread 'interrupted-thread)))
    ;(#+clisp bordeaux-threads:join-thread #+cmu green-threads:join-thread (get-thread self))))
    (bordeaux-threads:join-thread (get-thread self))))

(defmethod create ((self clmp-player) &key ((:height h) 0 h?) ((:width w) 0 w?) ((:row r) 0 r?) ((:column c) 0 c?))
  (setq *g-clmp-player* self)
  (when (and h? w? r? c?)
    (create-windows self :height h :width w :row r :column c))
  (create-in-fifo self)
  (create-out-fifo self)
  #+(or sbcl cmu)
  (create-mprocess self)
  #+clisp
  (create-mthread self)
  #+(or sbcl clisp)
  (create-thread self))

(defmethod destroy ((self clmp-player))
  #+(or sbcl clisp)
  (destroy-thread self)
  #+(or sbcl cmu)
  (destroy-mprocess self)
  #+clisp
  (destroy-mthread self)
  (destroy-out-fifo self)
  (destroy-in-fifo self)
  (destroy-windows self))

(defmethod printto-in-fifo ((self clmp-player) str)
  (with-open-file (in-fifo +in-fifo-path+ :direction :io :if-exists :supersede)
		  (write-line str in-fifo)
		  (clear-output in-fifo)))

(defmethod init-track ((self clmp-player))
  (get-artist self)
  (get-title self)
  (get-bitrate self)
  (get-genre self)
  (get-year self)
  (get-timepos self)
  (get-percpos self)
  (get-timelen self)
  (get-filename self)
  (get-volume self)
  (get-iseof self))

(defmethod start-playback ((self clmp-player) play-file)
  (load-file self play-file)
  (when (eq player 'mpv)
    (when (or (eq (get-state self) 'pause) (track-info-iseof (get-track self)))
      (play-pause-file self))
    (sleep 0.5)) ; wait while file is loaded
  (set-state 'play self)
  (init-track self)
  (setf (track-info-iseof (get-track self)) nil)
  #+sbcl
  (sb-posix:alarm +alarm-time+)
  #+cmu
  (clmp-libs:run-alarm (get-proc-handler self) (lambda () (when (eq (get-state *g-clmp-player*) 'play)
                                                            (get-iseof *g-clmp-player*);)
                                                            (get-timepos *g-clmp-player*)
                                                            (get-percpos *g-clmp-player*)
                                                            (parse-output *g-clmp-player*))) +alarm-time+))

(defmethod start-stream ((self clmp-player) stream)
  (load-stream self stream))

(defmethod play-pause-file ((self clmp-player))
  (cond ((eq (get-state self) 'stop) (return-from play-pause-file))
	((eq (get-state self) 'play) (set-state 'pause self))
	((eq (get-state self) 'pause) (set-state 'play self))
	(t (error "unknown value")))
  (cond ((eq player 'mplayer)
	 (printto-in-fifo self "pause"))
	((eq player 'mpv)
	 (printto-in-fifo self "cycle pause"))
	(t (error "non valid player")))
  (when (eq (get-state self) 'play)
    #+sbcl
    (sb-posix:alarm +alarm-time+)))

(defmethod stop-file ((self clmp-player))
  (when (or (eq (get-state self) 'play) (eq (get-state self) 'pause))
    (printto-in-fifo self "stop")
    (set-state 'stop self)))

(defmethod load-file ((self clmp-player) file)
  (set-play-filename file self)
  (cond ((eq player 'mplayer)
	 (printto-in-fifo self (format nil "loadfile '~a'" file)))
	((eq player 'mpv)
	 (printto-in-fifo self (format nil "loadfile ~a" file)))
	(t (error "non valid player"))))

(defmethod load-stream ((self clmp-player) stream)
  (cond ((eq player 'mplayer)
	 (printto-in-fifo self (format nil "loadfile '~a'" stream)))
	((eq player 'mpv)
	 (printto-in-fifo self (format nil "loadfile ~a" stream)))
	(t (error "non valid player"))))

(defmethod quit-player ((self clmp-player))
  (printto-in-fifo self "quit"))

(defmethod get-artist ((self clmp-player))
  (setf (track-info-artist (get-track self)) "Unknown")
  (cond ((eq player 'mplayer)
	 (printto-in-fifo self "get_meta_artist"))
	((eq player 'mpv)
	 ;(printto-in-fifo self "get_property metadata/artist")
	 (printto-in-fifo self "show_text ANS_metadata/artist=${metadata/artist}"))
	(t (error "non valid player"))))

(defmethod set-artist ((self clmp-player) artist)
  (let ((track (get-track self))
	(empty-string? #'(lambda (s) (string= s ""))))
    (if (funcall empty-string? artist)
	(setf (track-info-artist track) "Unknown")
      (setf (track-info-artist track) artist))))

(defmethod get-title ((self clmp-player))
  (setf (track-info-title (get-track self)) "Unknown")
  (cond ((eq player 'mplayer)
	 (printto-in-fifo self "get_meta_title"))
	((eq player 'mpv)
	 ;(printto-in-fifo self "get_property metadata/title")
	 (printto-in-fifo self "show_text ANS_metadata/title=${metadata/title}"))
	(t (error "non valid player"))))

(defmethod set-title ((self clmp-player) title)
  (let ((track (get-track self))
	(empty-string? #'(lambda (s) (string= s ""))))
    (if (funcall empty-string? title)
	(setf (track-info-title track) "Unknown")
      (setf (track-info-title track) title))))

(defmethod get-bitrate ((self clmp-player))
  (setf (track-info-bitrate (get-track self)) "Unknown")
    (cond ((eq player 'mplayer)
	   (printto-in-fifo self "get_audio_bitrate"))
	  ((eq player 'mpv)
	   ;(printto-in-fifo self "get_property packet-audio-bitrate")
	   (printto-in-fifo self "show_text ANS_packet-audio-bitrate=${packet-audio-bitrate}"))
	  (t (error "non valid player"))))

(defmethod set-bitrate ((self clmp-player) bitrate)
  (let ((track (get-track self))
    	(empty-string? #'(lambda (s) (string= s ""))))
    (if (funcall empty-string? bitrate)
	(setf (track-info-bitrate track) 0)
      (setf (track-info-bitrate track) bitrate))))

(defmethod get-genre ((self clmp-player))
  (setf (track-info-genre (get-track self)) "Unknown")
  (cond ((eq player 'mplayer)
	 (printto-in-fifo self "get_meta_genre"))
	((eq player 'mpv)
	 ;(printto-in-fifo self "get_property metadata/genre")
	 (printto-in-fifo self "show_text ANS_metadata/genre=${metadata/genre}"))
	(t (error "non valid player"))))

(defmethod set-genre ((self clmp-player) genre)
  (let ((track (get-track self))
    	(empty-string? #'(lambda (s) (string= s ""))))
    (if (funcall empty-string? genre)
	(setf (track-info-genre track) "Unknown")
      (setf (track-info-genre track) genre))))

(defmethod get-year ((self clmp-player))
  (setf (track-info-year (get-track self)) "Unknown")
  (cond ((eq player 'mplayer)
	 (printto-in-fifo self "get_meta_year"))
	((eq player 'mpv)
	 #+(or sbcl cmu)
	 (let ((s (make-string-output-stream)))
	   (handler-case (#+sbcl sb-ext:run-program #+cmu ext:run-program "/usr/bin/vendor_perl/exiftool" (list (get-play-filename self)) :output s)
			 (error () (#+sbcl sb-ext:run-program #+cmu ext:run-program "/usr/bin/exiftool" (list (get-play-filename self)) :output s)))
	   (let ((lines (cl-ppcre:split "(\\r+|\\n+)" (get-output-stream-string s)))
		 (year "Unknown"))
	     (block dolist-break
	       (dolist (line lines)
		 (let ((value (nth-value 1 (cl-ppcre:scan-to-strings "^Year\\s*:\\s*(.*)$" line))))
		   (when (not (null value))
		     (setf year (aref value 0))
		     (return-from dolist-break)))))
	     (printto-in-fifo self (format nil "show_text ANS_metadata/year=~a" year))))
	 #+clisp
	 (let ((cmd (format nil "exiftool '~a' | egrep '^Year' | sed -e 's/^Year *: *\\(.*\\)$/\\1/'" (get-play-filename self)))
	       (year "Unknown"))
	   (with-open-stream (stream (ext:run-shell-command cmd :output :stream))
			     (with-output-to-string (str)
						    (let ((line (read-line stream nil nil)))
						      (when (not (null line))
							(setf year (string-trim '(#\newline) (write-string line str)))))))
	   (printto-in-fifo self (format nil "print_text ANS_metadata/year=~a~%" year))))
	(t (error "non valid player"))))

(defmethod set-year ((self clmp-player) year)
  (let ((track (get-track self))
    	(empty-string? #'(lambda (s) (string= s ""))))
    (if (funcall empty-string? year)
	(setf (track-info-year track) "Unknown")
      (setf (track-info-year track) year))))

(defmethod inc-timepos ((self clmp-player))
  (cond ((eq player 'mplayer)
	 (printto-in-fifo self "step_property time_pos 5 +1"))
	((eq player 'mpv)
	 (printto-in-fifo self "add time-pos +5"))
	(t (error "non valid player")))
  (get-timepos self)
  (when (eq (get-state self) 'pause)
    (set-state 'play self)
    #+sbcl
    (sb-posix:alarm +alarm-time+)))

(defmethod dec-timepos ((self clmp-player))
  (cond ((eq player 'mplayer)
	 (printto-in-fifo self "step_property time_pos 5 -1"))
	((eq player 'mpv)
	 (printto-in-fifo self "add time-pos -5"))
	(t (error "non valid player")))
  (get-timepos self)
  (when (eq (get-state self) 'pause)
    (set-state 'play self)
    #+sbcl
    (sb-posix:alarm +alarm-time+)))

(defmethod get-timepos ((self clmp-player))
  (cond ((eq player 'mplayer)
	 (printto-in-fifo self "get_time_pos"))
	((eq player 'mpv)
	 ;(printto-in-fifo self "get_property time-pos")
	 (printto-in-fifo self "show_text ANS_time-pos=${time-pos}"))
	(t (error "non valid player"))))

(defmethod set-timepos ((self clmp-player) timepos)
  (let ((track (get-track self))
    	(empty-string? #'(lambda (s) (string= s ""))))
    (if (funcall empty-string? timepos)
	(setf (track-info-timepos track) 0)
      (setf (track-info-timepos track) (nth-value 0 (read-from-string timepos))))))

(defmethod get-percpos ((self clmp-player))
  (cond ((eq player 'mplayer)
	 (printto-in-fifo self "get_percent_pos"))
	((eq player 'mpv)
	 ;(printto-in-fifo self "get_property percent-pos")
	 (printto-in-fifo self "show_text ANS_percent-pos=${percent-pos}"))
	(t (error "non valid player"))))

(defmethod set-percpos ((self clmp-player) percpos)
  (let ((track (get-track self))
    	(empty-string? #'(lambda (s) (string= s ""))))
    (if (funcall empty-string? percpos)
	(setf (track-info-percpos track) 0)
      (setf (track-info-percpos track) (nth-value 0 (read-from-string percpos))))))

(defmethod get-timelen ((self clmp-player))
  (cond ((eq player 'mplayer)
	 (printto-in-fifo self "get_time_length"))
	((eq player 'mpv)
	 ;(printto-in-fifo self "get_property length")
	 (printto-in-fifo self "show_text ANS_length=${length}"))
	(t (error "non valid player"))))

(defmethod set-timelen ((self clmp-player) timelen)
  (let ((track (get-track self))
    	(empty-string? #'(lambda (s) (string= s ""))))
    (if (funcall empty-string? timelen)
	(setf (track-info-timelen track) 0)
      (setf (track-info-timelen track) (nth-value 0 (read-from-string timelen))))))

(defmethod get-filename ((self clmp-player))
  (setf (track-info-filename (get-track self)) "Unknown")
  (cond ((eq player 'mplayer)
	 (printto-in-fifo self "get_file_name"))
	((eq player 'mpv)
	 ;(printto-in-fifo self "get_property filename")
	 (printto-in-fifo self "show_text ANS_filename=${filename}"))
	(t (error "non valid player"))))

(defmethod set-filename ((self clmp-player) filename)
  (let ((track (get-track self))
    	(empty-string? #'(lambda (s) (string= s ""))))
    (if (funcall empty-string? filename)
	(progn
	  (setf (track-info-filename track) "Unknown"))
      (setf (track-info-filename track) filename))))

(defmethod inc-volume ((self clmp-player))
  (cond ((eq player 'mplayer)
	 (printto-in-fifo self "step_property volume 6 +1"))
	((eq player 'mpv)
	 (printto-in-fifo self "add volume +6"))
	 (t (error "non valid player")))
  (get-volume self)
  (when (eq (get-state self) 'pause)
    (set-state 'play self)
    #+sbcl
    (sb-posix:alarm +alarm-time+)))
;(defmethod inc-volume ((self clmp-player))
;  (sb-ext:run-program "/usr/bin/amixer" (list "set" "PCM" "6+" "unmute")))

(defmethod dec-volume ((self clmp-player))
  (cond ((eq player 'mplayer)
	 (printto-in-fifo self "step_property volume 6 -1"))
	((eq player 'mpv)
	  (printto-in-fifo self "add volume -6"))
	 (t (error "non valid player")))
  (get-volume self)
  (when (eq (get-state self) 'pause)
    (set-state 'play self)
    #+sbcl
    (sb-posix:alarm +alarm-time+)))
;(defmethod dec-volume ((self clmp-player))
;  (sb-ext:run-program "/usr/bin/amixer" (list "set" "PCM" "6-" "unmute")))

(defmethod get-volume ((self clmp-player))
  (cond ((eq player 'mplayer)
	 (printto-in-fifo self "get_property volume"))
	((eq player 'mpv)
	 ;(printto-in-fifo self "get_property volume")
	 (printto-in-fifo self "show_text ANS_volume=${volume}"))
	(t (error "non valid player"))))
;(defmethod get-volume ((self clmp-player))
;  (let ((s (make-string-output-stream)))
;    (run-shell-command "amixer get PCM | tail -1 | sed -E 's/.*\[([0-9]+)%\].*/\1/'" :output s)
;    (get-output-stream-string s)))

(defmethod set-volume ((self clmp-player) volume)
  (let ((track (get-track self))
    	(empty-string? #'(lambda (s) (string= s ""))))
    (if (funcall empty-string? volume)
	(setf (track-info-volume track) 0)
      (setf (track-info-volume track) (nth-value 0 (read-from-string volume))))))

(defmethod get-iseof ((self clmp-player))
  (when (eq player 'mpv)
    ;(printto-in-fifo self "get_property eof-reached")
    (printto-in-fifo self "show_text ANS_eof-reached=${eof-reached}")))

(defmethod set-iseof ((self clmp-player) iseof)
  (let ((track (get-track self))
	(empty-string? #'(lambda (s) (string= s ""))))
    (if (funcall empty-string? iseof)
	(setf (track-info-iseof track) nil)
      (setf (track-info-iseof track) iseof))))
