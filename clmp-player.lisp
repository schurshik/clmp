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
  (volume 0))

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
   #+sbcl
   (mprocess :initarg :mprocess
	    :initform 0
	    :reader get-mprocess
	    :writer set-mprocess)
   #+clisp
   (mthread :initarg :mthread
	    :initform nil
	    :reader get-mthread
	    :writer set-mthread)
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
	  :writer set-track)))

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
	  (cl-ncurses:mvwprintw right-window 1 1 (format nil "TRACK: ~a - ~a (~a)" artist title bitrate))
	  (cl-ncurses:mvwprintw right-window 3 1 (format nil "INFO:  ~a <~a>" genre year))
	  (cl-ncurses:mvwprintw right-window 5 1 (format nil "FILE:  ~a" filename))))
    (cl-ncurses:wrefresh right-window)))
  
(defmethod create-in-fifo ((self clmp-player))
  (if (probe-file 
       #+sbcl
       (make-pathname :directory +in-fifo-path+)
       #+clisp
       (pathname +in-fifo-path+))
      (delete-file +in-fifo-path+))
  #+sbcl
  (let ((mode (logior sb-posix:s-iwusr sb-posix:s-irusr)))
    (sb-posix:mkfifo +in-fifo-path+ mode))
  #+clisp
  (ext:run-program "mkfifo" :arguments (list +in-fifo-path+ "--mode=0600")))

(defmethod destroy-in-fifo ((self clmp-player))
  (if (probe-file
       #+sbcl
       (make-pathname :directory +in-fifo-path+)
       #+clisp
       (pathname +in-fifo-path+))
      (delete-file +in-fifo-path+)))

(defmethod create-out-fifo ((self clmp-player))
  (if (probe-file
       #+sbcl
       (make-pathname :directory +out-fifo-path+)
       #+clisp
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
    (set-out-fifo (open +out-fifo-path+ :direction :io :if-exists :supersede) self)))

(defmethod destroy-out-fifo ((self clmp-player))
  (close (get-out-fifo self))
  (if (probe-file
       #+sbcl
       (make-pathname :directory +out-fifo-path+)
       #+clisp
       (pathname +out-fifo-path+))
      (delete-file +out-fifo-path+)))

(defmethod exec-player ((self clmp-player))
  #+sbcl
  (sb-ext:run-program "/usr/bin/mplayer" (list "-idle" "-slave" "-input" (format nil "file=~a" +in-fifo-path+) "2>/dev/null") :output (get-out-fifo self))
  #+clisp
  (with-open-stream (istream
  		     (ext:make-pipe-input-stream (concatenate 'string "/usr/bin/mplayer" " -idle" " -slave" " -input" (format nil " file=~a" +in-fifo-path+) " 2>/dev/null")))
		    (loop (not nil) (loop for line = (read-line istream nil nil) while line do (write-line line (get-out-fifo self)))))
  #+sbcl
  (sb-ext:quit :unix-status 0)
  #+clisp
  (ext:quit 0))

#+sbcl
(defun signal-handle (sig info context)
  (declare (ignore info context))
  (when (= sig sb-posix:sigalrm)
    (when (eq (get-state *g-clmp-player*) 'play)
      (get-timepos *g-clmp-player*)
      (get-percpos *g-clmp-player*)
      (sb-posix:alarm +alarm-time+))))

#+clisp
(defun create-sleep-thread ()
  (setq *g-sleep-thread* (bordeaux-threads:make-thread (lambda () (catch 'break-sleep-thread
								    (loop (not nil)
									  (when (eq (get-state *g-clmp-player*) 'play)
									    (get-timepos *g-clmp-player*)
									    (get-percpos *g-clmp-player*)
									    (sleep +alarm-time+))))))))

#+sbcl
(defmethod create-mprocess ((self clmp-player))
  (let ((pid (sb-posix:fork)))
    (cond ((zerop pid) (exec-player self))
	  ((plusp pid) (set-mprocess pid self))
	  (t (error "can not create process")))
    (sb-sys:enable-interrupt sb-posix:sigalrm #'signal-handle)))

#+clisp
(defmethod create-mthread ((self clmp-player))
  (set-mthread (bordeaux-threads:make-thread (lambda () (catch 'break-mthread (exec-player self)))) self)
  (create-sleep-thread))

#+sbcl
(defmethod destroy-mprocess ((self clmp-player))
  (quit-player self)
  (sb-posix:waitpid (get-mprocess self) 0))

#+clisp
(defmethod destroy-mthread ((self clmp-player))
  (quit-player self)
  (bordeaux-threads:interrupt-thread (get-mthread self) (lambda () (throw 'break-mthread 'interrupted-mthread)))
  (bordeaux-threads:join-thread (get-mthread self))
  (bordeaux-threads:interrupt-thread *g-sleep-thread* (lambda () (throw 'break-sleep-thread 'interrupted-sleep-thread)))
  (bordeaux-threads:join-thread *g-sleep-thread*))

(defmethod create-thread ((self clmp-player))
  (let ((func (lambda ()
		(let ((read-fifo (get-out-fifo self))
		      (get-value #'(lambda (string regex)
				     (let ((value (nth-value 1 (cl-ppcre:scan-to-strings regex string))))
				       (if (not (null value))
					   (aref value 0)
					 nil)))))
		  (loop (not nil)
			(block iter
			  (let ((string (read-line read-fifo)))
			    (let ((artist (funcall get-value string "^ANS_META_ARTIST='(.*)'")))
			      (when (not (null artist))
				(set-artist self artist)
				(return-from iter)))
			    (let ((title (funcall get-value string "^ANS_META_TITLE='(.*)'")))
			      (when (not (null title))
				(set-title self title)
				(return-from iter)))
			    (let ((bitrate (funcall get-value string "^ANS_AUDIO_BITRATE='(.*)'")))
			      (when (not (null bitrate))
				(set-bitrate self bitrate)
				(return-from iter)))
			    (let ((genre (funcall get-value string "^ANS_META_GENRE='(.*)'")))
			      (when (not (null genre))
				(set-genre self genre)
				(return-from iter)))
			    (let ((year (funcall get-value string "^ANS_META_YEAR='(.*)'")))
			      (when (not (null year))
				(set-year self year)
				(return-from iter)))
			    (let ((timepos (funcall get-value string "^ANS_TIME_POSITION=(.*)$")))
			      (when (not (null timepos))
				(set-timepos self timepos)
				(render-left-window *g-clmp-player*)
				(return-from iter)))
			    (let ((percpos (funcall get-value string "^ANS_PERCENT_POSITION=(.*)$")))
			      (when (not (null percpos))
				(set-percpos self percpos)
				(render-left-window *g-clmp-player*)
				(return-from iter)))
			    (let ((timelen (funcall get-value string "^ANS_LENGTH=(.*)$")))
			      (when (not (null timelen))
				(set-timelen self timelen)
				(return-from iter)))
			    (let ((filename (funcall get-value string "^ANS_FILENAME='(.*)'")))
			      (when (not (null filename))
				(set-filename self filename)
				(render-right-window *g-clmp-player*)
				(return-from iter)))
			    (let ((volume (funcall get-value string "^ANS_volume=(.*)$")))
			      (when (not (null volume))
				(set-volume self volume)
				(render-left-window *g-clmp-player*)
				(return-from iter))))))))))
    #+sbcl
    (set-thread (sb-thread:make-thread func) self)
    #+clisp
    (set-thread (bordeaux-threads:make-thread (lambda () (catch 'break-thread (funcall func)))) self)))

(defmethod destroy-thread ((self clmp-player))
  #+sbcl
  (progn
    (sb-thread:terminate-thread (get-thread self))
    (sb-thread:join-thread (get-thread self) :timeout 10 :default 1))
  #+clisp
  (progn
    (bordeaux-threads:interrupt-thread (get-thread self) (lambda () (throw 'break-thread 'interrupted-thread)))
    (bordeaux-threads:join-thread (get-thread self))))

(defmethod create ((self clmp-player) &key ((:height h) 0 h?) ((:width w) 0 w?) ((:row r) 0 r?) ((:column c) 0 c?))
  (setq *g-clmp-player* self)
  (when (and h? w? r? c?)
    (create-windows self :height h :width w :row r :column c))
  (create-in-fifo self)
  (create-out-fifo self)
  #+sbcl
  (create-mprocess self)
  #+clisp
  (create-mthread self)
  (create-thread self))

(defmethod destroy ((self clmp-player))
  (destroy-thread self)
  #+sbcl
  (destroy-mprocess self)
  #+clisp
  (destroy-mthread self)
  (destroy-out-fifo self)
  (destroy-in-fifo self)
  (destroy-windows self))

(defmethod printto-in-fifo ((self clmp-player) string)
  (with-open-file (in-fifo +in-fifo-path+ :direction :io :if-exists :supersede)
		  (write-line string in-fifo)))

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
  (get-volume self))

(defmethod start-playback ((self clmp-player) play-file)
  ; TODO: check a format of the file is music
  (load-file self play-file)
  (set-state 'play self)
  (init-track self)
  #+sbcl
  (sb-posix:alarm +alarm-time+))

(defmethod play-pause-file ((self clmp-player))
  (cond ((eq (get-state self) 'stop) (return-from play-pause-file))
	((eq (get-state self) 'play) (set-state 'pause self))
	((eq (get-state self) 'pause) (set-state 'play self))
	(t (error "unknown value")))
  (printto-in-fifo self "pause")
  (when (eq (get-state self) 'play)
    #+sbcl
    (sb-posix:alarm +alarm-time+)))

(defmethod stop-file ((self clmp-player))
  (when (or (eq (get-state self) 'play) (eq (get-state self) 'pause))
    (printto-in-fifo self "stop")
    (set-state 'stop self)))

(defmethod load-file ((self clmp-player) file)
  (printto-in-fifo self (format nil "loadfile '~a'" file)))

(defmethod quit-player ((self clmp-player))
  (printto-in-fifo self "quit"))

(defmethod get-artist ((self clmp-player))
  (printto-in-fifo self "get_meta_artist"))

(defmethod set-artist ((self clmp-player) artist)
  (let ((track (get-track self))
	(empty-string? #'(lambda (s) (string= s ""))))
    (if (funcall empty-string? artist)
	(setf (track-info-artist track) "Unknown")
      (setf (track-info-artist track) artist))))

(defmethod get-title ((self clmp-player))
  (printto-in-fifo self "get_meta_title"))

(defmethod set-title ((self clmp-player) title)
  (let ((track (get-track self))
	(empty-string? #'(lambda (s) (string= s ""))))
    (if (funcall empty-string? title)
	(setf (track-info-title track) "Unknown")
      (setf (track-info-title track) title))))

(defmethod get-bitrate ((self clmp-player))
  (printto-in-fifo self "get_audio_bitrate"))

(defmethod set-bitrate ((self clmp-player) bitrate)
  (let ((track (get-track self))
    	(empty-string? #'(lambda (s) (string= s ""))))
    (if (funcall empty-string? bitrate)
	(setf (track-info-bitrate track) "Unknown")
      (setf (track-info-bitrate track) bitrate))))

(defmethod get-genre ((self clmp-player))
  (printto-in-fifo self "get_meta_genre"))

(defmethod set-genre ((self clmp-player) genre)
  (let ((track (get-track self))
    	(empty-string? #'(lambda (s) (string= s ""))))
    (if (funcall empty-string? genre)
	(setf (track-info-genre track) "Unknown")
      (setf (track-info-genre track) genre))))

(defmethod get-year ((self clmp-player))
  (printto-in-fifo self "get_meta_year"))

(defmethod set-year ((self clmp-player) year)
  (let ((track (get-track self))
    	(empty-string? #'(lambda (s) (string= s ""))))
    (if (funcall empty-string? year)
	(setf (track-info-year track) "Unknown")
      (setf (track-info-year track) year))))

(defmethod inc-timepos ((self clmp-player))
  (printto-in-fifo self "step_property time_pos 5 +1")
  (get-timepos self)
  (when (eq (get-state self) 'pause)
    (set-state 'play self)
    #+sbcl
    (sb-posix:alarm +alarm-time+)))

(defmethod dec-timepos ((self clmp-player))
  (printto-in-fifo self "step_property time_pos 5 -1")
  (get-timepos self)
  (when (eq (get-state self) 'pause)
    (set-state 'play self)
    #+sbcl
    (sb-posix:alarm +alarm-time+)))

(defmethod get-timepos ((self clmp-player))
  (printto-in-fifo self "get_time_pos"))

(defmethod set-timepos ((self clmp-player) timepos)
  (let ((track (get-track self))
    	(empty-string? #'(lambda (s) (string= s ""))))
    (if (funcall empty-string? timepos)
	(setf (track-info-timepos track) 0)
      (setf (track-info-timepos track) (nth-value 0 (read-from-string timepos))))))

(defmethod get-percpos ((self clmp-player))
  (printto-in-fifo self "get_percent_pos"))

(defmethod set-percpos ((self clmp-player) percpos)
  (let ((track (get-track self))
    	(empty-string? #'(lambda (s) (string= s ""))))
    (if (funcall empty-string? percpos)
	(setf (track-info-percpos track) 0)
      (setf (track-info-percpos track) (nth-value 0 (parse-integer percpos))))))

(defmethod get-timelen ((self clmp-player))
  (printto-in-fifo self "get_time_length"))

(defmethod set-timelen ((self clmp-player) timelen)
  (let ((track (get-track self))
    	(empty-string? #'(lambda (s) (string= s ""))))
    (if (funcall empty-string? timelen)
	(setf (track-info-timelen track) 0)
      (setf (track-info-timelen track) (nth-value 0 (read-from-string timelen))))))

(defmethod get-filename ((self clmp-player))
  (printto-in-fifo self "get_file_name"))

(defmethod set-filename ((self clmp-player) filename)
  (let ((track (get-track self))
    	(empty-string? #'(lambda (s) (string= s ""))))
    (if (funcall empty-string? filename)
	(progn
	  (setf (track-info-filename track) "Unknown"))
      (setf (track-info-filename track) filename))))

(defmethod inc-volume ((self clmp-player))
  (printto-in-fifo self "step_property volume 6 +1")
  (get-volume self)
  (when (eq (get-state self) 'pause)
    (set-state 'play self)
    #+sbcl
    (sb-posix:alarm +alarm-time+)))
;(defmethod inc-volume ((self clmp-player))
;  (sb-ext:run-program "/usr/bin/amixer" (list "set" "PCM" "6+" "unmute")))

(defmethod dec-volume ((self clmp-player))
  (printto-in-fifo self "step_property volume 6 -1")
  (get-volume self)
  (when (eq (get-state self) 'pause)
    (set-state 'play self)
    #+sbcl
    (sb-posix:alarm +alarm-time+)))
;(defmethod dec-volume ((self clmp-player))
;  (sb-ext:run-program "/usr/bin/amixer" (list "set" "PCM" "6-" "unmute")))

(defmethod get-volume ((self clmp-player))
  (printto-in-fifo self "get_property volume"))
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
