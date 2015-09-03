;; CLMP
;; clmp-whelp.lisp
;; Developer: Branitskiy Alexander <schurshick@yahoo.com>

(in-package :clmp)

(defclass clmp-whelp ()
  ((window :initarg :window
	   :initform nil
	   :reader get-window
	   :writer set-window)))

(defmethod create-window ((self clmp-whelp) &key ((:height h) 0 h?) ((:width w) 0 w?) ((:row r) 0 r?) ((:column c) 0 c?))
  (if (and h? w? r? c?)
      (set-window (cl-ncurses:newwin h w r c) self))
  (colorize-window (get-window self))
  ;(render-window self :height h :width w :row r :column c)
  )

(defmethod destroy-window ((self clmp-whelp))
  (cl-ncurses:delwin (get-window self)))

(defmethod create ((self clmp-whelp) &key ((:height h) 0 h?) ((:width w) 0 w?) ((:row r) 0 r?) ((:column c) 0 c?))
  (if (and h? w? r? c?)
      (create-window self :height h :width w :row r :column c)))

(defmethod destroy ((self clmp-whelp))
  (destroy-window self))

(defmethod render-window ((self clmp-whelp) &key ((:height h) 0 h?) ((:width w) 0 w?) ((:row r) 0 r?) ((:column c) 0 c?))
  (cl-ncurses:wclear (get-window self))
  (when (and h? w? r? c?)
    (cl-ncurses:wresize (get-window self) h w)
    (cl-ncurses:mvwin (get-window self) r c))
  (let ((help-info (list "      _                 "
                         "  ___| |_ __ ___  _ __  "
                         " / __| | '_ ` _ \\| '_ \\ "
                         "| (__| | | | | | | |_) |"
                         " \\___|_|_| |_| |_| .__/ "
                         "                 |_|    "
                         ""
                         "Frontend interface for mplayer and mpv based on cl-ncurses and written on COMMON LISP"
                         ""
                         "Navigation keys:"
                         "h      - go to the parent directory"
                         "j      - go to the next directory or file"
                         "k      - go to the previous directory or file"
                         "j      - go to the indicated directory or play file"
                         "ENTER  - go to the indicated directory or play file"
                         "PgUp   - scroll one page up"
                         "PgDown - scroll one page down"
                         "p      - play/pause a current file"
                         "s      - stop a current file"
                         "TAB    - play a next file"
                         "`      - play a previous file"
                         "+      - increase a volume"
                         "-      - decrease a volume"
                         ">      - go forward a track"
                         "<      - go backward a track"
                         "H      - show help information"
                         "ESCAPE - hide help information"
                         "q      - quit from the application"
                         ""
                         "Branitskiy Alexander"))
        (i 1))
    (dolist (str help-info)
      (let ((pos 0)
            (max-string-len (- (cl-ncurses:getmaxx (get-window self)) 2)))
        (loop do
             (cl-ncurses:wmove (get-window self) i 1)
             (let ((inc-pos (min (- (length str) pos) max-string-len)))
               (cl-ncurses:wprintw (get-window self) (subseq str pos (+ pos inc-pos)))
               (incf pos inc-pos))
             (incf i)
           until (= pos (length str)))))
  (cl-ncurses:wmove (get-window self) 1 1)
  (draw-frame (get-window self))
  (cl-ncurses:wrefresh (get-window self))))

(defmethod render ((self clmp-whelp) &key ((:height h) 0 h?) ((:width w) 0 w?) ((:row r) 0 r?) ((:column c) 0 c?))
  (if (and h? w? r? c?)
      (render-window self :height h :width w :row r :column c)
    (render-window self)))
