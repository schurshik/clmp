;; CLMP
;; clmp-fmanager.lisp
;; Developer: Branitskiy Alexander <schurshik@yahoo.com>

(in-package :clmp)

#+(or sbcl clisp)
(defconstant +home-dir+ (user-homedir-pathname))
#+cmu
(defconstant +home-dir+ (get-home-dir))

(defconstant +file-bin+ "/usr/bin/file")

(defclass clmp-fmanager ()
  ((window :initarg :window
	   :initform nil
	   :reader get-window
	   :writer set-window)
   (lsdir :initarg :lsdir
	  :initform '()
	  :reader get-lsdir
	  :writer set-lsdir)
   (curdir :initarg :curdir
	   :initform nil
	   :reader get-curdir
	   :writer set-curdir)
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

(defmethod create-window ((self clmp-fmanager) &key ((:height h) 0 h?) ((:width w) 0 w?) ((:row r) 0 r?) ((:column c) 0 c?) ((:line l) t l?))
  (if (and h? w? r? c?)
      (set-window (cl-ncurses:newwin h w r c) self))
  (colorize-window (get-window self))
  (change-dir self)
  (render-window self :height h :width w :row r :column c :line l))

(defmethod destroy-window ((self clmp-fmanager))
  (cl-ncurses:delwin (get-window self)))

(defmethod create ((self clmp-fmanager) &key ((:height h) 0 h?) ((:width w) 0 w?) ((:row r) 0 r?) ((:column c) 0 c?) ((:line l) t l?))
  (if (and h? w? r? c?)
      (create-window self :height h :width w :row r :column c :line l)))

(defmethod destroy ((self clmp-fmanager))
  (destroy-window self))

(defmethod render-window ((self clmp-fmanager) &key ((:height h) 0 h?) ((:width w) 0 w?) ((:row r) 0 r?) ((:column c) 0 c?) ((:line l) t l?))
  (cl-ncurses:wclear (get-window self))
  (when (and h? w? r? c?)
    (cl-ncurses:wresize (get-window self) h w)
    (cl-ncurses:mvwin (get-window self) r c))
  (draw-frame (get-window self))
  (print-curdir self)
  (print-filetype self)
  (print-lsdir self)
  (when (or (not l?) l)
    (hightlight-line self))
  (cl-ncurses:wrefresh (get-window self)))

(defmethod render ((self clmp-fmanager) &key ((:height h) 0 h?) ((:width w) 0 w?) ((:row r) 0 r?) ((:column c) 0 c?) ((:line l) t l?))
  (if (and h? w? r? c?)
      (render-window self :height h :width w :row r :column c :line l)
    (render-window self :line l)))

(defun dir-namestring (dir)
  (aref (nth-value 1 (cl-ppcre:scan-to-strings "/([^/]+/)$" dir)) 0))

(defun is-dir? (test-dir)
  (not (null (cl-ppcre:scan "/$" test-dir))))

(defmethod hightlight-line ((self clmp-fmanager))
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

(defmethod unhightlight-line ((self clmp-fmanager))
  (cl-ncurses:init-pair 3 cursor-color cl-ncurses:color_black)
  #+(or sbcl clisp)  
  (let ((window (get-window self)))
    (cl-ncurses:mvwchgat window (get-currow self) (get-curcol self) (- (cl-ncurses:getmaxx window) 2) cl-ncurses:a_normal 3 nil)))

(defmethod print-curdir ((self clmp-fmanager))
  (cl-ncurses:mvwprintw (get-window self) 0 0 (namestring (get-curdir self))))

(defmethod is-audio-video-file ((self clmp-fmanager) file warning)
  (let ((audio-video-file? #'(lambda (in-file)
			 #+(or sbcl cmu)
			 (let ((s (make-string-output-stream)))
			   (#+sbcl sb-ext:run-program #+cmu ext:run-program +file-bin+ (list in-file "--mime-type") :output s)
			   (let ((result-string (string-right-trim '(#\newline) (get-output-stream-string s))))
			     (if (not (and (null (search ": audio/" result-string)) (null (search ": video/" result-string))))
				 t
			       nil)))
			 #+clisp
			 (with-open-stream (stream (ext:run-program +file-bin+ :arguments (list in-file "--mime-type") :output :stream))
					   (let ((result-string (string-right-trim '(#\newline) (read-line stream nil nil))))
					     (if (not (and (null (search ": audio/" result-string)) (null (search ": video/" result-string))))
						 t
					       nil))))))
    (if (funcall audio-video-file? file)
	t
      (progn (unless (not warning)
	       (let ((window (get-window self)))
		  (cl-ncurses:mvwprintw window (- (cl-ncurses:getmaxy window) 1) 0 "this file is not audio and not video")
		  (cl-ncurses:wrefresh window)))
	     nil))))

(defmethod find-next-prev-rand-audio-video-file ((self clmp-fmanager) cur-full-filename find-where)
  (when (not (or (eq find-where 'next) (eq find-where 'prev) (eq find-where 'rand)))
    (return-from find-next-prev-rand-audio-video-file nil))
  (let* ((dir-name (directory-namestring (pathname cur-full-filename)))
  	 (cur-filename (file-namestring (pathname cur-full-filename)))
  	 (lsdir
  	  #+sbcl
  	  (directory (make-pathname :name :wild :type :wild :directory dir-name :defaults (namestring +home-dir+)))
  	  #+cmu
  	  (directory (make-pathname :name :wild :type :wild :directory dir-name))
  	  #+clisp
  	  (append (directory (concatenate 'string dir-name "*/")) (directory (concatenate 'string dir-name "*")))))
    (let ((i 0))
      (block dolist-break
  	(dolist (file lsdir)
	  (let ((filename (file-namestring (namestring file))))
	    (when (string= filename cur-filename)
	      (return-from dolist-break))
	    (incf i))))
      (let ((randlst #'(lambda (count)
			 (let ((lst ()))
			   (loop do
				 (let ((rnd (random count)))
				   (unless (find rnd lst)
				     (setq lst (cons rnd lst))))
				 while (< (list-length lst) count))
			   lst)))
	    (newlst #'(lambda (in-lst indexes)
			(let ((out-lst ()))
			  (dolist (index indexes)
				(setq out-lst (append out-lst (list (nth index in-lst)))))
			  out-lst))))
      (dolist (file (if (eq find-where 'next)
  			(append (subseq lsdir (+ i 1)) (subseq lsdir 0 i))
  		      (if (eq find-where 'prev)
			  (append (reverse (subseq lsdir 0 i)) (reverse (subseq lsdir (+ i 1))))
			(funcall newlst lsdir (funcall randlst (list-length lsdir))))))
  	(let ((full-filename (namestring file)))
  	  (when (is-audio-video-file self full-filename nil)
  	    (return-from find-next-prev-rand-audio-video-file full-filename))))))
    nil))

(defmethod print-filetype ((self clmp-fmanager))
  #+(or sbcl cmu)
  (let ((s (make-string-output-stream)))
    (#+sbcl sb-ext:run-program #+cmu ext:run-program +file-bin+ (list "--brief" (namestring (nth (get-curind self) (get-lsdir self)))) :output s)
    (let ((window (get-window self))
	  (result-string (string-right-trim '(#\newline) (get-output-stream-string s))))
      (cl-ncurses:mvwprintw window (- (cl-ncurses:getmaxy window) 1) 0 result-string)))
  #+clisp
  (with-open-stream (stream (ext:run-program +file-bin+ :arguments (list "--brief" (namestring (nth (get-curind self) (get-lsdir self)))) :output :stream))
		    (let ((window (get-window self))
			  (result-string (string-right-trim '(#\newline) (read-line stream nil nil))))
		      (cl-ncurses:mvwprintw window (- (cl-ncurses:getmaxy window) 1) 0 result-string))))

(defmethod print-lsdir ((self clmp-fmanager))
  (let ((row +startoffset-row+)
	(column +startoffset-column+)
	(upper-index (+ (- (get-curind self) (get-currow self)) +startoffset-row+)))
    (block dolist-break
      (let ((lst (subseq (get-lsdir self) upper-index)))
	(dolist (file lst)
	  (if (< row (- (cl-ncurses:getmaxy (get-window self)) +endoffset-row+))
	      (progn (let ((file-name (namestring file)))
		       (if (is-dir? file-name)
			   #+(or clisp cmu)
			   (if (and (= upper-index 0) (= row +startoffset-row+))
			       (cl-ncurses:mvwprintw (get-window self) row column "../")
			     (cl-ncurses:mvwprintw (get-window self) row column (dir-namestring file-name)))
			   #+sbcl
			   (cl-ncurses:mvwprintw (get-window self) row column (dir-namestring file-name))
			 (cl-ncurses:mvwprintw (get-window self) row column (file-namestring file-name))))
		     (incf row))
	    (return-from dolist-break)))))))

(defmethod change-dir ((self clmp-fmanager) &optional (dir +home-dir+))
  (let ((dir-name (namestring dir)))
    (when (and #+sbcl (probe-file dir) #+(or clisp cmu) (not nil) (is-dir? dir-name))
      (set-curdir dir self)
      (set-curind 0 self)
      (set-currow +startoffset-row+ self)
      (set-curcol +startoffset-column+ self)
      (if (string= dir-name "/")
	  (set-lsdir
	   #+sbcl
	   (directory (make-pathname :name :wild :type :wild :directory dir-name :defaults (namestring +home-dir+)))
	   #+cmu
	   (directory (make-pathname :name :wild :type :wild :directory dir-name))
	   #+clisp
	   (append (directory (concatenate 'string dir-name "*/")) (directory (concatenate 'string dir-name "*")))
	   self)
	(let ((parent-dir (merge-pathnames (make-pathname :directory '(:relative :up))
					   #+sbcl
					   (make-pathname :directory dir-name)
					   #+(or clisp cmu)
					   (pathname dir-name))))
	  (set-lsdir (cons parent-dir
			   #+sbcl
			   (directory (make-pathname :name :wild :type :wild :directory dir-name :defaults (namestring +home-dir+)))
			   #+cmu
			   (directory (make-pathname :name :wild :type :wild :directory dir-name))
			   #+clisp
			   (append (directory (concatenate 'string dir-name "*/")) (directory (concatenate 'string dir-name "*"))))
			   self))))))

(defmethod gointo-curdir ((self clmp-fmanager))
  (change-dir self (truename (nth (get-curind self) (get-lsdir self))))
  (render-window self))

(defmethod gointo-pardir ((self clmp-fmanager))
  (let ((curdir (namestring (get-curdir self))))
    (when (string/= curdir "/")
      (change-dir self (truename (nth 0 (get-lsdir self))))
      (render-window self))))

(defmethod press-down ((self clmp-fmanager))
  (let ((curind (get-curind self)))
    (when (< curind (- (list-length (get-lsdir self)) 1))
      (set-curind (+ curind 1) self)
      (let ((currow (get-currow self)))
	(if (< currow (- (cl-ncurses:getmaxy (get-window self)) +endoffset-row+ 1))
	    (progn (unhightlight-line self)
		   (set-currow (+ currow 1) self)
		   (render-window self))
	  (render-window self))))))

(defmethod press-up ((self clmp-fmanager))
  (let ((curind (get-curind self)))
    (when (> curind 0)
      (set-curind (- curind 1) self)
      (let ((currow (get-currow self)))
	(if (> currow +startoffset-row+)
	    (progn (unhightlight-line self)
		   (set-currow (- currow 1) self)
		   (render-window self))
	  (render-window self))))))

(defmethod press-pagedown ((self clmp-fmanager))
  (let ((curind (get-curind self)))
    (when (< curind (- (list-length (get-lsdir self)) 1))
      (if (< curind (- (list-length (get-lsdir self)) (- (cl-ncurses:getmaxy (get-window self)) +endoffset-row+ 1)))
	  (set-curind (+ curind (- (cl-ncurses:getmaxy (get-window self)) +endoffset-row+ 1)) self)
	(let ((offset (- (list-length (get-lsdir self)) curind 1)))
	  (set-curind (+ curind offset) self)
	  (let ((currow (get-currow self)))
	    (unhightlight-line self)
	    (set-currow (+ currow offset) self))))
      (render-window self))))

(defmethod press-pageup ((self clmp-fmanager))
  (let ((curind (get-curind self)))
    (when (> curind 0)
      (if (>= curind (- (cl-ncurses:getmaxy (get-window self)) +endoffset-row+ 1))
	  (set-curind (- curind (- (cl-ncurses:getmaxy (get-window self)) +endoffset-row+ 1)) self)
	(let ((offset curind))
	  (set-curind (- curind offset) self)
	  (let ((currow (get-currow self)))
	    (unhightlight-line self)
	    (set-currow (- currow offset) self))))
      (render-window self))))

(defmethod try-to-find-and-move ((self clmp-fmanager) s)
  (let ((i 0))
  	(dolist (file (get-lsdir self))
      (let ((filename (if (is-dir? (namestring file)) (dir-namestring (namestring file)) (file-namestring (namestring file)))))
        (when (and (<= (length s) (length filename)) (string= (subseq filename 0 (length s)) s))
          (set-curind i self)
          (set-currow +startoffset-row+ self)
          (render-window self)
          (return-from try-to-find-and-move t))
        (incf i)))
    (return-from try-to-find-and-move nil)))
