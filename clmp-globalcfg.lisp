;; CLMP
;; clmp-globalcfg.lisp
;; Developer: Branitskiy Alexander <schurshick@yahoo.com>

(in-package :clmp)

(setq frame-type 'wbox)
(setq foreground cl-ncurses:color_white)
(setq background cl-ncurses:color_black)

(setq config-file (namestring (make-pathname :directory
					     #+sbcl
					     (namestring (user-homedir-pathname))
					     #+clisp
					     (user-homedir-pathname)
					     :name ".clmp" :type "cfg")))

(defun read-config-file ()
  (if (probe-file 
       #+sbcl
       (make-pathname :directory config-file)
       #+clisp
       (pathname config-file))
      (let ((in (open (namestring config-file) :if-does-not-exist nil)))
	(when (not (null in))
	  (loop for line = (read-line in nil :eof)
		until (eql line :eof) do
		(block iter
		  (when (not (null (cl-ppcre:scan "^\\s*#.*" line)))
		    (return-from iter))
		  (when (not (null (cl-ppcre:scan "^\\s*$" line)))
		    (return-from iter))
		  (let ((value (nth-value 1 (cl-ppcre:scan-to-strings "^\\s*(\\S+)\\s*=\\s*(\\S+)\\s*$" line))))
		    (if (not (null value))
			(let ((lvalue (aref value 0))
			      (rvalue (aref value 1)))
			  (cond ((equal lvalue "frame-type")
				 (cond ((equal rvalue "wbox") (setq frame-type 'wbox))
				       ((equal rvalue "wborder") (setq frame-type 'wborder))
				       (t (error (format nil "non valid string ~a in config file" line)))))
				((equal lvalue "foreground")
				 (cond ((equal rvalue "white") (setq foreground cl-ncurses:color_white))
				       ((equal rvalue "black") (setq foreground cl-ncurses:color_black))
				       ((equal rvalue "green") (setq foreground cl-ncurses:color_green))
				       ((equal rvalue "red") (setq foreground cl-ncurses:color_red))
				       ((equal rvalue "cyan") (setq foreground cl-ncurses:color_cyan))
				       ((equal rvalue "magenta") (setq foreground cl-ncurses:color_magenta))
				       ((equal rvalue "blue") (setq foreground cl-ncurses:color_blue))
				       ((equal rvalue "yellow") (setq foreground cl-ncurses:color_yellow))
				       (t (error (format nil "non valid string ~a in config file" line)))))
				((equal lvalue "background")
				 (cond ((equal rvalue "black") (setq background cl-ncurses:color_black))
				       ((equal rvalue "white") (setq background cl-ncurses:color_white))
				       ((equal rvalue "green") (setq background cl-ncurses:color_green))
				       ((equal rvalue "red") (setq background cl-ncurses:color_red))
				       ((equal rvalue "cyan") (setq background cl-ncurses:color_cyan))
				       ((equal rvalue "magenta") (setq background cl-ncurses:color_magenta))
				       ((equal rvalue "blue") (setq background cl-ncurses:color_blue))
				       ((equal rvalue "yellow") (setq background cl-ncurses:color_yellow))
				       (t (error (format nil "non valid string ~a in config file" line)))))
				(t (error (format nil "non valid string ~a in config file" line)))))
		      (error (format nil "non valid string '~a' in config file" line))))))
		(close in)))
    (with-open-file (out (namestring config-file) :direction :output :if-exists :supersede)
		    (format out "####################~%")
		    (format out "# CLMP config file #~%")
		    (format out "####################~%")
		    (format out "# frame-type: wbox, wborder~%")
		    (format out "frame-type = wbox~%")
		    (format out "# foreground: white, black, green, red, cyan, magenta, blue, yellow~%")
		    (format out "foreground = white~%")
		    (format out "# background: black, white, green, red, cyan, magenta, blue, yellow~%")
		    (format out "background = black~%"))))

(defun draw-frame (window)
  (cond ((eq frame-type 'wbox)
	 (cl-ncurses:box window 0 0))
	((eq frame-type 'wborder)
	 (cl-ncurses:wborder window (char-int #\|) (char-int #\|) (char-int #\-) (char-int #\-) (char-int #\+) (char-int #\+) (char-int #\+) (char-int #\+)))
	(t (error "non valid parameter frame-type"))))

(defun colorize-window (window)
  (cl-ncurses:init-pair 1 foreground background)
  (cl-ncurses:wattron window (cl-ncurses:color-pair 1))
  (cl-ncurses:wbkgd window (cl-ncurses:color-pair 1)))