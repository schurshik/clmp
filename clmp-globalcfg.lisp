;; CLMP
;; clmp-globalcfg.lisp
;; Developer: Branitskiy Alexander <schurshick@yahoo.com>

(in-package :clmp)

(setq frame-type 'wbox)
(setq foreground cl-ncurses:color_white)
(setq background cl-ncurses:color_black)
(setq cursor-color cl-ncurses:color_yellow)
(setq play-mode 'play-once)

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
			      (rvalue (aref value 1))
			      (get-color-value #'(lambda (string-color)
						   (cond ((equal string-color "white") cl-ncurses:color_white)
							 ((equal string-color "black") cl-ncurses:color_black)
							 ((equal string-color "green") cl-ncurses:color_green)
							 ((equal string-color "red") cl-ncurses:color_red)
							 ((equal string-color "cyan") cl-ncurses:color_cyan)
							 ((equal string-color "magenta") cl-ncurses:color_magenta)
							 ((equal string-color "blue") cl-ncurses:color_blue)
							 ((equal string-color "yellow") foreground cl-ncurses:color_yellow)
							 (t (error (format nil "non valid string ~a in config file" line)))))))
			  (cond ((equal lvalue "frame-type")
				 (cond ((equal rvalue "wbox") (setq frame-type 'wbox))
				       ((equal rvalue "wborder") (setq frame-type 'wborder))
				       (t (error (format nil "non valid string ~a in config file" line)))))
				((equal lvalue "foreground")
				 (setq foreground (funcall get-color-value rvalue)))
				((equal lvalue "background")
				 (setq background (funcall get-color-value rvalue)))
				((equal lvalue "cursor-color")
				 (setq cursor-color (funcall get-color-value rvalue)))
				((equal lvalue "play-mode")
				 (cond ((equal rvalue "play-once") (setq play-mode 'play-once))
				       ((equal rvalue "play-repeatedly") (setq play-mode 'play-repeatedly))
				       ((equal rvalue "play-around") (setq play-mode 'play-around))
				       ((equal rvalue "play-reverse") (setq play-mode 'play-reverse))
				       ((equal rvalue "play-random") (setq play-mode 'play-random))
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
		    (format out "background = black~%")
		    (format out "# cursor-color: black, white, green, red, cyan, magenta, blue, yellow~%")
		    (format out "cursor-color = yellow~%")
		    (format out "# play-mode: play-once, play-repeatedly, play-around, play-reverse, play-random~%")
		    (format out "play-mode = play-once~%"))))

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
;  #+sbcl
;  (handler-case (sb-ext:run-program "/bin/echo" (list "-e" (format nil "\\e]12;~a\\a" cursor-color)) :output "/dev/stdout" :if-output-exists :append)
;    (error () (sb-ext:run-program "/usr/bin/echo" (list "-e" (format nil "\\e]12;~a\\a" cursor-color)) :output "/dev/stdout" :if-output-exists :append)))
;  #+clisp
;  (ext:run-program "echo" :arguments (list "-e" (format nil "\\e]12;~a\\a" "red")) :output "/dev/stdout" :if-output-exists :append))
