;; CLMP
;; clmp-main.lisp
;; Developer: Branitskiy Alexander <schurshick@yahoo.com>

(in-package :clmp)

(defun main (&rest argv)
  (declare (ignore argv))
  (read-config-file)
  (let ((iface (make-instance 'clmp-iface)))
    ;(create iface :height h :width w :row r :column c)
    (create iface)
    (process-keys iface)
    (destroy iface)))

(defun entry-point ()
  #+sbcl
  (apply 'main sb-ext:*posix-argv*)
  #+clisp
  (apply 'main ext:*args*))
