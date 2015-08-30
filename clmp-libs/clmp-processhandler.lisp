;; CLMP
;; clmp-processhandler.lisp
;; Developer: Branitskiy Alexander <schurshick@yahoo.com>

(in-package :clmp-libs)

(defconstant +sigint+ 2)

(defconstant +sigusr1+ 10)

(defconstant +sigusr2+ 12)

(defconstant +sigalrm+ 14)

;(in-package :cffi)

(defclass clmp-processhandler ()
  ((handlers :initarg :handlers
             :initform nil
             :reader get-handlers
             :writer set-handlers)))

(defmethod ins-handler ((self clmp-processhandler) sig foo)
  (when (null (get-handlers self))
    (set-handlers (make-hash-table) self))
  (if (nth-value 1 (gethash sig (get-handlers self)))
    (format t "clmp-processhandler:ins-handler: signal handler is already set~%")
  (progn
    (setf (gethash sig (get-handlers self)) foo)
    (cffi:defcallback func :void ((argument :int))
      (declare (ignore argument))
      (funcall foo))
    (cffi:foreign-funcall "signal" :int sig :pointer (cffi:callback func) :pointer (cffi:callback func)))))

(defmethod rm-handler ((self clmp-processhandler) sig)
  (when (null (get-handlers self))
    (format t "hash handlers is empty~%"))
  (if (nth-value 1 (gethash sig (get-handlers self)))
    (progn
      (remhash sig (get-handlers self))
      (let ((sig-dfl (cffi:null-pointer)))
        (cffi:foreign-funcall "signal" :int sig :pointer sig-dfl :pointer sig-dfl)))
  (format t "clmp-processhandler:rm-handler: such signal does not exist in the hash~%")))

(defmethod ign-handler ((self clmp-processhandler) sig)
  (let ((sig-ign 1))
    (cffi:foreign-funcall "signal" :int sig :pointer sig-ign :pointer sig-ign)))

(defmethod run-alarm ((self clmp-processhandler) foo sec)
  (cffi:defcallback f :void ((argument :int))
    (declare (ignore argument))
    (funcall foo)
    (cffi:foreign-funcall "alarm" :unsigned-int sec :unsigned-int))
  (cffi:foreign-funcall "signal" :int +sigalrm+ :pointer (cffi:callback f) :pointer (cffi:callback f))
  (cffi:foreign-funcall "alarm" :unsigned-int sec :unsigned-int))

(defmethod stop-alarm ((self clmp-processhandler))
  (ign-handler self +sigalrm+))
