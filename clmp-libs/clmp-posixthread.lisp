;; CLMP
;; clmp-processhandler.lisp
;; Developer: Branitskiy Alexander <schurshick@yahoo.com>

(in-package :clmp-libs)

;(in-package :cffi)

;(cffi:load-foreign-library "/lib/i386-linux-gnu/libpthread.so.0")

(cffi:load-foreign-library "/usr/lib/libpthread.so.0")

(cffi:defctype pthread-t :unsigned-long)

(defclass clmp-posixthread ()
  ((thread :initarg :thread
           :initform nil
           :reader get-thread
           :writer set-thread)))

(defmethod create-posixthread ((self clmp-posixthread) foo)
  (cffi:defcallback f :pointer ((arg :pointer))
    (declare (ignore arg))
    (funcall foo)
    (cffi:null-pointer))
  (cffi:with-foreign-object (th 'pthread-t)
    (cffi:foreign-funcall "pthread_create" :pointer th :pointer (cffi:null-pointer)
                          :pointer (cffi:callback f) :pointer (cffi:null-pointer))
    (set-thread th self)))

(defmethod join-posixthread ((self clmp-posixthread))
  (cffi:foreign-funcall "pthread_join" pthread-t (mem-ref (get-thread self) 'pthread-t)
                        :pointer (cffi:null-pointer)))
