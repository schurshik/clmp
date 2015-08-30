;; CLMP
;; clmp-sharedmemory.lisp
;; Developer: Branitskiy Alexander <schurshick@yahoo.com>

(in-package :clmp-libs)

;(in-package :cffi)

(defconstant +shm-name+ "shared-memory.clmp")

(cffi:load-foreign-library "/usr/lib/librt.so")

(cffi:defctype size-t :unsigned-long)

(defclass clmp-sharedmemory ()
  ((shm :initarg :shm
        :initform nil
        :reader get-shm
        :writer set-shm)
   (addr :initarg :addr
         :initform nil
         :reader get-addr
         :writer set-addr)
   (size :initarg :size
         :initform 0
         :reader get-size
         :writer set-size)))

(defmethod clear-sharedmemory ((self clmp-sharedmemory))
  (cffi:foreign-funcall "memset" :pointer (get-addr self) :int 0 size-t (get-size self)))

(defmethod create-sharedmemory ((self clmp-sharedmemory) size)
  (let* ((o-creat 64)
         (o-rdwr 2)
         (flag (logior o-creat o-rdwr))
         (mode 0777))
    (set-shm (cffi:foreign-funcall "shm_open" :string +shm-name+ :int flag :uint32 mode :int) self)
    (when (= (get-shm self) -1)
      (error "can not create shared memory (shm_open)"))
    (when (= (cffi:foreign-funcall "ftruncate" :int (get-shm self) :long size :int) -1)
      (error "can not create shared memory (ftuncate)")))
  (let ((prot-read 1)
        (prot-write 2)
        (map-shared 1))
    (set-addr (cffi:foreign-funcall "mmap" :pointer (cffi:null-pointer) size-t size :int (logior prot-read prot-write) :int map-shared :int (get-shm self) :uint64 0 :pointer) self))
  (set-size size self)
  (clear-sharedmemory self))

(defmethod write-to-sharedmemory ((self clmp-sharedmemory) str)
  (let ((len (length str)))
    (when (< (get-size self) (+ len 1))
      (error "size of shared memory is less than the string"))
    (cffi:with-foreign-object (s :char (+ len 1))
      (progn
        (loop for i from 0 to (- len 1) do
          (setf (cffi:mem-aref s :char i) (char-code (char str i))))
        (setf (cffi:mem-aref s :char len) 0)
        (cffi:foreign-funcall "memcpy" :pointer (get-addr self) :pointer s size-t len :pointer)))))

(defmethod read-from-sharedmemory ((self clmp-sharedmemory))
  (let ((len (cffi:foreign-funcall "strlen" :pointer (get-addr self) :int)))
;    ((len 0))
;    (loop for i = 0 while (not (= (cffi:mem-aref (get-addr self) :char i) 0)) do
;      (incf len))
    (let ((ret-str (make-string len)))
      (loop for i from 0 to (- len 1) do
        (setf (char ret-str i) (code-char (cffi:mem-aref (get-addr self) :char i))))
      (return-from read-from-sharedmemory ret-str))))

(defmethod destroy-sharedmemory ((self clmp-sharedmemory))
  (cffi:foreign-funcall "munmap" :pointer (get-addr self) size-t (get-size self))
  (cffi:foreign-funcall "close" :int (get-shm self))
  (cffi:foreign-funcall "shm_unlink" :string +shm-name+))
