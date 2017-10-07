;; CLMP
;; clmp-libs.asd
;; Developer: Branitskiy Alexander <schurshik@yahoo.com>

(defpackage :clmp-libs
  (:use :common-lisp)
  (:export #:clmp-processhandler #:run-alarm #:stop-alarm))
(asdf:defsystem :clmp-libs
  :name "clmp-libs"
  :author "Alexander Branitskiy <schurshik@yahoo.com>"
  :version "1.3.1"
  :maintainer "Alexander Branitskiy <schurshik@yahoo.com>"
  :license "BSD"
  :description "Auxiliary components to clmp."
  :long-description "Auxiliary components to clmp."
  :components ((:file "clmp-processhandler")
               (:file "clmp-posixthread")
               (:file "clmp-sharedmemory")))
