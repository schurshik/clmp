;; CLMP
;; clmp.asd
;; Developer: Branitskiy Alexander <schurshick@yahoo.com>

(defpackage :clmp
  (:use :common-lisp :uiop)
  (:export #:main #:entry-point))
(asdf:defsystem :clmp
  :name "clmp"
  :author "Alexander Branitskiy <schurshick@yahoo.com>"
  :version "1.0.0"
  :maintainer "Alexander Branitskiy <schurshick@yahoo.com>"
  :license "BSD"
  :description "Frontend interface to mplayer based on cl-ncurses."
  :long-description "Frontend interface to mplayer based on cl-ncurses."
  :build-operation program-op
  :entry-point "clmp:entry-point"
  :components ((:file "clmp-globalcfg")
	       (:file "clmp-fmanager" :depends-on ("clmp-globalcfg"))
	       (:file "clmp-player" :depends-on ("clmp-globalcfg"))
	       (:file "clmp-iface" :depends-on ("clmp-globalcfg" "clmp-fmanager" "clmp-player"))
	       (:file "clmp-main" :depends-on ("clmp-iface"))))
