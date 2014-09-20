#!/bin/bash

clncurses_path=`cat Makefile.in | egrep "^[ \t]*CLNCURSES_PATH" | sed -e "s/.*=[ \t]*\([^ \t]*\)$/\1/"`

cat <<EOF | patch ${clncurses_path}package.lisp 
@@ -24,7 +24,10 @@
 (in-package :cl-user)
 
 (defpackage #:cl-ncurses
+  #+sbcl
   (:use #:cl #:uffi)
+  #+clisp
+  (:use #:cl #:cffi-uffi-compat)
   (:export :SLK-LABEL :WINSTR :WINNSTR
            :HAS-COLORS :SETSCRREG
            :NONL :FALSE :SAVETTY
@@ -181,7 +184,7 @@
 (defparameter *ncurses-path*
   #-win32
   (find-foreign-library
-   "libncurses"
+   "libncursesw"
    *ncurses-search-paths*
    :drive-letters '("C" "D" "E")
    :types '("so" "dylib" "a" "dll"))
EOF

cat <<EOF | patch ${clncurses_path}cl-ncurses.asd
@@ -34,7 +34,9 @@
 
 (defsystem :cl-ncurses
 	   :version "0.1.4"
-	   :depends-on (:uffi)
+	   :depends-on
+	   #+sbcl (:uffi)
+	   #+clisp (:cffi-uffi-compat)
            :serial t
 	   :components ((:file "package")
 			(:file "add_wch" :depends-on ("package"))
EOF

cat <<EOF | patch ${clncurses_path}printw.lisp
@@ -33,27 +33,49 @@
 
 ; TODO: support a variable number of args
 ; C-prototype: int printw(const char *fmt, ...);
+#-clisp
 (def :int ((fmt :cstring))
      "printw")
+#+clisp
+(def :int ((fmt :string))
+     "printw")
 
 
 ; C-prototype: int wprintw(WINDOW *win, const char *fmt, ...);
+#-clisp
 (def :int ((win window-ptr)
            (fmt :cstring))
      "wprintw")
+#+clisp
+(def :int ((win window-ptr)
+           (fmt :string))
+     "wprintw")
 
 ; C-prototype: int mvprintw(int y, int x, const char *fmt, ...);
+#-clisp
 (def :int ((y   :int)
 	   (x   :int)
 	   (fmt :cstring))
      "mvprintw")
+#+clisp
+(def :int ((y   :int)
+	   (x   :int)
+	   (fmt :string))
+     "mvprintw")
 
 ; C-prototype: int mvwprintw(WINDOW *win, int y, int x, const char *fmt, ...);
+#-clisp
 (def :int ((win window-ptr)
 	   (y   :int)
 	   (x   :int)
 	   (fmt :cstring))
      "mvwprintw")
+#+clisp
+(def :int ((win window-ptr)
+	   (y   :int)
+	   (x   :int)
+	   (fmt :string))
+     "mvwprintw")
 
 ; C-prototype: int vwprintw(WINDOW *win, const char *fmt, va_list varglist);
 ; C-prototype: int vw_printw(WINDOW *win, const char *fmt, va_list varglist);

EOF

cat <<EOF | patch ${clncurses_path}attr.lisp
@@ -88,5 +88,19 @@
 ; C Prototype: int chgat(int n, attr_t attr, short color, const void *opts)
 ; C Prototype: int wchgat(WINDOW *win, int n, attr_t attr, short color, const void *opts)
 ; C Prototype: int mvchgat(int y, int x, int n, attr_t attr, short color, const void *opts)
+(def :int ((y :int)
+           (x :int)
+           (n :int)
+           (attr :int)
+           (color :short)
+           (opts :pointer-void))
+     "mvchgat")
 ; C Prototype: int mvwchgat(WINDOW *win, int y, int x, int n, attr_t attr, short color, const void *opts)
-
+(def :int ((win window-ptr)
+           (y :int)
+           (x :int)
+           (n :int)
+           (attr :int)
+           (color :short)
+           (opts :pointer-void))
+     "mvwchgat")
EOF
