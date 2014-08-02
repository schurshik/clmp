# CLMP
# Makefile
# Developer: Branitskiy Alexander <schurshick@yahoo.com>

SHELL := /bin/bash

include Makefile.in

PROJ = clmp
OUT = $(PROJ)
ASDF = $(PROJ)
SRC = clmp.asd clmp-main.lisp clmp-iface.lisp clmp-fmanager.lisp clmp-player.lisp \
      clmp-globalcfg.lisp

define CREATE_SCRIPT_FILE
  $(shell echo "(require :asdf)" > "$(OUT).lisp");
  $(shell echo "(require :sb-posix)" >> "$(OUT).lisp");
  $(shell echo '(push #p"$(UFFI_PATH)" asdf:*central-registry*)' >> "$(OUT).lisp");
  $(shell echo "(asdf:operate 'asdf:load-op 'uffi)" >> "$(OUT).lisp");
  $(shell echo '(push #p"$(CLNCURSES_PATH)" asdf:*central-registry*)' >> "$(OUT).lisp");
  $(shell echo "(asdf:operate 'asdf:load-op 'cl-ncurses)" >> "$(OUT).lisp");
  $(shell echo '(push #p"$(CLPPCRE_PATH)" asdf:*central-registry*)' >> "$(OUT).lisp");
  $(shell echo "(asdf:operate 'asdf:load-op 'cl-ppcre)" >> "$(OUT).lisp");
  $(shell echo '(push #p"$(CURDIR)/" asdf:*central-registry*)' >> "$(OUT).lisp");
  $(shell echo -n "(asdf:operate 'asdf:load-op 'clmp)" >> "$(OUT).lisp");
endef

ifeq ($(COMPILER), $(BUILDAPP))
  OPT = --eval "(require :asdf)" \
        --eval "(require :sb-posix)" \
        --eval '(push \#p"$(UFFI_PATH)" asdf:*central-registry*)' \
        --eval "(asdf:operate 'asdf:load-op 'uffi)" \
        --eval '(push \#p"$(CLNCURSES_PATH)" asdf:*central-registry*)' \
        --eval "(asdf:operate 'asdf:load-op 'cl-ncurses)" \
        --eval '(push \#p"$(CLPPCRE_PATH)" asdf:*central-registry*)' \
        --eval "(asdf:operate 'asdf:load-op 'cl-ppcre)" \
        --eval '(push \#p"$(shell pwd)/" asdf:*central-registry*)' \
        --eval "(asdf:operate 'asdf:load-op 'clmp)" \
        --entry "clmp:main" --output $(OUT)
#      --asdf-path $(CURDIR) --load-system $(ASDF)
else ifeq ($(COMPILER), $(SBCL))
  $(call CREATE_SCRIPT_FILE)
  OPT = --eval '(load "$(OUT).lisp")' \
        --eval '(sb-ext:run-program "/usr/bin/echo" (list "(clmp:entry-point)") :output "$(OUT).lisp" :if-output-exists :append)' \
        --eval '(compile-file "$(OUT).lisp")' --eval "(quit)" && mv "$(OUT).fasl" "$(OUT)" && rm -f "$(OUT).lisp" && chmod +x $(OUT)
else ifeq ($(COMPILER), $(SBCL_SCRIPT))
  $(call CREATE_SCRIPT_FILE)
  OPT = < <(echo '(let* ((in (open "$(OUT).lisp" :if-does-not-exist nil)) \
                         (content (make-string (file-length in)))) \
                    (read-sequence content in) \
                    (close in) \
                    (with-open-file (out "$(OUT).lisp" :direction :output :if-exists :overwrite) \
                      (format out "\#!$(COMPILER)~%~a~%~a" content "(progn (clmp:entry-point) (quit))"))) \
                  (sb-ext:run-program "/usr/bin/mv" (list "$(OUT).lisp" "$(OUT)") :output "/dev/null" :if-output-exists :append) \
                  (sb-ext:run-program "/usr/bin/chmod" (list "+x" "$(OUT)") :output "/dev/null" :if-output-exists :append) \
                  (quit)')
else
  $(error Compiler is not set $(COMPILER))
endif

$(OUT): $(SRC)
	$(COMPILER) $(OPT)

.PHONY: install clean uninstall

install:
	cp -f $(OUT) /usr/bin/

clean:
	rm -f $(OUT)

uninstall:
	rm -f /usr/bin/$(OUT)
