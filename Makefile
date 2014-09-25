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

MV = $(shell which mv)
RM = $(shell which rm)
CHMOD = $(shell which chmod)
ECHO = $(shell which echo)

define CREATE_SCRIPT_FILE
  $(info "comp = $(COMPILER)")
  $(if $(filter $(COMPILER), $(CLISP) $(CLISP_SCRIPT)),
    $(shell echo '(load "$(ASDF_FILE)")' > $(OUT).lisp),
    $(shell echo -n "" > $(OUT).lisp))
  $(shell echo "(require :asdf)" >> "$(OUT).lisp")
  $(if $(filter $(COMPILER), $(CLISP) $(CLISP_SCRIPT)),
    $(shell echo '(push #p"$(ALEXANDRIA_PATH)" asdf:*central-registry*)' >> "$(OUT).lisp") \
    $(shell echo "(asdf:operate 'asdf:load-op 'alexandria)" >> "$(OUT).lisp") \
    $(shell echo '(push #p"$(TRIVIAL_FEATURES_PATH)" asdf:*central-registry*)' >> "$(OUT).lisp") \
    $(shell echo "(asdf:operate 'asdf:load-op 'trivial-features)" >> "$(OUT).lisp") \
    $(shell echo '(push #p"$(BABEL_PATH)" asdf:*central-registry*)' >> "$(OUT).lisp") \
    $(shell echo "(asdf:operate 'asdf:load-op 'babel)" >> "$(OUT).lisp") \
    $(shell echo '(push #p"$(CFFI_PATH)" asdf:*central-registry*)' >> "$(OUT).lisp") \
    $(shell echo "(asdf:operate 'asdf:load-op 'cffi-uffi-compat)" >> "$(OUT).lisp") \
    $(shell echo '(push #p"$(BORDEAUX_PATH)" asdf:*central-registry*)' >> "$(OUT).lisp") \
    $(shell echo "(asdf:operate 'asdf:load-op 'bordeaux-threads)" >> "$(OUT).lisp"),
    $(shell echo '(push #p"$(SB_POSIX_PATH)" asdf:*central-registry*)' >> "$(OUT).lisp") \
    $(shell echo "(asdf:operate 'asdf:load-op 'sb-posix)" >> "$(OUT).lisp") \
    $(shell echo '(push #p"$(UFFI_PATH)" asdf:*central-registry*)' >> "$(OUT).lisp") \
    $(shell echo "(asdf:operate 'asdf:load-op 'uffi)" >> "$(OUT).lisp"))
  $(shell echo '(push #p"$(CLNCURSES_PATH)" asdf:*central-registry*)' >> "$(OUT).lisp")
  $(shell echo "(asdf:operate 'asdf:load-op 'cl-ncurses)" >> "$(OUT).lisp")
  $(shell echo '(push #p"$(CLPPCRE_PATH)" asdf:*central-registry*)' >> "$(OUT).lisp")
  $(shell echo "(asdf:operate 'asdf:load-op 'cl-ppcre)" >> "$(OUT).lisp")
  $(shell echo '(push #p"$(CURDIR)/" asdf:*central-registry*)' >> "$(OUT).lisp")
  $(shell echo -n "(asdf:operate 'asdf:load-op 'clmp)" >> "$(OUT).lisp")
endef

ifeq ($(COMPILER), $(BUILDAPP))
  #--eval '(load "$(ASDF_FILE)")'
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
  $(eval $(call CREATE_SCRIPT_FILE))
  OPT = --eval '(load "$(OUT).lisp")' \
        --eval '(sb-ext:run-program "$(ECHO)" (list "-e" "\\n(clmp:entry-point)" "\\n(quit)") :output "$(OUT).lisp" :if-output-exists :append)' \
        --eval '(compile-file "$(OUT).lisp")' \
        --eval '(sb-ext:run-program "$(MV)" (list "$(OUT).fasl" "$(OUT)") :output "/dev/null" :if-output-exists :append)' \
        --eval '(sb-ext:run-program "$(RM)" (list "-f" "$(OUT).lisp") :output "/dev/null" :if-output-exists :append)' \
        --eval '(sb-ext:run-program "$(CHMOD)" (list "+x" "$(OUT)") :output "/dev/null" :if-output-exists :append)' \
        --eval "(quit)"
else ifeq ($(COMPILER), $(SBCL_SCRIPT))
  $(eval $(call CREATE_SCRIPT_FILE))
  OPT = < <(echo '(let* ((in (open "$(OUT).lisp" :if-does-not-exist nil)) \
                         (content (make-string (file-length in)))) \
                    (read-sequence content in) \
                    (close in) \
                    (with-open-file (out "$(OUT).lisp" :direction :output :if-exists :overwrite) \
                      (format out "\#!$(COMPILER)~%~a~%~a" content "(progn (clmp:entry-point) (quit))"))) \
                  (sb-ext:run-program "$(MV)" (list "$(OUT).lisp" "$(OUT)") :output "/dev/null" :if-output-exists :append) \
                  (sb-ext:run-program "$(CHMOD)" (list "+x" "$(OUT)") :output "/dev/null" :if-output-exists :append) \
                  (quit)')
else ifeq ($(COMPILER), $(CLISP))
  $(eval $(call CREATE_SCRIPT_FILE))
  OPT = -x '(load "$(ASDF_FILE)")' \
        -x '(load "$(OUT).lisp")' \
        -x '(run-program "$(ECHO)" :arguments (list "-e" "\\n(clmp:entry-point)" "\\n(quit)") :output "$(OUT).lisp" :if-output-exists :append)' \
        -x '(compile-file "$(OUT).lisp")' \
        -x '(let* ((in (open "$(OUT).fas" :if-does-not-exist nil)) \
                   (content (make-string (file-length in)))) \
              (read-sequence content in) \
              (close in) \
              (with-open-file (out "$(OUT).fas" :direction :output :if-exists :overwrite) \
                (format out "\#!$(COMPILER)~%~a" content)))' \
        -x '(run-program "$(MV)" :arguments (list "$(OUT).fas" "$(OUT)") :output "/dev/null" :if-output-exists :append)' \
        -x '(run-program "$(CHMOD)" :arguments (list "+x" "$(OUT)") :output "/dev/null" :if-output-exists :append)' \
	-x "(quit)"
else ifeq ($(COMPILER), $(CLISP_SCRIPT))
  $(eval $(call CREATE_SCRIPT_FILE))
  OPT = -x '(load "$(ASDF_FILE)")' \
        -x '(load "$(OUT).lisp")' \
        -x '(run-program "$(ECHO)" :arguments (list "-e" "\\n(clmp:entry-point)" "\\n(quit)") :output "$(OUT).lisp" :if-output-exists :append)' \
        -x '(let* ((in (open "$(OUT).lisp" :if-does-not-exist nil)) \
                   (content (make-string (file-length in)))) \
              (read-sequence content in) \
              (close in) \
              (with-open-file (out "$(OUT).lisp" :direction :output :if-exists :overwrite) \
                (format out "\#!$(COMPILER)~%~a" content)))' \
        -x '(run-program "$(MV)" :arguments (list "$(OUT).lisp" "$(OUT)") :output "/dev/null" :if-output-exists :append)' \
        -x '(run-program "$(CHMOD)" :arguments (list "+x" "$(OUT)") :output "/dev/null" :if-output-exists :append)' \
	-x "(quit)"
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
