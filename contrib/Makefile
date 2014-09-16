### Makefile for contribs
#
# This file is in the public domain.

EMACS=emacs
LISP=sbcl

LOAD_PATH=-L . -L ..
CONTRIBS = $(patsubst sly-%.el,%,$(wildcard sly-*.el))
SLY_VERSION=$(shell grep "Version:" ../sly.el | grep -E -o "[0-9.]+$$")

ELFILES := $(shell find . -type f -iname "*.el")
ELCFILES := $(patsubst %.el,%.elc,$(ELFILES))

%.elc: %.el
	$(EMACS) -Q $(LOAD_PATH) --batch -f batch-byte-compile $<

compile: $(ELCFILES)
	$(EMACS) -Q --batch $(LOAD_PATH) \
	        --eval "(batch-byte-recompile-directory 0)" .

SELECTOR=t

$(CONTRIBS:%=check-%): CONTRIB_NAME=$(patsubst check-%,sly-%,$@)
$(CONTRIBS:%=check-%): SELECTOR=(tag contrib)
$(CONTRIBS:%=check-%): compile
	$(EMACS) -Q --batch $(LOAD_PATH) -L test                        \
	        --eval "(require (quote sly))"                        \
	        --eval "(sly-setup (quote ($(CONTRIB_NAME))))"        \
	        --eval "(mapc (lambda (sym)                             \
	                         (require                               \
	                           (intern (format \"%s-tests\" sym))   \
	                           nil t))                              \
	                      (sly-contrib-all-dependencies           \
	                        (quote $(CONTRIB_NAME))))"              \
	        --eval "(setq inferior-lisp-program \"$(LISP)\")"       \
	        --eval '(sly-batch-test (quote $(SELECTOR)))'

check-all: $(CONTRIBS:%=check-%)
