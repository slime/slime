### Makefile for SLY
#
# This file is in the public domain.

# Variables
#
EMACS=emacs
LISP=sbcl

LOAD_PATH=-L .

ELFILES := sly.el sly-autoloads.el $(wildcard lib/*.el)
ELCFILES := $(ELFILES:.el=.elc)

default: compile contrib-compile

all: compile

help:
	@printf "\
Main targets\n\
all        -- see compile\n\
compile    -- compile .el files\n\
check      -- run tests in batch mode\n\
clean      -- delete generated files\n\
doc-help   -- print help about doc targets\n\
help-vars  -- print info about variables\n\
help       -- print this message\n"

help-vars:
	@printf "\
Main make variables:\n\
EMACS     -- program to start Emacs ($(EMACS))\n\
LISP      -- program to start Lisp ($(LISP))\n\
SELECTOR  -- selector for ERT tests ($(SELECTOR))\n"

# Compilation
#
sly.elc: sly.el lib/hyperspec.elc

%.elc: %.el
	$(EMACS) -Q $(LOAD_PATH) --batch -f batch-byte-compile $<

compile: $(ELCFILES)

# Automated tests
#
SELECTOR=t

check: compile
	${EMACS} -Q --batch $(LOAD_PATH)				\
		--eval "(require 'sly-tests \"lib/sly-tests\")"	        \
		--eval "(sly-setup)"					\
		--eval "(setq inferior-lisp-program \"$(LISP)\")"	\
		--eval '(sly-batch-test (quote $(SELECTOR)))'

# run tests interactively
#
# FIXME: Not terribly useful until bugs in ert-run-tests-interactively
# are fixed.
test: compile
	${EMACS} -Q -nw $(LOAD_PATH)					\
		--eval "(require 'sly-tests \"lib/sly-tests\")"	        \
		--eval "(sly-setup)"					\
		--eval "(setq inferior-lisp-program \"$(LISP)\")"	\
		--eval '(sly-batch-test (quote $(SELECTOR)))'

elpa-sly:
	echo "Not implemented yet: elpa-sly target" && exit 255

elpa: elpa-sly contrib-elpa

# Cleanup
#
clean-fasls:
	find . -iname '*.fasl' -exec rm {} \;
clean: clean-fasls
	find . -iname '*.elc' -exec rm {} \;


# Contrib stuff. Should probably also go to contrib/
# 
MAKECONTRIB=$(MAKE) -C contrib EMACS="$(EMACS)" LISP="$(LISP)"
contrib-check-% check-%:
	$(MAKECONTRIB) $(@:contrib-%=%)
contrib-elpa:
	$(MAKECONTRIB) elpa-all
contrib-compile:
	$(MAKECONTRIB) compile

# Doc
#
doc-%:
	$(MAKE) -C doc $(@:doc-%=%)
doc: doc-help

.PHONY: clean elpa compile check doc dist
