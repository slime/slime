### Makefile for SLIME
#
# This file is in the public domain.

# Variables
#
EMACS=emacs
LISP=sbcl

LOAD_PATH=-L .

ELFILES := slime.el slime-autoloads.el slime-tests.el
ELCFILES := $(patsubst %.el,%.elc,$(ELFILES))

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
slime.elc: slime.el ChangeLog

%.elc: %.el
	$(EMACS) -Q $(LOAD_PATH) --batch -f batch-byte-compile $<

compile: $(ELCFILES)
	$(EMACS) -Q --batch --eval "(batch-byte-recompile-directory 0)" ./lib

# Automated tests
#
SELECTOR=(quote t)

check: compile
	${EMACS} -Q --batch $(LOAD_PATH)				\
		--eval "(require 'slime-tests)"				\
		--eval "(slime-setup)"					\
		--eval "(setq inferior-lisp-program \"$(LISP)\")"	\
		--eval "(slime-batch-test $(SELECTOR))"

elpa-slime:
	echo "Not implemented yet: elpa-slime target" && exit 255

elpa: elpa-slime contrib-elpa

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
