### Makefile for SLY
#
# This file is in the public domain.

# Variables
#
EMACS=emacs
LISP=sbcl

LOAD_PATH=-L . -L contrib/

ELFILES := sly.el sly-autoloads.el $(wildcard lib/*.el)
ELCFILES := $(ELFILES:.el=.elc)

CONTRIBS = $(patsubst contrib/sly-%.el,%,$(wildcard contrib/sly-*.el))

CONTRIB_ELFILES := $(wildcard contrib/*.el)
CONTRIB_ELCFILES := $(CONTRIB_ELFILES:.el=.elc)

TEST_ELFILES := $(wildcard test/*.el)
TEST_ELCFILES := $(TEST_ELFILES:.el=.elc)

all: compile compile-contrib

# Compilation
#
sly.elc: sly.el lib/hyperspec.elc

%.elc: %.el
	$(EMACS) -Q $(LOAD_PATH) --batch -f batch-byte-compile $<

compile: $(ELCFILES)
compile-contrib: $(CONTRIB_ELCFILES)
compile-test: $(TEST_ELCFILES)

# Automated tests
#
SELECTOR=t

check: check-core check-fancy

check-core: compile
	$(EMACS) -Q --batch $(LOAD_PATH)				\
		--eval "(require 'sly-tests \"lib/sly-tests\")"	\
		--eval "(sly-setup)"					\
		--eval "(setq inferior-lisp-program \"$(LISP)\")"	\
		--eval '(sly-batch-test (quote $(SELECTOR)))'

check-%: CONTRIB_NAME=$(patsubst check-%,sly-%,$@)
check-%: CONTRIB_SELECTOR=(tag contrib)
check-%: compile contrib/sly-%.elc test/sly-%-tests.elc
	$(EMACS) -Q --batch $(LOAD_PATH) -L test			\
		--eval "(require (quote sly))"				\
		--eval "(sly-setup (quote ($(CONTRIB_NAME))))"		\
		--eval "(require					\
			  (intern					\
			    (format					\
			       \"%s-tests\" (quote $(CONTRIB_NAME)))))" \
		--eval "(setq inferior-lisp-program \"$(LISP)\")"	\
		--eval '(sly-batch-test (quote (tag contrib)))'	


check-fancy: compile compile-contrib
	$(EMACS) -Q --batch  $(LOAD_PATH) -L test			\
		--eval "(require (quote sly))"				\
		--eval "(sly-setup (quote (sly-fancy)))"		\
		--eval "(mapc (lambda (sym)				\
				 (require				\
				   (intern (format \"%s-tests\" sym))	\
				   nil t))				\
			      (sly-contrib-all-dependencies		\
				(quote sly-fancy)))"			\
		--eval '(setq inferior-lisp-program "$(LISP)")'		\
		--eval '(sly-batch-test (quote (tag contrib)))'	


# Cleanup
#
FASLREGEX = .*\.\(fasl\|ufasl\|sse2f\|lx32fsl\|abcl\|fas\|lib\|trace\)$$

clean-fasls:
	find . -regex '$(FASLREGEX)' -exec rm -v {} \;
	[ -d ~/.sly/fasl ] && rm -rf ~/.sly/fasl || true

clean: clean-fasls
	find . -iname '*.elc' -exec rm {} \;

# Doc
#
doc-%:
	$(MAKE) -C doc $(@:doc-%=%)
doc: doc-help

# Help
#
help:
	@printf "\
Main targets\n\
all             -- compile all .el files\n\
compile         -- compile just core SLY\n\
compile-contrib -- compile just contribs\n\
check           -- run tests in batch mode\n\
clean           -- delete generated files\n\
doc-help        -- print help about doc targets\n\
help-vars       -- print info about variables\n\
help            -- print this message\n"

help-vars:
	@printf "\
Main make variables:\n\
EMACS     -- program to start Emacs ($(EMACS))\n\
LISP      -- program to start Lisp ($(LISP))\n\
SELECTOR  -- selector for ERT tests ($(SELECTOR))\n"

.PHONY: all clean compile compile-contrib check check-core \
	check-fancy dochelp help-vars
