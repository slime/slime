# Variables
#
EMACS_BIN ?= emacs
LISP_BIN ?= sbcl
SLIME_VERSION=$(shell grep "Version:" slime.el | grep -E -o "[0-9.]+$$")
CONTRIBS = $(patsubst contrib/slime-%.el,%,$(wildcard contrib/slime-*.el))
EMACS_23=$(shell $(EMACS_BIN) --version | grep -E 23)
EMACS_24=$(shell $(EMACS_BIN) --version | grep -E 24)
ERT=https://raw.github.com/ohler/ert/c619b56c5bc6a866e33787489545b87d79973205/lisp/emacs-lisp/ert.el
ERTX=https://raw.github.com/ohler/ert/c619b56c5bc6a866e33787489545b87d79973205/lisp/emacs-lisp/ert-x.el
CL_LIB=http://elpa.gnu.org/packages/cl-lib-0.3.el

# emacs 24.4 allows us to add to the end of the load path using `:'
# which is what we want in these version 24, especially since
# cl-lib.el might be in the dir to shadow emacs's own.
#
ifeq ($(shell $(EMACS_BIN) --version | grep -E 24.\(3.5\|4\)),)
    COLON =
else
    COLON = :
endif
LOAD_PATH=-L $(COLON). -L $(COLON)./contrib

# Dependencies
#
ensure_cl_lib:
	[ -f cl-lib.el ] || curl -o cl-lib.el $(CL_LIB)

ensure_ert:
	[ -f ert.el ]    || curl -o ert.el $(ERT)
	[ -f ert-x.el ]  || curl -o ert-x.el $(ERT)

# Compilation
#
%.elc: %.el
	${EMACS_BIN} -Q $(LOAD_PATH) --batch \
		-f batch-byte-compile $<
compile: ensure_cl_lib
	${EMACS_BIN} -Q $(LOAD_PATH) --batch \
		--eval "(batch-byte-recompile-directory 0)" .

# Automated tests
#
SELECTOR=t
OPTIONS=--batch

$(CONTRIBS:%=check-%): TEST_CONTRIBS=$(patsubst check-%,slime-%,$@)
$(CONTRIBS:%=check-%) check: ensure_ert compile
	${EMACS_BIN} -Q $(LOAD_PATH) $(OPTIONS)			\
		--eval "(require 'slime-tests)"			\
		--eval "(slime-setup '($(TEST_CONTRIBS)))"		\
		--eval "(setq inferior-lisp-program \"$(LISP_BIN)\")"	\
		--eval "(slime-batch-test $(SELECTOR))"		\


# ELPA builds for contribs
#
$(CONTRIBS:%=elpa-%): CONTRIB=$(@:elpa-%=%)
$(CONTRIBS:%=elpa-%): CONTRIB_EL=$(CONTRIB:%=contrib/slime-%.el)
$(CONTRIBS:%=elpa-%): CONTRIB_CL=$(CONTRIB:%=contrib/swank-%.lisp)
$(CONTRIBS:%=elpa-%): CONTRIB_VERSION=$(shell (			\
					  grep "Version:" $(CONTRIB_EL) \
					  || echo $(SLIME_VERSION)	\
					) | grep -E -o "[0-9.]+$$" )
$(CONTRIBS:%=elpa-%): PACKAGE=$(CONTRIB:%=slime-%-$(CONTRIB_VERSION))
$(CONTRIBS:%=elpa-%): PACKAGE_EL=$(CONTRIB:%=slime-%-pkg.el)
$(CONTRIBS:%=elpa-%):
	elpa_dir=elpa/$(PACKAGE);					\
	mkdir -p $$elpa_dir;						\
	emacs --batch $(CONTRIB_EL)					\
	--eval "(require 'cl-lib)"					\
	--eval "(search-forward \"define-slime-contrib\")"		\
	--eval "(up-list -1)"						\
	--eval "(pp							\
		(pcase (read (point-marker))				\
		  (\`(define-slime-contrib ,name ,docstring . ,rest)    \
		   \`(define-package ,name \"$(CONTRIB_VERSION)\"	\
		,docstring						\
		,(cons '(slime \"$(SLIME_VERSION)\")			\
		 (cl-loop for form in rest				\
		     when (eq :slime-dependencies (car form))		\
		     append (cl-loop for contrib in (cdr form)		\
				     if (atom contrib)			\
				     collect				\
				       \`(,contrib \"$(SLIME_VERSION)\")\
				     else				\
				     collect contrib))))))))"	>	\
	$$elpa_dir/$(PACKAGE_EL);					\
	cp $(CONTRIB_EL) $$elpa_dir;					\
	[ -r $(CONTRIB_CL) ] && cp $(CONTRIB_CL) $$elpa_dir;		\
	ls $$elpa_dir;
	cd elpa && tar cvf $(PACKAGE).tar $(PACKAGE)

elpa-slime:
	echo "Not implemented yet: elpa-slime target" && exit 255
elpa-contribs: $(CONTRIBS:%=elpa-%)
elpa: elpa-slime elpa-contribs

# Cleanup
#
clean-fasls:
	rm -rf *.fasl contrib/*.fasl
clean: clean-fasls
	rm -rf *.elc contrib/*.elc

# Legacy dists
#
dist:
	mkdir -p dist
	git archive HEAD | gzip > dist/slime-dist-$(SLIME_VERSION).gz

# Doc
#
doc-%: DOCTARGET=$(@:doc-%=%)
doc-%:
	cd doc && $(MAKE) $(DOCTARGET)
doc: doc-all

.PHONY: clean elpa compile check doc dist
