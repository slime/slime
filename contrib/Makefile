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

# ELPA builds for contribs
#
$(CONTRIBS:%=elpa-%): CONTRIB=$(@:elpa-%=%)
$(CONTRIBS:%=elpa-%): CONTRIB_EL=$(CONTRIB:%=sly-%.el)
$(CONTRIBS:%=elpa-%): CONTRIB_CL=$(CONTRIB:%=slynk-%.lisp)
$(CONTRIBS:%=elpa-%): CONTRIB_VERSION=$(shell (                          \
	                                  grep "Version:" $(CONTRIB_EL)  \
	                                  || echo $(SLY_VERSION)       \
	                                ) | grep -E -o "[0-9.]+$$" )
$(CONTRIBS:%=elpa-%): PACKAGE=$(CONTRIB:%=sly-%-$(CONTRIB_VERSION))
$(CONTRIBS:%=elpa-%): PACKAGE_EL=$(CONTRIB:%=sly-%-pkg.el)
$(CONTRIBS:%=elpa-%): ELPA_DIR=elpa/$(PACKAGE)
$(CONTRIBS:%=elpa-%): compile
	elpa_dir=$(ELPA_DIR)
	mkdir -p $$elpa_dir;                                             \
	emacs --batch $(CONTRIB_EL)                                      \
	--eval "(require 'cl-lib)"                                       \
	--eval "(search-forward \"define-sly-contrib\")"               \
	--eval "(up-list -1)"                                            \
	--eval "(pp                                                      \
	        (pcase (read (point-marker))                             \
	          (\`(define-sly-contrib ,name ,docstring . ,rest)     \
	           \`(define-package ,name \"$(CONTRIB_VERSION)\"        \
	        ,docstring                                               \
	        ,(cons '(sly \"$(SLY_VERSION)\")                     \
	         (cl-loop for form in rest                               \
	             when (eq :sly-dependencies (car form))            \
	             append (cl-loop for contrib in (cdr form)           \
	                             if (atom contrib)                   \
	                             collect                             \
	                               \`(,contrib \"$(SLY_VERSION)\") \
	                             else                                \
	                             collect contrib))))))))"   >        \
	$$elpa_dir/$(PACKAGE_EL);                                        \
	cp $(CONTRIB_EL) $$elpa_dir;                                     \
	[ -r $(CONTRIB_CL) ] && cp $(CONTRIB_CL) $$elpa_dir;             \
	ls $$elpa_dir
	cd elpa && tar cvf $(PACKAGE).tar $(PACKAGE)
	rm -rf $(ELPA_DIR)

elpa-all: $(CONTRIBS:%=elpa-%)

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
