# This file has been placed in the public domain.
#
# Where to put the info file(s). NB: the GNU Coding Standards (GCS)
# and the Filesystem Hierarchy Standard (FHS) differ on where info
# files belong. The GCS says /usr/local/info; the FHS says
# /usr/local/share/info. Many distros obey the FHS, but people who
# installed their emacs from source probably have a GCS-ish file
# hierarchy.
infodir=/usr/local/info

# What command to use to install info file(s)
INSTALL_CMD=install -m 644

# Info files generated here.
infofiles=sly.info

TEXI = sly.texi contributors.texi

help:
	@echo -e "\
Most important targets:\n\
all               generate info, pdf, and html documents\n\
sly.info        generate the sly.info file\n\
sly.html        generate a single html file\n\
html/index.html   generate on html file per node in html/ directory\n\
html.tgz          create a tarball of all html files\n\
clean             remove generated files"

all: sly.info sly.pdf html/index.html

sly.dvi: $(TEXI)
	texi2dvi sly.texi

sly.ps: sly.dvi
	dvips -o $@ $<

sly.info: $(TEXI)
	makeinfo $<

sly.html: $(TEXI)
	texi2html --css-include=sly.css $<

html/index.html: $(TEXI)
	makeinfo -o html --css-include=sly.css --html $<

html.tgz: html/index.html
	tar -czf $@ html

DOCDIR=/project/sly/public_html/doc

publish: sly.html
	rm -rf gh-pages
	git clone git@github.com:capitaomorte/sly.git --branch gh-pages --single-branch gh-pages
	cp sly.html gh-pages/index.html
	cd gh-pages \
	&& git add index.html \
	&& git commit -a -m "Automatic documentation update" \
	&& git push origin gh-pages

sly.pdf: $(TEXI)
	texi2pdf $<

install: install-info

uninstall: uninstall-info

# Create contributors.texi, a texinfo table listing all known
# contributors of code.
#
# Explicitly includes Eric Marsden (pre-ChangeLog hacker)
#
# The gist of this horror show is that the contributor list is piped
# into texinfo-tabulate.awk with one name per line, sorted
# alphabetically.
contributors.texi: Makefile texinfo-tabulate.awk
	git log --format='%aN' | \
	sort | \
	uniq -c | \
	sort -nr| \
	sed -e 's/^[^A-Z]*//' | \
	awk -f texinfo-tabulate.awk \
	> $@

#.INTERMEDIATE: contributors.texi

# Debian's install-info wants a --section argument.
install-info: section=$(shell grep INFO-DIR-SECTION $(infofiles) | sed 's/INFO-DIR-SECTION //')
install-info: sly.info
	mkdir -p $(infodir)
	$(INSTALL_CMD) $(infofiles) $(infodir)/$(infofiles)
	@if (install-info --version && \
		install-info --version 2>&1 | sed 1q | grep -i -v debian) >/dev/null 2>&1; then \
		echo "install-info --info-dir=$(infodir) $(infodir)/$(infofiles)";\
		install-info --info-dir="$(infodir)" "$(infodir)/$(infofiles)" || :;\
	else \
		echo "install-info --infodir=$(infodir) --section $(section) $(section) $(infodir)/$(infofiles)" && \
		install-info --infodir="$(infodir)" --section $(section) ${section} "$(infodir)/$(infofiles)" || :; fi

uninstall-info:
	@if (install-info --version && \
		install-info --version 2>&1 | sed 1q | grep -i -v debian) >/dev/null 2>&1; then \
		echo "install-info --info-dir=$(infodir) --remove $(infodir)/$(infofiles)";\
		install-info --info-dir="$(infodir)" --remove "$(infodir)/$(infofiles)" || :;\
	else \
		echo "install-info --infodir=$(infodir) --remove $(infodir)/$(infofiles)";\
		install-info --infodir="$(infodir)" --remove "$(infodir)/$(infofiles)" || :; fi
	rm -f $(infodir)/$(infofiles)

clean:
	rm -f contributors.texi
	rm -f sly.aux sly.cp sly.cps sly.fn sly.fns sly.ky
	rm -f sly.kys sly.log sly.pg sly.tmp sly.toc sly.tp
	rm -f sly.vr sly.vrs
	rm -f sly.info sly.pdf sly.dvi sly.ps sly.html
	rm -rf html html.tgz
