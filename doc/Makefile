all: slime.ps slime.info

slime.ps: slime.dvi
	dvips -o $@ $<

slime.dvi: slime.texi contributors.texi
	texi2dvi slime.texi

slime.pdf: slime.texi
	texi2pdf $<

slime.info: slime.texi
	makeinfo $<

# Create contributors.texi, a texinfo table listing all known
# contributors of code.
#
# Explicitly includes Eric Marsden (pre-ChangeLog hacker)
#
# The gist of this horror show is that the contributor list is piped
# into texinfo-tabulate.awk with one name per line, sorted
# alphabetically.
contributors.texi: ../ChangeLog Makefile texinfo-tabulate.awk
	cat ../ChangeLog | \
	grep '^[0-9]' | \
	sed -e 's/^[^ ]* *//' -e 's/ *<.*//' | \
	(cat; echo 'Eric Marsden') | \
	sort -u | \
	awk -f texinfo-tabulate.awk \
	> $@

.INTERMEDIATE: contributors.texi

