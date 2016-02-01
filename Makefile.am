SUBDIRS = src/lib src/tests

pkgconfigdir = $(libdir)/pkgconfig # needed for automake < 1.15
pkgconfig_DATA = datetime-fortran.pc

scrub-clean :
	./scrub-clean.sh

CLEANFILES = datetime-fortran.pc

EXTRA_DIST = CONTRIBUTORS LICENSE README.md \
	mkdist.sh scrub-clean.sh

dist-hook:
	./mkdist.sh



