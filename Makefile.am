SUBDIRS = src/lib src/tests

# Next line required by automake < 1.15
pkgconfigdir = $(libdir)/pkgconfig
pkgconfig_DATA = datetime-fortran.pc

scrub-clean :
	./scrub-clean.sh

CLEANFILES = datetime-fortran.pc

EXTRA_DIST = CONTRIBUTORS LICENSE README.md scrub-clean.sh

#dist-hook:
#	./mkdist.sh
