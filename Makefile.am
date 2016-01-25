#libdatetime_la_SOURCES = datetime.f90

AM_FCFLAGS = -Wall -O0 -C -fbacktrace

# Note that we specify '.' first to ensure this dir is build before tests
SUBDIRS = . tests

pkgconfig_DATA = datetime-fortran.pc

#datetime_SOURCES = datetime.f90
lib_LIBRARIES = libdatetime.a
#datetime_moduledir = $(libdir)
#datetime_module_LIBRARIES = datetime_module.mod

libdatetime_a_SOURCES = datetime.f90

datetime_module.mod datetime.o : datetime.f90
	$(FC) -c datetime.f90

libdatetime.a : datetime.o
	ar r libdatetime.a datetime.o

CLEANFILES =*.mod datetime-fortran.pc

#MAINTAINERCLEANFILES = *.gz aclocal.m4 depcomp install.sh missing \
#	autoscan.log Makefile.in test-driver

scrub-clean :
	scrub-clean.sh


install-exec-hook : datetime_module.mod
	cp datetime_module.mod $(libdir)

uninstall-hook :
	rm $(libdir)/datetime_module.mod
