nodist_include_HEADERS = datetime_module.mod
#nodist_HEADERS = datetime_module.mod
#nodist_SOURCES = *.mod

lib_LIBRARIES = libdatetime.a

libdatetime_a_SOURCES = datetime_module.f90

CLEANFILES = *.mod

# Mostly unchanged from original upstream Makefile
OBJS = datetime_module.o

.f90.o:
	$(FC) -c $(FCFLAGS) $<

libdatetime.a : datetime_module.o
	ar ruv libdatetime.a $(OBJS)

datetime_module.mod : datetime_module.o

#install-exec-hook : datetime_module.mod
#	cp datetime_module.mod $(libdir)

#uninstall-hook :
#	rm $(libdir)/datetime_module.mod

datetime_module.o : datetime_module.f90
