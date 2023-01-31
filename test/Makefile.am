noinst_PROGRAMS = datetime_tests
EXTRA_DIST = tests-env.sh
datetime_tests_SOURCES = datetime_tests.f90
CLEANFILES = *.mod

# Mostly unchanged from original upstream Makefile
LIB = ../src
INCLUDE = ../src
OBJ = datetime_tests.o

.f90.o:
	$(FC) -c $(FCFLAGS) -I$(INCLUDE) $<

datetime_tests$(EXEEXT): datetime_tests.o
	$(FC) $(FCFLAGS) $(OBJ) -L$(LIB) -ldatetime -o $@

#bin_SCRIPTS = . tests/tests-env.sh
AM_TESTS_ENVIRONMENT = . ./tests-env.sh
AM_TESTS_FD_REDIRECT = 9>&2
TESTS = tests-env.sh
