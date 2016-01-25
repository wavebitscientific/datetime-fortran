#
# datetime-fortran Makefile
#
LIB = ../lib
INCLUDE = ../lib

.SUFFIXES: .F90 .f90 .o
.PHONY: all datetime_tests clean

OBJ = datetime_tests.o

.f90.o:
	$(FC) -c $(FCFLAGS) -I$(INCLUDE) $<

all:
	$(MAKE) datetime_tests

datetime_tests: datetime_tests.o
	$(FC) $(FCFLAGS) $(OBJ) -L$(LIB) -ldatetime -o $@

datetime_tests.o: datetime_tests.f90

clean:
	rm -f *.o *.mod datetime_tests
