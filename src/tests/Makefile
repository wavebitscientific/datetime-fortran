# datetime-fortran Makefile

FC = gfortran
FCFLAGS = -Wall -O0 -C -fbacktrace
LIB = ../lib
INCLUDE = ../lib

.SUFFIXES: .F90 .f90 .o
.PHONY: all tests clean

OBJ = datetime_tests.o

.f90.o:
	$(FC) -c $(FCFLAGS) -I$(INCLUDE) $<

all:
	$(MAKE) tests

tests: datetime_tests.o
	$(FC) $(FCFLAGS) $(OBJ) -L$(LIB) -ldatetime -o $@

datetime_tests.o: datetime_tests.f90

clean:
	rm -f *.o *.mod *.a
