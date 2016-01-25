#
# datetime-fortran Makefile
#
########################################################################
# Compiler and flags

### GNU ###
FC      = gfortran
FCFLAGS = -cpp -g -O0 -C -fbacktrace

### Intel ###
#FC      = ifort
#FCFLAGS = -cpp -g -O0 -C -traceback -assume realloc_lhs

### Cray ###
#FC      = ftn
#FCFLAGS = -O 0 -e Z -g

########################################################################
# Targets

.PHONY: all datetime tests

all: tests

datetime:
	@echo "Building $@"
	$(MAKE) FC=$(FC) FCFLAGS="$(FCFLAGS)" --directory=src/lib

tests: datetime
	@echo "Building $@"
	$(MAKE) FC=$(FC) FCFLAGS="$(FCFLAGS)" --directory=src/tests
	cp src/tests/datetime_tests .

clean:
	$(MAKE) --directory=src/lib clean
	$(MAKE) --directory=src/tests clean
	rm -v datetime_tests
