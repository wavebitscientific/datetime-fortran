
FC      = gfortran
FCFLAGS = -O0 -C -fbacktrace -Wall

# Object list
OBJS = datetime.o

# Rules
all: tests

tests: datetime.o datetime_tests.f90
	$(FC) datetime_tests.f90 $(FCFLAGS) -o $@ $(OBJS)
	 
# Dependencies
datetime.o: datetime.f90
	$(FC) -c $(FCFLAGS) datetime.f90

.PHONY:
clean:
	rm -f tests *.o *.mod
