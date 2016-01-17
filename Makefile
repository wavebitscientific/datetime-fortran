
FC      = gfortran
FCFLAGS = -Wall -O0 -C -fbacktrace

# Rules
all: datetime.f90
	$(FC) -c $(FCFLAGS) datetime.f90
	ar r libdatetime.a datetime.o
	 
.PHONY:
clean:
	rm -f datetime.o datetime_module.mod libdatetime.a
