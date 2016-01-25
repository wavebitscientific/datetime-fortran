# datetime-fortran Makefile

FC = gfortran
FCFLAGS = -Wall -O0 -C -fbacktrace

.SUFFIXES: .F90 .f90 .o
.PHONY: all datetime clean

OBJS = datetime.o mod_datetime.o mod_timedelta.o mod_strftime.o mod_clock.o mod_constants.o

.f90.o:
	$(FC) -c $(FCFLAGS) $<

.F90.o:
	$(FC) -c $(FCFLAGS) $<

all:
	$(MAKE) datetime
	ar ruv libdatetime.a $(OBJS)

datetime: datetime.o

datetime.o:      datetime.f90 mod_clock.o mod_datetime.o mod_strftime.o mod_timedelta.o
mod_clock.o:     mod_clock.f90 mod_datetime.o mod_timedelta.o
mod_datetime.o:  mod_datetime.f90 mod_constants.o mod_strftime.o mod_timedelta.o
mod_timedelta.o: mod_timedelta.f90 
mod_strftime.o:  mod_strftime.f90 
mod_constants.o: mod_constants.f90 

clean:
	rm -f *.o *.mod *.a
