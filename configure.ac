AC_PREREQ([2.69])
AC_INIT([datetime-fortran], [1.3.0])
AC_CONFIG_SRCDIR([datetime.f90])

# foreign flag tells that we are not conforming to the GNU standards
# and hence can ommit NEWS, README, COPYING
AM_INIT_AUTOMAKE([foreign]) 


# for package configuration
PKG_PROG_PKG_CONFIG
PKG_INSTALLDIR

AC_PROG_FC([gfortran], [Fortran 90])
AC_PROG_RANLIB

AC_CONFIG_FILES([Makefile])
AC_CONFIG_FILES([datetime-fortran.pc:datetime-fortran.pc.in])
AC_OUTPUT
