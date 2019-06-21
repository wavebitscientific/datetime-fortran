module mod_constants
!=======================================================================
!
! mod_constants: Basic constants and time conversion factors.
!
!=======================================================================

use,intrinsic :: iso_fortran_env,only:real32,real64

implicit none

private

public :: zero,one,d2h,h2d,d2m,m2d,m2h,s2d,d2s,h2s,s2h,m2s,s2m,MAXSTRLEN

real(kind=real64),parameter :: zero = 0.0_REAL64 !! 0
real(kind=real64),parameter :: one  = 1.0_REAL64 !! 1

! Constant multipliers that transform a number 
! of some time unit to another:
real(kind=real64),parameter :: d2h  = 24.0_REAL64     !! day    -> hour
real(kind=real64),parameter :: h2d  = one/d2h         !! hour   -> day
real(kind=real64),parameter :: d2m  = d2h*60.0_REAL64 !! day    -> minute
real(kind=real64),parameter :: m2d  = one/d2m         !! minute -> day
real(kind=real64),parameter :: m2h  = one/60.0_REAL64 !! minute -> hour
real(kind=real64),parameter :: s2d  = m2d/60.0_REAL64 !! second -> day
real(kind=real64),parameter :: d2s  = 86400.0_REAL64  !! day    -> second
real(kind=real64),parameter :: h2s  = 3600.0_REAL64   !! hour   -> second
real(kind=real64),parameter :: s2h  = one/h2s         !! second -> hour
real(kind=real64),parameter :: m2s  = 60.0_REAL64     !! minute -> second
real(kind=real64),parameter :: s2m  = one/m2s         !! second -> minute
 
! Maximum string length for strftime.
! Constant for now; may become a preprocessor macro later.
integer,parameter :: MAXSTRLEN = 99

!=======================================================================
endmodule mod_constants
