module mod_timedelta
!=======================================================================
!
! mod_timedelta: Module that provides the timedelta class and its
!                type-bound methods and operators.
!
!=======================================================================

use,intrinsic :: iso_fortran_env,only:real32,real64

implicit none

private

public :: timedelta

type :: timedelta

  !! Class of objects that define difference between two datetime 
  !! instances. 

  private

  integer :: days         = 0 !! number of days
  integer :: hours        = 0 !! number of hours
  integer :: minutes      = 0 !! number of minutes
  integer :: seconds      = 0 !! number of seconds
  integer :: milliseconds = 0 !! number of milliseconds

  contains

  ! getter functions
  procedure,pass(self),public :: getDays
  procedure,pass(self),public :: getHours
  procedure,pass(self),public :: getMinutes
  procedure,pass(self),public :: getSeconds
  procedure,pass(self),public :: getMilliseconds

  ! public methods
  procedure,public :: total_seconds

  ! operator overloading procedures
  procedure,private :: timedelta_plus_timedelta
  procedure,private :: timedelta_minus_timedelta
  procedure,private :: unary_minus_timedelta
  procedure,private :: eq
  procedure,private :: neq
  procedure,private :: gt
  procedure,private :: ge
  procedure,private :: lt
  procedure,private :: le

  generic :: operator(+)  => timedelta_plus_timedelta
  generic :: operator(-)  => timedelta_minus_timedelta,&
                             unary_minus_timedelta
  generic :: operator(==) => eq
  generic :: operator(/=) => neq
  generic :: operator(>)  => gt
  generic :: operator(>=) => ge
  generic :: operator(<)  => lt
  generic :: operator(<=) => le

endtype timedelta

interface timedelta
  module procedure :: timedelta_constructor
endinterface timedelta

!=======================================================================
contains



pure elemental type(timedelta) function timedelta_constructor(days,&
  hours,minutes,seconds,milliseconds)

  !! Constructor function for the `timedelta` class. 

  integer,intent(in),optional :: days         !! number of days
  integer,intent(in),optional :: hours        !! number of hours
  integer,intent(in),optional :: minutes      !! number of minutes
  integer,intent(in),optional :: seconds      !! number of seconds
  integer,intent(in),optional :: milliseconds !! number of milliseconds

  if(present(days))then
    timedelta_constructor % days = days
  else
    timedelta_constructor % days = 0
  endif

  if(present(hours))then
    timedelta_constructor % hours = hours
  else
    timedelta_constructor % hours = 0
  endif

  if(present(minutes))then
    timedelta_constructor % minutes = minutes
  else
    timedelta_constructor % minutes = 0
  endif

  if(present(seconds))then
    timedelta_constructor % seconds = seconds
  else
    timedelta_constructor % seconds = 0
  endif

  if(present(milliseconds))then
    timedelta_constructor % milliseconds = milliseconds
  else
    timedelta_constructor % milliseconds = 0
  endif

endfunction timedelta_constructor



! timedelta getters
!=======================================================================

pure elemental integer function getDays(self)
  !! Returns the number of days.
  class(timedelta),intent(in) :: self !! `timedelta` instance
  getDays = self % days
endfunction getDays



pure elemental integer function getHours(self)
  !! Returns the number of hours.
  class(timedelta),intent(in) :: self !! `timedelta` instance
  getHours = self % hours
endfunction getHours



pure elemental integer function getMinutes(self)
  !! Returns the number of minutes.
  class(timedelta),intent(in) :: self !! `timedelta` instance
  getMinutes = self % minutes
endfunction getMinutes



pure elemental integer function getSeconds(self)
  !! Returns the number of seconds.
  class(timedelta),intent(in) :: self !! `timedelta` instance
  getSeconds = self % seconds
endfunction getSeconds



pure elemental integer function getMilliseconds(self)
  !! Returns the number of milliseconds.
  class(timedelta),intent(in) :: self !! `timedelta` instance
  getMilliseconds = self % milliseconds
endfunction getMilliseconds



pure elemental real(kind=real64) function total_seconds(self)

  !! Returns a total number of seconds contained in a `timedelta` 
  !! instance.

  class(timedelta),intent(in) :: self !! `timedelta` instance

  total_seconds = self % days*86400._real64& 
                + self % hours*3600._real64&
                + self % minutes*60._real64&
                + self % seconds           &
                + self % milliseconds*1e-3_real64

endfunction total_seconds



pure elemental function timedelta_plus_timedelta(t0,t1) result(t)

  !! Adds two `timedelta` instances together and returns a `timedelta` 
  !! instance. Overloads the operator `+`.

  class(timedelta),intent(in) :: t0 !! lhs `timedelta` instance
  type(timedelta), intent(in) :: t1 !! rhs `timedelta` instance
  type(timedelta)             :: t  !! result

  t = timedelta(days         = t0 % days         + t1 % days,   &
                hours        = t0 % hours        + t1 % hours,  &
                minutes      = t0 % minutes      + t1 % minutes,&
                seconds      = t0 % seconds      + t1 % seconds,&
                milliseconds = t0 % milliseconds + t1 % milliseconds)

endfunction timedelta_plus_timedelta



pure elemental function timedelta_minus_timedelta(t0,t1) result(t)

  !! Subtracts a `timedelta` instance from another. Returns a 
  !! `timedelta` instance. Overloads the operator `-`.

  class(timedelta),intent(in) :: t0 !! lhs `timedelta` instance
  type(timedelta), intent(in) :: t1 !! lhs `timedelta` instance
  type(timedelta)             :: t  !! result

  t = t0 + (-t1)

endfunction timedelta_minus_timedelta



pure elemental function unary_minus_timedelta(t0) result(t)

  !! Takes a negative of a `timedelta` instance. Overloads the operator 
  !! `-`.

  class(timedelta),intent(in) :: t0 !! `timedelta` instance
  type(timedelta)             :: t  !! result

  t % days         = -t0 % days
  t % hours        = -t0 % hours
  t % minutes      = -t0 % minutes
  t % seconds      = -t0 % seconds
  t % milliseconds = -t0 % milliseconds

endfunction unary_minus_timedelta
  


pure elemental logical function eq(td0,td1)

  !! `timedelta` object comparison operator. Returns `.true.` if `td0` 
  !! is equal to `td1` and `.false.` otherwise. Overloads the operator 
  !! `==`.

  class(timedelta),intent(in) :: td0 !! lhs `timedelta` instance
  type(timedelta), intent(in) :: td1 !! rhs `timedelta` instance

  eq = td0 % total_seconds() == td1 % total_seconds()

endfunction eq



pure elemental logical function neq(td0,td1)

  !! `timedelta` object comparison operator. Returns `.true.` if `td0` 
  !! is not equal to `td1` and `.false.` otherwise. Overloads the 
  !! operator `/=`.

  class(timedelta),intent(in) :: td0 !! lhs `timedelta` instance
  type(timedelta), intent(in) :: td1 !! rhs `timedelta` instance

  neq = .not. (td0 % total_seconds() == td1 % total_seconds())

endfunction neq



pure elemental logical function gt(td0,td1)

  !! `timedelta` object comparison operator. Returns `.true.` if
  !! `td0` is greater than `td1` and `.false.` otherwise. Overloads the 
  !! operator `>`.

  class(timedelta),intent(in) :: td0 !! lhs `timedelta` instance
  type(timedelta), intent(in) :: td1 !! rhs `timedelta` instance

  gt = td0 % total_seconds() > td1 % total_seconds()

endfunction gt



pure elemental logical function ge(td0,td1)

  !! `timedelta` object comparison operator. Returns `.true.` if `td0` 
  !! is greater than or equal to `td1` and `.false.` otherwise. 
  !! Overloads the operator >=.

  class(timedelta),intent(in) :: td0 !! lhs `timedelta` instance
  type(timedelta), intent(in) :: td1 !! rhs `timedelta` instance

  ge = td0 % total_seconds() >= td1 % total_seconds()

endfunction ge



pure elemental logical function lt(td0,td1)

  !! `timedelta` object comparison operator. Returns `.true.` if `td0` 
  !! is less than `td1` and `.false.` otherwise. Overloads the operator 
  !! `<`.

  class(timedelta),intent(in) :: td0 !! lhs `timedelta` instance
  type(timedelta), intent(in) :: td1 !! rhs `timedelta` instance

  lt = td0 % total_seconds() < td1 % total_seconds()

endfunction lt



pure elemental logical function le(td0,td1)

  !! `timedelta` object comparison operator. Returns `.true.` if `td0` 
  !! is less than or equal to `td1` and `.false.` otherwise. Overloads 
  !! the operator `<=`.

  class(timedelta),intent(in) :: td0 !! lhs `timedelta` instance
  type(timedelta), intent(in) :: td1 !! rhs `timedelta` instance

  le = td0 % total_seconds() <= td1 % total_seconds()

endfunction le
!=======================================================================
endmodule mod_timedelta
