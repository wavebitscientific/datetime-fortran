module mod_clock
!=======================================================================
!
! mod_clock
!
!=======================================================================

use,intrinsic :: iso_fortran_env,only:real32,real64
use,intrinsic :: iso_c_binding,only:c_char,c_int,c_null_char
use :: mod_datetime,only:datetime
use :: mod_timedelta,only:timedelta

implicit none

private

! Derived types:
public :: clock

type :: clock

  !! A clock object with a start, stop and current times, tick interval
  !! and tick methods.

  type(datetime) :: startTime
  type(datetime) :: stopTime
  type(datetime) :: currentTime

  type(timedelta) :: tickInterval

  ! May become Alarm class in some future release;
  ! for now, just a switch
  logical :: alarm = .false.

  ! Clock status flags
  logical :: started = .false.
  logical :: stopped = .false.

  contains

  procedure :: reset
  procedure :: tick

endtype clock
!=======================================================================
contains


!=======================================================================
pure elemental subroutine reset(self)

  !! Resets the clock to its start time.

  class(clock),intent(inout) :: self

  self % currentTime = self % startTime

  self % started = .false.
  self % stopped = .false.

endsubroutine reset
!=======================================================================



!=======================================================================
pure elemental subroutine tick(self)

  !! Increments the currentTime of the clock instance by one tickInterval.

  class(clock),intent(inout) :: self

  if(self % stopped)then
    return
  endif

  if(.not.self % started)then
    self % started = .true.
    self % currentTime = self % startTime
  endif

  self % currentTime = self % currentTime + self % tickInterval

  if(self % currentTime >= self % stopTime)then
    self % stopped = .true.
  endif

endsubroutine tick
!=======================================================================
endmodule mod_clock
