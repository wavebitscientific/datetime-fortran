module mod_strftime
!=======================================================================
!
! mod_strftime: Interfaces to strftime and strptime procedures from
! from C/C++ standard library.
!
!=======================================================================

use,intrinsic :: iso_c_binding,only:c_char,c_int

implicit none

private

public :: tm_struct
public :: c_strftime
public :: c_strptime

type,bind(c) :: tm_struct

  !! A derived type provided for compatibility with C/C++ time struct.
  !! Allows for calling strftime and strptime procedures through the 
  !! iso_c_binding.

  integer(kind=c_int) :: tm_sec   !! Seconds      [0-60] (1 leap second)
  integer(kind=c_int) :: tm_min   !! Minutes      [0-59]
  integer(kind=c_int) :: tm_hour  !! Hours        [0-23]
  integer(kind=c_int) :: tm_mday  !! Day          [1-31]
  integer(kind=c_int) :: tm_mon   !! Month        [0-11]
  integer(kind=c_int) :: tm_year  !! Year - 1900
  integer(kind=c_int) :: tm_wday  !! Day of week  [0-6]
  integer(kind=c_int) :: tm_yday  !! Days in year [0-365]
  integer(kind=c_int) :: tm_isdst !! DST          [-1/0/1]

endtype tm_struct
!=======================================================================



interface

  !! Interface to C procedures strftime and strptime through 
  !! iso_c_binding.

  function c_strftime(str,slen,format,tm)&
    bind(c,name='strftime') result(rc)

    !! Returns a formatted time string, given input time struct and 
    !! format. Refer to C standard library documentation for more 
    !! information. 

    import :: c_char,c_int
    import :: tm_struct

    implicit none

    ! Arguments
    character(kind=c_char),dimension(*),intent(out) :: str    !! result string
    integer(kind=c_int),value,          intent(in)  :: slen   !! string length
    character(kind=c_char),dimension(*),intent(in)  :: format !! time format 
    type(tm_struct),                    intent(in)  :: tm     !! tm_struct instance
    integer(kind=c_int)                             :: rc     !! return code

  endfunction c_strftime



  function c_strptime(str,format,tm) bind(c,name='strptime') result(rc)

    !! Returns a time struct object based on the input time string str,
    !! formatted using format. Refer to C standard library documentation 
    !! for more information.

    import :: c_char,c_int
    import :: tm_struct

    implicit none

    ! Arguments
    character(kind=c_char),dimension(*),intent(in)  :: str    !! input string
    character(kind=c_char),dimension(*),intent(in)  :: format !! time format
    type(tm_struct),                    intent(out) :: tm     !! result tm_struct
    integer(kind=c_int)                             :: rc     !! return code

  endfunction c_strptime

endinterface
!=======================================================================
endmodule mod_strftime
