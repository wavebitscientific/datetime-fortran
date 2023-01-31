# some compilers (e.g. MinGW) may not have strptime in C

include(CheckFortranSourceCompiles)

check_fortran_source_compiles(
"program test
use iso_c_binding
implicit none
type,bind(c) :: tm_struct
integer(C_INT) :: tm_sec,tm_min,tm_hour,tm_mday,tm_mon,tm_year,tm_wday,tm_yday,tm_isdst
end type
interface
integer(C_INT) function strptime(str,format,tm) bind(C)
import
character(kind=c_char), intent(in) :: str(*), format(*)
type(tm_struct), intent(out) :: tm
end function
end interface

integer(C_INT) :: rc
character(kind=c_char, len=20), parameter :: str = '2018-01-01 12:00:00'
character(kind=c_char, len=18), parameter :: fmt = '%Y-%m-%d %H:%M:%S'
type(tm_struct) :: tm

rc = strptime(str,fmt,tm)

end program"
HAVE_C_STRPTIME
SRC_EXT f90
)
