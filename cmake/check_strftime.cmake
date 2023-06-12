include(CheckFortranSourceRuns)

check_fortran_source_runs(
"program test
use iso_c_binding
implicit none
type,bind(c) :: tm_struct
integer(C_INT) :: tm_sec=0,tm_min=0,tm_hour=0,tm_mday=1,tm_mon=0,tm_year=100,tm_wday=0,tm_yday=0,tm_isdst=0
end type
interface
type(C_PTR) function strftime(str, slen, format, tm) bind(c)
import
character(kind=c_char), intent(out) :: str(*)
integer(c_int), value, intent(in) :: slen
character(kind=c_char), intent(in) :: format(*)
type(tm_struct), intent(in) :: tm
end function
end interface

type(C_PTR) :: rc
character(kind=c_char,len=10) :: str
character(kind=c_char,len=8) :: fmt = '%G %V %u'
type(tm_struct) :: tm
rc = strftime(str, len(str, kind=C_INT), fmt // c_null_char, tm)
if(.not. c_associated(rc)) error stop 'strftime failed'
print '(a)', str(:index(str, c_null_char)-1)
end program"
HAVE_C_STRFTIME
)
