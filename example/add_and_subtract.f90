program add_and_subtract
!! Adds and subtracts datetimes
use datetime_module, only : timedelta, datetime

implicit none

type (datetime) :: a, b
type (timedelta) :: t

!> add 86400 seconds
a = datetime(2020, 2, 28)
b = a + timedelta(seconds=86400)
if (b /= datetime(2020, 2, 29)) error stop 'expected 2020-02-29'

!> subtract
b = b - timedelta(seconds=35)
if (b /= datetime(2020, 2, 28, 23, 59, 25)) error stop 'expected 2020-02-28T23:59:25'

!> total_seconds is floating point
t = b - b - timedelta(milliseconds=234)

if (abs(t%total_seconds() + 0.234) > 0.0001) error stop 'expected -0.234 seconds'

end program add_and_subtract
