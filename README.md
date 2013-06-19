## Description

*datetime-fortran* is a time and date manipulation library for Fortran.
It provides derived types for date ([*datetime*](#datetime)), 
time and time difference representation ([*timedelta*](#timedelta))
as well as arithmetic operators and associated methods for their manipulation.
It also provides an interface to C/C++ **tm** struct, and associated
*strftime()* and *strptime()* functions.
*datetime-fortran* came about due to the lack of time handling facilities in standard Fortran language.
*datetime-fortran* is written and maintained by Milan Curcic of University of Miami.
It is freely available under the [GNU General Public License version 3](http://www.gnu.org/licenses/gpl.html).
Please send suggestions and bug reports by [e-mail](mailto:milan@orca.rsmas.miami.edu) or
through this Github page. See the list of [current issues](https://github.com/milancurcic/datetime-fortran/issues)
if you would like to contribute to the code.

## Features

* Derived types: [*datetime*](#datetime), [*timedelta*](#timedelta), [*tm_struct*](#tm_struct);

* Overloaded arithmetic operators `+` and `-` for *datetime* and *timedelta* objects;

* Overloaded comparison operators `>`, `>=`, `<`, `<=`, `==` and `/=` for *datetime* and *timedelta* objects;

* Interfaces to C/C++ routines [*strftime*](#strftime) and [*strptime*](#strptime) through `ISO_C_BINDING`;

* Lightweight and portable;

* Free to modify and distribute under the terms of the [GNU General Public License version 3](http://www.gnu.org/licenses/gpl.html).


## API

<a id="top"></a>

* [Derived Types](#derived-types)
    * [*datetime*](#datetime)
        * <a href="#addmilliseconds">*addMilliseconds*</a>
        * <a href="#addseconds">*addSeconds*</a>
        * <a href="#addminutes">*addMinutes*</a>
        * <a href="#addhours">*addHours*</a>
        * <a href="#adddays">*addDays*</a>
        * <a href="#isocalendar">*isocalendar*</a>
        * <a href="#isoformat">*isoformat*</a>
        * <a href="#isvalid">*isValid*</a>
        * <a href="#now">*now*</a>
        * <a href="#secondsSinceEpoch">*secondsSinceEpoch*</a>
        * <a href="#tm">*tm*</a>
        * <a href="#weekday">*weekday*</a>
        * <a href="#weekdayLong">*weekdayLong*</a>
        * <a href="#weekdayShort">*weekdayShort*</a>
        * <a href="#yearday">*yearday*</a>
    * <a href="#timedelta">*timedelta*</a>
        * [*total_seconds*](#total_seconds)
    * <a href="#tm_struct">*tm_struct*</a>
* <a href="#operators">Overloaded operators</a>
* <a href="#public-procedures">Public procedures</a>
    * <a href="#date2num">*date2num*</a>
    * <a href="#daysinmonth">*daysInMonth*</a>
    * <a href="#daysinyear">*daysInYear*</a>
    * <a href="#isleapyear">*isLeapYear*</a>
    * <a href="#num2date">*num2date*</a>
    * <a href="#strftime">*strftime*</a>
    * <a href="#strptime">*strptime*</a>


<a id="derived-types"><h2>Derived Types</h2></a>

*datetime-fortran* library provides the following derived types:
[*datetime*](#datetime), [*timedelta*](#timedelta), and [*tm_struct*](#tm_struct).

<a id="datetime"><h3>**datetime**</h3></a>

Main date and time object, defined as:

```fortran
TYPE :: datetime

  ! COMPONENTS:
  INTEGER :: year        = 1 ! Year                   [1-HUGE(year)]
  INTEGER :: month       = 1 ! Month in year          [1-12]
  INTEGER :: day         = 1 ! Day in month           [1-31]
  INTEGER :: hour        = 0 ! Hour in day            [0-23]
  INTEGER :: minute      = 0 ! Minute in hour         [0-59]
  INTEGER :: second      = 0 ! Second in minute       [0-59]
  INTEGER :: millisecond = 0 ! Milliseconds in second [0-999]

  CONTAINS

  ! METHODS:
  PROCEDURE :: addMilliseconds
  PROCEDURE :: addSeconds
  PROCEDURE :: addMinutes
  PROCEDURE :: addHours
  PROCEDURE :: addDays
  PROCEDURE :: isocalendar
  PROCEDURE :: isoformat
  PROCEDURE :: isValid
  PROCEDURE :: now
  PROCEDURE :: secondsSinceEpoch
  PROCEDURE :: tm
  PROCEDURE :: weekday
  PROCEDURE :: weekdayLong
  PROCEDURE :: weekdayShort
  PROCEDURE :: yearday

ENDTYPE datetime
```

[*datetime*](#datetime) components are initialized by default, so all arguments are optional.
Arguments may be provided as positional arguments, in the order of their declaration,
or as keyword arguments, in any order. If both positional and keyword arguments are used,
no positional arguments may appear after a keyword argument. 

Example usage:

```fortran
USE datetime_module,ONLY:datetime

TYPE(datetime) :: a

! Initialize as default:
a = datetime()                                     ! 0001-01-01 00:00:00

! Components can be specified by position:
a = datetime(1984,12,10)                           ! 1984-12-10 00:00:00

! Or by keyword:
a = datetime(month=1,day=1,year=1970)              ! 1970-01-01 00:00:00

! Or combined:
a = datetime(2013,2,minute=23,day=12,month=5)      ! 2013-02-05 00:23:00

! Do not do this:
a = datetime(year=2013,2,minute=23,day=12,month=5) ! ILLEGAL
```

Note that the current implementation of [*datetime*](#datetime) does 
not currently support time zone or daylight saving time (DST) information,
and is thus "naive" (open to interpretation).

#### See also

[Back to top](#top)

### addMilliseconds<a id="addmilliseconds"></a>

```fortran
PURE ELEMENTAL SUBROUTINE addMilliseconds(self,ms)

  ! ARGUMENTS:
  CLASS(datetime),INTENT(INOUT) :: self
  INTEGER,        INTENT(IN)    :: ms   ! Number of milliseconds to add
```

Used internally by binary arithmetic operators + and - when 
adding/subtracting a [timedelta](#timedelta) instance to/from a 
[datetime](#datetime) instance. In general, there is no need to use 
this method from external programs. However, it may be convenient
and create less overhead if the operation needs to be performed
on a large array of [datetime](#datetime) instances.

#### Arguments

`ms` Integer number of milliseconds to add. May be negative for subtraction.

#### Return value

None

#### Example usage

```fortran
USE datetime_module,ONLY:datetime

TYPE(datetime) :: a

! Initialize:
a = datetime(2013,1,1,0,0,0,0)           ! 2013-01-01 00:00:00.000

! Add:
CALL a%addMilliseconds(100)   ! a becomes: 2013-01-01 00:00:00.100
```

#### See also

* [*addSeconds*](#addseconds)

* [*addMinutes*](#addminutes)

* [*addHours*](#addhours)

* [*addDays*](#adddays)

[Back to top](#top)


### addSeconds<a id="addseconds"></a>

```fortran
PURE ELEMENTAL SUBROUTINE addSeconds(self,s)

  ! ARGUMENTS:
  CLASS(datetime),INTENT(INOUT) :: self
  INTEGER,        INTENT(IN)    :: s    ! Number of seconds to add
```

Used internally by binary arithmetic operators + and - when
adding/subtracting a [timedelta](#timedelta) instance to/from a
[datetime](#datetime) instance. In general, there is no need to use
this method from external programs. However, it may be convenient
and create less overhead if the operation needs to be performed
on a large array of [datetime](#datetime) instances.

#### Arguments

`s` Integer number of seconds to add. May be negative for subtraction.

#### Return value

None

#### Example usage

```fortran
USE datetime_module,ONLY:datetime

TYPE(datetime) :: a

! Initialize:
a = datetime(2013,1,1,0,0,0,0)           ! 2013-01-01 00:00:00.000

! Add:
CALL a%addSeconds(10)         ! a becomes: 2013-01-01 00:00:10.000
```
#### See also

* [*addMilliseconds*](#addmilliseconds)

* [*addMinutes*](#addminutes)

* [*addHours*](#addhours)

* [*addDays*](#adddays)

[Back to top](#top)

### addMinutes<a id="addminutes"></a>

```fortran
PURE ELEMENTAL SUBROUTINE addMinutes(self,m)

  ! ARGUMENTS:
  CLASS(datetime),INTENT(INOUT) :: self
  INTEGER,        INTENT(IN)    :: m    ! Number of minutes to add
```
Used internally by binary arithmetic operators + and - when
adding/subtracting a [timedelta](#timedelta) instance to/from a
[datetime](#datetime) instance. In general, there is no need to use
this method from external programs. However, it may be convenient
and create less overhead if the operation needs to be performed
on a large array of [datetime](#datetime) instances.

#### Arguments

`m` Integer number of minutes to add. May be negative for subtraction.

#### Return value

None

#### Example usage

```fortran
USE datetime_module,ONLY:datetime

TYPE(datetime) :: a

! Initialize:
a = datetime(2013,1,1,0,0,0,0)           ! 2013-01-01 00:00:00.000

! Add:
CALL a%addMinutes(10)         ! a becomes: 2013-01-01 00:10:00.000
```

#### See also

[Back to top](#top)

### addHours<a id="addhours"></a>

```fortran
PURE ELEMENTAL SUBROUTINE addHours(self,h)

  ! ARGUMENTS:
  CLASS(datetime),INTENT(INOUT) :: self
  INTEGER,        INTENT(IN)    :: h    ! Number of hours to add
```

Used internally by binary arithmetic operators + and - when
adding/subtracting a [timedelta](#timedelta) instance to/from a
[datetime](#datetime) instance. In general, there is no need to use
this method from external programs. However, it may be convenient
and create less overhead if the operation needs to be performed
on a large array of [datetime](#datetime) instances.

#### Arguments

`h` Integer number of hours to add. May be negative for subtraction.

#### Return value

None

#### Example usage

```fortran
USE datetime_module,ONLY:datetime

TYPE(datetime) :: a

! Initialize:
a = datetime(2013,1,1,0,0,0,0)           ! 2013-01-01 00:00:00.000

! Add:
CALL a%addHours(12)           ! a becomes: 2013-01-01 12:00:00.000
```

#### See also

* [*addMilliseconds*](#addmilliseconds)

* [*addSeconds*](#addseconds)

* [*addMinutes*](#addminutes)

* [*addDays*](#adddays)

[Back to top](#top)

### addDays<a id="adddays"></a>

```fortran
PURE ELEMENTAL SUBROUTINE addDays(self,d)

  ! ARGUMENTS:
  CLASS(datetime),INTENT(INOUT) :: self
  INTEGER,        INTENT(IN)    :: d    ! Number of days to add
```

Used internally by binary arithmetic operators + and - when
adding/subtracting a [timedelta](#timedelta) instance to/from a
[datetime](#datetime) instance. In general, there is no need to use
this method from external programs. However, it may be convenient
and create less overhead if the operation needs to be performed
on a large array of [datetime](#datetime) instances.

#### Arguments

`d` Integer number of days to add. May be negative for subtraction.

#### Return value

None

#### Example usage

```fortran
USE datetime_module,ONLY:datetime

TYPE(datetime) :: a

! Initialize:
a = datetime(2013,1,1,0,0,0,0)           ! 2013-01-01 00:00:00.000

! Add:
CALL a%addDays(7)             ! a becomes: 2013-01-08 00:00:00.000
```

#### See also

* [*addMilliseconds*](#addmilliseconds)

* [*addSeconds*](#addseconds)

* [*addMinutes*](#addminutes)

* [*addHours*](#addhours)

[Back to top](#top)

### isocalendar<a id="isocalendar"></a>

```fortran
FUNCTION isocalendar(self)

  ! ARGUMENTS:
  CLASS(datetime),INTENT(IN) :: self
  INTEGER,DIMENSION(3)       :: isocalendar
```

Returns an array of 3 integers: year, week number, and week day, 
as defined by [ISO 8601 week date](http://en.wikipedia.org/wiki/ISO_week_date).
The ISO calendar is a widely used variant of the Gregorian calendar.
The ISO year consists of 52 or 53 full weeks. 
A week starts on a Monday (1) and ends on a Sunday (7). 
The first week of an ISO year is the first (Gregorian) calendar week 
of a year containing a Thursday. 
This is called week number 1, and the ISO year of that Thursday 
is the same as its Gregorian year.

[*datetime%isocalendar()*](#isocalendar) is equivalent to Python's 
[*datetime.datetime.isocalendar()*](http://docs.python.org/2/library/datetime.html#datetime.datetime.isocalendar).

#### Arguments

None

#### Return value

`isocalendar` A rank 1 integer array of length 3. Contains year, week number
and week day.

#### Example usage

```fortran
USE datetime_module,ONLY:datetime

TYPE(datetime) :: a

a = datetime(2013,1,1)
WRITE(*,*)a%isocalendar() ! Prints: 2013  1  2
```

#### See also

* [*weekday*](#weekday)

[Back to top](#top)

### isoformat<a id="isoformat"></a>

```fortran
PURE ELEMENTAL CHARACTER(LEN=23) FUNCTION isoformat(self,sep)

  ! ARGUMENTS:
  CLASS(datetime), INTENT(IN)          :: self
  CHARACTER(LEN=1),INTENT(IN),OPTIONAL :: sep
```

Returns a character string of length 23 that contains date and time in ISO 8601 
format.

[*datetime%isoformat()*](#isoformat) is equivalent to Python's 
[*datetime.datetime.isoformat()*](http://docs.python.org/2/library/datetime.html#datetime.datetime.isoformat),
with the only difference being that [*datetime%isoformat()*](#isoformat) returns the milliseconds 
at the end of the string, where as 
[*datetime.datetime.isoformat()*](http://docs.python.org/2/library/datetime.html#datetime.datetime.isoformat)
returns microseconds.

#### Arguments

`sep` is an optional argument that specifies which character of length 1 will
separate date and time entries. If ommited, defaults to `T`.

#### Example usage

```fortran
USE datetime_module,ONLY:datetime

TYPE(datetime) :: a

a = datetime(1984,12,10,13,5,0)

! Without arguments:
WRITE(*,*)a%isoformat()        ! Prints 1984-12-10T13:05:00.000

! With a specified separator:
WRITE(*,*)a%isoformat(' ')     ! Prints 1984-12-10 13:05:00.000
```

#### See also

[Back to top](#top)

### isValid<a id="isvalid"></a>

```fortran
PURE ELEMENTAL LOGICAL FUNCTION isValid(self)
  
  ! ARGUMENTS:
  CLASS(datetime),INTENT(IN) :: self
```

Returns `.TRUE.` if all [*datetime*](#datetime) instance components 
have valid values, and .FALSE. otherwise. Components have valid values
if they are within the range indicated in [*datetime*](#datetime) 
derived type description.

Useful for debugging and validating user input.

#### Example usage

```fortran
USE datetime_module,ONLY:datetime

TYPE(datetime) :: a

a = datetime(1984,12,10,13,5,0)

WRITE(*,*)a%isValid()   ! .TRUE.

a = datetime(1984,13,10,13,5,0)

WRITE(*,*)a%isValid()   ! .FALSE.
```

#### See also

[Back to top](#top)

### now<a id="now"></a>

```fortran
TYPE(datetime) FUNCTION now(self)

  ! ARGUMENTS:
  CLASS(datetime),INTENT(IN) :: self
```

Returns the [*datetime*](#datetime) instance representing 
the current machine time. Currently does not support specifying a timezone.

#### Example usage

```fortran
USE datetime_module,ONLY:datetime

TYPE(datetime) :: a

a = a%now()   ! Assigns current machine time to a
```

#### See also

[Back to top](#top)

### secondsSinceEpoch<a id="secondssinceepoch"></a>

```fortran
INTEGER FUNCTION secondsSinceEpoch(self)

  ! ARGUMENTS
  CLASS(datetime),INTENT(IN) :: self
```

A wrapper around a [*strftime*](#strftime) call. 
Returns an integer number of seconds since the 
UNIX Epoch, 1970-01-01 00:00:00 +0000 (UTC).

#### Example usage

```fortran
USE datetime_module,ONLY:datetime

TYPE(datetime) :: a 

! Initialize:
a = datetime(2013,1,1)

WRITE(*,*)a%secondsSinceEpoch() 
```

#### See also

[Back to top](#top)

### tm<a id="tm"></a>

```fortran
PURE ELEMENTAL TYPE(tm_struct) FUNCTION tm(self)

  ! ARGUMENTS:
  CLASS(datetime),INTENT(IN) :: self
```

Returns a [*tm_struct*](#tm_struct) instance that matches the 
time and date information in the caller [*datetime*](#datetime)
instance.

#### Example usage

```fortran
USE datetime_module,ONLY:datetime

TYPE(datetime)  :: a
TYPE(tm_struct) :: tm

! Initialize:
a = datetime(2013,1,1)

! Get tm_struct from datetime:
tm = a%tm()
```

#### See also

[Back to top](#top)

### weekday<a id="weekday"></a>

```fortran
PURE ELEMENTAL INTEGER FUNCTION weekday(self)

  CLASS(datetime),INTENT(IN) :: self
```

A [*datetime*](#datetime)-bound method to calculate day of the week using
 [Zeller's congruence](http://en.wikipedia.org/wiki/Zeller%27s_congruence). 
Returns an integer scalar in the range of [0-6], starting from Sunday.

#### Example usage

```fortran
USE datetime_module,ONLY:datetime

TYPE(datetime)  :: a

! Initialize:
a = datetime(2013,1,1)

WRITE(*,*)a%weekday()    ! 2
```

#### See also

[*weekdayLong*](#weekdaylong)

[*weekdayShort*](#weekdayshort)

[Back to top](#top)

### weekdayLong<a id="weekdaylong"></a>

```fortran
PURE ELEMENTAL CHARACTER(LEN=9) FUNCTION weekdayLong(self)

  CLASS(datetime),INTENT(IN) :: self

```

Returns the full name of the day of the week.

#### Example usage

```fortran
USE datetime_module,ONLY:datetime

TYPE(datetime)  :: a

! Initialize:
a = datetime(2013,1,1)

WRITE(*,*)a%weekdayLong()    ! Tuesday
```

#### See also

* [*weekday*](#weekday)

* [*weekdayShort*](#weekdayshort)

[Back to top](#top)

### weekdayShort<a id="weekdayshort"></a>

```fortran
PURE ELEMENTAL CHARACTER(LEN=3) FUNCTION weekdayShort(self)

  CLASS(datetime),INTENT(IN) :: self

```

Returns the abbreviated (e.g. Mon) name of the day of the week.

#### Example usage

```fortran
USE datetime_module,ONLY:datetime

TYPE(datetime)  :: a

! Initialize:
a = datetime(2013,1,1)

WRITE(*,*)a%weekdayShort()    ! Tue
```

#### See also

* [*weekday*](#weekday)

* [*weekdayLong*](#weekdaylong)

[Back to top](#top)

### yearday<a id="yearday"></a>

```fortran
PURE ELEMENTAL INTEGER FUNCTION yearday(self)

  CLASS(datetime),INTENT(IN) :: self
```

[*datetime*](#datetime)-bound procedure. 
Returns integer day of the year (ordinal date).
Equals to `1` for any January 1, `365` for a December 31 on a non-leap year,
and `366` for a December 31 on a leap year.

#### Example usage

```fortran
USE datetime_module,ONLY:datetime

TYPE(datetime)  :: a

! Initialize:
a = datetime(2013,5,1)

WRITE(*,*)a%yearday()    ! 121 
```

#### See also

* [*isocalendar*](#isocalendar)

[Back to top](#top)

### **timedelta**

Represents a duration of time, and a difference between
two [*datetime*](#datetime) objects. It is defined as:

```fortran
TYPE :: timedelta

  ! COMPONENTS:
  INTEGER :: days         = 0
  INTEGER :: hours        = 0
  INTEGER :: minutes      = 0
  INTEGER :: seconds      = 0
  INTEGER :: milliseconds = 0

  CONTAINS

  ! METHODS:
  PROCEDURE :: total_seconds

ENDTYPE timedelta
```

All arguments are optional and default to 0. 
Similarly to [*datetime*](#datetime) objects,
[*timedelta*](#timedelta) instances can be initialized 
using positional and/or keyword arguments.
In addition, a [*timedelta*](#timedelta) object is a result
of subtraction between two [*datetime*](#datetime) objects.

#### Example usage

```fortran
USE datetime_module,ONLY:datetime,timedelta

TYPE(datetime)  :: a,b
TYPE(timedelta) :: c

! Initialize as default
c = timedelta()

! Positional arguments:
c = timedelta(0,1,15,0,0)      ! 1 hour and 15 minutes

! Keyword arguments:
c = timedelta(days=1,hours=12) ! 1 day and 12 hours

! Difference between datetimes:
a = datetime(2013,5,12,32,0,0) ! 2013-05-12 32:00:00
b = datetime(2012,9,18,14,0,0) ! 2012-09-18 14:00:00

! Subtract to get timedelta:
c = a-b 
```

[Back to top](#top)

### total_seconds<a id="totalseconds"></a>

```fortran
PURE ELEMENTAL REAL(KIND=real_dp) FUNCTION total_seconds(self)

  ! ARGUMENTS:
  CLASS(timedelta),INTENT(IN) :: self
```

A [*timedelta*](#timedelta)-bound method that returns a number 
of seconds contained in the time interval defined by the 
[*timedelta*](#timedelta) instance. This method is equivalent
to Python's [*datetime.timedelta.total_seconds*](http://docs.python.org/2/library/datetime.html#timedelta-objects) function.

#### Arguments

None

#### Return value

`total_seconds` A total number of seconds (of type `REAL(KIND=real_dp)`) 
contained in the [*timedelta*](#timedelta) instance.

#### Example usage

```fortran
USE datetime_module,ONLY:timedelta

TYPE(timedelta) :: td

td = timedelta(days=5,hours=12,minutes=15,seconds=7,milliseconds=123)

WRITE(*,*)td%total_seconds()   ! 476107.12300000002
```

[Back to top](#top)

### **tm_struct**<a id="tm_struct"></a>

Time object compatible with C/C++ *tm* struct. Available mainly 
for the purpose of calling *strftime()* and *strptime()* procedures.

```fortran
TYPE,BIND(c) :: tm_struct

  ! COMPONENTS:
  INTEGER(KIND=c_int) :: tm_sec   ! Seconds      [0-60] (1 leap second)
  INTEGER(KIND=c_int) :: tm_min   ! Minutes      [0-59]
  INTEGER(KIND=c_int) :: tm_hour  ! Hours        [0-23]
  INTEGER(KIND=c_int) :: tm_mday  ! Day          [1-31]
  INTEGER(KIND=c_int) :: tm_mon   ! Month        [0-11]
  INTEGER(KIND=c_int) :: tm_year  ! Year - 1900
  INTEGER(KIND=c_int) :: tm_wday  ! Day of week  [0-6]
  INTEGER(KIND=c_int) :: tm_yday  ! Days in year [0-365]
  INTEGER(KIND=c_int) :: tm_isdst ! DST          [-1/0/1]

  ! METHODS: None.

ENDTYPE tm_struct
```

#### See also

[Back to top](#top)

## Overloaded operators<a id="#operators"></a>

#### See also

[Back to top](#top)

## Public procedures<a id="#public-procedures"></a>
    
### date2num<a id="date2num"></a>

```fortran
PURE ELEMENTAL REAL(KIND=real_dp) FUNCTION date2num(d)

  TYPE(datetime),INTENT(IN) :: d
```

Returns the number of days since *0001-01-01 00:00:00 UTC*,
given a [*datetime*](#datetime) instance `d`.

This function is similar in what it returns to analogous functions
in Python ([*matplotlib.dates.date2num*](http://matplotlib.org/api/dates_api.html#matplotlib.dates.date2num))
and MATLAB's [*datenum*](http://www.mathworks.com/help/matlab/ref/datenum.html).
Note that [*matplotlib.dates.date2num*](http://matplotlib.org/api/dates_api.html#matplotlib.dates.date2num) returns the number of days since *0001-01-01 00:00:00 UTC* plus 1
(for historical reasons),
and MATLAB's [*datenum*](http://www.mathworks.com/help/matlab/ref/datenum.html)
returns the number of days since *0000-01-01 00:00:00 UTC*.
In *datetime-fortran*, we choose the reference time of *0001-01-01 00:00:00 UTC*
as we consider it to be the least astonishing for the average user.
Thus, MATLAB and Python users should be cautious when using 
*datetime-fortran*'s [*date2num()*](#date2num) function.

[date2num](#date2num) is the inverse function of [num2date](#num2date),
so by definition, `a == num2date(date2num(a))` evaluates as `.TRUE.`
for any `datetime` instance `a`.
Similarly, `b == date2num(num2date(b))` evaluates as `.TRUE.`
for any variable `b` of type `REAL(KIND=real_dp)`.

#### Arguments

`d` A [*datetime*](#datetime) instance.

#### Return value

`date2num` A `REAL(KIND=real_dp)` number of days since *0001-01-01 00:00:00 UTC*.
`real_dp` is defined as:

```fortran
INTEGER,PARAMETER :: real_dp = KIND(1d0)
```

#### Example usage

```fortran
USE datetime_module,ONLY:datetime,date2num

TYPE(datetime)  :: a

! Initialize:
a = datetime(2013,1,1,6)

WRITE(*,*)date2num(a)   ! 734869.25000000000
```

#### See also

* [*datetime*](#datetime)

* [*num2date*](#num2date)

[Back to top](#top)

### daysInMonth<a id="daysinmonth"></a>

```fortran
PURE ELEMENTAL INTEGER FUNCTION daysInMonth(month,year)

  ! ARGUMENTS:
  INTEGER,INTENT(IN) :: month
  INTEGER,INTENT(IN) :: year
```

Returns the number of days in month for a given month and year.
This function is declared as `ELEMENTAL`, so it can be called
with scalar or n-dimensional array arguments.

#### Arguments

`month` Integer number of month in year. Valid values are in the range [1-12].

`year` Integer year.

#### Return value

Returns an integer number of days in requested month and year.
Returns `0` if `month` is not in valid range.

#### Example usage

```fortran
USE datetime_module,ONLY:daysInMonth

! January on leap year:
WRITE(*,*)daysInMonth(1,2012)   ! 31

! February on leap year:
WRITE(*,*)daysInMonth(2,2012)   ! 29

! February on non-leap year
WRITE(*,*)daysInMonth(2,2013)   ! 28
```

#### See also

* [*daysInYear*](#daysinyear)

[Back to top](#top)

### daysInYear<a id="daysinyear"></a>

```fortran
PURE ELEMENTAL INTEGER FUNCTION daysInYear(year)

  ! ARGUMENTS:
  INTEGER,INTENT(IN) :: year
```

Given an integer `year`, returns an integer number of days in that year.
Calls the [*isLeapYear*](#isleapyear) function.

#### Arguments

`year` An `INTEGER` scalar containing the desired year number.

#### Return value

`daysInYear An `INTEGER` scalar. Represents the number of days in `year`. 

#### Example usage

```fortran
USE datetime_module,ONLY:daysInYear

! Leap year:
WRITE(*,*)daysInYear(2012)   ! 366

! Non-leap year:
WRITE(*,*)daysInYear(2013)   ! 365
```

#### See also

* [*daysInMonth*](#daysinmonth)

* [*isLeapYear*](#isleapyear)

[Back to top](#top)

### isLeapYear<a id="isleapyear"></a>

```fortran
```

#### Arguments

#### Return value

#### Example usage

#### See also

[Back to top](#top)

### num2date<a id="num2date"></a>

```fortran
PURE ELEMENTAL TYPE(datetime) FUNCTION num2date(num)

  REAL(KIND=real_dp),INTENT(IN) :: num
```

Given the number of days since *0001-01-01 00:00:00 UTC*, returns a
correspoding [datetime](#datetime) instance.

This function is similar to analogous function
in Python ([*matplotlib.dates.num2date*](http://matplotlib.org/api/dates_api.html#matplotlib.dates.num2date)).

[num2date](#num2date) is the inverse function of [date2num](#date2num),
so by definition, `a == num2date(date2num(a))` evaluates as `.TRUE.`
for any `datetime` instance `a`.
Similarly, `b == date2num(num2date(b))` evaluates as `.TRUE.`
for any variable `b` of type `REAL(KIND=real_dp)`.

#### Arguments

`num` Number of days since *0001-01-01 00:00:00 UTC*.

#### Return value

`num2date` A [*datetime*](#datetime) instance.

#### Example usage



#### See also

* [*date2num*](#date2num)

* [*datetime*](#datetime)

[Back to top](#top)

### strftime<a id="strftime"></a>

```fortran
FUNCTION strftime(str,slen,format,tm)BIND(c,name='strftime')RESULT(rc)

  CHARACTER(KIND=c_char),DIMENSION(*),INTENT(OUT) :: str   
  INTEGER(KIND=c_int),VALUE,          INTENT(IN)  :: slen   
  CHARACTER(KIND=c_char),DIMENSION(*),INTENT(IN)  :: format 
  TYPE(tm_struct),                    INTENT(IN)  :: tm     
  INTEGER(KIND=c_int)                             :: rc
```

An interface to a C/C++ standard library routine. 
Copies into `str` the content of format, expanding its format specifiers 
into the corresponding values that represent the time described in `tm`, 
with a limit of `slen` characters.

#### Arguments

`str` is the destination character string with the requested date and time. 

`slen` is the maximum number of characters to be copied to `str`, 
including the terminating null-character, `CHAR(0)`.

`format` is the character string containing any combination of regular characters and special format specifiers. 
These format specifiers are replaced by the function to the corresponding values to represent the time specified in `tm`. 
For more information on format specifiers see http://www.cplusplus.com/reference/ctime/strftime/.

`tm` is an instance of the type `tm_struct`, containing date and time values to be processed.

#### Return value

If the resulting string fits in less than `slen` characters including the terminating null-character, 
the total number of characters copied to `str` (not including the terminating null-character) is returned.
Otherwise, zero is returned and the contents of the array are indeterminate.

#### Example usage

#### See also

[Back to top](#top)

### strptime<a id="strptime"></a>

```fortran
FUNCTION strptime(str,format,tm)BIND(c,name='strptime')RESULT(rc)

  CHARACTER(KIND=c_char),DIMENSION(*),INTENT(IN)  :: str
  CHARACTER(KIND=c_char),DIMENSION(*),INTENT(IN)  :: format
  TYPE(tm_struct),                    INTENT(OUT) :: tm
  INTEGER(KIND=c_int)                             :: rc
```

An interface to a C/C++ standard library routine.
Converts the character string `str` to values which are stored in `tm`, using the format specified by `format`.

#### Arguments

`str` is the character string containing date and time information.

`format` is the character string containing any combination of regular characters and special format specifiers,
describing the date and time information in `str`.

`tm` is an instance of the type `tm_struct`, in which the date and time values will be filled upon successful completion
of the strptime function.

#### Return value

#### Example usage

#### See also

[Back to top](#top)

#### Tested with the following compilers:
---
* gfortran 4.7.2
* ifort 13.1.1.163

