
## Description

*datetime-fortran* is a time and date manipulation library for Fortran.
It is freely available under the [GNU General Public License](http://www.gnu.org/licenses/gpl.html).

## API


<a id="top"></a>

* [Derived Types](#derived-types)
    * <a href="#datetime">*datetime*</a>
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
        * <a href="#total_seconds">*total_seconds*</a>
    * <a href="#tm_struct">*tm_struct*</a>
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

*datetime* components are initialized by default, so all arguments are optional.
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

<h3>*datetime*-bound procedures</h3>

<a id="addmilliseconds"><h4>addMilliseconds</h4></a>
<a id="addseconds"><h4>addSeconds</h4></a>
<a id="addminutes"><h4>addMinutes</h4></a>
<a id="addhours"><h4>addHours</h4></a>
<a id="adddays"><h4>addDays</h4></a>
<a id="isocalendar"><h4>isocalendar</h4></a>
<a id="isoformat"><h4>isoformat</h4></a>
<h4><a id="isvalid">isValid</a></h4>
<a id="now"><h4>now</h4></a>
<a id="secondssinceepoch"><h4>secondsSinceEpoch</h4></a>
<a id="tm"><h4>tm</h4></a>
<a id="weekday"><h4>weekday</h4></a>
<a id="weekdaylong"><h4>weekdayLong</h4></a>
<a id="weekdayshort"><h4>weekdayShort</h4></a>
<a id="yearday"><h4>yearday</h4></a>

[Back to top](#top)

<a id="timedelta"><h3>**timedelta**</h3></a>

Main time difference object

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
[Back to top](#top)

<a id="tm_struct"><h3>**tm_struct**</h3></a>

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

[Back to top](#top)

<a id="#public-procedures"><h2>Public procedures</h2></a>
    
<a id="date2num"><h3>date2num</h3></a>
<a id="daysinmonth"><h3>daysInMonth</h3></a>
<a id="daysinyear"><h3>daysInYear</h3></a>
<a id="isleapyear"><h3>isLeapYear</h3></a>
<a id="num2date"><h3>num2date</h3></a>
<a id="strftime"><h3>strftime</h3></a>
<a id="strptime"><h3>strptime</h3></a>

[Back to top](#top)

#### Tested with the following compilers:
---
* gfortran 4.7.2
* ifort 13.1.1.163

