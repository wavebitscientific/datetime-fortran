datetime-fortran
================

## Description

Time and date manipulation library for Fortran.
It is freely available under the [GNU General Public License](http://www.gnu.org/licenses/gpl.html).

## API

<a id='top'></a>

* <a href='#derived_types'>Derived Types</a>
    * <a href='#datetime'>*datetime*</a>
        * <a href='#addmilliseconds'>*addMilliseconds*</a>
        * <a href='#addseconds'>*addSeconds*</a>
        * <a href='#addminutes'>*addMinutes*</a>
        * <a href='#addhours'>*addHours*</a>
        * <a href='#adddays'>*addDays*</a>
        * <a href='#isocalendar'>*isocalendar*</a>
        * <a href='#isoformat'>*isoformat*</a>
        * <a href='#isvalid'>*isValid*</a>
        * <a href='#now'>*now*</a>
        * <a href='#secondsSinceEpoch'>*secondsSinceEpoch*</a>
        * <a href='#tm'>*tm*</a>
        * <a href='#weekday'>*weekday*</a>
        * <a href='#weekdayLong'>*weekdayLong*</a>
        * <a href='#weekdayShort'>*weekdayShort*</a>
        * <a href='#yearday'>*yearday*</a>
    * <a href='#timedelta'>*timedelta*</a>
        * <a href='#total_seconds'>*total_seconds*</a>
    * <a href='#tm_struct'>*tm_struct*</a>
* <a href='#public'>Public procedures</a>
    * <a href='#date2num'>*date2num*</a>
    * <a href='#daysinmonth'>*daysInMonth*</a>
    * <a href='#daysinyear'>*daysInYear*</a>
    * <a href='#isleapyear'>*isLeapYear*</a>
    * <a href='#num2date'>*num2date*</a>
    * <a href='#strftime'>*strftime*</a>
    * <a href='#strptime'>*strptime*</a>


<a id='derived_types'><h3>Derived Types</h3></a>

*datetime-fortran* library provides the following derived types:

<a id='datetime'><h4>**datetime**</h4></a>

Main time object, modeled after Python's *datetime.datetime* class:

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
[Back to top](#top)

<a id='timedelta'><h4>**timedelta**</h4></a>

Main time difference object, modeled after Python's *datetime.timedelta* class:

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

<a id='tm_struct'><h4>**tm_struct**</h4></a>

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

<a id='public'><h3>Public procedures</h3></a>

[Back to top](#top)

#### Tested with the following compilers:
---
* gfortran 4.7.2
* ifort 13.1.1.163

