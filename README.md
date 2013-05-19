
## Description

*datetime-fortran* is a time and date manipulation library for Fortran.
It provides derived types for date ([*datetime*](#datetime)), 
time and time difference representation ([*timedelta*](#timedelta))
as well as arithmetic and associated methods for their manipulation.
It also provides an interface to C/C++ **tm** struct, and associated
*strftime* and *strptime* functions.
*datetime-fortran* came about due to the lack of time handling facilities in standard Fortran language.
*datetime-fortran* is written and managed by Milan Curcic of University of Miami.
It is freely available under the [GNU General Public License](http://www.gnu.org/licenses/gpl.html).
Please send suggestions and bug reports to [here](mailto:milan@orca.rsmas.miami.edu).

## API

<a id="top"></a>

* [Derived Types](#derived-types)
    * [*datetime*](#datetime)
        * [Methods](#datetimemethods)
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
        * [Operators](#datetimeoperators)
    * <a href="#timedelta">*timedelta*</a>
        * [Methods](#datetimemethods)
            * [*total_seconds*](#total_seconds)
        * [Operators](#timedeltaoperators)
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

[Back to top](#top)


### Methods<a id="datetimemethods"></a>

#### addMilliseconds<a id="addmilliseconds"></a>

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

Example usage:

```fortran
USE datetime_module,ONLY:datetime

TYPE(datetime) :: a

! Initialize:
a = datetime(2013,1,1,0,0,0,0)           ! 2013-01-01 00:00:00.000

! Add:
CALL a%addMilliseconds(100)   ! a becomes: 2013-01-01 00:00:00.100
```

[Back to top](#top)


#### addSeconds<a id="addseconds"></a>

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

Example usage:

```fortran
USE datetime_module,ONLY:datetime

TYPE(datetime) :: a

! Initialize:
a = datetime(2013,1,1,0,0,0,0)           ! 2013-01-01 00:00:00.000

! Add:
CALL a%addSeconds(10)         ! a becomes: 2013-01-01 00:00:10.000
```

[Back to top](#top)

#### addMinutes<a id="addminutes"></a>

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

Example usage:

```fortran
USE datetime_module,ONLY:datetime

TYPE(datetime) :: a

! Initialize:
a = datetime(2013,1,1,0,0,0,0)           ! 2013-01-01 00:00:00.000

! Add:
CALL a%addMinutes(10)         ! a becomes: 2013-01-01 00:10:00.000
```

[Back to top](#top)

#### addHours<a id="addhours"></a>

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

Example usage:

```fortran
USE datetime_module,ONLY:datetime

TYPE(datetime) :: a

! Initialize:
a = datetime(2013,1,1,0,0,0,0)           ! 2013-01-01 00:00:00.000

! Add:
CALL a%addHours(12)           ! a becomes: 2013-01-01 12:00:00.000
```

[Back to top](#top)

#### addDays<a id="adddays"></a>

```fortran
PURE ELEMENTAL SUBROUTINE addDays(self,d)

  CLASS(datetime),INTENT(INOUT) :: self
  INTEGER,        INTENT(IN)    :: d    ! Number of days to add
```

Used internally by binary arithmetic operators + and - when
adding/subtracting a [timedelta](#timedelta) instance to/from a
[datetime](#datetime) instance. In general, there is no need to use
this method from external programs. However, it may be convenient
and create less overhead if the operation needs to be performed
on a large array of [datetime](#datetime) instances.

Example usage:

```fortran
USE datetime_module,ONLY:datetime

TYPE(datetime) :: a

! Initialize:
a = datetime(2013,1,1,0,0,0,0)           ! 2013-01-01 00:00:00.000

! Add:
CALL a%addDays(7)             ! a becomes: 2013-01-08 00:00:00.000
```


[Back to top](#top)

#### isocalendar<a id="isocalendar"></a>

```fortran
```

[Back to top](#top)

#### isoformat<a id="isoformat"></a>

```fortran
```

[Back to top](#top)

#### isValid<a id="isvalid"></a>

```fortran
```

[Back to top](#top)

#### now<a id="now"></a>

```fortran
```

[Back to top](#top)

#### secondsSinceEpoch<a id="secondssinceepoch"></a>

```fortran
```

[Back to top](#top)

#### tm<a id="tm"></a>

```fortran
```

[Back to top](#top)

#### weekday<a id="weekday"></a>

```fortran
```

[Back to top](#top)

#### weekdayLong<a id="weekdaylong"></a>

```fortran
```

[Back to top](#top)

#### weekdayShort<a id="weekdayshort"></a>

```fortran
```

[Back to top](#top)

#### yearday<a id="yearday"></a>

```fortran
```

[Back to top](#top)


### Methods <a id="datetimemethods"></a>

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

Example usage:

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

### Methods<a id="timedeltamethods"></a>

### Operators<a id="timedeltaoperators"></a>

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

[Back to top](#top)

## Public procedures<a id="#public-procedures"></a>
    
### date2num<a id="date2num"></a>
### daysInMonth<a id="daysinmonth"></a>
### daysInYear<a id="daysinyear"></a>
### isLeapYear<a id="isleapyear"></a>
### num2date<a id="num2date"></a>
### strftime<a id="strftime"></a>
### strptime<a id="strptime"></a>

[Back to top](#top)

#### Tested with the following compilers:
---
* gfortran 4.7.2
* ifort 13.1.1.163

