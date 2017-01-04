## datetime-fortran

[![Build Status](https://travis-ci.org/wavebitscientific/datetime-fortran.svg?branch=master)](https://travis-ci.org/wavebitscientific/datetime-fortran)
[![GitHub issues](https://img.shields.io/github/issues/wavebitscientific/datetime-fortran.svg)](https://github.com/wavebitscientific/datetime-fortran/issues)

datetime-fortran is a time and date manipulation library for Fortran.
It provides classes for date and time ([*datetime*](#datetime)), 
and time difference representation ([*timedelta*](#timedelta))
as well as arithmetic and comparison operators and associated methods for their manipulation.
It also provides an interface to C/C++ **tm** struct, and associated
[*strftime*](#strftime) and [*strptime*](#strptime) functions.
Since version 0.2.0, also provides a [*clock*](#clock) class.
*datetime-fortran* came about due to the lack of time handling facilities in standard Fortran language.
It is freely available under the [BSD-3](http://opensource.org/licenses/BSD-3-Clause) license.
Please send suggestions and bug reports by [e-mail](mailto:caomaco@gmail.com) or
through this Github page. See the list of [current issues](https://github.com/milancurcic/datetime-fortran/issues)
if you would like to contribute to the code.

## Features

* Classes: [*datetime*](#datetime), [*timedelta*](#timedelta), 
[*clock*](#clock), [*tm_struct*](#tm_struct);

* Arithmetic operators `+` and `-` for *datetime* and *timedelta* objects;

* Comparison operators `>`, `>=`, `<`, `<=`, `==` and `/=` for *datetime* and *timedelta* objects;

* Basic timezone handling and arithmetic;

* Interfaces to C/C++ routines [*c_strftime*](#c_strftime) and [*c_strptime*](#c_strptime) through `ISO_C_BINDING`.

* Lightweight and portable;

* Free to modify and distribute under the terms of the [BSD-3](http://opensource.org/licenses/BSD-3-Clause) license.

* Release version 1.1.0 compiles and passes all tests with the following compilers:

   * GNU (gfortran) ~~4.7.2~~, 4.8.2, 4.8.4, 5.1.1, 5.2.0
   * Intel (ifort) 13.1.1, 14.0.2, 15.0.0, 16.0.0
   * Portland Group (pgf90) 13.6-0, 13.10-0 (thanks to Timothy Hilton)
   * IBM AIX (xlf) 14.1.0.5 (thanks to Bjoern Hendrik Fock)
   * Cray Fortran (ftn) 8.3.11, on Cray XC-30

* Current release (1.4.3) compiles and passes all tests with the following compilers:
  * GNU (gfortran) 5.1.1, 5.2.0, 5.3.1
  * Intel (ifort) 16.0.0, 17.0.0 

## Contributing

You can contribute to the project in the following ways:
  * Use it
  * Compile it on different machines and different Fortran compilers and report successes or failures
  * Find and report bugs
  * Request a new feature or functionality
  * Fork the repo, implement new feature or functionality, submit a pull request
  * Show it to your friends and colleagues

## API

<a id="top"></a>

* [Derived Types](#derived-types)
    * [*datetime*](#datetime)
        * [*addMilliseconds*](#addmilliseconds)
        * [*addSeconds*](#addseconds)
        * [*addMinutes*](#addminutes)
        * [*addHours*](#addhours)
        * [*addDays*](#adddays)
        * [*isocalendar*](#isocalendar)
        * [*isoformat*](#isoformat)
        * [*isValid*](#isvalid)
        * [*now*](#now)
        * [*secondsSinceEpoch*](#secondssinceepoch)
        * [*strftime*](#strftime)
        * [*tm*](#tm)
        * [*tzOffset*](#tzoffset)
        * [*utc*](#utc)
        * [*weekday*](#weekday)
        * [*weekdayLong*](#weekdaylong)
        * [*weekdayShort*](#weekdayshort)
        * [*yearday*](#yearday)
    * [*timedelta*](#timedelta)
        * [*total_seconds*](#total_seconds)
    * [*clock*](#clock)
        * [*reset*](#reset)
        * [*tick*](#tick)
    * [*tm_struct*](#tm_struct)
* [Overloaded operators](#overloaded-operators)
    * [Arithmetic operators](#arithmetic-operators)
    * [Comparison operators](#comparison-operators)
* [Public procedures](#public-procedures)
    * [*c_strftime*](#c_strftime)
    * [*c_strptime*](#c_strptime)
    * [*date2num*](#date2num)
    * [*datetimeRange*](#datetimerange)
    * [*daysInMonth*](#daysinmonth)
    * [*daysInYear*](#daysinyear)
    * [*isLeapYear*](#isleapyear)
    * [*num2date*](#num2date)
    * [*strptime*](#strptime)
    * [*tm2date*](#tm2date)


<a id="derived-types"><h2>Derived Types</h2></a>

*datetime-fortran* library provides the following derived types:
[*datetime*](#datetime), [*timedelta*](#timedelta),  
[*clock*](#clock) and [*tm_struct*](#tm_struct).

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

  REAL :: tz = 0 ! Timezone offset from UTC [hours]

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
  PROCEDURE :: strftime
  PROCEDURE :: tm
  PROCEDURE :: tzOffset
  PROCEDURE :: utc
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

#### Example usage

```fortran
USE datetime_module,ONLY:datetime

TYPE(datetime) :: a

! Initialize as default:
a = datetime()                                      ! 0001-01-01 00:00:00

! Components can be specified by position:
a = datetime(1984,12,10)                            ! 1984-12-10 00:00:00

! Or by keyword:
a = datetime(month=1,day=1,year=1970)               ! 1970-01-01 00:00:00

! Or combined:
a = datetime(2013,2,minute=23,day=5)                ! 2013-02-05 00:23:00

! With timezone offset:
a = datetime(2013,2,minute=23,day=5,tz=-4)          ! 2013-02-05 00:23:00 -0400

! Do not use positional after keyword arguments:
a = datetime(year=2013,2,minute=23,day=5)  ! ILLEGAL
```

Note that the current implementation of [*datetime*](#datetime) 
does not support daylight saving time (DST) information.

#### See also

* [*timedelta*](#timedelta)

* [*tm_struct*](#tm_struct)

[Back to top](#top)

<hr>

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

<hr>

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
<hr>

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
<hr>

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
<hr>

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
<hr>

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
<hr>

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
<hr>

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
<hr>

### now<a id="now"></a>

```fortran
TYPE(datetime) FUNCTION now(self)

  ! ARGUMENTS:
  CLASS(datetime),INTENT(IN) :: self
```

Returns the [*datetime*](#datetime) instance representing 
the current machine time. Does not support timezones.

#### Arguments

None.

#### Return value

`self` A `datetime` instance with current machine time.

#### Example usage

```fortran
USE datetime_module,ONLY:datetime

TYPE(datetime) :: a

a = a%now()   ! Assigns current machine time to a
```

#### See also

[Back to top](#top)
<hr>

### secondsSinceEpoch<a id="secondssinceepoch"></a>

```fortran
INTEGER FUNCTION secondsSinceEpoch(self)

  ! ARGUMENTS
  CLASS(datetime),INTENT(IN) :: self
```

Returns an integer number of seconds since the 
UNIX Epoch, `1970-01-01 00:00:00 +0000` (UTC).

#### Arguments

None.

#### Return value

`secondsSinceEpoch` An `INTEGER` scalar containing number of seconds since 
UNIX Epoch.

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
<hr>



### strftime<a id="strfime"></a>

```fortran
CHARACTER(LEN=MAXSTRLEN) FUNCTION strftime(self,format)

  ! ARGUMENTS:
  CLASS(datetime), INTENT(IN) :: self
  CHARACTER(LEN=*),INTENT(IN) :: format
```

A *datetime*-bound method that serves as a wrapper around the 
C routine *strftime*. 
`datetime%strftime` takes only the format string as argument,
and returns the character string representation of the time 
information contained in the datetime instance. Thus, this function
takes care of the conversion to `tm_struct` and calling the raw C *strftime*.
Because Fortran does not allow assumed-length character strings as 
the type of the function result, a fixed length of `MAXSTRLEN` is used.
`MAXSTRLEN` is currently set to `99`. It is assumed that the desired 
time string is shorter than this value.
Any resulting string shorter than `MAXSTRLEN` is padded with spaces,
so it is best to trim the result using the `TRIM` intrinsic function
(see the usage example below).
This *datetime*-bound method is available since version `0.3.0`.

#### Arguments

`format` A character string describing the desired format of date and time.
Same as the format for the raw C [*strftime*](#c_strftime).

#### Return value

A `CHARACTER(LEN=MAXSTRLEN)` representation of *datetime* using `format`.

#### Example usage

```fortran
USE datetime_module

TYPE(datetime)  :: a

a = a % now()
WRITE(*,*)a%isoformat()

WRITE(*,*)TRIM(a%strftime("%Y %B %d"))
```

#### See also

* [*c_strftime*](#c_strftime)

[Back to top](#top)
<hr>


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

* [*tm_struct*](#tm_struct)

[Back to top](#top)
<hr>

### tzOffset

```fortran
PURE ELEMENTAL CHARACTER(LEN=5) FUNCTION tzOffset(self)

  ! ARGUMENTS:
  CLASS(datetime),INTENT(IN) :: self
```

Given a [*datetime*](#datetime) instance, returns a character string with timezone 
offset in hours from UTC (Coordinated Universal Time), in format `+hhmm`
or `-hhmm`, depending on the sign, where `hh` are hours and `mm` are minutes.

#### Arguments

None.

#### Return value

`tzOffset` A `CHARACTER(LEN=5)` in the form `+hhmm`
or `-hhmm`, depending on the sign.

#### Example usage

```fortran
USE datetime_module,ONLY:datetime

TYPE(datetime)  :: a
TYPE(tm_struct) :: tm

! Initialize a datetime instance with timezone offset of -4.75 hours:
a = datetime(2013,1,1,tz=-4.75)

! Write tzOffset on screen:
WRITE(*,*)a%tzOffset        ! -0445 (offset of 4 hours and 45 minutes)
```

#### See also

[Back to top](#top)
<hr>

### utc

```fortran
PURE ELEMENTAL TYPE(datetime) FUNCTION utc(self)

  ! ARGUMENTS:
  CLASS(datetime),INTENT(IN) :: self
```

Returns the datetime instance at Coordinated Universal Time (UTC). 

#### Arguments

None.

#### Return value

`utc` A `datetime` instance with at UTC (tz = 0).

#### Example usage

```fortran
USE datetime_module,ONLY:datetime

TYPE(datetime)  :: a
TYPE(tm_struct) :: tm

! Initialize a datetime instance with timezone offset of -4.75 hours:
a = datetime(2013,1,1,tz=-4.75)

WRITE(*,*)a%isoformat()//a%tzOffset() ! 2013-01-01T00:00:00.000-0445

! Convert a to UTC:
a = a%utc()

WRITE(*,*)a%isoformat()//a%tzOffset() ! 2013-01-01T04:45:00.000+0000
```

#### See also

* [*tzOffset*](#tzoffset)

[Back to top](#top)
<hr>

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

* [*weekdayLong*](#weekdaylong)

* [*weekdayShort*](#weekdayshort)

[Back to top](#top)
<hr>

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
<hr>

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
<hr>

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
<hr>

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
USE datetime_module

TYPE(datetime)  :: a,b
TYPE(timedelta) :: c

! Initialize as default
c = timedelta()

! Positional arguments:
c = timedelta(0,1,15,0,0)      ! 1 hour and 15 minutes

! Keyword arguments:
c = timedelta(days=1,hours=12) ! 1 day and 12 hours

! Difference between datetimes:
a = datetime(2013,5,12,22,0,0) ! 2013-05-12 22:00:00
b = datetime(2012,9,18,14,0,0) ! 2012-09-18 14:00:00

! Subtract to get timedelta:
c = a-b 
```

[Back to top](#top)
<hr>

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
<hr>

### **clock**<a id="clock"></a>

A generic clock object that contains start and stop times,
tick increment and reset and tick methods. 
Most useful when needing to keep track of many [*datetime*](#datetime) instances
that change at different rates, for example, physical models with different 
time steps.

Definition:

```fortran
TYPE :: clock

  ! COMPONENTS:
  TYPE(datetime) :: startTime   = datetime()
  TYPE(datetime) :: stopTime    = datetime()
  TYPE(datetime) :: currentTime = datetime()

  TYPE(timedelta) :: tickInterval = timedelta()

  LOGICAL :: alarm = .FALSE.

  ! Clock status flags 
  LOGICAL :: started = .FALSE.
  LOGICAL :: stopped = .FALSE.

  CONTAINS

  ! METHODS:
  PROCEDURE :: reset
  PROCEDURE :: tick

ENDTYPE clock
```

[*clock*](#clock) components are initialized by default, and all arguments
are optional. However, a [*clock*](#clock) instance must be initialized
with some sane values of `clock%startTime`, `clock%stopTime` and `clock%tickIncrement`
in order to be useful.

#### Example usage

```fortran
USE datetime_module

TYPE(clock)    :: myClock
TYPE(datetime) :: myTime

! Initialize myTime
myTime = myTime%now()

! Initialize myClock
! Starts from myTime, stops 1 hour later, 1 minute per tick 
myClock = clock(startTime    = myTime,                   &
                stopTime     = myTime+timedelta(hours=1),&
                tickInterval = timedelta(minutes=1))

DO

  CALL myClock % tick()

  ! Report current time after each tick
  WRITE(*,*)myClock % currentTime % isoformat(' ')

  ! If clock has reached stopTime, exit loop
  IF(myClock % stopped)THEN
    EXIT
  ENDIF

ENDDO
```
#### See also

* [*datetime*](#datetime)

* [*timedelta*](#timedelta)


[Back to top](#top)
<hr>

### reset

```fortran
PURE ELEMENTAL SUBROUTINE reset(self)

  ! ARGUMENTS:
  CLASS(clock),INTENT(INOUT) :: self
```

Resets the clock to its start time.

#### Arguments

None

#### Return value

None

#### Example usage

```fortran
CALL myClock%reset() ! Resets myClock%currentTime to myClock%startTime
```

#### See also

[Back to top](#top)
<hr>

### tick

```fortran
PURE ELEMENTAL SUBROUTINE tick(self)

  ! ARGUMENTS:
  CLASS(clock),INTENT(INOUT) :: self
```

Increments the `currentTime` of the clock instance by one `tickInterval`.
Sets the `clock%stopped` flag to `.TRUE.` if `clock%currentTime` equals
or exceeds `clock%stopTime`.

#### Arguments

None

#### Return value

None

#### Example usage

See [*clock*](#clock) for an example.

#### See also

[Back to top](#top)
<hr>


### **tm_struct**<a id="tm_struct"></a>

Time object compatible with C/C++ *tm* struct. Available mainly 
for the purpose of calling [*c_strftime*](#c_strftime) 
and [*c_strptime*](#c_strptime) procedures.

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

* [*datetime*](#datetime)

* [*tm*](#tm)

* [*strftime*](#c_strftime)

* [*strptime*](#c_strptime)

[Back to top](#top)
<hr>

## Overloaded operators<a id="#operators"></a>

The *datetime-fortran* library provides arithmetic and comparison operators
for [*datetime*](#datetime) and [*timedelta*](#timedelta) objects.

### Arithmetic operators

Addition (`+`) and subtraction (`-`) operators are available for 
the following combination of derived type pairs:

* `datetime  + timedelta`, returns a `datetime` instance;

* `timedelta + datetime`, returns a `datetime` instance;

* `timedelta + timedelta`, returns a `timedelta` instance;

* `timedelta - timedelta`, returns a `timedelta` instance;

* `datetime  - datetime`, returns a `timedelta` instance;

* `-timedelta` (unary minus), returns a `timedelta` instance.

Note that `datetime  - datetime` operation accounts for timezone (`tz`) 
offsets in each of the `datetime` instances. 
The resulting `timedelta`thus  includes the difference between timezones.

### Comparison operators

*datetime-fortran* supports following binary comparison operators for 
[*datetime*](#datetime) and [*timedelta*](#timedelta) objects:
`==`, `/=`, `>`, `>=`, `<` and `<=`.

Since version 1.0.5, all comparison operators respect the timezone
parameter of the datetime instances, so the operands are first 
adjusted to UTC time before making the comparison.

[Back to top](#top)
<hr>

## Public procedures<a id="#public-procedures"></a>
    
### c_strftime<a id="c_strftime"></a>

```fortran
FUNCTION c_strftime(str,slen,format,tm)BIND(c,name='strftime')RESULT(rc)

  ! ARGUMENTS:
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

Note: This function was renamed from *strftime* to *c_strftime* in version 0.3.0 
to avoid name conflict with *datetime*-bound method [*strftime*](#strftime).
If working with *datetime* instances, use [*datetime%strftime*](#strftime) instead.

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

```fortran
USE datetime_module

TYPE(datetime)    :: a
CHARACTER(LEN=20) :: res
INTEGER           :: rc

a = a % now()

rc = c_strftime(res,20,"%Y %B %d"//CHAR(0),a%tm())
```

#### See also

* [*datetime%strftime*](#strftime)

* [*c_strptime*](#c_strptime)

* [*strptime*](#strptime)

* [*tm*](#tm)

* [*tm_struct*](#tm_struct)


[Back to top](#top)
<hr>

### c_strptime<a id="c_strptime"></a>

```fortran
FUNCTION c_strptime(str,format,tm)BIND(c,name='strptime')RESULT(rc)

  CHARACTER(KIND=c_char),DIMENSION(*),INTENT(IN)  :: str
  CHARACTER(KIND=c_char),DIMENSION(*),INTENT(IN)  :: format
  TYPE(tm_struct),                    INTENT(OUT) :: tm
  CHARACTER(KIND=c_char,LEN=1)                    :: rc
```

An interface to a C/C++ standard library routine.
Converts the character string `str` to values which are stored in `tm`, using the format specified by `format`.

Note: This function was renamed from *strptime* to *c_strptime* in version 0.3.0 to avoid 
name conflicts with [*strptime*](#strptime) which operates on *datetime* instances.
If working with *datetime* instances, use [*strptime*](#strptime) instead.

#### Arguments

`str` is the character string containing date and time information.

`format` is the character string containing any combination of regular characters and special format specifiers,
describing the date and time information in `str`.

`tm` is an instance of the type `tm_struct`, in which the date and time values will be filled upon successful completion
of the [*c_strptime*](#c_strptime) function.

#### Return value

Upon successful completion, [*c_strptime*](#c_strptime) returns the character 
following the last character parsed. Otherwise, a null character is returned.

#### Example usage

Extracting time difference between two time strings using [*c_strptime*](#c_strptime)
and [*tm2date*](#tm2date):

```fortran
USE datetime_module

TYPE(datetime)  :: date1,date2
TYPE(tm_struct) :: ctime
TYPE(timedelta) :: timediff

! Return code for strptime
CHARACTER(LEN=1) :: rc

! Example times in "YYYYMMDD hhmmss" format
CHARACTER(LEN=15) :: str1 = "20130512 091519"
CHARACTER(LEN=15) :: str2 = "20131116 120418"

! Get tm_struct instance from str1
rc = c_strptime(str1,"%Y%m%d %H%M%S"//CHAR(0),ctime)
date1 = tm2date(ctime)

! Get tm_struct instance from str2
rc = c_strptime(str2,"%Y%m%d %H%M%S"//CHAR(0),ctime)
date2 = tm2date(ctime)

timediff = date2-date1

WRITE(*,*)timediff
WRITE(*,*)timediff%total_seconds()
```

This example outputs the following:

```
        188           2          48          58        1000
   16253339.0000000
```

#### See also

* [*strptime*](#strptime)

* [*tm2date*](#tm2date)

[Back to top](#top)
<hr>


### date2num<a id="date2num"></a>

```fortran
PURE ELEMENTAL REAL(KIND=real_dp) FUNCTION date2num(d)

  ! ARGUMENTS:
  TYPE(datetime),INTENT(IN) :: d
```

Returns the number of days since `0001-01-01 00:00:00 UTC`,
given a [*datetime*](#datetime) instance `d`.

This function is similar in what it returns to analogous functions
in Python ([*matplotlib.dates.date2num*](http://matplotlib.org/api/dates_api.html#matplotlib.dates.date2num))
and MATLAB's [*datenum*](http://www.mathworks.com/help/matlab/ref/datenum.html).
Note that [*matplotlib.dates.date2num*](http://matplotlib.org/api/dates_api.html#matplotlib.dates.date2num) returns the number of days since `0001-01-01 00:00:00 UTC` plus `1`
(for historical reasons),
and MATLAB's [*datenum*](http://www.mathworks.com/help/matlab/ref/datenum.html)
returns the number of days since `0000-01-01 00:00:00 UTC`.
In *datetime-fortran*, we choose the reference time of `0001-01-01 00:00:00 UTC`
as we consider it to be the least astonishing for the average user.
Thus, MATLAB and Python users should be cautious when using 
*datetime-fortran*'s [*date2num()*](#date2num) function.

Since version 1.0.5, [date2num](#date2num) is timezone aware, i.e.
the datetime instance is first converted to UTC before calculating
the number of days.

[date2num](#date2num) is the inverse function of [num2date](#num2date),
so by definition, `a % utc() == num2date(date2num(a))` evaluates as `.TRUE.`
for any `datetime` instance `a`.

#### Arguments

`d` A [*datetime*](#datetime) instance.

#### Return value

`date2num` A `REAL(KIND=real_dp)` number of days since `0001-01-01 00:00:00 UTC`.
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
<hr>

### datetimeRange<a id="datetimerange"></a>

```fortran
PURE FUNCTION datetimeRange(d0,d1,t)

  ! ARGUMENTS:
  TYPE(datetime), INTENT(IN) :: d0
  TYPE(datetime), INTENT(IN) :: d1
  TYPE(timedelta),INTENT(IN) :: t
```

Given start and end [*datetime*](#datetime) instances `d0` and `d1`, 
and time increment as [*timedelta*](#timedelta) instance `t`, 
returns an array of datetime instances.
The number of elements is the number of whole time increments 
contained between datetimes d0 and d1.

#### Arguments

`d0` A [*datetime*](#datetime) instance with start time. Will be the first element
of the resulting array.

`d1` A [*datetime*](#datetime) instance with end time. Will be the equal to or greater than 
the last element of the resulting array.

`t` A [*timedelta*](#timedelta) instance being the time increment for the resulting array.

#### Return value

`datetimeRange` An array of [*datetime*](#datetime) instances of length
 `FLOOR((d1-d0)/t)+1`

#### Example usage

```fortran
TYPE(datetime)  :: a,b
TYPE(timedelta) :: td

TYPE(datetime),DIMENSION(:),ALLOCATABLE :: dtRange

a  = datetime(2014,5,1)
b  = datetime(2014,5,3)
td = timedelta(days=1)

dtRange = datetimeRange(a,b,td)

! Returns: 
!     
! dtRange = [datetime(2014,5,1),
!            datetime(2014,5,2),
!            datetime(2014,5,3)]

a  = datetime(2014,5,1)
b  = datetime(2014,5,3)
td = timedelta(hours=7)

dtRange = datetimeRange(a,b,td)

! Returns: 
!     
! dtRange = [datetime(2014,5,1,0),
!            datetime(2014,5,1,7),
!            datetime(2014,5,1,14),
!            datetime(2014,5,1,21),
!            datetime(2014,5,2, 4),
!            datetime(2014,5,2,11),
!            datetime(2014,5,2,18)]
```

#### See also

* [*datetime*](#datetime)

* [*timedelta*](#timedelta)

[Back to top](#top)
<hr>

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
<hr>

### daysInYear<a id="daysinyear"></a>

```fortran
PURE ELEMENTAL INTEGER FUNCTION daysInYear(year)

  ! ARGUMENTS:
  INTEGER,INTENT(IN) :: year
```

Given an integer `year`, returns an integer number of days in that year.
Calls the [*isLeapYear*](#isleapyear) function.

#### Arguments

`year` An `INTEGER` scalar or array containing the desired year number(s).

#### Return value

`daysInYear` An `INTEGER` scalar or array. Represents the number of days in `year`. 

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
<hr>

### isLeapYear<a id="isleapyear"></a>

```fortran
PURE ELEMENTAL LOGICAL FUNCTION isLeapYear(year)

  ! ARGUMENTS:
  INTEGER,INTENT(IN) :: year
```

Returns a `LOGICAL` value indicating whether the reqested year is a leap year.

#### Arguments

`year` An `INTEGER` scalar or array representing year number.

#### Return value

`isLeapYear` A `LOGICAL` scalar or array indicating whether a given year is leap year.

#### Example usage

```fortran
USE datetime_module,ONLY:isLeapYear

! Leap year:
WRITE(*,*)isLeapYear(2012)   ! .TRUE.

! Non-leap year:
WRITE(*,*)isLeapYear(2013)   ! .FALSE.
```

#### See also

* [*daysInYear*](#daysInYear)

[Back to top](#top)
<hr>

### num2date<a id="num2date"></a>

```fortran
PURE ELEMENTAL TYPE(datetime) FUNCTION num2date(num)

  ! ARGUMENTS:
  REAL(KIND=real_dp),INTENT(IN) :: num
```

Given the number of days since `0001-01-01 00:00:00 UTC`, returns a
correspoding [datetime](#datetime) instance.

This function is similar to analogous function
in Python ([*matplotlib.dates.num2date*](http://matplotlib.org/api/dates_api.html#matplotlib.dates.num2date)).

[num2date](#num2date) is the inverse function of [date2num](#date2num),
so by definition, `a == num2date(date2num(a))` evaluates as `.TRUE.`
for any `datetime` instance `a`.
Similarly, `b == date2num(num2date(b))` evaluates as `.TRUE.`
for any variable `b` of type `REAL(KIND=real_dp)`.

#### Arguments

`num` Number of days since `0001-01-01 00:00:00 UTC`.

#### Return value

`num2date` A [*datetime*](#datetime) instance.

#### Example usage

```fortran
USE datetime_module,ONLY:datetime,num2date

TYPE(datetime)  :: a

a = num2date(734869.25d0) ! a becomes datetime(2013,1,1,6,0,0,0)
```

#### See also

* [*date2num*](#date2num)

* [*datetime*](#datetime)

[Back to top](#top)
<hr>


### strptime<a id="strptime"></a>

```fortran
TYPE(datetime) FUNCTION strptime(str,format)

  ! ARGUMENTS:
  CHARACTER(LEN=*),INTENT(IN) :: str
  CHARACTER(LEN=*),INTENT(IN) :: format
```

A wrapper function around [*c_strptime*](#c_strptime).
Given a character string `str` with the format `format`, returns 
an appropriate *datetime* instance containing that time information.
This function is analogous to Python's *datetime.datetime.strptime()* function.
Available since version 0.3.0. 

#### Arguments

`str` is the character string containing date and time information.

`format` is the character string containing any combination of regular characters and special format specifiers,
describing the date and time information in `str`.

#### Return value

Upon successful completion, [*strptime*](#strptime) returns the [*datetime*](#datetime)
instance corresponding to the time information contained in *str*.

#### Example usage

Extracting time difference between two time strings using [*strptime*](#strptime):

```fortran
USE datetime_module

TYPE(datetime)  :: date1,date2
TYPE(timedelta) :: timediff

! Example times in "YYYYMMDD hhmmss" format
CHARACTER(LEN=15) :: str1 = "20130512 091519"
CHARACTER(LEN=15) :: str2 = "20131116 120418"

date1 = strptime(str1,"%Y%m%d %H%M%S")
date2 = strptime(str2,"%Y%m%d %H%M%S")

timediff = date2-date1

WRITE(*,*)timediff
WRITE(*,*)timediff%total_seconds()
```

This example outputs the following:

```
        188           2          48          58        1000
   16253339.0000000
```
This is the same example as in [*c_strptime*](#c_strptime) but with fewer
necessary steps.

#### See also

* [*c_strptime*](#c_strptime)

* [*tm2date*](#tm2date)

[Back to top](#top)

### tm2date

```fortran
PURE ELEMENTAL TYPE(datetime) FUNCTION tm2date(ctime)

  ! ARGUMENTS:
  TYPE(tm_struct),INTENT(IN) :: ctime
```

Given a [*tm_struct*](#tm_struct) instance, 
returns a corresponding [*datetime*](#datetime) instance.
Mostly useful for obtaining a *datetime* instance after a *tm_struct*
is returned from [*strptime*](#strptime).

#### Arguments

`ctime` A `tm_struct` instance.

#### Return value

`tm2date` A `datetime` instance.

#### Example usage

See example usage for [*strptime*](#strptime).

#### See also

* [*datetime*](#datetime)

* [*tm_struct*](#tm_struct)

* [*strptime*](#strptime)

[Back to top](#top)
<hr>
