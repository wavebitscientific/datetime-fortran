!
! datetime-fortran - A Fortran library for time and date manipulation
! Copyright (c) 2013, 2014: Milan Curcic
!
! This program is free software: you can redistribute it and/or modify
! it under the terms of the GNU General Public License as published by
! the Free Software Foundation, either version 3 of the License, or
! (at your option) any later version.
!
! This program is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.
!
! You should have received a copy of the GNU General Public License
! along with this program.  If not, see <http://www.gnu.org/licenses/>.
!
MODULE datetime_module
!======================================================================>
!
! VERSION: 1.0.6
!
! LAST UPDATE: 2014-12-13
!
! AUTHOR: Milan Curcic
!         University of Miami
!         e-mail: milan@orca.rsmas.miami.edu
!
! DESCRIPTION: A Fortran module that provides time and date manipulation 
!              facilities. It tries to emulate Python's datetime module 
!              API, but differs from it where convenient or necessary. 
!              Conforms to standard Fortran 2003 and later, so a fairly 
!              recent Fortran compiler is necessary.
!
! CONTAINS:
!
!     TYPES: 
!
!         datetime  - Main datetime object
!         timedelta - Time difference object
!         clock     - A generic clock container
!         tm_struct - For compatibility with C/C++ procedures 
!
!     DATETIME METHODS:
!
!         PROCEDURE :: addMilliseconds
!         PROCEDURE :: addSeconds
!         PROCEDURE :: addMinutes
!         PROCEDURE :: addHours
!         PROCEDURE :: addDays
!         PROCEDURE :: isocalendar
!         PROCEDURE :: isoformat
!         PROCEDURE :: isValid
!         PROCEDURE :: now
!         PROCEDURE :: secondsSinceEpoch
!         PROCEDURE :: strftime
!         PROCEDURE :: tm
!         PROCEDURE :: tzOffset
!         PROCEDURE :: utc
!         PROCEDURE :: weekday
!         PROCEDURE :: weekdayShort
!         PROCEDURE :: weekdayLong
!         PROCEDURE :: yearday
!
!     TIMEDELTA METHODS:
!
!         PROCEDURE :: total_seconds
!
!     CLOCK METHODS:
!
!         PROCEDURE :: reset
!         PROCEDURE :: tick
!
!     PUBLIC PROCEDURES:
!
!         FUNCTION c_strftime
!         FUNCTION c_strptime
!         FUNCTION date2num
!         FUNCTION datetimeRange
!         FUNCTION daysInMonth
!         FUNCTION daysInYear
!         FUNCTION isLeapYear
!         FUNCTION num2date
!         FUNCTION strptime
!         FUNCTION tm2date
!
!======================================================================>

USE,INTRINSIC :: iso_c_binding,ONLY:c_char,c_int,c_null_char

IMPLICIT NONE

PRIVATE

! Derived types:
PUBLIC :: clock
PUBLIC :: datetime
PUBLIC :: timedelta
PUBLIC :: tm_struct ! May become PRIVATE in the future

! Operators:
PUBLIC :: OPERATOR(+)
PUBLIC :: OPERATOR(-)
PUBLIC :: OPERATOR(>)
PUBLIC :: OPERATOR(<)
PUBLIC :: OPERATOR(>=)
PUBLIC :: OPERATOR(<=)
PUBLIC :: OPERATOR(==)
PUBLIC :: OPERATOR(/=)

! Procedures:
PUBLIC :: c_strftime ! May become PRIVATE in the future
PUBLIC :: c_strptime ! May become PRIVATE in the future
PUBLIC :: date2num
PUBLIC :: datetimeRange
PUBLIC :: daysInMonth
PUBLIC :: daysInYear
PUBLIC :: isLeapYear
PUBLIC :: num2date
PUBLIC :: strptime
PUBLIC :: tm2date

! 8-byte floating point kind:
INTEGER,PARAMETER :: real_dp = KIND(1d0)

! Constant multipliers that transform a number 
! of some time unit to another:
REAL(KIND=real_dp),PARAMETER :: one = 1d0      ! 1
REAL(KIND=real_dp),PARAMETER :: d2h = 24d0     ! day    -> hour
REAL(KIND=real_dp),PARAMETER :: h2d = one/d2h  ! hour   -> day
REAL(KIND=real_dp),PARAMETER :: d2m = d2h*60d0 ! day    -> minute
REAL(KIND=real_dp),PARAMETER :: m2d = one/d2m  ! minute -> day
REAL(KIND=real_dp),PARAMETER :: m2h = one/60d0 ! minute -> hour
REAL(KIND=real_dp),PARAMETER :: s2d = m2d/60d0 ! second -> day
REAL(KIND=real_dp),PARAMETER :: d2s = 86400d0  ! day    -> second
REAL(KIND=real_dp),PARAMETER :: h2s = 3600d0   ! hour   -> second
REAL(KIND=real_dp),PARAMETER :: s2h = one/h2s  ! second -> hour
REAL(KIND=real_dp),PARAMETER :: m2s = 60d0     ! minute -> second
REAL(KIND=real_dp),PARAMETER :: s2m = one/m2s  ! second -> minute
 
! Maximum string length for strftime.
! Constant for now; may become a preprocessor macro later.
INTEGER,PARAMETER :: MAXSTRLEN = 99

! Derived types:

TYPE :: datetime
!======================================================================>
!
! A main datetime class for date and time representation. It is modeled 
! after Python's datetime.datetime class, and has similar components 
! and methods (but not all).
!
!======================================================================>

  ! COMPONENTS:
  INTEGER :: year        = 1 ! Year                   [1-HUGE(year)]
  INTEGER :: month       = 1 ! Month in year          [1-12]
  INTEGER :: day         = 1 ! Day in month           [1-31]
  INTEGER :: hour        = 0 ! Hour in day            [0-23]
  INTEGER :: minute      = 0 ! Minute in hour         [0-59]
  INTEGER :: second      = 0 ! Second in minute       [0-59]
  INTEGER :: millisecond = 0 ! Milliseconds in second [0-999]

  REAL(KIND=real_dp) :: tz = 0 ! Timezone offset from UTC [hours]

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
!======================================================================>



TYPE :: timedelta
!======================================================================>
!
! Class of objects that define difference between two datetime 
! instances. Modeled after Python's datetime.timedelta class.
!
!======================================================================>

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
!======================================================================>



TYPE :: clock
!======================================================================>
!
! A clock object with a start, stop and current times, tick interval 
! and tick methods. 
!
!======================================================================>

  ! COMPONENTS:
  TYPE(datetime) :: startTime   = datetime(1)
  TYPE(datetime) :: stopTime    = datetime(1)
  TYPE(datetime) :: currentTime = datetime(1)

  TYPE(timedelta) :: tickInterval = timedelta(0)

  ! May become Alarm class in some future release; 
  ! for now, just a switch
  LOGICAL :: alarm = .FALSE.

  ! Clock status flags 
  LOGICAL :: started = .FALSE.
  LOGICAL :: stopped = .FALSE.

  CONTAINS

  ! METHODS:
  PROCEDURE :: reset
  PROCEDURE :: tick 

ENDTYPE clock
!======================================================================>



TYPE,BIND(c) :: tm_struct
!======================================================================>
!
! A derived type provided for compatibility with C/C++ time struct. 
! Allows for calling strftime and strptime procedures through the 
! iso_c_binding.
!
!======================================================================>

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
!======================================================================>



!======================================================================>
! Operator procedure interfaces:

INTERFACE OPERATOR(+)
  MODULE PROCEDURE datetime_plus_timedelta
  MODULE PROCEDURE timedelta_plus_datetime
  MODULE PROCEDURE timedelta_plus_timedelta
ENDINTERFACE

INTERFACE OPERATOR(-)
  MODULE PROCEDURE datetime_minus_datetime
  MODULE PROCEDURE datetime_minus_timedelta
  MODULE PROCEDURE timedelta_minus_timedelta
  MODULE PROCEDURE unary_minus_timedelta
ENDINTERFACE

INTERFACE OPERATOR(==)
  MODULE PROCEDURE eq
  MODULE PROCEDURE eq_td
ENDINTERFACE

INTERFACE OPERATOR(/=)
  MODULE PROCEDURE neq
  MODULE PROCEDURE neq_td
ENDINTERFACE

INTERFACE OPERATOR(>)
  MODULE PROCEDURE gt
  MODULE PROCEDURE gt_td
ENDINTERFACE

INTERFACE OPERATOR(<)
  MODULE PROCEDURE lt
  MODULE PROCEDURE lt_td
ENDINTERFACE

INTERFACE OPERATOR(>=)
  MODULE PROCEDURE ge
  MODULE PROCEDURE ge_td
ENDINTERFACE

INTERFACE OPERATOR(<=)
  MODULE PROCEDURE le
  MODULE PROCEDURE le_td
ENDINTERFACE

!======================================================================>



!======================================================================>
!
! INTERFACE: To C procedures strftime and strptime through 
! iso_c_binding.
!
!======================================================================>
INTERFACE



  FUNCTION c_strftime(str,slen,format,tm)BIND(c,name='strftime')RESULT(rc)
  !====================================================================>
  !
  ! Returns a formatted time string, given input time struct and format. 
  ! Refer to C standard library documentation for more information. 
  ! 
  !====================================================================>

    IMPORT :: c_char,c_int
    IMPORT :: tm_struct

    IMPLICIT NONE

    ! ARGUMENTS:
    CHARACTER(KIND=c_char),DIMENSION(*),INTENT(OUT) :: str
    INTEGER(KIND=c_int),VALUE,          INTENT(IN)  :: slen
    CHARACTER(KIND=c_char),DIMENSION(*),INTENT(IN)  :: format
    TYPE(tm_struct),                    INTENT(IN)  :: tm
    INTEGER(KIND=c_int)                             :: rc

  ENDFUNCTION c_strftime
  !====================================================================>



  FUNCTION c_strptime(str,format,tm)BIND(c,name='strptime')RESULT(rc)
  !====================================================================>
  !
  ! Returns a time struct object based on the input time string str, 
  ! formatted using format. Refer to C standard library documentation 
  ! for more information.
  ! 
  !====================================================================>

    IMPORT :: c_char,c_int
    IMPORT :: tm_struct

    IMPLICIT NONE

    ! ARGUMENTS:
    CHARACTER(KIND=c_char),DIMENSION(*),INTENT(IN)  :: str
    CHARACTER(KIND=c_char),DIMENSION(*),INTENT(IN)  :: format
    TYPE(tm_struct),                    INTENT(OUT) :: tm
    INTEGER(KIND=c_int)                             :: rc

  ENDFUNCTION c_strptime
  !====================================================================>


ENDINTERFACE
!======================================================================>
CONTAINS



!::: Datetime-bound methods ::::::::::::::::::::::::::::::::::::::::::::

PURE ELEMENTAL SUBROUTINE addMilliseconds(self,ms)
!======================================================================>
!
! datetime-bound procedure. Adds an integer number of milliseconds to 
! self. Called by datetime addition (+) and subtraction (-) operators.
!
!======================================================================>

  ! ARGUMENTS:
  CLASS(datetime),INTENT(INOUT) :: self
  INTEGER,        INTENT(IN)    :: ms

  self % millisecond = self % millisecond+ms

  DO
    IF(self % millisecond >= 1000)THEN
      CALL self % addSeconds(self % millisecond/1000)
      self % millisecond = MOD(self % millisecond,1000)
    ELSEIF(self % millisecond < 0)THEN
      CALL self % addSeconds(self % millisecond/1000-1)
      self % millisecond = MOD(self % millisecond,1000)+1000
    ELSE
      EXIT
    ENDIF
  ENDDO

ENDSUBROUTINE addMilliseconds
!======================================================================>



PURE ELEMENTAL SUBROUTINE addSeconds(self,s)
!======================================================================>
!
! datetime-bound procedure. Adds an integer number of seconds to self. 
! Called by datetime addition (+) and subtraction (-) operators.
!
!======================================================================>

  ! ARGUMENTS:
  CLASS(datetime),INTENT(INOUT) :: self
  INTEGER,        INTENT(IN)    :: s

  self % second = self % second+s

  DO
    IF(self % second >= 60)THEN
      CALL self % addMinutes(self % second/60)
      self % second = MOD(self % second,60)
    ELSEIF(self % second < 0)THEN
      CALL self % addMinutes(self % second/60-1)
      self % second = MOD(self % second,60)+60
    ELSE
      EXIT
    ENDIF
  ENDDO

ENDSUBROUTINE addSeconds
!======================================================================>



PURE ELEMENTAL SUBROUTINE addMinutes(self,m)
!======================================================================>
!
! datetime-bound procedure. Adds an integer number of minutes to self. 
! Called by datetime addition (+) and subtraction (-) operators.
!
!======================================================================>

  ! ARGUMENTS:
  CLASS(datetime),INTENT(INOUT) :: self
  INTEGER,        INTENT(IN)    :: m

  self % minute = self % minute+m

  DO
    IF(self % minute >= 60)THEN
      CALL self % addHours(self % minute/60)
      self % minute = MOD(self % minute,60)
    ELSEIF(self % minute < 0)THEN
      CALL self % addHours(self % minute/60-1)
      self % minute = MOD(self % minute,60)+60
    ELSE
      EXIT
    ENDIF
  ENDDO

ENDSUBROUTINE addMinutes
!======================================================================>



PURE ELEMENTAL SUBROUTINE addHours(self,h)
!======================================================================>
!
! datetime-bound procedure. Adds an integer number of hours to self. 
! Called by datetime addition (+) and subtraction (-) operators.
!
!======================================================================>
  
  ! ARGUMENTS:
  CLASS(datetime),INTENT(INOUT) :: self
  INTEGER,        INTENT(IN)    :: h

  self % hour = self % hour+h

  DO
    IF(self % hour >= 24)THEN
      CALL self % addDays(self % hour/24)
      self % hour = MOD(self % hour,24)
    ELSEIF(self % hour < 0)THEN
      CALL self % addDays(self % hour/24-1)
      self % hour = MOD(self % hour,24)+24
    ELSE
      EXIT
    ENDIF
  ENDDO

ENDSUBROUTINE addHours
!======================================================================>



PURE ELEMENTAL SUBROUTINE addDays(self,d)
!======================================================================>
!
! datetime-bound procedure. Adds an integer number of days to self. 
! Called by datetime addition (+) and subtraction (-) operators.
!
!======================================================================>

  ! ARGUMENTS:
  CLASS(datetime),INTENT(INOUT) :: self
  INTEGER,        INTENT(IN)    :: d

  INTEGER :: daysInCurrentMonth

  self % day = self % day+d
  DO
    daysInCurrentMonth = daysInMonth(self % month,self % year)
    IF(self % day > daysInCurrentMonth)THEN
      self % day = self % day-daysInCurrentMonth
      self % month = self % month+1
      IF(self % month > 12)THEN
        self % year = self % year+self % month/12
        self % month = MOD(self % month,12)
      ENDIF
    ELSEIF(self % day < 1)THEN
      self % month = self % month-1
      IF(self % month < 1)THEN
        self % year = self % year+self % month/12-1
        self % month = 12+MOD(self % month,12)
      ENDIF
      self % day = self % day+daysInMonth(self % month,self % year)
    ELSE
      EXIT
    ENDIF 
  ENDDO

ENDSUBROUTINE addDays
!======================================================================>



PURE ELEMENTAL CHARACTER(LEN=23) FUNCTION isoformat(self,sep)
!======================================================================>
!
! Returns character string with time in ISO 8601 format.
!
!======================================================================>

  ! ARGUMENTS:
  CLASS(datetime), INTENT(IN)          :: self
  CHARACTER(LEN=1),INTENT(IN),OPTIONAL :: sep

  CHARACTER(LEN=1)                     :: separator

  IF(PRESENT(sep))THEN
    separator = sep
  ELSE
    separator = 'T'
  ENDIF

  isoformat = int2str(self % year,       4)//'-'//      &
              int2str(self % month,      2)//'-'//      &
              int2str(self % day,        2)//separator//&
              int2str(self % hour,       2)//':'//      &
              int2str(self % minute,     2)//':'//      &
              int2str(self % second,     2)//'.'//      &
              int2str(self % millisecond,3)

ENDFUNCTION isoformat
!======================================================================>



PURE ELEMENTAL LOGICAL FUNCTION isValid(self)
!======================================================================>
!
! datetime-bound method that checks whether the datetime
! instance has valid component values. Returns .TRUE. if the datetime
! instance is valid, and .FALSE. otherwise.
!
!======================================================================>

  ! ARGUMENTS:
  CLASS(datetime),INTENT(IN) :: self

  isValid = .TRUE.

  IF(self % year < 1)THEN
    isValid = .FALSE.
    RETURN
  ENDIF

  IF(self % month < 1 .OR. self % month > 12)THEN
    isValid = .FALSE.
    RETURN
  ENDIF

  IF(self % day < 1 .OR. &
     self % day > daysInMonth(self % month,self % year))THEN
    isValid = .FALSE.
    RETURN
  ENDIF
 
  IF(self % hour < 0 .OR. self % hour > 23)THEN    
    isValid = .FALSE.
    RETURN
  ENDIF

  IF(self % minute < 0 .OR. self % minute > 59)THEN    
    isValid = .FALSE.
    RETURN
  ENDIF

  IF(self % second < 0 .OR. self % second > 59)THEN    
    isValid = .FALSE.
    RETURN
  ENDIF

  IF(self % millisecond < 0 .OR. self % millisecond > 999)THEN    
    isValid = .FALSE.
    RETURN
  ENDIF

ENDFUNCTION isValid
!======================================================================>



TYPE(datetime) FUNCTION now(self)
!======================================================================>
!
! datetime-bound procedure. Returns current time.
!
!======================================================================>

  ! ARGUMENTS:
  CLASS(datetime),INTENT(IN) :: self

  CHARACTER(LEN=5)     :: zone
  INTEGER,DIMENSION(8) :: values

  INTEGER :: hour,minute

  ! Obtain local machine time zone information
  CALL date_and_time(zone=zone,values=values)

  READ(UNIT=zone(1:3),FMT='(I3)')hour
  READ(UNIT=zone(4:5),FMT='(I2)')minute

  now = datetime(year        = values(1),&
                 month       = values(2),&
                 day         = values(3),&
                 hour        = values(5),&
                 minute      = values(6),&
                 second      = values(7),&
                 millisecond = values(8))

  now % tz = hour+minute*m2h

ENDFUNCTION now
!======================================================================>



PURE ELEMENTAL INTEGER FUNCTION weekday(self)
!======================================================================>
!
! datetime-bound method to calculate day of the week using
! Zeller's congruence. Returns an integer scalar in the range of [0-6], 
! starting from Sunday.
!
!======================================================================>

  ! ARGUMENTS:
  CLASS(datetime),INTENT(IN) :: self

  INTEGER :: year,month
  INTEGER :: j,k

  year  = self % year
  month = self % month

  IF(month <= 2)THEN
    month = month+12
    year  = year-1
  ENDIF

  j = year/100
  k = MOD(year,100)

  weekday = MOD(self % day+((month+1)*26)/10+k+k/4+j/4+5*j,7)-1

  IF(weekday < 0)weekday = 6

ENDFUNCTION weekday
!======================================================================>



PURE ELEMENTAL CHARACTER(LEN=9) FUNCTION weekdayLong(self)
!======================================================================>
!
! datetime-bound procedure. Returns the name of the day
! of the week.
!
!======================================================================>

  ! ARGUMENTS:
  CLASS(datetime),INTENT(IN) :: self

  CHARACTER(LEN=9),PARAMETER,DIMENSION(7) :: &
  days = ['Sunday   ','Monday   ','Tuesday  ','Wednesday',&
          'Thursday ','Friday   ','Saturday ']

  weekdayLong = days(self % weekday()+1)

ENDFUNCTION weekdayLong
!======================================================================>



PURE ELEMENTAL CHARACTER(LEN=3) FUNCTION weekdayShort(self)
!======================================================================>
!
! datetime-bound procedure. Returns a 3-character 
! representation of the name of the day of the week.
!
!======================================================================>

  ! ARGUMENTS:
  CLASS(datetime),INTENT(IN) :: self

  CHARACTER(LEN=3),PARAMETER,DIMENSION(7) :: &
                   days = ['Sun','Mon','Tue','Wed','Thu','Fri','Sat']

  weekdayShort = days(self % weekday()+1)

ENDFUNCTION weekdayShort
!======================================================================>



FUNCTION isocalendar(self)
!======================================================================>
!
! datetime-bound procedure. Returns an array of 3 integers,
! year, week number, and week day, as defined by ISO 8601 week date.
! Essentially a wrapper around C strftime() function.
!
!======================================================================>

  ! ARGUMENTS:
  CLASS(datetime),INTENT(IN) :: self

  INTEGER,DIMENSION(3) :: isocalendar
  INTEGER              :: year,week,wday
  INTEGER              :: rc
  CHARACTER(LEN=20)    :: string

  rc = c_strftime(string,LEN(string),'%G %V %u'//c_null_char,&
                  self % tm())  

  READ(UNIT=string(1:4),FMT='(I4)')year
  READ(UNIT=string(6:7),FMT='(I2)')week
  READ(UNIT=string(9:9),FMT='(I1)')wday

  isocalendar = [year,week,wday]

ENDFUNCTION isocalendar
!======================================================================>



INTEGER FUNCTION secondsSinceEpoch(self)
!======================================================================>
!
! datetime-bound procedure. Returns an integer number of 
! seconds since the UNIX Epoch, 1970-01-01 00:00:00.
! Note that this is a wrapper around C's strftime('%s'), so the number
! of seconds will reflect the time zone of the local machine on which
! the function is being called. 
!
!======================================================================>

  ! ARGUMENTS:
  CLASS(datetime),INTENT(IN) :: self

  INTEGER           :: rc
  CHARACTER(LEN=11) :: string

  string = self % strftime('%s')
  READ(UNIT=string,FMT='(I10)')secondsSinceEpoch

ENDFUNCTION secondsSinceEpoch
!======================================================================>



FUNCTION strftime(self,format)
!======================================================================>
!
! datetime-bound procedure that provides a wrapper around C/C++
! strftime function. 
!
!======================================================================>

  ! ARGUMENTS:
  CLASS(datetime), INTENT(IN)  :: self
  CHARACTER(LEN=*),INTENT(IN)  :: format

  CHARACTER(LEN=:),ALLOCATABLE :: strftime

  INTEGER                  :: n,rc
  CHARACTER(LEN=MAXSTRLEN) :: resultString
  TYPE(tm_struct)          :: tm

  resultString = ""
  rc = c_strftime(resultString,MAXSTRLEN,TRIM(format)//c_null_char,&
                  self % tm())
  strftime = TRIM(resultString)
  n = LEN(strftime)
  strftime = strftime(1:n-1)

ENDFUNCTION strftime
!======================================================================>



PURE ELEMENTAL TYPE(tm_struct) FUNCTION tm(self)
!======================================================================>
!
! datetime-bound procedure. Returns a respective tm_struct 
! instance.
!
!======================================================================>

  ! ARGUMENTS:
  CLASS(datetime),INTENT(IN) :: self

  tm % tm_sec   = self % second
  tm % tm_min   = self % minute
  tm % tm_hour  = self % hour
  tm % tm_mday  = self % day
  tm % tm_mon   = self % month-1
  tm % tm_year  = self % year-1900
  tm % tm_wday  = self % weekday()
  tm % tm_yday  = self % yearday()-1
  tm % tm_isdst = -1

ENDFUNCTION tm
!======================================================================>



PURE ELEMENTAL CHARACTER(LEN=5) FUNCTION tzOffset(self)
!======================================================================>
!
! Returns a character string with timezone offset in hours from UTC,
! in format +/-[hh][mm].
!
!======================================================================>

  ! ARGUMENTS:
  CLASS(datetime),INTENT(IN) :: self

  INTEGER :: hours,minutes

  IF(self % tz < 0)THEN
    tzOffset(1:1) = '-'
  ELSE
    tzOffset(1:1) = '+'
  ENDIF

  hours   = INT(ABS(self % tz))
  minutes = NINT((ABS(self % tz)-hours)*60)

  IF(minutes == 60)THEN
    minutes = 0
    hours = hours+1
  ENDIF

  WRITE(UNIT=tzOffset(2:5),FMT='(2I2.2)')hours,minutes

ENDFUNCTION tzOffset
!======================================================================>



PURE ELEMENTAL TYPE(datetime) FUNCTION utc(self)
!======================================================================>
!
! Returns the datetime instance at Coordinated Universal Time (UTC). 
!
!======================================================================>

  ! ARGUMENTS:
  CLASS(datetime),INTENT(IN) :: self

  INTEGER :: hours,minutes,sgn

  hours   = INT(ABS(self % tz))
  minutes = NINT((ABS(self % tz)-hours)*60)
  sgn     = INT(SIGN(one,self % tz))

  utc      = self-timedelta(hours=sgn*hours,minutes=sgn*minutes)
  utc % tz = 0

ENDFUNCTION utc
!======================================================================>



PURE ELEMENTAL INTEGER FUNCTION yearday(self)
!======================================================================>
!
! datetime-bound procedure. Returns integer day of the
! year (ordinal date).
!
!======================================================================>

  ! ARGUMENTS:
  CLASS(datetime),INTENT(IN) :: self

  INTEGER :: month

  yearday = 0
  DO month=1,self % month-1
    yearday = yearday+daysInMonth(month,self % year)
  ENDDO
  yearday = yearday+self % day

ENDFUNCTION yearday
!======================================================================>



!::: Datetime operators ::::::::::::::::::::::::::::::::::::::::::::::::

PURE ELEMENTAL FUNCTION datetime_plus_timedelta(d0,t) RESULT(d)
!======================================================================>
!
! Adds a timedelta instance to a datetime instance.
! Returns a new datetime instance. Overloads the operator +.
!
!======================================================================>

  ! ARGUMENTS:
  TYPE(datetime), INTENT(IN) :: d0
  TYPE(timedelta),INTENT(IN) :: t
  TYPE(datetime)             :: d

  ! Initialize:
  d = d0 

  IF(t % milliseconds /= 0)CALL d % addMilliseconds(t % milliseconds)
  IF(t % seconds      /= 0)CALL d % addSeconds(t % seconds)
  IF(t % minutes      /= 0)CALL d % addMinutes(t % minutes)
  IF(t % hours        /= 0)CALL d % addHours(t % hours)
  IF(t % days         /= 0)CALL d % addDays(t % days)

ENDFUNCTION datetime_plus_timedelta
!======================================================================>



PURE ELEMENTAL FUNCTION timedelta_plus_datetime(t,d0) RESULT(d)
!======================================================================>
!
! Adds a timedelta instance to a datetime instance.
! Returns a new datetime instance. Overloads the operator +.
!
!======================================================================>

  ! ARGUMENTS:
  TYPE(timedelta),INTENT(IN) :: t
  TYPE(datetime), INTENT(IN) :: d0
  TYPE(datetime)             :: d

  d = d0+t

ENDFUNCTION timedelta_plus_datetime
!======================================================================>



PURE ELEMENTAL FUNCTION datetime_minus_timedelta(d0,t) RESULT(d)
!======================================================================>
!
! Subtracts a timedelta instance from a datetime instance.
! Returns a new datetime instance. Overloads the operator -.
!
!======================================================================>

  ! ARGUMENTS:
  TYPE(datetime), INTENT(IN) :: d0
  TYPE(timedelta),INTENT(IN) :: t
  TYPE(datetime)             :: d

  ! Initialize:
  d = d0

  IF(t % milliseconds /= 0)CALL d % addMilliseconds(-t % milliseconds)
  IF(t % seconds      /= 0)CALL d % addSeconds(-t % seconds)
  IF(t % minutes      /= 0)CALL d % addMinutes(-t % minutes)
  IF(t % hours        /= 0)CALL d % addHours(-t % hours)
  IF(t % days         /= 0)CALL d % addDays(-t % days)

ENDFUNCTION datetime_minus_timedelta
!======================================================================>



PURE ELEMENTAL FUNCTION datetime_minus_datetime(d0,d1) RESULT(t)
!======================================================================>
!
! Subtracts a datetime instance from another datetime 
! instance. Returns a timedelta instance. Overloads the operator -.
!
!======================================================================>

  ! ARGUMENTS:
  TYPE(datetime),INTENT(IN) :: d0,d1
  TYPE(timedelta)           :: t

  REAL(KIND=real_dp) :: daysDiff
  INTEGER            :: days,hours,minutes,seconds,milliseconds
  INTEGER            :: sign_

  daysDiff = date2num(d0)-date2num(d1)

  IF(daysDiff < 0)THEN
    sign_ = -1
    daysDiff = ABS(daysDiff)
  ELSE
    sign_ = 1
  ENDIF

  days         = INT(daysDiff)
  hours        = INT((daysDiff-days)*d2h)
  minutes      = INT((daysDiff-days-hours*h2d)*d2m)
  seconds      = INT((daysDiff-days-hours*h2d-minutes*m2d)*d2s)
  milliseconds = NINT((daysDiff-days-hours*h2d-minutes*m2d&
                               -seconds*s2d)*d2s*1d3)

  t = timedelta(sign_*days,sign_*hours,sign_*minutes,sign_*seconds,&
                sign_*milliseconds)

ENDFUNCTION datetime_minus_datetime
!======================================================================>



PURE ELEMENTAL FUNCTION timedelta_plus_timedelta(t0,t1) RESULT(t)
!======================================================================>
! 
! Adds two timedelta instances together. Returns a timedelta instance. 
! Overloads the operator +.
!
!======================================================================>

  ! ARGUMENTS:
  TYPE(timedelta),INTENT(IN) :: t0
  TYPE(timedelta),INTENT(IN) :: t1
  TYPE(timedelta)            :: t

  t = timedelta(days         = t0 % days         + t1 % days,   &
                hours        = t0 % hours        + t1 % hours,  &
                minutes      = t0 % minutes      + t1 % minutes,&
                seconds      = t0 % seconds      + t1 % seconds,&
                milliseconds = t0 % milliseconds + t1 % milliseconds)

ENDFUNCTION timedelta_plus_timedelta
!======================================================================>


PURE ELEMENTAL FUNCTION timedelta_minus_timedelta(t0,t1) RESULT(t)
!======================================================================>
! 
! Subtracts a timedelta instance from another. Returns a timedelta 
! instance. Overloads the operator +.
!
!======================================================================>

  ! ARGUMENTS:
  TYPE(timedelta),INTENT(IN) :: t0
  TYPE(timedelta),INTENT(IN) :: t1
  TYPE(timedelta)            :: t

  t = t0 + (-t1)

ENDFUNCTION timedelta_minus_timedelta
!======================================================================>



PURE ELEMENTAL LOGICAL FUNCTION gt(d0,d1)
!======================================================================>
!
! datetime object comparison operator. Returns .TRUE. if
! d0 is greater than d1, and .FALSE. otherwise. Overloads the 
! operator >.
!
!======================================================================>

  ! ARGUMENTS:
  TYPE(datetime),INTENT(IN) :: d0,d1

  TYPE(datetime) :: d0_utc,d1_utc

  ! Convert to UTC before making comparison
  d0_utc = d0 % utc()
  d1_utc = d1 % utc()

  ! Year comparison block
  IF(d0_utc % year > d1_utc % year)THEN
    gt = .TRUE.
  ELSEIF(d0_utc % year < d1_utc % year)THEN
    gt = .FALSE.
  ELSE

    ! Month comparison block
    IF(d0_utc % month > d1_utc % month)THEN
      gt = .TRUE.
    ELSEIF(d0_utc % month < d1_utc % month)THEN
      gt = .FALSE.
    ELSE

      ! Day comparison block
      IF(d0_utc % day > d1_utc % day)THEN
        gt = .TRUE.
      ELSEIF(d0_utc % day < d1_utc % day)THEN
        gt = .FALSE.
      ELSE

        ! Hour comparison block
        IF(d0_utc % hour > d1_utc % hour)THEN
          gt = .TRUE.
        ELSEIF(d0_utc % hour < d1_utc % hour)THEN
          gt = .FALSE.
        ELSE

          ! Minute comparison block
          IF(d0_utc % minute > d1_utc % minute)THEN
            gt = .TRUE.
          ELSEIF(d0_utc % minute < d1_utc % minute)THEN
            gt = .FALSE.
          ELSE

            ! Second comparison block
            IF(d0_utc % second > d1_utc % second)THEN
              gt = .TRUE.
            ELSEIF(d0_utc % second < d1_utc % second)THEN
              gt = .FALSE.
            ELSE

              ! Millisecond comparison block
              IF(d0_utc % millisecond > d1_utc % millisecond)THEN
                gt = .TRUE.
              ELSE
                gt = .FALSE.
              ENDIF

            ENDIF
          ENDIF
        ENDIF
      ENDIF
    ENDIF
  ENDIF

ENDFUNCTION gt
!======================================================================>



PURE ELEMENTAL LOGICAL FUNCTION lt(d0,d1)
!======================================================================>
!
! datetime object comparison operator. Returns .TRUE. if
! d0 is less than d1, and .FALSE. otherwise. Overloads the operator <.
!
!======================================================================>

  ! ARGUMENTS:
  TYPE(datetime),INTENT(IN) :: d0,d1

  lt = d1 > d0

ENDFUNCTION lt
!======================================================================>



PURE ELEMENTAL LOGICAL FUNCTION eq(d0,d1)
!======================================================================>
!
! datetime object comparison operator. Returns .TRUE. if
! d0 is equal to d1, and .FALSE. otherwise. Overloads the operator ==.
!
!======================================================================>

  ! ARGUMENTS:
  TYPE(datetime),INTENT(IN) :: d0,d1

  TYPE(datetime) :: d0_utc,d1_utc

  ! Convert to UTC before making comparison
  d0_utc = d0 % utc()
  d1_utc = d1 % utc()

  eq = d0_utc % year        == d1_utc % year   .AND. &
       d0_utc % month       == d1_utc % month  .AND. &
       d0_utc % day         == d1_utc % day    .AND. &
       d0_utc % hour        == d1_utc % hour   .AND. &
       d0_utc % minute      == d1_utc % minute .AND. &
       d0_utc % second      == d1_utc % second .AND. &
       d0_utc % millisecond == d1_utc % millisecond

ENDFUNCTION eq
!======================================================================>



PURE ELEMENTAL LOGICAL FUNCTION neq(d0,d1)
!======================================================================>
!
! datetime object comparison operator. Returns .TRUE. if d0 is not equal 
! to d1, and .FALSE. otherwise. Overloads the operator /=.
!
!======================================================================>

  ! ARGUMENTS:
  TYPE(datetime),INTENT(IN) :: d0,d1

  neq = .NOT. d0 == d1

ENDFUNCTION neq
!======================================================================>



PURE ELEMENTAL LOGICAL FUNCTION ge(d0,d1)
!======================================================================>
!
! datetime object comparison operator. Returns .TRUE. if
! d0 is greater or equal than d1, and .FALSE. otherwise. Overloads the 
! operator >=.
!
!======================================================================>

  ! ARGUMENTS:
  TYPE(datetime),INTENT(IN) :: d0,d1

  ge = d0 > d1 .OR. d0 == d1

ENDFUNCTION ge
!======================================================================>



PURE ELEMENTAL LOGICAL FUNCTION le(d0,d1)
!======================================================================>
!
! datetime object comparison operator. Returns .TRUE. if
! d0 is less or equal than d1, and .FALSE. otherwise. Overloads the 
! operator <=.
!
!======================================================================>

  ! ARGUMENTS:
  TYPE(datetime),INTENT(IN) :: d0,d1

  le = d1 > d0 .OR. d0 == d1

ENDFUNCTION le
!======================================================================>


!::: Timedelta-bound methods :::::::::::::::::::::::::::::::::::::::::::

PURE ELEMENTAL REAL(KIND=real_dp) FUNCTION total_seconds(self)
!======================================================================>
!
! timedelta-bound procedure. Returns a total number of 
! seconds contained in a timedelta instance.
!
!======================================================================>

  ! ARGUMENTS:
  CLASS(timedelta),INTENT(IN) :: self

  total_seconds = self % days*86400d0 &
                 +self % hours*3600d0 &
                 +self % minutes*60d0 &
                 +self % seconds      &
                 +self % milliseconds*1d-3

ENDFUNCTION total_seconds
!======================================================================>


!::: Timedelta operators :::::::::::::::::::::::::::::::::::::::::::::::

PURE ELEMENTAL FUNCTION unary_minus_timedelta(t0) RESULT(t)
!======================================================================>
!
! Takes a negative of a timedelta instance. Overloads the 
! operator -.
!
!======================================================================>

  ! ARGUMENTS:
  TYPE(timedelta),INTENT(IN) :: t0
  TYPE(timedelta)            :: t

  t % days         = -t0 % days
  t % hours        = -t0 % hours
  t % minutes      = -t0 % minutes
  t % seconds      = -t0 % seconds
  t % milliseconds = -t0 % milliseconds

ENDFUNCTION unary_minus_timedelta
!======================================================================>
  


PURE ELEMENTAL LOGICAL FUNCTION eq_td(td0,td1)
!======================================================================>
!
! timedelta object comparison operator. Returns .TRUE. if
! td0 is equal to td1, and .FALSE. otherwise. Overloads the operator ==.
!
!======================================================================>

  ! ARGUMENTS:
  TYPE(timedelta),INTENT(IN) :: td0,td1

  eq_td = td0 % total_seconds() == td1 % total_seconds()

ENDFUNCTION eq_td
!======================================================================>



PURE ELEMENTAL LOGICAL FUNCTION neq_td(td0,td1)
!======================================================================>
!
! timedelta object comparison operator. Returns .TRUE. if
! td0 is not equal to td1, and .FALSE. otherwise. Overloads the 
! operator /=.
!
!======================================================================>

  ! ARGUMENTS:
  TYPE(timedelta),INTENT(IN) :: td0,td1

  neq_td = .NOT. (td0 % total_seconds() == td1 % total_seconds())

ENDFUNCTION neq_td
!======================================================================>



PURE ELEMENTAL LOGICAL FUNCTION gt_td(td0,td1)
!======================================================================>
!
! timedelta object comparison operator. Returns .TRUE. if
! td0 is greater than td1, and .FALSE. otherwise. Overloads the 
! operator >.
!
!======================================================================>

  ! ARGUMENTS:
  TYPE(timedelta),INTENT(IN) :: td0,td1

  gt_td = td0 % total_seconds() > td1 % total_seconds()

ENDFUNCTION gt_td
!======================================================================>



PURE ELEMENTAL LOGICAL FUNCTION ge_td(td0,td1)
!======================================================================>
!
! timedelta object comparison operator. Returns .TRUE. if
! td0 is greater than or equal to td1, and .FALSE. otherwise. Overloads 
! the operator >=.
!
!======================================================================>

  ! ARGUMENTS:
  TYPE(timedelta),INTENT(IN) :: td0,td1

  ge_td = td0 % total_seconds() >= td1 % total_seconds()

ENDFUNCTION ge_td
!======================================================================>



PURE ELEMENTAL LOGICAL FUNCTION lt_td(td0,td1)
!======================================================================>
!
! timedelta object comparison operator. Returns .TRUE. if
! td0 is less than td1, and .FALSE. otherwise. Overloads the operator <.
!
!======================================================================>

  ! ARGUMENTS:
  TYPE(timedelta),INTENT(IN) :: td0,td1

  lt_td = td0 % total_seconds() < td1 % total_seconds()

ENDFUNCTION lt_td
!======================================================================>



PURE ELEMENTAL LOGICAL FUNCTION le_td(td0,td1)
!======================================================================>
!
! timedelta object comparison operator. Returns .TRUE. if
! td0 is less than or equal to td1, and .FALSE. otherwise. Overloads the 
! operator <=.
!
!======================================================================>

  ! ARGUMENTS:
  TYPE(timedelta),INTENT(IN) :: td0,td1

  le_td = td0 % total_seconds() <= td1 % total_seconds()

ENDFUNCTION le_td
!======================================================================>



!::: Clock-bound methods :::::::::::::::::::::::::::::::::::::::::::::::

PURE ELEMENTAL SUBROUTINE reset(self)
!======================================================================>
!
! Resets the clock to its start time.
!
!======================================================================>

  ! ARGUMENTS:
  CLASS(clock),INTENT(INOUT) :: self

  self % currentTime = self % startTime

  self % started = .FALSE.
  self % stopped = .FALSE.

ENDSUBROUTINE reset
!======================================================================>



PURE ELEMENTAL SUBROUTINE tick(self)
!======================================================================>
!
! Increments the currentTime of the clock instance by one tickInterval.
!
!======================================================================>

  ! ARGUMENTS:
  CLASS(clock),INTENT(INOUT) :: self

  IF(self % stopped)THEN
    RETURN
  ENDIF

  IF(.NOT.self % started)THEN
    self % started = .TRUE.
    self % currentTime = self % startTime
  ENDIF

  self % currentTime = self % currentTime + self % tickInterval

  IF(self % currentTime >= self % stopTime)THEN
    self % stopped = .TRUE.
  ENDIF

ENDSUBROUTINE tick
!======================================================================>



!--- PUBLIC PROCEDURES ----------------------------------------------->

PURE ELEMENTAL LOGICAL FUNCTION isLeapYear(year)
!======================================================================>
!
! Given an integer year, returns .TRUE. if year is leap
! year, and .FALSE. otherwise.
!
!======================================================================>

  ! ARGUMENTS:
  INTEGER,INTENT(IN) :: year

  isLeapYear = (MOD(year,4) == 0 .AND. .NOT. MOD(year,100) == 0)&
          .OR. (MOD(year,400) == 0)

ENDFUNCTION isLeapYear
!======================================================================>



PURE FUNCTION datetimeRange(d0,d1,t)
!======================================================================>
!
! Given start and end datetime instances d0 and d1, and time increment
! as timedelta instance t, returns an array of datetime instances.
! The number of elements is the number of whole time increments 
! contained between datetimes d0 and d1.
!
!======================================================================>

  ! ARGUMENTS:
  TYPE(datetime), INTENT(IN) :: d0
  TYPE(datetime), INTENT(IN) :: d1
  TYPE(timedelta),INTENT(IN) :: t

  REAL(KIND=real_dp) :: datenum0,datenum1,increment
  REAL(KIND=real_dp) :: eps

  TYPE(datetime),DIMENSION(:),ALLOCATABLE :: datetimeRange

  INTEGER :: n,nm

  eps = 1e-10_real_dp

  datenum0 = date2num(d0)
  datenum1 = date2num(d1)

  increment = t % total_seconds() * s2d

  nm = FLOOR((datenum1-datenum0+eps)/increment)+1

  ALLOCATE(datetimeRange(nm))

  DO n = 1,nm
    datetimeRange(n) = num2date(datenum0 + (n-1)*increment)
  ENDDO

ENDFUNCTION datetimeRange
!======================================================================>



PURE ELEMENTAL INTEGER FUNCTION daysInMonth(month,year)
!======================================================================>
!
! Given integer month and year, returns an integer number
! of days in that particular month.
!
!======================================================================>

  ! ARGUMENTS:
  INTEGER,INTENT(IN) :: month
  INTEGER,INTENT(IN) :: year

  INTEGER,PARAMETER,DIMENSION(12) :: &
          days = [31,28,31,30,31,30,31,31,30,31,30,31]

  IF(month < 1 .OR. month > 12)THEN
    ! Should raise an error and abort here, however we want to keep
    ! the pure and elemental attributes. Make sure this function is 
    ! called with the month argument in range. 
    daysInMonth = 0
    RETURN
  ENDIF

  IF(month == 2 .AND. isLeapYear(year))THEN
    daysInMonth = 29
  ELSE
    daysInMonth = days(month)
  ENDIF

ENDFUNCTION daysInMonth
!======================================================================>



PURE ELEMENTAL INTEGER FUNCTION daysInYear(year)
!======================================================================>
!
! Given an integer year, returns an integer number of days
! in that year.
!
!======================================================================>

  ! ARGUMENTS:
  INTEGER,INTENT(IN) :: year

  IF(isLeapYear(year))THEN
    daysInYear = 366
  ELSE
    daysInYear = 365
  ENDIF

ENDFUNCTION daysInYear
!======================================================================>



PURE ELEMENTAL REAL(KIND=real_dp) FUNCTION date2num(d)
!======================================================================>
!
! Given a datetime instance d, returns number of days since 
! 0001-01-01 00:00:00.
!
! Since version 1.0.5, this function is timezone aware, i.e. we first
! switch to UTC time, then we evaluate the number of days. This may
! affect some of the existing programs using this function.
!
!======================================================================>

  ! ARGUMENTS:
  TYPE(datetime),INTENT(IN) :: d

  TYPE(datetime) :: d_utc
  INTEGER :: year

  ! Convert to UTC first
  d_utc = d % utc()

  ! d_utc % year must be positive:
  IF(d_utc % year < 1)THEN
    date2num = 0
    RETURN
  ENDIF

  date2num = 0
  DO year = 1,d_utc % year-1
    date2num = date2num + daysInYear(year)
  ENDDO

  date2num = date2num + d_utc % yearday()  &
                      + d_utc % hour*h2d   &
                      + d_utc % minute*m2d &
                      + (d_utc % second+1d-3*d_utc % millisecond)*s2d
 
ENDFUNCTION date2num
!======================================================================>



PURE ELEMENTAL TYPE(datetime) FUNCTION num2date(num)
!======================================================================>
!
! Given number of days since 0001-01-01 00:00:00, returns a 
! correspoding datetime instance.
!
!======================================================================>

  ! ARGUMENTS:
  REAL(KIND=real_dp),INTENT(IN) :: num

  INTEGER :: year,month,day,hour,minute,second,millisecond
  REAL(KIND=real_dp) :: days,totseconds

  ! num must be positive:
  IF(num < 0)THEN
    num2date = datetime(1)
    RETURN
  ENDIF

  days = num

  year = 1
  DO
    IF(INT(days) <= daysInYear(year))EXIT
    days = days-daysInYear(year)
    year = year+1
  ENDDO

  month = 1
  DO
    IF(INT(days) <= daysInMonth(month,year))EXIT
    days = days-daysInMonth(month,year)
    month = month+1
  ENDDO

  day         = INT(days)
  totseconds  = (days-day)*d2s
  hour        = INT(totseconds*s2h)
  minute      = INT((totseconds-hour*h2s)*s2m)
  second      = INT(totseconds-hour*h2s-minute*m2s)
  millisecond = NINT((totseconds-INT(totseconds))*1d3)

  num2date = datetime(year,month,day,hour,minute,second,millisecond,tz=0)

  ! Handle a special case caused by floating-point arithmethic:
  IF(num2date % millisecond == 1000)THEN
    num2date % millisecond = 0
    CALL num2date % addSeconds(1)
  ENDIF

ENDFUNCTION num2date
!======================================================================>



TYPE(datetime) FUNCTION strptime(str,format)
!======================================================================>
!
! A wrapper function around C/C++ strptime. 
! Returns a datetime instance. 
!
!======================================================================>

  ! ARGUMENTS:
  CHARACTER(LEN=*),INTENT(IN) :: str
  CHARACTER(LEN=*),INTENT(IN) :: format

  INTEGER         :: rc
  TYPE(tm_struct) :: tm

  rc = c_strptime(TRIM(str)//c_null_char,TRIM(format)//c_null_char,tm)
  strptime = tm2date(tm)

ENDFUNCTION strptime
!======================================================================>



PURE ELEMENTAL TYPE(datetime) FUNCTION tm2date(ctime)
!======================================================================>
!
! Given a tm_struct instance, returns a corresponding datetime instance.
!
!======================================================================>

  ! ARGUMENTS:
  TYPE(tm_struct),INTENT(IN) :: ctime

  tm2date%millisecond = 0
  tm2date%second      = ctime % tm_sec
  tm2date%minute      = ctime % tm_min
  tm2date%hour        = ctime % tm_hour
  tm2date%day         = ctime % tm_mday
  tm2date%month       = ctime % tm_mon+1
  tm2date%year        = ctime % tm_year+1900
  tm2date%tz          = 0

ENDFUNCTION tm2date
!======================================================================>



!--- PRIVATE PROCEDURES ----------------------------------------------->

PURE FUNCTION int2str(i,length)
!======================================================================>
!
! Converts an integer i into a character string of requested length, 
! pre-pending zeros if necessary.
!
!======================================================================>

  ! ARGUMENTS:
  INTEGER,INTENT(IN) :: i
  INTEGER,INTENT(IN) :: length

  CHARACTER(LEN=length) :: int2str
  CHARACTER(LEN=2)      :: string

  WRITE(UNIT=string,FMT='(I2)')length
  WRITE(UNIT=int2str,FMT='(I'//string//'.'//string//')')i

ENDFUNCTION int2str
!======================================================================>
ENDMODULE datetime_module
