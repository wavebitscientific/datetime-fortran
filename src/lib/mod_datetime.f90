!
! datetime-fortran - A Fortran library for date and time manipulation
! Copyright (c) 2013-2016, Wavebit Scientific LLC
! All rights reserved.
! 
! Licensed under the BSD-3 clause license. See LICENSE for details.
!
module mod_datetime
!=======================================================================
!

use,intrinsic :: iso_fortran_env,only:real32,real64
use,intrinsic :: iso_c_binding,only:c_char,c_int,c_null_char
use :: mod_timedelta,only:timedelta
use :: mod_strftime, only:tm_struct,c_strftime,c_strptime
use :: mod_constants

implicit none

private

! Derived types:
public :: datetime

public :: operator(+)

! Procedures:
public :: date2num
public :: datetimeRange
public :: daysInMonth
public :: daysInYear
public :: isLeapYear
public :: num2date
public :: strptime
public :: tm2date

! Derived types:

type :: datetime
!=======================================================================
!
! A main datetime class for date and time representation. It is modeled 
! after Python's datetime.datetime class, and has similar components 
! and methods (but not all).
!
!=======================================================================

  private

  integer :: year        = 1 ! Year                   [1-HUGE(year)]
  integer :: month       = 1 ! Month in year          [1-12]
  integer :: day         = 1 ! Day in month           [1-31]
  integer :: hour        = 0 ! Hour in day            [0-23]
  integer :: minute      = 0 ! Minute in hour         [0-59]
  integer :: second      = 0 ! Second in minute       [0-59]
  integer :: millisecond = 0 ! Milliseconds in second [0-999]

  real(kind=real64) :: tz = 0 ! Timezone offset from UTC [hours]

  contains

  ! getter functions
  procedure,pass(self),public :: getYear
  procedure,pass(self),public :: getMonth
  procedure,pass(self),public :: getDay
  procedure,pass(self),public :: getHour
  procedure,pass(self),public :: getMinute
  procedure,pass(self),public :: getSecond
  procedure,pass(self),public :: getMillisecond
  procedure,pass(self),public :: getTz

  ! public methods
  procedure,pass(self),public :: isocalendar
  procedure,pass(self),public :: isoformat
  procedure,pass(self),public :: isValid
  procedure,nopass,    public :: now
  procedure,pass(self),public :: secondsSinceEpoch
  procedure,pass(self),public :: strftime
  procedure,pass(self),public :: tm
  procedure,pass(self),public :: tzOffset
  procedure,pass(self),public :: utc
  procedure,pass(self),public :: weekday
  procedure,pass(self),public :: weekdayLong
  procedure,pass(self),public :: weekdayShort
  procedure,pass(self),public :: yearday

  ! private methods
  procedure :: addMilliseconds
  procedure :: addSeconds
  procedure :: addMinutes
  procedure :: addHours
  procedure :: addDays

  ! operator overloading procedures
  procedure,private :: datetime_plus_timedelta
  !procedure,private :: timedelta_plus_datetime
  procedure,private :: datetime_minus_datetime
  procedure,private :: datetime_minus_timedelta 
  procedure,private :: eq
  procedure,private :: neq
  procedure,private :: gt
  procedure,private :: ge
  procedure,private :: lt
  procedure,private :: le

  generic :: operator(+) => datetime_plus_timedelta!,&
                            !timedelta_plus_datetime
  generic :: operator(-) => datetime_minus_datetime,&
                            datetime_minus_timedelta

  generic :: operator(==) => eq
  generic :: operator(/=) => neq
  generic :: operator(>)  => gt
  generic :: operator(>=) => ge
  generic :: operator(<)  => lt
  generic :: operator(<=) => le

endtype datetime

interface datetime
  module procedure :: datetime_constructor
endinterface datetime

interface operator(+)
  module procedure :: timedelta_plus_datetime
endinterface

!=======================================================================
contains



!=======================================================================
pure elemental type(datetime) function datetime_constructor(year,month,&
  day,hour,minute,second,millisecond,tz)

  integer,          intent(in),optional :: year
  integer,          intent(in),optional :: month
  integer,          intent(in),optional :: day
  integer,          intent(in),optional :: hour
  integer,          intent(in),optional :: minute
  integer,          intent(in),optional :: second
  integer,          intent(in),optional :: millisecond
  real(kind=real64),intent(in),optional :: tz

  if(present(year))then
    datetime_constructor % year = year    
  else
    datetime_constructor % year = 1
  endif

  if(present(month))then
    datetime_constructor % month = month
  else
    datetime_constructor % month = 1
  endif

  if(present(day))then
    datetime_constructor % day = day
  else
    datetime_constructor % day = 1
  endif

  if(present(hour))then
    datetime_constructor % hour = hour
  else
    datetime_constructor % hour = 0
  endif

  if(present(minute))then
    datetime_constructor % minute = minute
  else
    datetime_constructor % minute = 0
  endif

  if(present(second))then
    datetime_constructor % second = second
  else
    datetime_constructor % second = 0
  endif

  if(present(millisecond))then
    datetime_constructor % millisecond = millisecond
  else
    datetime_constructor % millisecond = 0
  endif

  if(present(tz))then
    datetime_constructor % tz = tz
  else
    datetime_constructor % tz = 0
  endif

endfunction datetime_constructor
!=======================================================================



! datetime getters  
!=======================================================================
pure elemental integer function getYear(self)
  class(datetime),intent(in) :: self
  getYear = self % year
endfunction getYear
!=======================================================================



!=======================================================================
pure elemental integer function getMonth(self)
  class(datetime),intent(in) :: self
  getMonth = self % month
endfunction getMonth
!=======================================================================



!=======================================================================
pure elemental integer function getDay(self)
  class(datetime),intent(in) :: self
  getDay = self % day
endfunction getDay
!=======================================================================



!=======================================================================
pure elemental integer function getHour(self)
  class(datetime),intent(in) :: self
  getHour = self % hour
endfunction getHour
!=======================================================================



!=======================================================================
pure elemental integer function getMinute(self)
  class(datetime),intent(in) :: self
  getMinute = self % minute
endfunction getMinute
!=======================================================================



!=======================================================================
pure elemental integer function getSecond(self)
  class(datetime),intent(in) :: self
  getSecond = self % second
endfunction getSecond
!=======================================================================



!=======================================================================
pure elemental integer function getMillisecond(self)
  class(datetime),intent(in) :: self
  getMillisecond = self % millisecond
endfunction getMillisecond
!=======================================================================



!=======================================================================
pure elemental real(kind=real64) function getTz(self)
  class(datetime),intent(in) :: self
  getTz = self % tz
endfunction getTz
!=======================================================================




pure elemental subroutine addMilliseconds(self,ms)
!=======================================================================
!
! datetime-bound procedure. Adds an integer number of milliseconds to 
! self. Called by datetime addition (+) and subtraction (-) operators.
!
!=======================================================================

  ! ARGUMENTS:
  class(datetime),intent(inout) :: self
  integer,        intent(in)    :: ms

  self % millisecond = self % millisecond+ms

  do
    if(self % millisecond >= 1000)then
      call self % addSeconds(self % millisecond/1000)
      self % millisecond = mod(self % millisecond,1000)
    elseif(self % millisecond < 0)then
      call self % addSeconds(self % millisecond/1000-1)
      self % millisecond = mod(self % millisecond,1000)+1000
    else
      exit
    endif
  enddo

endsubroutine addMilliseconds
!=======================================================================



pure elemental subroutine addSeconds(self,s)
!=======================================================================
!
! datetime-bound procedure. Adds an integer number of seconds to self. 
! Called by datetime addition (+) and subtraction (-) operators.
!
!=======================================================================

  ! ARGUMENTS:
  class(datetime),intent(inout) :: self
  integer,        intent(in)    :: s

  self % second = self % second+s

  do
    if(self % second >= 60)then
      call self % addMinutes(self % second/60)
      self % second = mod(self % second,60)
    elseif(self % second < 0)then
      call self % addMinutes(self % second/60-1)
      self % second = mod(self % second,60)+60
    else
      exit
    endif
  enddo

endsubroutine addSeconds
!=======================================================================



pure elemental subroutine addMinutes(self,m)
!=======================================================================
!
! datetime-bound procedure. Adds an integer number of minutes to self. 
! Called by datetime addition (+) and subtraction (-) operators.
!
!=======================================================================

  ! ARGUMENTS:
  class(datetime),intent(inout) :: self
  integer,        intent(in)    :: m

  self % minute = self % minute+m

  do
    if(self % minute >= 60)then
      call self % addHours(self % minute/60)
      self % minute = mod(self % minute,60)
    elseif(self % minute < 0)then
      call self % addHours(self % minute/60-1)
      self % minute = mod(self % minute,60)+60
    else
      exit
    endif
  enddo

endsubroutine addMinutes
!=======================================================================



pure elemental subroutine addHours(self,h)
!=======================================================================
!
! datetime-bound procedure. Adds an integer number of hours to self. 
! Called by datetime addition (+) and subtraction (-) operators.
!
!=======================================================================
  
  ! ARGUMENTS:
  class(datetime),intent(inout) :: self
  integer,        intent(in)    :: h

  self % hour = self % hour+h

  do
    if(self % hour >= 24)then
      call self % addDays(self % hour/24)
      self % hour = mod(self % hour,24)
    elseif(self % hour < 0)then
      call self % addDays(self % hour/24-1)
      self % hour = mod(self % hour,24)+24
    else
      exit
    endif
  enddo

endsubroutine addHours
!=======================================================================



pure elemental subroutine addDays(self,d)
!=======================================================================
!
! datetime-bound procedure. Adds an integer number of days to self. 
! Called by datetime addition (+) and subtraction (-) operators.
!
!=======================================================================

  ! ARGUMENTS:
  class(datetime),intent(inout) :: self
  integer,        intent(in)    :: d

  integer :: daysInCurrentMonth

  self % day = self % day+d
  do
    daysInCurrentMonth = daysInMonth(self % month,self % year)
    if(self % day > daysInCurrentMonth)then
      self % day = self % day-daysInCurrentMonth
      self % month = self % month+1
      if(self % month > 12)then
        self % year = self % year+self % month/12
        self % month = mod(self % month,12)
      endif
    elseif(self % day < 1)then
      self % month = self % month-1
      if(self % month < 1)then
        self % year = self % year+self % month/12-1
        self % month = 12+mod(self % month,12)
      endif
      self % day = self % day+daysInMonth(self % month,self % year)
    else
      exit
    endif 
  enddo

endsubroutine addDays
!=======================================================================



pure elemental character(len=23) function isoformat(self,sep)
!=======================================================================
!
! Returns character string with time in ISO 8601 format.
!
!=======================================================================

  ! ARGUMENTS:
  class(datetime), intent(in)          :: self
  character(len=1),intent(in),OPTIONAL :: sep

  character(len=1)                     :: separator

  if(PRESENT(sep))then
    separator = sep
  else
    separator = 'T'
  endif

  isoformat = int2str(self % year,       4)//'-'//      &
              int2str(self % month,      2)//'-'//      &
              int2str(self % day,        2)//separator//&
              int2str(self % hour,       2)//':'//      &
              int2str(self % minute,     2)//':'//      &
              int2str(self % second,     2)//'.'//      &
              int2str(self % millisecond,3)

endfunction isoformat
!=======================================================================



pure elemental logical function isValid(self)
!=======================================================================
!
! datetime-bound method that checks whether the datetime
! instance has valid component values. Returns .true. if the datetime
! instance is valid, and .false. otherwise.
!
!=======================================================================

  ! ARGUMENTS:
  class(datetime),intent(in) :: self

  isValid = .true.

  if(self % year < 1)then
    isValid = .false.
    return
  endif

  if(self % month < 1 .or. self % month > 12)then
    isValid = .false.
    return
  endif

  if(self % day < 1 .or. &
     self % day > daysInMonth(self % month,self % year))then
    isValid = .false.
    return
  endif
 
  if(self % hour < 0 .or. self % hour > 23)then    
    isValid = .false.
    return
  endif

  if(self % minute < 0 .or. self % minute > 59)then    
    isValid = .false.
    return
  endif

  if(self % second < 0 .or. self % second > 59)then    
    isValid = .false.
    return
  endif

  if(self % millisecond < 0 .or. self % millisecond > 999)then    
    isValid = .false.
    return
  endif

endfunction isValid
!=======================================================================



type(datetime) function now()
!=======================================================================
!
! datetime-bound procedure. Returns current time.
!
!=======================================================================

  character(len=5)     :: zone
  integer,dimension(8) :: values

  integer :: hour,minute

  ! Obtain local machine time zone information
  call date_and_time(zone=zone,values=values)

  read(unit=zone(1:3),fmt='(I3)')hour
  read(unit=zone(4:5),fmt='(I2)')minute

  now = datetime(year        = values(1),&
                 month       = values(2),&
                 day         = values(3),&
                 hour        = values(5),&
                 minute      = values(6),&
                 second      = values(7),&
                 millisecond = values(8))

  now % tz = hour+minute*m2h

endfunction now
!=======================================================================



pure elemental integer function weekday(self)
!=======================================================================
!
! datetime-bound method to calculate day of the week using
! Zeller's congruence. Returns an integer scalar in the range of [0-6], 
! starting from Sunday.
!
!=======================================================================

  ! ARGUMENTS:
  class(datetime),intent(in) :: self

  integer :: year,month
  integer :: j,k

  year  = self % year
  month = self % month

  if(month <= 2)then
    month = month+12
    year  = year-1
  endif

  j = year/100
  k = mod(year,100)

  weekday = mod(self % day+((month+1)*26)/10+k+k/4+j/4+5*j,7)-1

  if(weekday < 0)weekday = 6

endfunction weekday
!=======================================================================



pure elemental character(len=9) function weekdayLong(self)
!=======================================================================
!
! datetime-bound procedure. Returns the name of the day
! of the week.
!
!=======================================================================

  ! ARGUMENTS:
  class(datetime),intent(in) :: self

  character(len=9),parameter,dimension(7) :: &
  days = ['Sunday   ','Monday   ','Tuesday  ','Wednesday',&
          'Thursday ','Friday   ','Saturday ']

  weekdayLong = days(self % weekday()+1)

endfunction weekdayLong
!=======================================================================



pure elemental character(len=3) function weekdayShort(self)
!=======================================================================
!
! datetime-bound procedure. Returns a 3-character 
! representation of the name of the day of the week.
!
!=======================================================================

  ! ARGUMENTS:
  class(datetime),intent(in) :: self

  character(len=3),parameter,dimension(7) :: &
                   days = ['Sun','Mon','Tue','Wed','Thu','Fri','Sat']

  weekdayShort = days(self % weekday()+1)

endfunction weekdayShort
!=======================================================================



function isocalendar(self)
!=======================================================================
!
! datetime-bound procedure. Returns an array of 3 integers,
! year, week number, and week day, as defined by ISO 8601 week date.
! Essentially a wrapper around C strftime() function.
!
!=======================================================================

  ! ARGUMENTS:
  class(datetime),intent(in) :: self

  integer,dimension(3) :: isocalendar
  integer              :: year,week,wday
  integer              :: rc
  character(len=20)    :: string

  rc = c_strftime(string,len(string),'%G %V %u'//c_null_char,&
                  self % tm())  

  read(unit=string(1:4),fmt='(I4)')year
  read(unit=string(6:7),fmt='(I2)')week
  read(unit=string(9:9),fmt='(I1)')wday

  isocalendar = [year,week,wday]

endfunction isocalendar
!=======================================================================



integer function secondsSinceEpoch(self)
!=======================================================================
!
! datetime-bound procedure. Returns an integer number of 
! seconds since the UNIX Epoch, 1970-01-01 00:00:00.
! Note that this is a wrapper around C's strftime('%s'), so the number
! of seconds will reflect the time zone of the local machine on which
! the function is being called. 
!
!=======================================================================

  ! ARGUMENTS:
  class(datetime),intent(in) :: self

  character(len=11) :: string

  string = self % strftime('%s')
  read(unit=string,fmt='(I10)')secondsSinceEpoch

endfunction secondsSinceEpoch
!=======================================================================



function strftime(self,format)
!=======================================================================
!
! datetime-bound procedure that provides a wrapper around C/C++
! strftime function. 
!
!=======================================================================

  ! ARGUMENTS:
  class(datetime), intent(in)  :: self
  character(len=*),intent(in)  :: format

  character(len=:),allocatable :: strftime

  integer                  :: n,rc
  character(len=MAXSTRLEN) :: resultString

  resultString = ""
  rc = c_strftime(resultString,MAXSTRLEN,trim(format)//c_null_char,&
                  self % tm())
  strftime = trim(resultString)
  n = len(strftime)
  strftime = strftime(1:n-1)

endfunction strftime
!=======================================================================



pure elemental type(tm_struct) function tm(self)
!=======================================================================
!
! datetime-bound procedure. Returns a respective tm_struct 
! instance.
!
!=======================================================================

  ! ARGUMENTS:
  class(datetime),intent(in) :: self

  tm % tm_sec   = self % second
  tm % tm_min   = self % minute
  tm % tm_hour  = self % hour
  tm % tm_mday  = self % day
  tm % tm_mon   = self % month-1
  tm % tm_year  = self % year-1900
  tm % tm_wday  = self % weekday()
  tm % tm_yday  = self % yearday()-1
  tm % tm_isdst = -1

endfunction tm
!=======================================================================



pure elemental character(len=5) function tzOffset(self)
!=======================================================================
!
! Returns a character string with timezone offset in hours from UTC,
! in format +/-[hh][mm].
!
!=======================================================================

  ! ARGUMENTS:
  class(datetime),intent(in) :: self

  integer :: hours,minutes

  if(self % tz < 0)then
    tzOffset(1:1) = '-'
  else
    tzOffset(1:1) = '+'
  endif

  hours   = int(abs(self % tz))
  minutes = nint((abs(self % tz)-hours)*60)

  if(minutes == 60)then
    minutes = 0
    hours = hours+1
  endif

  write(unit=tzOffset(2:5),fmt='(2I2.2)')hours,minutes

endfunction tzOffset
!=======================================================================



pure elemental type(datetime) function utc(self)
!=======================================================================
!
! Returns the datetime instance at Coordinated Universal Time (UTC). 
!
!=======================================================================

  ! ARGUMENTS:
  class(datetime),intent(in) :: self

  integer :: hours,minutes,sgn

  hours   = int(abs(self % tz))
  minutes = nint((abs(self % tz)-hours)*60)
  sgn     = int(sign(one,self % tz))

  utc      = self-timedelta(hours=sgn*hours,minutes=sgn*minutes)
  utc % tz = 0

endfunction utc
!=======================================================================



pure elemental integer function yearday(self)
!=======================================================================
!
! datetime-bound procedure. Returns integer day of the
! year (ordinal date).
!
!=======================================================================

  ! ARGUMENTS:
  class(datetime),intent(in) :: self

  integer :: month

  yearday = 0
  do month=1,self % month-1
    yearday = yearday+daysInMonth(month,self % year)
  enddo
  yearday = yearday+self % day

endfunction yearday
!=======================================================================



!::: Datetime operators ::::::::::::::::::::::::::::::::::::::::::::::::


pure elemental function datetime_plus_timedelta(d0,t) result(d)
!=======================================================================
!
! Adds a timedelta instance to a datetime instance.
! Returns a new datetime instance. Overloads the operator +.
!
!=======================================================================

  class(datetime),intent(in) :: d0
  class(timedelta),intent(in) :: t
  type(datetime)             :: d

  integer :: milliseconds,seconds,minutes,hours,days

  ! initialize:
  d = d0

  milliseconds = t % getMilliseconds()
  seconds      = t % getSeconds()
  minutes      = t % getMinutes()
  hours        = t % getHours()
  days         = t % getDays()

  if(milliseconds /= 0)call d % addMilliseconds(milliseconds)
  if(seconds      /= 0)call d % addSeconds(seconds)
  if(minutes      /= 0)call d % addMinutes(minutes)
  if(hours        /= 0)call d % addHours(hours)
  if(days         /= 0)call d % addDays(days)

endfunction datetime_plus_timedelta
!=======================================================================


pure elemental function timedelta_plus_datetime(t,d0) result(d)
!=======================================================================
!
! Adds a timedelta instance to a datetime instance.
! Returns a new datetime instance. Overloads the operator +.
!
!=======================================================================

  ! ARGUMENTS:
  class(timedelta),intent(in) :: t
  class(datetime), intent(in) :: d0
  type(datetime)             :: d

  d = d0 + t

endfunction timedelta_plus_datetime
!=======================================================================



pure elemental function datetime_minus_timedelta(d0,t) result(d)
!=======================================================================
!
! Subtracts a timedelta instance from a datetime instance.
! Returns a new datetime instance. Overloads the operator -.
!
!=======================================================================

  ! ARGUMENTS:
  class(datetime), intent(in) :: d0
  type(timedelta),intent(in) :: t
  type(datetime)             :: d

  d = d0 + (-t)

  ! Initialize:
  !d = d0

  !if(t % milliseconds /= 0)call d % addMilliseconds(-t % milliseconds)
  !if(t % seconds      /= 0)call d % addSeconds(-t % seconds)
  !if(t % minutes      /= 0)call d % addMinutes(-t % minutes)
  !if(t % hours        /= 0)call d % addHours(-t % hours)
  !if(t % days         /= 0)call d % addDays(-t % days)

endfunction datetime_minus_timedelta
!=======================================================================




pure elemental function datetime_minus_datetime(d0,d1) result(t)
!=======================================================================
!
! Subtracts a datetime instance from another datetime 
! instance. Returns a timedelta instance. Overloads the operator -.
!
!=======================================================================

  class(datetime),intent(in) :: d0
  type(datetime),intent(in) :: d1
  type(timedelta)           :: t

  real(kind=real64) :: daysDiff
  integer :: days,hours,minutes,seconds,milliseconds
  integer :: sign_

  daysDiff = date2num(d0)-date2num(d1)

  if(daysDiff < 0)then
    sign_ = -1
    daysDiff = ABS(daysDiff)
  else
    sign_ = 1
  endif

  days         = int(daysDiff)
  hours        = int((daysDiff-days)*d2h)
  minutes      = int((daysDiff-days-hours*h2d)*d2m)
  seconds      = int((daysDiff-days-hours*h2d-minutes*m2d)*d2s)
  milliseconds = nint((daysDiff-days-hours*h2d-minutes*m2d&
                               -seconds*s2d)*d2s*1e3_real64)

  t = timedelta(sign_*days,sign_*hours,sign_*minutes,sign_*seconds,&
                sign_*milliseconds)

endfunction datetime_minus_datetime
!=======================================================================




pure elemental logical function gt(d0,d1)
!=======================================================================
!
! datetime object comparison operator. Returns .true. if
! d0 is greater than d1, and .false. otherwise. Overloads the 
! operator >.
!
!=======================================================================

  ! ARGUMENTS:
  class(datetime),intent(in) :: d0
  type(datetime), intent(in) :: d1

  type(datetime) :: d0_utc,d1_utc

  ! Convert to UTC before making comparison
  d0_utc = d0 % utc()
  d1_utc = d1 % utc()

  ! Year comparison block
  if(d0_utc % year > d1_utc % year)then
    gt = .true.
  elseif(d0_utc % year < d1_utc % year)then
    gt = .false.
  else

    ! Month comparison block
    if(d0_utc % month > d1_utc % month)then
      gt = .true.
    elseif(d0_utc % month < d1_utc % month)then
      gt = .false.
    else

      ! Day comparison block
      if(d0_utc % day > d1_utc % day)then
        gt = .true.
      elseif(d0_utc % day < d1_utc % day)then
        gt = .false.
      else

        ! Hour comparison block
        if(d0_utc % hour > d1_utc % hour)then
          gt = .true.
        elseif(d0_utc % hour < d1_utc % hour)then
          gt = .false.
        else

          ! Minute comparison block
          if(d0_utc % minute > d1_utc % minute)then
            gt = .true.
          elseif(d0_utc % minute < d1_utc % minute)then
            gt = .false.
          else

            ! Second comparison block
            if(d0_utc % second > d1_utc % second)then
              gt = .true.
            elseif(d0_utc % second < d1_utc % second)then
              gt = .false.
            else

              ! Millisecond comparison block
              if(d0_utc % millisecond > d1_utc % millisecond)then
                gt = .true.
              else
                gt = .false.
              endif

            endif
          endif
        endif
      endif
    endif
  endif

endfunction gt
!=======================================================================



pure elemental logical function lt(d0,d1)
!=======================================================================
!
! datetime object comparison operator. Returns .true. if
! d0 is less than d1, and .false. otherwise. Overloads the operator <.
!
!=======================================================================

  class(datetime),intent(in) :: d0
  type(datetime), intent(in) :: d1

  lt = d1 > d0

endfunction lt
!=======================================================================



pure elemental logical function eq(d0,d1)
!=======================================================================
!
! datetime object comparison operator. Returns .true. if
! d0 is equal to d1, and .false. otherwise. Overloads the operator ==.
!
!=======================================================================

  class(datetime),intent(in) :: d0
  type(datetime), intent(in) :: d1

  type(datetime) :: d0_utc,d1_utc

  ! Convert to UTC before making comparison
  d0_utc = d0 % utc()
  d1_utc = d1 % utc()

  eq = d0_utc % year        == d1_utc % year   .and. &
       d0_utc % month       == d1_utc % month  .and. &
       d0_utc % day         == d1_utc % day    .and. &
       d0_utc % hour        == d1_utc % hour   .and. &
       d0_utc % minute      == d1_utc % minute .and. &
       d0_utc % second      == d1_utc % second .and. &
       d0_utc % millisecond == d1_utc % millisecond

endfunction eq
!=======================================================================



pure elemental logical function neq(d0,d1)
!=======================================================================
!
! datetime object comparison operator. Returns .true. if d0 is not equal 
! to d1, and .false. otherwise. Overloads the operator /=.
!
!=======================================================================

  class(datetime),intent(in) :: d0
  type(datetime), intent(in) :: d1

  neq = .not. d0 == d1

endfunction neq
!=======================================================================



pure elemental logical function ge(d0,d1)
!=======================================================================
!
! datetime object comparison operator. Returns .true. if
! d0 is greater or equal than d1, and .false. otherwise. Overloads the 
! operator >=.
!
!=======================================================================

  class(datetime),intent(in) :: d0
  type(datetime), intent(in) :: d1

  ge = d0 > d1 .or. d0 == d1

endfunction ge
!=======================================================================



pure elemental logical function le(d0,d1)
!=======================================================================
!
! datetime object comparison operator. Returns .true. if
! d0 is less or equal than d1, and .false. otherwise. Overloads the 
! operator <=.
!
!=======================================================================

  class(datetime),intent(in) :: d0
  type(datetime), intent(in) :: d1

  le = d1 > d0 .or. d0 == d1

endfunction le
!=======================================================================



!--- public procedures -------------------------------------------------

pure elemental logical function isLeapYear(year)
!=======================================================================
!
! Given an integer year, returns .true. if year is leap
! year, and .false. otherwise.
!
!=======================================================================

  ! ARGUMENTS:
  integer,intent(in) :: year

  isLeapYear = (mod(year,4) == 0 .and. .not. mod(year,100) == 0)&
          .or. (mod(year,400) == 0)

endfunction isLeapYear
!=======================================================================



pure function datetimeRange(d0,d1,t)
!=======================================================================
!
! Given start and end datetime instances d0 and d1, and time increment
! as timedelta instance t, returns an array of datetime instances.
! The number of elements is the number of whole time increments 
! contained between datetimes d0 and d1.
!
!=======================================================================

  ! ARGUMENTS:
  type(datetime), intent(in) :: d0
  type(datetime), intent(in) :: d1
  type(timedelta),intent(in) :: t

  real(kind=real64) :: datenum0,datenum1,increment
  real(kind=real64) :: eps

  type(datetime),dimension(:),allocatable :: datetimeRange

  integer :: n,nm

  eps = 1e-10_real64

  datenum0 = date2num(d0)
  datenum1 = date2num(d1)

  increment = t % total_seconds() * s2d

  nm = floor((datenum1-datenum0+eps)/increment)+1

  allocate(datetimeRange(nm))

  do n = 1,nm
    datetimeRange(n) = num2date(datenum0 + (n-1)*increment)
  enddo

endfunction datetimeRange
!=======================================================================



pure elemental integer function daysInMonth(month,year)
!=======================================================================
!
! Given integer month and year, returns an integer number
! of days in that particular month.
!
!=======================================================================

  ! ARGUMENTS:
  integer,intent(in) :: month
  integer,intent(in) :: year

  integer,parameter,dimension(12) :: &
          days = [31,28,31,30,31,30,31,31,30,31,30,31]

  if(month < 1 .or. month > 12)then
    ! Should raise an error and abort here, however we want to keep
    ! the pure and elemental attributes. Make sure this function is 
    ! called with the month argument in range. 
    daysInMonth = 0
    return
  endif

  if(month == 2 .and. isLeapYear(year))then
    daysInMonth = 29
  else
    daysInMonth = days(month)
  endif

endfunction daysInMonth
!=======================================================================



pure elemental integer function daysInYear(year)
!=======================================================================
!
! Given an integer year, returns an integer number of days
! in that year.
!
!=======================================================================

  ! ARGUMENTS:
  integer,intent(in) :: year

  if(isLeapYear(year))then
    daysInYear = 366
  else
    daysInYear = 365
  endif

endfunction daysInYear
!=======================================================================



pure elemental real(kind=real64) function date2num(d)
!=======================================================================
!
! Given a datetime instance d, returns number of days since 
! 0001-01-01 00:00:00.
!
! Since version 1.0.5, this function is timezone aware, i.e. we first
! switch to UTC time, then we evaluate the number of days. This may
! affect some of the existing programs using this function.
!
!=======================================================================

  ! ARGUMENTS:
  type(datetime),intent(in) :: d

  type(datetime) :: d_utc
  integer :: year

  ! Convert to UTC first
  d_utc = d % utc()

  ! d_utc % year must be positive:
  if(d_utc % year < 1)then
    date2num = 0
    return
  endif

  date2num = 0
  do year = 1,d_utc % year-1
    date2num = date2num + daysInYear(year)
  enddo

  date2num = date2num          &
           + d_utc % yearday() &
           + d_utc % hour*h2d  &
           + d_utc % minute*m2d&
           + (d_utc % second+1e-3_real64*d_utc % millisecond)*s2d
 
endfunction date2num
!=======================================================================



pure elemental type(datetime) function num2date(num)
!=======================================================================
!
! Given number of days since 0001-01-01 00:00:00, returns a 
! correspoding datetime instance.
!
!=======================================================================

  ! ARGUMENTS:
  real(kind=real64),intent(in) :: num

  integer :: year,month,day,hour,minute,second,millisecond
  real(kind=real64) :: days,totseconds

  ! num must be positive:
  if(num < 0)then
    num2date = datetime(1)
    return
  endif

  days = num

  year = 1
  do
    if(int(days) <= daysInYear(year))exit
    days = days-daysInYear(year)
    year = year+1
  enddo

  month = 1
  do
    if(inT(days) <= daysInMonth(month,year))exit
    days = days-daysInMonth(month,year)
    month = month+1
  enddo

  day         = int(days)
  totseconds  = (days-day)*d2s
  hour        = int(totseconds*s2h)
  minute      = int((totseconds-hour*h2s)*s2m)
  second      = int(totseconds-hour*h2s-minute*m2s)
  millisecond = nint((totseconds-int(totseconds))*1e3_real64)

  num2date = datetime(year,month,day,hour,minute,second,millisecond,tz=zero)

  ! Handle a special case caused by floating-point arithmethic:
  if(num2date % millisecond == 1000)then
    num2date % millisecond = 0
    call num2date % addSeconds(1)
  endif

  if(num2date % second == 60)then
    num2date % second = 0
    call num2date % addMinutes(1)
  endif
  if(num2date % minute == 60)then
    num2date % minute = 0
    call num2date % addHours(1)
  endif
  if(num2date % hour == 60)then
    num2date % hour = 0
    call num2date % addDays(1)
  endif

endfunction num2date
!=======================================================================



type(datetime) function strptime(str,format)
!=======================================================================
!
! A wrapper function around C/C++ strptime. 
! Returns a datetime instance. 
!
!=======================================================================

  ! ARGUMENTS:
  character(len=*),intent(in) :: str
  character(len=*),intent(in) :: format

  integer         :: rc
  type(tm_struct) :: tm

  rc = c_strptime(trim(str)//c_null_char,trim(format)//c_null_char,tm)
  strptime = tm2date(tm)

endfunction strptime
!=======================================================================



pure elemental type(datetime) function tm2date(ctime)
!=======================================================================
!
! Given a tm_struct instance, returns a corresponding datetime instance.
!
!=======================================================================

  ! ARGUMENTS:
  type(tm_struct),intent(in) :: ctime

  tm2date % millisecond = 0
  tm2date % second      = ctime % tm_sec
  tm2date % minute      = ctime % tm_min
  tm2date % hour        = ctime % tm_hour
  tm2date % day         = ctime % tm_mday
  tm2date % month       = ctime % tm_mon+1
  tm2date % year        = ctime % tm_year+1900
  tm2date % tz          = 0

endfunction tm2date
!=======================================================================



!--- private procedures ------------------------------------------------

pure function int2str(i,length)
!=======================================================================
!
! Converts an integer i into a character string of requested length, 
! pre-pending zeros if necessary.
!
!=======================================================================

  ! ARGUMENTS:
  integer,intent(in) :: i
  integer,intent(in) :: length

  character(len=length) :: int2str
  character(len=2)      :: string

  write(unit=string,fmt='(I2)')length
  write(unit=int2str,fmt='(I'//string//'.'//string//')')i

endfunction int2str
!=======================================================================
endmodule mod_datetime
