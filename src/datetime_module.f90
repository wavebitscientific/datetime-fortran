module datetime_module

  use, intrinsic :: iso_fortran_env, only: real32, real64, stderr => error_unit
  use, intrinsic :: iso_c_binding, only: c_char, c_int, c_null_char

  implicit none

  private

  public :: datetime, timedelta, clock
  public :: date2num
  public :: datetimeRange
  public :: daysInMonth
  public :: daysInYear
  public :: isLeapYear
  public :: num2date
  public :: strptime
  public :: tm2date
  public :: tm_struct
  public :: c_strftime
  public :: c_strptime

  real(real64), parameter :: zero = 0._real64, one = 1._real64

  ! Constant multipliers to transform a number of some time unit to another
  real(real64), parameter :: d2h = 24._real64 ! day -> hour
  real(real64), parameter :: h2d = one / d2h ! hour -> day
  real(real64), parameter :: d2m = d2h * 60._real64 ! day -> minute
  real(real64), parameter :: m2d = one / d2m ! minute -> day
  real(real64), parameter :: m2h = one / 60 ! minute -> hour
  real(real64), parameter :: s2d = m2d / 60 ! second -> day
  real(real64), parameter :: d2s = 86400._real64 ! day -> second
  real(real64), parameter :: h2s = 3600._real64 ! hour -> second
  real(real64), parameter :: s2h = one / h2s ! second -> hour
  real(real64), parameter :: m2s = 60._real64 ! minute -> second
  real(real64), parameter :: s2m = one / m2s ! second -> minute

  integer, parameter :: MAXSTRLEN = 99 ! maximum string length for strftime

  type :: datetime

    private

    integer :: year = 1 ! year [1-HUGE(year)]
    integer :: month = 1 ! month in year [1-12]
    integer :: day = 1 ! day in month [1-31]
    integer :: hour = 0 ! hour in day [0-23]
    integer :: minute = 0 ! minute in hour [0-59]
    integer :: second = 0 ! second in minute [0-59]
    integer :: millisecond = 0 ! milliseconds in second [0-999]
    real(real64) :: tz = 0 ! timezone offset from UTC [hours]

  contains

    ! getter functions
    procedure, pass(self), public :: getYear
    procedure, pass(self), public :: getMonth
    procedure, pass(self), public :: getDay
    procedure, pass(self), public :: getHour
    procedure, pass(self), public :: getMinute
    procedure, pass(self), public :: getSecond
    procedure, pass(self), public :: getMillisecond
    procedure, pass(self), public :: getTz

    ! public methods
    procedure, pass(self), public :: isocalendar
    procedure, pass(self), public :: isoformat
    procedure, pass(self), public :: isValid
    procedure, nopass,     public :: now
    procedure, pass(self), public :: secondsSinceEpoch
    procedure, pass(self), public :: strftime
    procedure, pass(self), public :: tm
    procedure, pass(self), public :: tzOffset
    procedure, pass(self), public :: isoweekday
    procedure, pass(self), public :: isoweekdayLong
    procedure, pass(self), public :: isoweekdayShort
    procedure, pass(self), public :: utc
    procedure, pass(self), public :: weekday
    procedure, pass(self), public :: weekdayLong
    procedure, pass(self), public :: weekdayShort
    procedure, pass(self), public :: yearday

    ! private methods
    procedure, pass(self), private :: addMilliseconds
    procedure, pass(self), private :: addSeconds
    procedure, pass(self), private :: addMinutes
    procedure, pass(self), private :: addHours
    procedure, pass(self), private :: addDays

    ! operator overloading procedures
    procedure, pass(d0), private :: datetime_plus_timedelta
    procedure, pass(d0), private :: timedelta_plus_datetime
    procedure, pass(d0), private :: datetime_minus_datetime
    procedure, pass(d0), private :: datetime_minus_timedelta
    procedure, pass(d0), private :: datetime_eq
    procedure, pass(d0), private :: datetime_neq
    procedure, pass(d0), private :: datetime_gt
    procedure, pass(d0), private :: datetime_ge
    procedure, pass(d0), private :: datetime_lt
    procedure, pass(d0), private :: datetime_le

    generic :: operator(+)  => datetime_plus_timedelta, &
                               timedelta_plus_datetime
    generic :: operator(-)  => datetime_minus_datetime, &
                               datetime_minus_timedelta
    generic :: operator(==) => datetime_eq
    generic :: operator(/=) => datetime_neq
    generic :: operator(>)  => datetime_gt
    generic :: operator(>=) => datetime_ge
    generic :: operator(<)  => datetime_lt
    generic :: operator(<=) => datetime_le

  end type datetime

  interface datetime
    module procedure :: datetime_constructor
  endinterface datetime

  type :: timedelta
    private

    integer :: days = 0
    integer :: hours = 0
    integer :: minutes = 0
    integer :: seconds = 0
    integer :: milliseconds = 0

  contains

    procedure, pass(self), public :: getDays
    procedure, pass(self), public :: getHours
    procedure, pass(self), public :: getMinutes
    procedure, pass(self), public :: getSeconds
    procedure, pass(self), public :: getMilliseconds

    procedure, public :: total_seconds

    procedure, private :: timedelta_plus_timedelta
    procedure, private :: timedelta_minus_timedelta
    procedure, private :: unary_minus_timedelta
    procedure, private :: timedelta_eq
    procedure, private :: timedelta_neq
    procedure, private :: timedelta_gt
    procedure, private :: timedelta_ge
    procedure, private :: timedelta_lt
    procedure, private :: timedelta_le

    generic :: operator(+)  => timedelta_plus_timedelta
    generic :: operator(-)  => timedelta_minus_timedelta, unary_minus_timedelta
    generic :: operator(==) => timedelta_eq
    generic :: operator(/=) => timedelta_neq
    generic :: operator(>)  => timedelta_gt
    generic :: operator(>=) => timedelta_ge
    generic :: operator(<)  => timedelta_lt
    generic :: operator(<=) => timedelta_le

  end type timedelta

  interface timedelta
    module procedure :: timedelta_constructor
  endinterface timedelta

  type,bind(c) :: tm_struct
    ! Derived type for compatibility with C and C++ struct tm.
    ! Enables calling strftime and strptime using iso_c_binding.
    ! See http://www.cplusplus.com/reference/ctime/tm for reference.
    integer(c_int) :: tm_sec = 0 ! Seconds [0-60] (1 leap second)
    integer(c_int) :: tm_min = 0 ! Minutes [0-59]
    integer(c_int) :: tm_hour = 0 ! Hours [0-23]
    integer(c_int) :: tm_mday = 0 ! Day [1-31]
    integer(c_int) :: tm_mon = 0 ! Month [0-11]
    integer(c_int) :: tm_year = 0 ! Year - 1900
    integer(c_int) :: tm_wday = 0 ! Day of week [0-6]
    integer(c_int) :: tm_yday = 0 ! Days in year [0-365]
    integer(c_int) :: tm_isdst = 0 ! DST [-1/0/1]
  end type tm_struct

  interface

    function c_strftime(str, slen, format, tm) bind(c, name='strftime') result(rc)
      ! Returns a formatted time string, given input time struct and format.
      ! See https://www.cplusplus.com/reference/ctime/strftime for reference.
      import :: c_char, c_int, tm_struct
      character(kind=c_char), intent(out) :: str(*) ! result string
      integer(c_int), value, intent(in) :: slen ! string length
      character(kind=c_char), intent(in) :: format(*) ! time format
      type(tm_struct), intent(in) :: tm ! tm_struct instance
      integer(c_int) :: rc ! return code
    end function c_strftime

    function c_strptime(str,format,tm) bind(c,name='strptime') result(rc)
      ! Interface to POSIX strptime.
      ! Returns a time struct object based on the input time string str and format.
      ! See http://man7.org/linux/man-pages/man3/strptime.3.html for reference.
      import :: c_char, c_int, tm_struct
      character(kind=c_char), intent(in) :: str(*) ! input string
      character(kind=c_char), intent(in) :: format(*) ! time format
      type(tm_struct), intent(out) :: tm ! result tm_struct
      integer(c_int) :: rc ! return code
    end function c_strptime

  end interface


  type :: clock
    type(datetime) :: startTime
    type(datetime) :: stopTime
    type(datetime) :: currentTime
    type(timedelta) :: tickInterval
    logical :: alarm = .false.
    logical :: started = .false.
    logical :: stopped = .false.
  contains
    procedure :: reset
    procedure :: tick
  end type clock

contains


  pure elemental subroutine reset(self)
    ! Resets the clock to its start time.
    class(clock), intent(in out) :: self
    self % currentTime = self % startTime
    self % started = .false.
    self % stopped = .false.
  end subroutine reset


  pure elemental subroutine tick(self)
    ! Increments the currentTime of the clock instance by one tickInterval.
    class(clock), intent(in out) :: self
    if (self % stopped) return
    if (.not. self % started) then
      self % started = .true.
      self % currentTime = self % startTime
    end if
    self % currentTime = self % currentTime + self % tickInterval
    if (self % currentTime >= self % stopTime) self % stopped = .true.
  end subroutine tick


  pure elemental type(datetime) function datetime_constructor( &
    year, month, day, hour, minute, second, millisecond, tz)
    ! Constructor function for the `datetime` class.
    integer, intent(in), optional :: year, month, day, hour, minute, second, millisecond
    real(real64), intent(in), optional :: tz ! timezone offset in hours

    datetime_constructor % year = 1
    if (present(year)) datetime_constructor % year = year

    datetime_constructor % month = 1
    if (present(month)) datetime_constructor % month = month

    datetime_constructor % day = 1
    if (present(day)) datetime_constructor % day = day

    datetime_constructor % hour = 0
    if (present(hour)) datetime_constructor % hour = hour

    datetime_constructor % minute = 0
    if (present(minute)) datetime_constructor % minute = minute

    datetime_constructor % second = 0
    if (present(second)) datetime_constructor % second = second

    datetime_constructor % millisecond = 0
    if (present(millisecond)) datetime_constructor % millisecond = millisecond

    datetime_constructor % tz = 0
    if (present(tz)) datetime_constructor % tz = tz

  end function datetime_constructor


  pure elemental integer function getYear(self)
    ! Returns the year component
    class(datetime), intent(in) :: self
    getYear = self % year
  end function getYear


  pure elemental integer function getMonth(self)
    ! Returns the year component
    class(datetime), intent(in) :: self
    getMonth = self % month
  end function getMonth


  pure elemental integer function getDay(self)
    ! Returns the year component
    class(datetime), intent(in) :: self
    getDay = self % day
  end function getDay


  pure elemental integer function getHour(self)
    ! Returns the year component
    class(datetime), intent(in) :: self
    getHour = self % hour
  end function getHour


  pure elemental integer function getMinute(self)
    ! Returns the year component
    class(datetime), intent(in) :: self
    getMinute = self % minute
  end function getMinute


  pure elemental integer function getSecond(self)
    ! Returns the year component
    class(datetime), intent(in) :: self
    getSecond = self % second
  end function getSecond


  pure elemental integer function getMillisecond(self)
    ! Returns the year component
    class(datetime), intent(in) :: self
    getMillisecond = self % millisecond
  end function getMillisecond


  pure elemental real(real64) function getTz(self)
    ! Returns the timezone offset component
    class(datetime), intent(in) :: self
    getTz = self % tz
  end function getTz


  pure elemental subroutine addMilliseconds(self, ms)
    ! Adds an integer number of milliseconds to self. Called by `datetime`
    ! addition (`+`) and subtraction (`-`) operators.
    class(datetime), intent(in out) :: self
    integer, intent(in) :: ms
    self % millisecond = self % millisecond + ms
    do
      if (self % millisecond >= 1000) then
        call self % addSeconds(self % millisecond / 1000)
        self % millisecond = mod(self % millisecond, 1000)
      else if (self % millisecond < 0) then
        call self % addSeconds(self % millisecond / 1000 - 1)
        self % millisecond = mod(self % millisecond, 1000) + 1000
      else
        exit
      end if
    end do
  end subroutine addMilliseconds


  pure elemental subroutine addSeconds(self, s)
    ! Adds an integer number of seconds to self. Called by `datetime`
    ! addition (`+`) and subtraction (`-`) operators.
    class(datetime), intent(in out) :: self
    integer, intent(in) :: s
    self % second = self % second + s
    do
      if (self % second >= 60) then
        call self % addMinutes(self % second / 60)
        self % second = mod(self % second, 60)
      else if (self % second < 0) then
        call self % addMinutes(self % second / 60 - 1)
        self % second = mod(self % second, 60) + 60
      else
        exit
      end if
    end do
  end subroutine addSeconds


  pure elemental subroutine addMinutes(self,m)
    ! Adds an integer number of minutes to self. Called by `datetime`
    ! addition (`+`) and subtraction (`-`) operators.
    class(datetime), intent(in out) :: self
    integer, intent(in) :: m
    self % minute = self % minute + m
    do
      if (self % minute >= 60) then
        call self % addHours(self % minute / 60)
        self % minute = mod(self % minute, 60)
      else if (self % minute < 0) then
        call self % addHours(self % minute / 60 - 1)
        self % minute = mod(self % minute, 60) + 60
      else
        exit
      end if
    end do
  end subroutine addMinutes


  pure elemental subroutine addHours(self,h)
    ! Adds an integer number of hours to self. Called by `datetime`
    ! addition (`+`) and subtraction (`-`) operators.
    class(datetime), intent(in out) :: self
    integer, intent(in) :: h
    self % hour = self % hour + h
    do
      if (self % hour >= 24) then
        call self % addDays(self % hour / 24)
        self % hour = mod(self % hour, 24)
      else if (self % hour < 0) then
        call self % addDays(self % hour / 24 - 1)
        self % hour = mod(self % hour, 24) + 24
      else
        exit
      end if
    end do
  end subroutine addHours


  pure elemental subroutine addDays(self, d)
    ! Adds an integer number of dayss to self. Called by `datetime`
    ! addition (`+`) and subtraction (`-`) operators.
    class(datetime), intent(in out) :: self
    integer, intent(in) :: d
    integer :: daysInCurrentMonth
    self % day = self % day + d
    do
      daysInCurrentMonth = daysInMonth(self % month, self % year)
      if (self % day > daysInCurrentMonth) then
        self % day = self % day - daysInCurrentMonth
        self % month = self % month+1
        if (self % month > 12) then
          self % year = self % year + self % month/12
          self % month = mod(self % month, 12)
        end if
      else if (self % day < 1) then
        self % month = self % month-1
        if (self % month < 1) then
          self % year = self % year + self % month / 12 - 1
          self % month = 12 + mod(self % month, 12)
        end if
        self % day = self % day + daysInMonth(self % month, self % year)
      else
        exit
      end if
    end do
  end subroutine addDays


  pure elemental character(23) function isoformat(self,sep)
    ! Returns character string with time in ISO 8601 format.
    class(datetime), intent(in) :: self
    character, intent(in), optional :: sep
    character :: separator

    separator = 'T'
    if (present(sep)) separator = sep

    ! TODO below is a bit cumbersome and was implemented
    ! at a time before the interface to strftime. Now we
    ! could do something like:
    !
    ! isoformat = self % strftime('%Y-%m-%d'//separator//'%H:%M:%S')
    !
    isoformat = int2str(self % year,       4)//'-'//      &
                int2str(self % month,      2)//'-'//      &
                int2str(self % day,        2)//separator//&
                int2str(self % hour,       2)//':'//      &
                int2str(self % minute,     2)//':'//      &
                int2str(self % second,     2)//'.'//      &
                int2str(self % millisecond,3)

  end function isoformat


  pure elemental logical function isValid(self)
    ! Checks whether the `datetime` instance has valid component values.
    ! Returns `.true.` if the `datetime` instance is valid, and `.false.`
    ! otherwise.
    class(datetime), intent(in) :: self

    ! assume valid
    isValid = .true.

    if (self % year < 1) then
      isValid = .false.
      return
    end if

    if (self % month < 1 .or. self % month > 12) then
      isValid = .false.
      return
    end if

    if (self % day < 1 .or. &
       self % day > daysInMonth(self % month,self % year)) then
      isValid = .false.
      return
    end if

    if (self % hour < 0 .or. self % hour > 23) then
      isValid = .false.
      return
    end if

    if (self % minute < 0 .or. self % minute > 59) then
      isValid = .false.
      return
    end if

    if (self % second < 0 .or. self % second > 59) then
      isValid = .false.
      return
    end if

    if (self % millisecond < 0 .or. self % millisecond > 999) then
      isValid = .false.
      return
    end if

  end function isValid


  type(datetime) function now()
    ! Returns a `datetime` instance with current time.
    character(5) :: zone
    integer :: values(8)
    integer :: hour, minute

    ! Obtain local machine time zone information
    call date_and_time(zone=zone, values=values)

    read(zone(1:3), '(i3)') hour
    read(zone(4:5), '(i2)') minute

    now = datetime(year = values(1), month = values(2), day = values(3), &
                   hour = values(5), minute = values(6), second = values(7), &
                   millisecond = values(8))

    now % tz = hour + minute * m2h

  end function now


  pure elemental integer function weekday(self)
    ! Returns the day of the week calculated using Zeller's congruence.
    ! Returned value is an integer scalar in the range [0-6], such that:
    !
    ! 0: Sunday
    ! 1: Monday
    ! 2: Tuesday
    ! 3: Wednesday
    ! 4: Thursday
    ! 5: Friday
    ! 6: Saturday
    class(datetime), intent(in) :: self
    integer :: year, month, j, k

    year  = self % year
    month = self % month

    if (month <= 2) then
      month = month + 12
      year  = year - 1
    end if

    j = year / 100
    k = mod(year, 100)

    weekday = mod(self % day + ((month + 1) * 26) / 10 + k + k / 4 + j / 4 + 5 * j, 7) -1

    if (weekday < 0) weekday = 6

  end function weekday


  pure elemental integer function isoweekday(self)
    ! Returns the day of the week per ISO 8601 returned from weekday().
    ! Returned value is an integer scalar in the range [1-7].
    class(datetime), intent(in) :: self
    isoweekday = self % weekday()
    if (isoweekday == 0) isoweekday = 7
  end function isoweekday


  pure elemental character(9) function weekdayLong(self)
    ! Returns the full name of the day of the week.
    class(datetime), intent(in) :: self
    character(9), parameter :: &
      days(*) = ['Sunday   ', 'Monday   ', 'Tuesday  ','Wednesday', &
                 'Thursday ', 'Friday   ', 'Saturday ']
    weekdayLong = days(self % weekday() + 1)
  end function weekdayLong


  pure elemental character(9) function isoweekdayLong(self)
    ! Returns the full name of the day of the week for ISO 8601
    ! ordered weekdays.
    class(datetime), intent(in) :: self
    character(9), parameter :: &
      days(7) = ['Monday   ','Tuesday  ','Wednesday','Thursday ', &
                 'Friday   ','Saturday ','Sunday   ']
    isoweekdayLong = days(self % isoweekday())
  end function isoweekdayLong


  pure elemental character(3) function weekdayShort(self)
    ! Returns the short (3-letter) name of the day of the week.
    class(datetime), intent(in) :: self
    character(3), parameter :: days(7) = ['Sun', 'Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat']
    weekdayShort = days(self % weekday() + 1)
  end function weekdayShort


  pure elemental character(3) function isoweekdayShort(self)
    ! Returns the short (3-letter) name of the day of the week
    ! based on ISO 8601 ordering.
    class(datetime), intent(in) :: self
    character(3), parameter :: days(7) = ['Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat', 'Sun']
    isoweekdayShort = days(self % isoweekday())
  end function isoweekdayShort


  function isocalendar(self)
    ! Returns an array of 3 integers, year, week number, and week day,
    ! as defined by ISO 8601 week date. Essentially a wrapper around C
    ! `strftime` function.
    class(datetime), intent(in) :: self
    integer :: isocalendar(3)
    integer :: year, week, wday, rc
    character(20) :: string

    rc = c_strftime(string, len(string), '%G %V %u' // c_null_char, self % tm())

    read(string(1:4), '(i4)') year
    read(string(6:7), '(i2)') week
    read(string(9:9), '(i1)') wday

    isocalendar = [year, week, wday]

  end function isocalendar


  integer function secondsSinceEpoch(self)
    ! Returns an integer number of seconds since the UNIX Epoch,
    ! `1970-01-01 00:00:00`.
    ! Since Windows does not have strftime('%s'), we implement this
    ! using datetime itself
    class(datetime), intent(in) :: self
    type(timedelta) :: td
    type(datetime) :: dummy

    dummy = datetime(self%year, self%month, self%day, self%hour, self%minute, self%second)

    td = datetime_minus_datetime(dummy, datetime(1970,1,1,0,0,0))
    secondsSinceEpoch = td%total_seconds()

  end function secondsSinceEpoch


  function strftime(self, format)
    ! Wrapper around C and C++ `strftime` function.
    class(datetime), intent(in) :: self
    character(*), intent(in)  :: format
    character(:), allocatable :: strftime
    integer :: n, rc
    character(MAXSTRLEN) :: resultString
    resultString = ""
    rc = c_strftime(resultString, len(resultString), trim(format) // c_null_char, &
                    self % tm())
    if (rc == 0) write(stderr,*) "datetime:strftime failure, format: ", trim(format)
    strftime = resultString(1:len_trim(resultString)-1)  !< strip null
  end function strftime


  pure elemental type(tm_struct) function tm(self)
    ! Returns a `tm_struct` instance of the current `datetime`.
    class(datetime), intent(in) :: self
    tm % tm_sec = self % second
    tm % tm_min = self % minute
    tm % tm_hour = self % hour
    tm % tm_mday = self % day
    tm % tm_mon = self % month - 1
    tm % tm_year = self % year - 1900
    tm % tm_wday = self % weekday()
    tm % tm_yday = self % yearday() - 1
    tm % tm_isdst = -1
  end function tm


  pure elemental character(5) function tzOffset(self)
    ! Returns a character string with timezone offset in hours from UTC,
    ! in format +/-[hh][mm].
    class(datetime), intent(in) :: self
    integer :: hours,minutes

    if (self % tz < 0) then
      tzOffset(1:1) = '-'
    else
      tzOffset(1:1) = '+'
    end if

    hours = int(abs(self % tz))
    minutes = nint((abs(self % tz) - hours) * 60)

    if (minutes == 60) then
      minutes = 0
      hours = hours + 1
    end if

    write(tzOffset(2:5), '(2i2.2)') hours, minutes

  end function tzOffset


  pure elemental type(datetime) function utc(self)
    ! Returns the `datetime` instance at Coordinated Universal Time (UTC).
    class(datetime), intent(in) :: self
    integer :: hours, minutes, sgn
    hours = int(abs(self % tz))
    minutes = nint((abs(self % tz) - hours) * 60)
    sgn = int(sign(one, self % tz))
    utc = self - timedelta(hours=sgn * hours, minutes=sgn * minutes)
    utc % tz = 0
  end function utc


  pure elemental integer function yearday(self)
    ! Returns the integer day of the year (ordinal date).
    class(datetime), intent(in) :: self
    integer :: month
    yearday = 0
    do month = 1, self % month-1
      yearday = yearday + daysInMonth(month, self % year)
    end do
    yearday = yearday + self % day
  end function yearday


  pure elemental function datetime_plus_timedelta(d0,t) result(d)
    ! Adds a `timedelta` instance to a `datetime` instance, and returns a
    ! new `datetime` instance. Overloads the operator `+`.
    class(datetime), intent(in) :: d0
    class(timedelta), intent(in) :: t
    type(datetime) :: d

    integer :: milliseconds, seconds, minutes, hours, days

    d = datetime(year = d0 % getYear(),               &
                 month = d0 % getMonth(),             &
                 day = d0 % getDay(),                 &
                 hour = d0 % getHour(),               &
                 minute = d0 % getMinute(),           &
                 second = d0 % getSecond(),           &
                 millisecond = d0 % getMillisecond(), &
                 tz = d0 % getTz())

    milliseconds = t % getMilliseconds()
    seconds = t % getSeconds()
    minutes = t % getMinutes()
    hours = t % getHours()
    days = t % getDays()

    if (milliseconds /= 0) call d % addMilliseconds(milliseconds)
    if (seconds /= 0) call d % addSeconds(seconds)
    if (minutes /= 0) call d % addMinutes(minutes)
    if (hours /= 0) call d % addHours(hours)
    if (days /= 0) call d % addDays(days)

  end function datetime_plus_timedelta


  pure elemental function timedelta_plus_datetime(t,d0) result(d)
    ! Adds a `timedelta` instance to a `datetime` instance, and returns a
    ! new `datetime` instance. Overloads the operator `+`.
    class(timedelta), intent(in) :: t
    class(datetime), intent(in) :: d0
    type(datetime) :: d
    d = d0 + t
  end function timedelta_plus_datetime


  pure elemental function datetime_minus_timedelta(d0,t) result(d)
    ! Subtracts a `timedelta` instance from a `datetime` instance and
    ! returns a new `datetime` instance. Overloads the operator `-`.
    class(datetime), intent(in) :: d0
    class(timedelta), intent(in) :: t
    type(datetime) :: d
    d = d0 + (-t)
  end function datetime_minus_timedelta


  pure elemental function datetime_minus_datetime(d0,d1) result(t)
    ! Subtracts a `datetime` instance from another `datetime` instance,
    ! and returns a `timedelta` instance. Overloads the operator `-`.
    class(datetime), intent(in) :: d0, d1
    type(timedelta) :: t
    real(real64) :: daysDiff
    integer :: days,hours,minutes,seconds,milliseconds
    integer :: sign_

    daysDiff = date2num(d0)-date2num(d1)

    if (daysDiff < 0) then
      sign_ = -1
      daysDiff = ABS(daysDiff)
    else
      sign_ = 1
    end if

    days         = int(daysDiff)
    hours        = int((daysDiff-days)*d2h)
    minutes      = int((daysDiff-days-hours*h2d)*d2m)
    seconds      = int((daysDiff-days-hours*h2d-minutes*m2d)*d2s)
    milliseconds = nint((daysDiff-days-hours*h2d-minutes*m2d&
                                 -seconds*s2d)*d2s*1e3_real64)

    t = timedelta(sign_*days,sign_*hours,sign_*minutes,sign_*seconds,&
                  sign_*milliseconds)

  end function datetime_minus_datetime


  pure elemental logical function datetime_gt(d0,d1) result(res)
    ! `datetime` comparison operator that returns `.true.` if `d0` is
    ! greater than `d1` and `.false.` otherwise. Overloads the
    ! operator `>`.
    class(datetime), intent(in) :: d0, d1
    type(datetime) :: d0_utc, d1_utc

    ! Convert to UTC before making comparison
    d0_utc = d0 % utc()
    d1_utc = d1 % utc()

    ! Compare years
    if (d0_utc % year > d1_utc % year) then
      res = .true.
    else if (d0_utc % year < d1_utc % year) then
      res = .false.
    else

      ! Compare months
      if (d0_utc % month > d1_utc % month) then
        res = .true.
      else if (d0_utc % month < d1_utc % month) then
        res = .false.
      else

        ! Compare days
        if (d0_utc % day > d1_utc % day) then
          res = .true.
        else if (d0_utc % day < d1_utc % day) then
          res = .false.
        else

          ! Compare hours
          if (d0_utc % hour > d1_utc % hour) then
            res = .true.
          else if (d0_utc % hour < d1_utc % hour) then
            res = .false.
          else

            ! Compare minutes
            if (d0_utc % minute > d1_utc % minute) then
              res = .true.
            else if (d0_utc % minute < d1_utc % minute) then
              res = .false.
            else

              ! Compare seconds
              if (d0_utc % second > d1_utc % second) then
                res = .true.
              else if (d0_utc % second < d1_utc % second) then
                res = .false.
              else

                ! Compare milliseconds
                if (d0_utc % millisecond > d1_utc % millisecond) then
                  res = .true.
                else
                  res = .false.
                end if

              end if
            end if
          end if
        end if
      end if
    end if

  end function datetime_gt


  pure elemental logical function datetime_lt(d0,d1) result(res)
    ! `datetime` comparison operator that returns `.true.` if `d0` is
    ! less than `d1` and `.false.` otherwise. Overloads the operator `<`.
    class(datetime), intent(in) :: d0, d1
    res = d1 > d0
  end function datetime_lt


  pure elemental logical function datetime_eq(d0,d1) result(res)
    ! `datetime` comparison operator that returns `.true.` if `d0` is
    ! equal to `d1` and `.false.` otherwise. Overloads the operator `==`.
    class(datetime), intent(in) :: d0, d1
    type(datetime) :: d0_utc, d1_utc

    ! Convert to UTC before making comparison
    d0_utc = d0 % utc()
    d1_utc = d1 % utc()

    res = d0_utc % year        == d1_utc % year   .and. &
          d0_utc % month       == d1_utc % month  .and. &
          d0_utc % day         == d1_utc % day    .and. &
          d0_utc % hour        == d1_utc % hour   .and. &
          d0_utc % minute      == d1_utc % minute .and. &
          d0_utc % second      == d1_utc % second .and. &
          d0_utc % millisecond == d1_utc % millisecond

  end function datetime_eq


  pure elemental logical function datetime_neq(d0,d1) result(res)
    ! `datetime` comparison operator that eturns `.true.` if `d0` is
    ! not equal to `d1` and `.false.` otherwise. Overloads the operator `/=`.
    class(datetime), intent(in) :: d0, d1
    res = .not. d0 == d1
  end function datetime_neq


  pure elemental logical function datetime_ge(d0,d1) result(res)
    ! `datetime` comparison operator. Returns `.true.` if `d0` is greater
    ! than or equal to `d1` and `.false.` otherwise. Overloads the
    ! operator `>=`.
    class(datetime), intent(in) :: d0, d1
    res = d0 > d1 .or. d0 == d1
  end function datetime_ge


  pure elemental logical function datetime_le(d0,d1) result(res)
    ! `datetime` comparison operator. Returns `.true.` if `d0` is less
    ! than or equal to `d1`, and `.false.` otherwise. Overloads the
    ! operator `<=`.
    class(datetime), intent(in) :: d0, d1
    res = d1 > d0 .or. d0 == d1
  end function datetime_le


  pure elemental logical function isLeapYear(year)
    ! Returns `.true.` if year is leap year and `.false.` otherwise.
    integer, intent(in) :: year
    isLeapYear = (mod(year,4) == 0 .and. .not. mod(year,100) == 0)&
            .or. (mod(year,400) == 0)
  end function isLeapYear


  pure function datetimeRange(d0, d1, t)
    ! Given start and end `datetime` instances `d0` and `d1` and time
    ! increment as `timedelta` instance `t`, returns an array of
    ! `datetime` instances. The number of elements is the number of whole
    ! time increments contained between datetimes `d0` and `d1`.
    type(datetime), intent(in) :: d0, d1
    type(timedelta), intent(in) :: t
    real(real64) :: datenum0, datenum1, eps, increment
    type(datetime), allocatable :: datetimeRange(:)
    integer :: n, nm
    eps = 1e-10_real64
    datenum0 = date2num(d0)
    datenum1 = date2num(d1)
    increment = t % total_seconds() * s2d
    nm = floor((datenum1 - datenum0 + eps) / increment) + 1
    allocate(datetimeRange(nm))
    do n = 1, nm
      datetimeRange(n) = num2date(datenum0 + (n - 1) * increment)
    end do
  end function datetimeRange


  pure elemental integer function daysInMonth(month,year)
    ! Given integer month and year, returns an integer number
    ! of days in that particular month.
    integer, intent(in) :: month, year

    integer, parameter :: days(*) = [31, 28, 31, 30, 31, 30, &
                                     31, 31, 30, 31, 30, 31]

    if (month < 1 .or. month > 12) then
      ! Should raise an error and abort here, however we want to keep
      ! the pure and elemental attributes. Make sure this function is
      ! called with the month argument in range.
      daysInMonth = 0
      return
    end if

    if (month == 2 .and. isLeapYear(year)) then
      daysInMonth = 29
    else
      daysInMonth = days(month)
    end if

  end function daysInMonth


  pure elemental integer function daysInYear(year)
    ! Returns the number of days in year.
    integer, intent(in) :: year
    if (isLeapYear(year)) then
      daysInYear = 366
    else
      daysInYear = 365
    end if
  end function daysInYear


  pure elemental real(real64) function date2num(d)
    ! Given a datetime instance d, returns number of days since
    ! `0001-01-01 00:00:00`, taking into account the timezone offset.
    type(datetime), intent(in) :: d
    type(datetime) :: d_utc
    integer :: year

    ! Convert to UTC first
    d_utc = d % utc()

    ! d_utc % year must be positive:
    if (d_utc % year < 1) then
      date2num = 0
      return
    end if

    date2num = 0
    do year = 1,d_utc % year-1
      date2num = date2num + daysInYear(year)
    end do

    date2num = date2num          &
             + d_utc % yearday() &
             + d_utc % hour*h2d  &
             + d_utc % minute*m2d&
             + (d_utc % second+1e-3_real64*d_utc % millisecond)*s2d

  end function date2num


  pure elemental type(datetime) function num2date(num)
    ! Given number of days since `0001-01-01 00:00:00`, returns a
    ! correspoding `datetime` instance.
    real(real64), intent(in) :: num
    integer :: year, month, day, hour, minute, second, millisecond
    real(real64) :: days, totseconds

    ! num must be positive
    if (num < 0) then
      num2date = datetime(1)
      return
    end if

    days = num

    year = 1
    do
      if (int(days) <= daysInYear(year))exit
      days = days-daysInYear(year)
      year = year+1
    end do

    month = 1
    do
      if (inT(days) <= daysInMonth(month,year))exit
      days = days-daysInMonth(month,year)
      month = month+1
    end do

    day         = int(days)
    totseconds  = (days-day)*d2s
    hour        = int(totseconds*s2h)
    minute      = int((totseconds-hour*h2s)*s2m)
    second      = int(totseconds-hour*h2s-minute*m2s)
    millisecond = nint((totseconds-int(totseconds))*1e3_real64)

    num2date = datetime(year,month,day,hour,minute,second,millisecond,tz=zero)

    ! Handle a special case caused by floating-point arithmethic:
    if (num2date % millisecond == 1000) then
      num2date % millisecond = 0
      call num2date % addSeconds(1)
    end if

    if (num2date % second == 60) then
      num2date % second = 0
      call num2date % addMinutes(1)
    end if
    if (num2date % minute == 60) then
      num2date % minute = 0
      call num2date % addHours(1)
    end if
    if (num2date % hour == 24) then
      num2date % hour = 0
      call num2date % addDays(1)
    end if

  end function num2date


  type(datetime) function strptime(str,format)
    ! A wrapper function around C/C++ strptime function.
    ! Returns a `datetime` instance.
    character(*), intent(in) :: str, format
    integer :: rc
    type(tm_struct) :: tm
    rc = c_strptime(trim(str) // c_null_char, trim(format) // c_null_char, tm)
    strptime = tm2date(tm)
  end function strptime


  pure elemental type(datetime) function tm2date(ctime)
    ! Given a `tm_struct` instance, returns a corresponding `datetime`
    ! instance.
    type(tm_struct), intent(in) :: ctime

    tm2date % millisecond = 0
    tm2date % second      = ctime % tm_sec
    tm2date % minute      = ctime % tm_min
    tm2date % hour        = ctime % tm_hour
    tm2date % day         = ctime % tm_mday
    tm2date % month       = ctime % tm_mon+1
    tm2date % year        = ctime % tm_year+1900
    tm2date % tz          = 0

  end function tm2date


  pure function int2str(i, length)
    ! Converts an integer `i` into a character string of requested length,
    ! pre-pending zeros if necessary.
    integer, intent(in) :: i, length
    character(length) :: int2str
    character(2) :: string
    write(string, '(i2)') length
    write(int2str, '(i' // string // '.' // string //')') i
  end function int2str


  pure elemental type(timedelta) function timedelta_constructor( &
    days, hours, minutes, seconds, milliseconds)
    ! Constructor function for the `timedelta` class.
    integer, intent(in), optional :: days, hours, minutes, seconds, milliseconds

    timedelta_constructor % days = 0
    if (present(days)) timedelta_constructor % days = days

    timedelta_constructor % hours = 0
    if (present(hours)) timedelta_constructor % hours = hours

    timedelta_constructor % minutes = 0
    if (present(minutes)) timedelta_constructor % minutes = minutes

    timedelta_constructor % seconds = 0
    if (present(seconds)) timedelta_constructor % seconds = seconds

    timedelta_constructor % milliseconds = 0
    if (present(milliseconds)) timedelta_constructor % milliseconds = milliseconds

  end function timedelta_constructor


  pure elemental integer function getDays(self)
    ! Returns the number of days.
    class(timedelta), intent(in) :: self
    getDays = self % days
  end function getDays


  pure elemental integer function getHours(self)
    ! Returns the number of hours.
    class(timedelta), intent(in) :: self
    getHours = self % hours
  end function getHours


  pure elemental integer function getMinutes(self)
    ! Returns the number of minutes.
    class(timedelta), intent(in) :: self
    getMinutes = self % minutes
  end function getMinutes


  pure elemental integer function getSeconds(self)
    ! Returns the number of seconds.
    class(timedelta), intent(in) :: self
    getSeconds = self % seconds
  end function getSeconds


  pure elemental integer function getMilliseconds(self)
    ! Returns the number of milliseconds.
    class(timedelta), intent(in) :: self
    getMilliseconds = self % milliseconds
  end function getMilliseconds


  pure elemental real(real64) function total_seconds(self)
    ! Returns a total number of seconds contained in a `timedelta`
    ! instance.
    class(timedelta), intent(in) :: self
    total_seconds = self % days*86400._real64 &
                  + self % hours*3600._real64 &
                  + self % minutes*60._real64 &
                  + self % seconds            &
                  + self % milliseconds*1e-3_real64
  end function total_seconds


  pure elemental type(timedelta) function timedelta_plus_timedelta(t0,t1) result(t)
    ! Adds two `timedelta` instances together and returns a `timedelta`
    ! instance. Overloads the operator `+`.
    class(timedelta), intent(in) :: t0, t1
    t = timedelta(days         = t0 % days         + t1 % days,    &
                  hours        = t0 % hours        + t1 % hours,   &
                  minutes      = t0 % minutes      + t1 % minutes, &
                  seconds      = t0 % seconds      + t1 % seconds, &
                  milliseconds = t0 % milliseconds + t1 % milliseconds)
  end function timedelta_plus_timedelta


  pure elemental type(timedelta) function timedelta_minus_timedelta(t0,t1) result(t)
    ! Subtracts a `timedelta` instance from another. Returns a
    ! `timedelta` instance. Overloads the operator `-`.
    class(timedelta), intent(in) :: t0, t1
    t = t0 + (-t1)
  end function timedelta_minus_timedelta


  pure elemental type(timedelta) function unary_minus_timedelta(t0) result(t)
    ! Takes a negative of a `timedelta` instance. Overloads the operator `-`.
    class(timedelta), intent(in) :: t0
    t % days         = -t0 % days
    t % hours        = -t0 % hours
    t % minutes      = -t0 % minutes
    t % seconds      = -t0 % seconds
    t % milliseconds = -t0 % milliseconds
  end function unary_minus_timedelta


  pure elemental logical function timedelta_eq(td0,td1) result(res)
    ! `timedelta` object comparison operator. Returns `.true.` if `td0`
    ! is equal to `td1` and `.false.` otherwise. Overloads the operator
    ! `==`.
    class(timedelta), intent(in) :: td0, td1
    res = td0 % total_seconds() == td1 % total_seconds()
  end function timedelta_eq


  pure elemental logical function timedelta_neq(td0,td1) result(res)
    ! `timedelta` object comparison operator. Returns `.true.` if `td0`
    ! is not equal to `td1` and `.false.` otherwise. Overloads the
    ! operator `/=`.
    class(timedelta), intent(in) :: td0, td1
    res = td0 % total_seconds() /= td1 % total_seconds()
  end function timedelta_neq


  pure elemental logical function timedelta_gt(td0,td1) result(res)
    ! `timedelta` object comparison operator. Returns `.true.` if
    ! `td0` is greater than `td1` and `.false.` otherwise. Overloads the
    ! operator `>`.
    class(timedelta), intent(in) :: td0, td1
    res = td0 % total_seconds() > td1 % total_seconds()
  end function timedelta_gt


  pure elemental logical function timedelta_ge(td0,td1) result(res)
    ! `timedelta` object comparison operator. Returns `.true.` if `td0`
    ! is greater than or equal to `td1` and `.false.` otherwise.
    ! Overloads the operator >=.
    class(timedelta), intent(in) :: td0, td1
    res = td0 % total_seconds() >= td1 % total_seconds()
  end function timedelta_ge


  pure elemental logical function timedelta_lt(td0,td1) result(res)
    ! `timedelta` object comparison operator. Returns `.true.` if `td0`
    ! is less than `td1` and `.false.` otherwise. Overloads the operator
    ! `<`.
    class(timedelta), intent(in) :: td0, td1
    res = td0 % total_seconds() < td1 % total_seconds()
  end function timedelta_lt


  pure elemental logical function timedelta_le(td0,td1) result(res)
    ! `timedelta` object comparison operator. Returns `.true.` if `td0`
    ! is less than or equal to `td1` and `.false.` otherwise. Overloads
    ! the operator `<=`.
    class(timedelta), intent(in) :: td0, td1
    res = td0 % total_seconds() <= td1 % total_seconds()
  end function timedelta_le

end module datetime_module
