module datetime_tests
  use datetime_module
  use, intrinsic :: iso_fortran_env, only: real64

  implicit none

  private
  public :: assert, test_datetime

contains

  logical function assert(condition, test_name)
    logical, intent(in) :: condition
    character(*), intent(in) :: test_name
    character(60) :: output_test_name

    assert = condition
    output_test_name = test_name

    if (assert) then
      print *, 'test ' // output_test_name // ': ' // &
        char(27) // '[32mPASS' // char(27) // '[0m'
    else
      print '(a)', 'test ' // output_test_name // ': ' // &
        char(27) // '[31mFAIL' // char(27) // '[0m'
    end if

  end function assert


  subroutine initialize_tests(tests, ntests)
    logical, allocatable, intent(in out) :: tests(:)
    integer, intent(in) :: ntests
    if (allocated(tests)) deallocate(tests)
    allocate(tests(ntests))
  end subroutine initialize_tests


  subroutine report_tests(tests, test_failed)
    ! Takes the test counter as input and reports
    ! the total number of test passes and failures.
    logical, intent(in) :: tests(:)
    logical, optional, intent(out) :: test_failed

    integer :: n, ntests, nsuccess, nfailure

    ntests = size(tests)

    nsuccess = 0
    nfailure = 0
    do n = 1, ntests
      if (tests(n)) then
        nsuccess = nsuccess + 1
      else
        nfailure = nfailure + 1
      end if
    end do

    print '(a, i3, a)', 'Ran a total of ', ntests, ' tests.'
    print '(i3, a, i3, a)', nsuccess, ' tests PASSED,  ', &
                            nfailure, ' tests FAILED.'

    if (present(test_failed)) then
      test_failed = .false.
      if (nfailure /= 0) test_failed = .true.
    end if

  end subroutine report_tests


  subroutine test_datetime
    type(datetime) :: a
    type(timedelta) :: td
    type(clock) :: c

    type(datetime), allocatable :: dtRange(:)

    real(real64) :: eps = tiny(1._real64)
    logical, allocatable :: tests(:)
    logical :: test_failed
    integer :: i, n, ntests, tzOffset

    print *

    ! Test counter; modify if adding new tests
    ntests = 191

    call initialize_tests(tests, ntests)

    n = 1

    ! Empty constructor
    tests(n) = assert(datetime() == datetime(1, 1, 1), &
                      'empty datetime() constructor')
    n = n + 1

    ! Empty time initialization
    tests(n) = assert(datetime(2014, 1, 1) == datetime(2014, 1, 1, 0, 0, 0, 0), &
                      'semi-empty datetime() constructor')
    n = n + 1

    ! Increment milliseconds
    tests(n) = assert(datetime(2014, 1, 1, 0, 0, 0, 0) + timedelta(milliseconds= 100) &
                   == datetime(2014, 1, 1, 0, 0, 0, 100),                             &
                   'datetime + timedelta(milliseconds = 100)')
    n = n + 1

    ! Decrement milliseconds
    tests(n) = assert(datetime(2014, 1, 1, 0, 0, 0, 0) + timedelta(milliseconds=-100) &
                   == datetime(2013, 12, 31, 23, 59, 59, 900),                        &
                   'datetime + timedelta(milliseconds = -100)')
    n = n + 1

    ! Increment seconds
    tests(n) = assert(datetime(2014, 1, 1, 0, 0, 0, 0) + timedelta(seconds=1) &
                   == datetime(2014, 1, 1, 0, 0, 1, 0),                       &
                   'datetime + timedelta(seconds = 1)')
    n = n + 1

    ! Decrement seconds
    tests(n) = assert(datetime(2014, 1, 1, 0, 0, 0, 0) + timedelta(seconds=-1) &
                   == datetime(2013, 12, 31, 23, 59, 59, 0),                   &
                   'datetime + timedelta(seconds = -1)')
    n = n + 1

    ! Increment minutes
    tests(n) = assert(datetime(2014, 1, 1, 0, 0, 0, 0) + timedelta(minutes=1) &
                   == datetime(2014, 1, 1, 0, 1, 0, 0),                       &
                   'datetime + timedelta(minutes = 1)')
    n = n + 1

    ! Decrement minutes
    tests(n) = assert(datetime(2014, 1, 1, 0, 0, 0, 0) + timedelta(minutes=-1) &
                   == datetime(2013, 12, 31, 23, 59, 0, 0),                    &
                   'datetime + timedelta(minutes = -1)')
    n = n + 1

    ! Increment hours
    tests(n) = assert(datetime(2014, 1, 1, 0, 0, 0, 0) + timedelta(hours=1) &
                   == datetime(2014, 1, 1, 1, 0, 0, 0),                     &
                   'datetime + timedelta(hours = 1)')
    n = n + 1

    ! Decrement hours
    tests(n) = assert(datetime(2014, 1, 1, 0, 0, 0, 0) + timedelta(hours=-1) &
                   == datetime(2013, 12, 31, 23, 0, 0, 0),                   &
                   'datetime + timedelta(hours = -1)')
    n = n + 1

    ! Increment days
    tests(n) = assert(datetime(2014, 1, 1, 0, 0, 0, 0) + timedelta(days= 1) &
                   == datetime(2014, 1, 2, 0, 0, 0, 0),                     &
                   'datetime + timedelta(days = 1)')
    n = n + 1

    ! Decrement days
    tests(n) = assert(datetime(2014, 1, 1, 0, 0, 0, 0) + timedelta(days=-1)&
                   == datetime(2013, 12, 31, 0, 0, 0, 0),                   &
                   'datetime + timedelta(days = -1)')
    n = n + 1

    ! Test various overflow situations

    a = datetime(2014, 1, 1, 0, 0, 0)

    tests(n) = assert(a+timedelta(seconds=3) == a+timedelta(milliseconds=3000), &
                      'Seconds overflow in addMilliseconds (3000 milliseconds)')
    n = n + 1

    tests(n) = assert(a-timedelta(seconds=3) == a-timedelta(milliseconds=3000), &
                      'Seconds overflow in addMilliseconds (-3000 milliseconds)')
    n = n + 1

    tests(n) = assert(a+timedelta(minutes=6) == a+timedelta(seconds=360), &
                      'Minutes overflow in addSeconds (360 seconds)')
    n = n + 1

    tests(n) = assert(a-timedelta(minutes=6) == a-timedelta(seconds=360), &
                      'Minutes overflow in addSeconds (-360 seconds)')
    n = n + 1

    tests(n) = assert(a+timedelta(hours=6) == a+timedelta(minutes=360), &
                      'Hours overflow in addMinutes (360 minutes)')
    n = n + 1

    tests(n) = assert(a-timedelta(hours=6) == a-timedelta(minutes=360), &
                      'Hours overflow in addMinutes (-360 minutes)')
    n = n + 1

    tests(n) = assert(a+timedelta(days=3) == a+timedelta(hours=72), &
                      'Days overflow in addHours (72 hours)')
    n = n + 1

    tests(n) = assert(a-timedelta(days=3) == a-timedelta(hours=72), &
                      'Days overflow in addHours (-72 hours)')
    n = n + 1

    print '(71("-"))'

    ! Test subtracting into previous months:
    tests(n) = assert(datetime(2014, 2,  1, 0, 0, 0, 0) + timedelta(days=-1)&
                   == datetime(2014, 1, 31, 0, 0, 0, 0),                     &
                   'decrement datetime into January')
    n = n + 1

    tests(n) = assert(datetime(2014, 3,  1, 0, 0, 0, 0) + timedelta(days=-1)&
                   == datetime(2014, 2, 28, 0, 0, 0, 0),                     &
                   'decrement datetime into February')
    n = n + 1

    tests(n) = assert(datetime(2014, 4,  1, 0, 0, 0, 0) + timedelta(days=-1)&
                   == datetime(2014, 3, 31, 0, 0, 0, 0),                     &
                   'decrement datetime into March')
    n = n + 1

    tests(n) = assert(datetime(2014, 5,  1, 0, 0, 0, 0) + timedelta(days=-1)&
                   == datetime(2014, 4, 30, 0, 0, 0, 0),                     &
                   'decrement datetime into April')
    n = n + 1

    tests(n) = assert(datetime(2014, 6,  1, 0, 0, 0, 0) + timedelta(days=-1)&
                   == datetime(2014, 5, 31, 0, 0, 0, 0),                     &
                   'decrement datetime into May')
    n = n + 1

    tests(n) = assert(datetime(2014, 7,  1, 0, 0, 0, 0) + timedelta(days=-1)&
                 == datetime(2014, 6, 30, 0, 0, 0, 0),                     &
                   'decrement datetime into June')
    n = n + 1

    tests(n) = assert(datetime(2014, 8,  1, 0, 0, 0, 0) + timedelta(days=-1)&
                   == datetime(2014, 7, 31, 0, 0, 0, 0),                     &
                   'decrement datetime into July')
    n = n + 1

    tests(n) = assert(datetime(2014, 9,  1, 0, 0, 0, 0) + timedelta(days=-1)&
                   == datetime(2014, 8, 31, 0, 0, 0, 0),                     &
                   'decrement datetime into August')
    n = n + 1

    tests(n) = assert(datetime(2014, 10,  1, 0, 0, 0, 0) + timedelta(days=-1)&
                   == datetime(2014,  9, 30, 0, 0, 0, 0),                     &
                   'decrement datetime into September')
    n = n + 1

    tests(n) = assert(datetime(2014, 11,  1, 0, 0, 0, 0) + timedelta(days=-1)&
                   == datetime(2014, 10, 31, 0, 0, 0, 0),                     &
                   'decrement datetime into October')
    n = n + 1

    tests(n) = assert(datetime(2014, 12,  1, 0, 0, 0, 0) + timedelta(days=-1)&
                   == datetime(2014, 11, 30, 0, 0, 0, 0),                     &
                   'decrement datetime into November')
    n = n + 1

    tests(n) = assert(datetime(2015,  1,  1, 0, 0, 0, 0) + timedelta(days=-1)&
                   == datetime(2014, 12, 31, 0, 0, 0, 0),                     &
                   'decrement datetime into December')
    n = n + 1

    print '(71("-"))'

    ! datetime difference
    tests(n) = assert(datetime(2014, 1, 2)-datetime(2014, 1, 1)&
                       == timedelta(days=1), &
                      'datetime-datetime == timedelta(days = 1)')
    n = n + 1

    tests(n) = assert(datetime(2014, 1, 1, 2)-datetime(2014, 1, 1, 1)&
                       == timedelta(hours=1),                   &
                      'datetime-datetime == timedelta(hours = 1)')
    n = n + 1

    tests(n) = assert(datetime(2014, 1, 1, 1, 2)-datetime(2014, 1, 1, 1, 1)&
                      == timedelta(minutes=1),                      &
                      'datetime-datetime == timedelta(minutes = 1)')
    n = n + 1

    tests(n) = assert(datetime(2014, 1, 1, 1, 1, 2)-datetime(2014, 1, 1, 1, 1, 1)&
                      == timedelta(seconds=1),                          &
                      'datetime-datetime == timedelta(seconds = 1)')
    n = n + 1

    tests(n) = assert(datetime(2014, 1, 1, 1, 1, 1, 2)&
                     -datetime(2014, 1, 1, 1, 1, 1, 1)&
                   == timedelta(milliseconds=1), &
                   'datetime-datetime == timedelta(milliseconds = 1)')
    n = n + 1

    print '(71("-"))'

    ! datetime comparison tests
    tests(n) = assert(datetime(2014, 1, 2, 3, 4, 5, 6) &
                   == datetime(2014, 1, 2, 3, 4, 5, 6), &
                   'datetime == datetime')
    n = n + 1

    tests(n) = assert(datetime(2014, 1, 2, 9, 4, 5, 6, tz=6._real64) &
                   == datetime(2014, 1, 2, 3, 4, 5, 6, tz=0._real64), &
                   'datetime == datetime,  timezone test 1')
    n = n + 1

    tests(n) = assert(datetime(2014, 1, 2, 3, 4, 5, 6, tz=-6._real64) &
                   == datetime(2014, 1, 2, 9, 4, 5, 6, tz= 0._real64), &
                   'datetime == datetime,  timezone test 2')
    n = n + 1

    tests(n) = assert(datetime(2013, 1, 2, 3, 4, 5, 6) &
                   /= datetime(2014, 1, 2, 3, 4, 5, 6), &
                   'datetime /= datetime')
    n = n + 1

    tests(n) = assert(datetime(2014, 1, 2, 4, 4, 5, 6) &
                    > datetime(2014, 1, 2, 3, 4, 5, 6), &
                    'datetime > datetime')
    n = n + 1

    tests(n) = assert(datetime(2014, 1, 2, 4, 4, 5, 6) &
                   >= datetime(2014, 1, 2, 3, 4, 5, 6), &
                   'datetime >= datetime (greater)')
    n = n + 1

    tests(n) = assert(datetime(2014, 1, 2, 3, 4, 5, 6) &
                   >= datetime(2014, 1, 2, 3, 4, 5, 6), &
                   'datetime >= datetime (equal)')
    n = n + 1

    tests(n) = assert(datetime(2014, 1, 2, 3, 4, 5, 6) &
                    < datetime(2014, 1, 2, 4, 4, 5, 6), &
                    'datetime < datetime')
    n = n + 1

    tests(n) = assert(datetime(2014, 1, 2, 3, 4, 5, 6) &
                   <= datetime(2014, 1, 2, 4, 4, 5, 6), &
                   'datetime <= datetime (less)')
    n = n + 1

    tests(n) = assert(datetime(2014, 1, 2, 3, 4, 5, 6) &
                   <= datetime(2014, 1, 2, 3, 4, 5, 6), &
                   'datetime <= datetime (equal)')
    n = n + 1

    print '(71("-"))'

    ! Test datetime % isoformat()

    a = datetime(2014, 1, 1, 0, 0, 0, 0)

    tests(n) = assert(a % isoformat() == '2014-01-01T00:00:00.000', &
                      'datetime % isoformat,  default separator')
    n = n + 1

    tests(n) = assert(a % isoformat('T') == '2014-01-01T00:00:00.000', &
                      'datetime % isoformat,  T separator')
    n = n + 1

    tests(n) = assert(a % isoformat(' ') == '2014-01-01 00:00:00.000', &
                      'datetime % isoformat,  blank separator')
    n = n + 1

    print '(71("-"))'

    ! Test datetime % strftime() and strptime()

    a = datetime(2014, 1, 2, 3, 4, 5)

    tests(n) = assert(a % strftime('%Y-%m-%d %H:%M:%S')&
                      == '2014-01-02 03:04:05',         &
                      'datetime % strftime')
    n = n + 1

    tests(n) = assert(strptime('2014-01-02 03:04:05', '%Y-%m-%d %H:%M:%S')&
                   == datetime(2014, 1, 2, 3, 4, 5),                           &
                   'datetime % strptime')
    n = n + 1

    tests(n) = assert(strptime(a % strftime('%Y-%m-%d %H:%M:%S'), &
                               '%Y-%m-%d %H:%M:%S') == a,         &
               'strptime(datetime % strftime(fmt), fmt) == datetime')
    n = n + 1

    print '(71("-"))'

    ! datetime % isocalendar: test all examples from
    ! http://en.wikipedia.org/wiki/ISO_week_date

    a = datetime(2005, 1, 1)
    tests(n) = assert(ALL(a % isocalendar() == [2004, 53, 6]), &
                     'datetime(2005, 1, 1) % isocalendar() == [2004, 53, 6]')
    n = n + 1

    a = datetime(2005, 1, 2)
    tests(n) = assert(ALL(a % isocalendar() == [2004, 53, 7]), &
                     'datetime(2005, 1, 2) % isocalendar() == [2004, 53, 7]')
    n = n + 1

    a = datetime(2005, 12, 31)
    tests(n) = assert(ALL(a % isocalendar() == [2005, 52, 6]), &
                     'datetime(2005, 12, 31) % isocalendar() == [2005, 52, 6]')
    n = n + 1

    a = datetime(2007, 1, 1)
    tests(n) = assert(ALL(a % isocalendar() == [2007, 1, 1]), &
                     'datetime(2007, 1, 1) % isocalendar() == [2007, 1, 1]')
    n = n + 1

    a = datetime(2007, 12, 30)
    tests(n) = assert(ALL(a % isocalendar() == [2007, 52, 7]), &
                     'datetime(2007, 12, 30) % isocalendar() == [2007, 52, 7]')
    n = n + 1

    a = datetime(2007, 12, 31)
    tests(n) = assert(ALL(a % isocalendar() == [2008, 1, 1]), &
                     'datetime(2007, 12, 31) % isocalendar() == [2008, 1, 1]')
    n = n + 1

    a = datetime(2008, 1, 1)
    tests(n) = assert(ALL(a % isocalendar() == [2008, 1, 2]), &
                     'datetime(2008, 1, 1) % isocalendar() == [2008, 1, 2]')
    n = n + 1

    a = datetime(2008, 12, 28)
    tests(n) = assert(ALL(a % isocalendar() == [2008, 52, 7]), &
                     'datetime(2008, 12, 28) % isocalendar() == [2008, 52, 7]')
    n = n + 1

    a = datetime(2008, 12, 29)
    tests(n) = assert(ALL(a % isocalendar() == [2009, 1, 1]), &
                     'datetime(2008, 12, 29) % isocalendar() == [2009, 1, 1]')
    n = n + 1

    a = datetime(2008, 12, 30)
    tests(n) = assert(ALL(a % isocalendar() == [2009, 1, 2]), &
                     'datetime(2008, 12, 30) % isocalendar() == [2009, 1, 2]')
    n = n + 1

    a = datetime(2008, 12, 31)
    tests(n) = assert(ALL(a % isocalendar() == [2009, 1, 3]), &
                     'datetime(2008, 12, 31) % isocalendar() == [2009, 1, 3]')
    n = n + 1

    a = datetime(2009, 1, 1)
    tests(n) = assert(ALL(a % isocalendar() == [2009, 1, 4]), &
                     'datetime(2009, 1, 1) % isocalendar() == [2009, 1, 4]')
    n = n + 1

    a = datetime(2009, 12, 31)
    tests(n) = assert(ALL(a % isocalendar() == [2009, 53, 4]), &
                     'datetime(2009, 12, 31) % isocalendar() == [2009, 53, 4]')
    n = n + 1

    a = datetime(2010, 1, 1)
    tests(n) = assert(ALL(a % isocalendar() == [2009, 53, 5]), &
                     'datetime(2010, 1, 1) % isocalendar() == [2009, 53, 5]')
    n = n + 1

    a = datetime(2010, 1, 2)
    tests(n) = assert(ALL(a % isocalendar() == [2009, 53, 6]), &
                     'datetime(2010, 1, 2) % isocalendar() == [2009, 53, 6]')
    n = n + 1

    a = datetime(2010, 1, 3)
    tests(n) = assert(ALL(a % isocalendar() == [2009, 53, 7]), &
                     'datetime(2010, 1, 3) % isocalendar() == [2009, 53, 7]')
    n = n + 1

    print '(71("-"))'

    ! datetime % isValid()

    a = datetime(1, 1, 1)
    tests(n) = assert(a % isValid(), 'datetime(1, 1, 1) is valid')
    n = n + 1

    a = datetime(0, 1, 1)
    tests(n) = assert(.not. a % isValid(), 'datetime(0, 1, 1) is not valid')
    n = n + 1

    a = datetime(-1, 1, 1)
    tests(n) = assert(.not. a % isValid(), 'datetime(-1, 1, 1) is not valid')
    n = n + 1

    a = datetime(2014, 1, 1)
    tests(n) = assert(a % isValid(), 'datetime(2014, 1, 1) is valid')
    n = n + 1

    a = datetime(2014, 0, 1)
    tests(n) = assert(.not. a % isValid(), 'datetime(2014, 0, 1) is not valid')
    n = n + 1

    a = datetime(2014, 1, 0)
    tests(n) = assert(.not. a % isValid(), 'datetime(2014, 1, 0) is not valid')
    n = n + 1

    a = datetime(2014, 2, 28)
    tests(n) = assert(a % isValid(), 'datetime(2014, 2, 28) is valid')
    n = n + 1

    a = datetime(2014, 2, 29)
    tests(n) = assert(.not. a % isValid(), 'datetime(2014, 2, 29) is not valid')
    n = n + 1

    a = datetime(2012, 2, 29)
    tests(n) = assert(a % isValid(), 'datetime(2012, 2, 29) is valid')
    n = n + 1

    a = datetime(2012, 3, 31)
    tests(n) = assert(a % isValid(), 'datetime(2012, 3, 31) is valid')
    n = n + 1

    a = datetime(2012, 3, 32)
    tests(n) = assert(.not. a % isValid(), 'datetime(2012, 3, 32) is not valid')
    n = n + 1

    a = datetime(2012, 3, 31, 0, 0, 0)
    tests(n) = assert(a % isValid(), 'datetime(2012, 3, 31, 0, 0, 0) is valid')
    n = n + 1

    a = datetime(2012, 3, 31, 24, 0, 0)
    tests(n) = assert(.not. a % isValid(), 'datetime(2012, 3, 31, 24, 0, 0) is not valid')
    n = n + 1

    a = datetime(2012, 3, 31, 0, 60, 0)
    tests(n) = assert(.not. a % isValid(), 'datetime(2012, 3, 31, 0, 60, 0) is not valid')
    n = n + 1

    a = datetime(2012, 3, 31, 0, 0, 60)
    tests(n) = assert(.not. a % isValid(), 'datetime(2012, 3, 31, 0, 0, 60) is not valid')
    n = n + 1

    a = datetime(2012, 3, 31, 0, 0, 0, 1000)
    tests(n) = assert(.not. a % isValid(), 'datetime(2012, 3, 31, 0, 0, 0, 1000) is not valid')
    n = n + 1

    print '(71("-"))'

    ! datetime % secondsSinceEpoch
    a = datetime(1970, 1, 1, 0, 0, 0)

    tests(n) = assert(a % secondsSinceEpoch() == 0, &
                      'datetime % secondsSinceEpoch(),  0 seconds')
    n = n + 1

    a = datetime(1970, 1, 1, 1, 0, 0)
    tests(n) = assert(a % secondsSinceEpoch() == 3600, &
                      'datetime % secondsSinceEpoch(),  1 hour')
    n = n + 1

    a = datetime(1969, 12, 31, 23, 0, 0)
    tests(n) = assert(a % secondsSinceEpoch() == -3600, &
                      'datetime % secondsSinceEpoch(),  -1 hour')
    n = n + 1

    print '(71("-"))'

    ! datetime % tzOffset

    a = datetime(2014, 1, 1, 0, 0, 0, tz=0._real64)
    tests(n) = assert(a % tzOffset() == '+0000', &
                      'datetime % tzOffset(),  +0000')
    n = n + 1

    a = datetime(2014, 1, 1, 0, 0, 0, tz=-3.5_real64)
    tests(n) = assert(a % tzOffset() == '-0330', &
                      'datetime % tzOffset(),  -0330')
    n = n + 1

    a = datetime(2014, 1, 1, 0, 0, 0, tz=5.75_real64)
    tests(n) = assert(a % tzOffset() == '+0545', &
                      'datetime % tzOffset(),  +0545')
    n = n + 1

    print '(71("-"))'

    ! datetime % utc()

    a = datetime(2014, 1, 1, 0, 0, 0, tz=0._real64)
    tests(n) = assert(a % utc() == a, 'datetime % utc(),  +0000')
    n = n + 1

    !a = datetime(2014, 1, 1, 0, 0, 0, tz=6.)
    !b = a-timedelta(hours=6)
    !b % tz = 0
    !tests(n) = assert(a % utc() == b, 'datetime % utc(),  +0600')
    !n = n + 1

    !a = datetime(2014, 1, 1, 0, 0, 0, tz=-6.)
    !b = a+timedelta(hours=6)
    !b % tz = 0
    !tests(n) = assert(a % utc() == b, 'datetime % utc(),  -0600')
    !!n = n + 1

    print '(71("-"))'

    ! datetime % weekday()

    a = datetime(2014, 1, 1)
    tests(n) = assert(a % weekday() == 3, 'datetime % weekday(),  Wednesday')
    n = n + 1

    a = datetime(2014, 1, 2)
    tests(n) = assert(a % weekday() == 4, 'datetime % weekday(),  Thursday')
    n = n + 1

    a = datetime(2014, 1, 3)
    tests(n) = assert(a % weekday() == 5, 'datetime % weekday(),  Friday')
    n = n + 1

    a = datetime(2014, 1, 4)
    tests(n) = assert(a % weekday() == 6, 'datetime % weekday(),  Saturday')
    n = n + 1

    a = datetime(2014, 1, 5)
    tests(n) = assert(a % weekday() == 0, 'datetime % weekday(),  Sunday')
    n = n + 1

    a = datetime(2014, 1, 6)
    tests(n) = assert(a % weekday() == 1, 'datetime % weekday(),  Monday')
    n = n + 1

    a = datetime(2014, 1, 7)
    tests(n) = assert(a % weekday() == 2, 'datetime % weekday(),  Tuesday')
    n = n + 1

    print '(71("-"))'

    ! datetime % isoweekday()

    a = datetime(2014, 1, 1)
    tests(n) = assert(a % isoweekday() == 3, 'datetime % isoweekday(),  Wednesday')
    n = n + 1

    a = datetime(2014, 1, 2)
    tests(n) = assert(a % isoweekday() == 4, 'datetime % isoweekday(),  Thursday')
    n = n + 1

    a = datetime(2014, 1, 3)
    tests(n) = assert(a % isoweekday() == 5, 'datetime % isoweekday(),  Friday')
    n = n + 1

    a = datetime(2014, 1, 4)
    tests(n) = assert(a % isoweekday() == 6, 'datetime % isoweekday(),  Saturday')
    n = n + 1

    a = datetime(2014, 1, 5)
    tests(n) = assert(a % isoweekday() == 7, 'datetime % isoweekday(),  Sunday')
    n = n + 1

    a = datetime(2014, 1, 6)
    tests(n) = assert(a % isoweekday() == 1, 'datetime % isoweekday(),  Monday')
    n = n + 1

    a = datetime(2014, 1, 7)
    tests(n) = assert(a % isoweekday() == 2, 'datetime % isoweekday(),  Tuesday')
    n = n + 1

    print '(71("-"))'

    ! datetime % weekdayLong()

    a = datetime(2014, 1, 1)
    tests(n) = assert(a % weekdayLong() == 'Wednesday', &
                      'datetime % weekdayLong(),  Wednesday')
    n = n + 1

    a = datetime(2014, 1, 2)
    tests(n) = assert(a % weekdayLong() == 'Thursday', &
                      'datetime % weekdayLong(),  Thursday')
    n = n + 1

    a = datetime(2014, 1, 3)
    tests(n) = assert(a % weekdayLong() == 'Friday', &
                      'datetime % weekdayLong(),  Friday')
    n = n + 1

    a = datetime(2014, 1, 4)
    tests(n) = assert(a % weekdayLong() == 'Saturday', &
                      'datetime % weekdayLong(),  Saturday')
    n = n + 1

    a = datetime(2014, 1, 5)
    tests(n) = assert(a % weekdayLong() == 'Sunday', &
                      'datetime % weekdayLong(),  Sunday')
    n = n + 1

    a = datetime(2014, 1, 6)
    tests(n) = assert(a % weekdayLong() == 'Monday', &
                      'datetime % weekdayLong(),  Monday')
    n = n + 1

    a = datetime(2014, 1, 7)
    tests(n) = assert(a % weekdayLong() == 'Tuesday', &
                      'datetime % weekdayLong(),  Tuesday')
    n = n + 1

    print '(71("-"))'

    ! datetime % isoweekdayLong()

    a = datetime(2014, 1, 1)
    tests(n) = assert(a % isoweekdayLong() == 'Wednesday', &
                      'datetime % isoweekdayLong(),  Wednesday')
    n = n + 1

    a = datetime(2014, 1, 2)
    tests(n) = assert(a % isoweekdayLong() == 'Thursday', &
                      'datetime % isoweekdayLong(),  Thursday')
    n = n + 1

    a = datetime(2014, 1, 3)
    tests(n) = assert(a % isoweekdayLong() == 'Friday', &
                      'datetime % isoweekdayLong(),  Friday')
    n = n + 1

    a = datetime(2014, 1, 4)
    tests(n) = assert(a % isoweekdayLong() == 'Saturday', &
                      'datetime % isoweekdayLong(),  Saturday')
    n = n + 1

    a = datetime(2014, 1, 5)
    tests(n) = assert(a % isoweekdayLong() == 'Sunday', &
                      'datetime % isoweekdayLong(),  Sunday')
    n = n + 1

    a = datetime(2014, 1, 6)
    tests(n) = assert(a % isoweekdayLong() == 'Monday', &
                      'datetime % isoweekdayLong(),  Monday')
    n = n + 1

    a = datetime(2014, 1, 7)
    tests(n) = assert(a % isoweekdayLong() == 'Tuesday', &
                      'datetime % isoweekdayLong(),  Tuesday')
    n = n + 1

    print '(71("-"))'

    ! datetime % weekdayShort()

    a = datetime(2014, 1, 1)
    tests(n) = assert(a % weekdayShort() == 'Wed', &
                      'datetime % weekdayShort(),  Wed')
    n = n + 1

    a = datetime(2014, 1, 2)
    tests(n) = assert(a % weekdayShort() == 'Thu', &
                      'datetime % weekdayShort(),  Thu')
    n = n + 1

    a = datetime(2014, 1, 3)
    tests(n) = assert(a % weekdayShort() == 'Fri', &
                      'datetime % weekdayShort(),  Fri')
    n = n + 1

    a = datetime(2014, 1, 4)
    tests(n) = assert(a % weekdayShort() == 'Sat', &
                      'datetime % weekdayShort(),  Sat')
    n = n + 1

    a = datetime(2014, 1, 5)
    tests(n) = assert(a % weekdayShort() == 'Sun', &
                      'datetime % weekdayShort(),  Sun')
    n = n + 1

    a = datetime(2014, 1, 6)
    tests(n) = assert(a % weekdayShort() == 'Mon', &
                      'datetime % weekdayShort(),  Mon')
    n = n + 1

    a = datetime(2014, 1, 7)
    tests(n) = assert(a % weekdayShort() == 'Tue', &
                      'datetime % weekdayShort(),  Tue')
    n = n + 1

    print '(71("-"))'

    ! datetime % isoweekdayShort()

    a = datetime(2014, 1, 1)
    tests(n) = assert(a % isoweekdayShort() == 'Wed', &
                      'datetime % isoweekdayShort(),  Wed')
    n = n + 1

    a = datetime(2014, 1, 2)
    tests(n) = assert(a % isoweekdayShort() == 'Thu', &
                      'datetime % isoweekdayShort(),  Thu')
    n = n + 1

    a = datetime(2014, 1, 3)
    tests(n) = assert(a % isoweekdayShort() == 'Fri', &
                      'datetime % isoweekdayShort(),  Fri')
    n = n + 1

    a = datetime(2014, 1, 4)
    tests(n) = assert(a % isoweekdayShort() == 'Sat', &
                      'datetime % isoweekdayShort(),  Sat')
    n = n + 1

    a = datetime(2014, 1, 5)
    tests(n) = assert(a % isoweekdayShort() == 'Sun', &
                      'datetime % isoweekdayShort(),  Sun')
    n = n + 1

    a = datetime(2014, 1, 6)
    tests(n) = assert(a % isoweekdayShort() == 'Mon', &
                      'datetime % isoweekdayShort(),  Mon')
    n = n + 1

    a = datetime(2014, 1, 7)
    tests(n) = assert(a % isoweekdayShort() == 'Tue', &
                      'datetime % isoweekdayShort(),  Tue')
    n = n + 1

    print '(71("-"))'

    ! datetime % yearday()

    a = datetime(2014, 1, 1)
    tests(n) = assert(a % yearday() == 1, &
                      'datetime(2014, 1, 1) % yearday() == 1')
    n = n + 1

    a = datetime(2014, 2, 1)
    tests(n) = assert(a % yearday() == 32, &
                      'datetime(2014, 2, 1) % yearday() == 32')
    n = n + 1

    a = datetime(2014, 3, 1)
    tests(n) = assert(a % yearday() == 60, &
                      'datetime(2014, 3, 1) % yearday() == 60')
    n = n + 1

    a = datetime(2014, 4, 1)
    tests(n) = assert(a % yearday() == 91, &
                      'datetime(2014, 4, 1) % yearday() == 91')
    n = n + 1

    a = datetime(2014, 5, 1)
    tests(n) = assert(a % yearday() == 121, &
                      'datetime(2014, 5, 1) % yearday() == 121')
    n = n + 1

    a = datetime(2014, 6, 1)
    tests(n) = assert(a % yearday() == 152, &
                      'datetime(2014, 6, 1) % yearday() == 152')
    n = n + 1

    a = datetime(2014, 7, 1)
    tests(n) = assert(a % yearday() == 182, &
                      'datetime(2014, 7, 1) % yearday() == 182')
    n = n + 1

    a = datetime(2014, 8, 1)
    tests(n) = assert(a % yearday() == 213, &
                      'datetime(2014, 8, 1) % yearday() == 213')
    n = n + 1

    a = datetime(2014, 9, 1)
    tests(n) = assert(a % yearday() == 244, &
                      'datetime(2014, 9, 1) % yearday() == 244')
    n = n + 1

    a = datetime(2014, 10, 1)
    tests(n) = assert(a % yearday() == 274, &
                      'datetime(2014, 10, 1) % yearday() == 275')
    n = n + 1

    a = datetime(2014, 11, 1)
    tests(n) = assert(a % yearday() == 305, &
                      'datetime(2014, 11, 1) % yearday() == 305')
    n = n + 1

    a = datetime(2014, 12, 1)
    tests(n) = assert(a % yearday() == 335, &
                      'datetime(2014, 12, 1) % yearday() == 335')
    n = n + 1

    print '(71("-"))'

    ! Timedelta tests
    tests(n) = assert(timedelta() == timedelta(0, 0, 0, 0, 0), &
                      'timedelta empty constructor')
    n = n + 1

    td = timedelta(milliseconds=1)
    tests(n) = assert(td % total_seconds() >= 1D-3-eps &
                .AND. td % total_seconds() <= 1D-3+eps, &
                'timedelta % total_seconds(),  milliseconds conversion')
    n = n + 1

    td = timedelta(seconds=1)
    tests(n) = assert(td % total_seconds() == 1, &
                     'timedelta % total_seconds(),  seconds conversion')
    n = n + 1

    td = timedelta(minutes=1)
    tests(n) = assert(td % total_seconds() == 60, &
                     'timedelta % total_seconds(),  minutes conversion')
    n = n + 1

    td = timedelta(hours=1)
    tests(n) = assert(td % total_seconds() == 3600, &
                     'timedelta % total_seconds(),  hours conversion')
    n = n + 1

    td = timedelta(days=1)
    tests(n) = assert(td % total_seconds() == 86400, &
                     'timedelta % total_seconds(),  days conversion')
    n = n + 1

    print '(71("-"))'

    ! Test date2num and num2date

    a = a % now()
    tests(n) = assert(a % utc() == num2date(date2num(a)), &
                      'datetime % utc() == num2date(date2num(datetime)) (now)')
    n = n + 1

    ! Test for overflowing month
    a = datetime(2014, 11, 30, 1)
    tests(n) = assert(a == num2date(date2num(a)), &
                      'datetime == num2date(date2num(datetime)) (overflowing month)')
    n = n + 1

    ! Test for overflowing year
    a = datetime(2014, 12, 31, 1)
    tests(n) = assert(a == num2date(date2num(a)), &
                      'datetime == num2date(date2num(datetime)) (overflowing year)')
    n = n + 1

    print '(71("-"))'

    ! datetimeRange tests

    allocate(dtRange(3))

    dtRange = [datetime(2014, 1, 1), &
               datetime(2014, 1, 2), &
               datetime(2014, 1, 3)]
    tests(n) = assert(ALL(datetimeRange(datetime(2014, 1, 1), &
                                        datetime(2014, 1, 3), &
                                        timedelta(days=1)) &
                          == dtRange),                      &
                      'datetimeRange,  day increment')
    n = n + 1

    dtRange = [datetime(2014, 1, 1, 0), &
               datetime(2014, 1, 1, 1), &
               datetime(2014, 1, 1, 2)]
    tests(n) = assert(ALL(datetimeRange(datetime(2014, 1, 1, 0), &
                                        datetime(2014, 1, 1, 2), &
                                        timedelta(hours=1))  &
                          == dtRange),                        &
                      'datetimeRange,  hour increment')
    n = n + 1

    dtRange = [datetime(2014, 1, 1, 0, 0), &
               datetime(2014, 1, 1, 0, 1), &
               datetime(2014, 1, 1, 0, 2)]
    tests(n) = assert(ALL(datetimeRange(datetime(2014, 1, 1, 0, 0), &
                                        datetime(2014, 1, 1, 0, 2), &
                                        timedelta(minutes=1))  &
                          == dtRange),                          &
                      'datetimeRange,  minute increment')
    n = n + 1

    dtRange = [datetime(2014, 1, 1, 0, 0, 0), &
               datetime(2014, 1, 1, 0, 0, 1), &
               datetime(2014, 1, 1, 0, 0, 2)]
    tests(n) = assert(ALL(datetimeRange(datetime(2014, 1, 1, 0, 0, 0), &
                                        datetime(2014, 1, 1, 0, 0, 2), &
                                        timedelta(seconds=1))    &
                          == dtRange),                            &
                      'datetimeRange,  second increment')
    n = n + 1

    deallocate(dtRange)
    allocate(dtRange(7))

    dtRange = [datetime(2012, 1, 1, 0, 0, 0), &
               datetime(2012, 1, 1, 1, 0, 0), &
               datetime(2012, 1, 1, 2, 0, 0), &
               datetime(2012, 1, 1, 3, 0, 0), &
               datetime(2012, 1, 1, 4, 0, 0), &
               datetime(2012, 1, 1, 5, 0, 0), &
               datetime(2012, 1, 1, 6, 0, 0)]
    tests(n) = assert(ALL(datetimeRange(datetime(2012, 1, 1, 0, 0, 0),        &
                                        datetime(2012, 1, 1, 6, 0, 0),        &
                                        timedelta(hours=1)) == dtRange) &
                      .AND. SIZE(datetimeRange(datetime(2012, 1, 1, 0, 0, 0), &
                                               datetime(2012, 1, 1, 6, 0, 0), &
                                               timedelta(hours=1)))     &
                         == SIZE(dtRange), 'datetimeRange,  rounding test')
    n = n + 1

    print '(71("-"))'

    ! isLeapYear tests

    tests(n) = assert(.not. isLeapYear(1), 'isLeapYear(1) == F')
    n = n + 1

    tests(n) = assert(isLeapYear(4), 'isLeapYear(4) == T')
    n = n + 1

    tests(n) = assert(.not. isLeapYear(100), 'isLeapYear(100) == F')
    n = n + 1

    tests(n) = assert(isLeapYear(400), 'isLeapYear(400) == T')
    n = n + 1

    tests(n) = assert(isLeapYear(2000), 'isLeapYear(2000) == T')
    n = n + 1

    tests(n) = assert(.not. isLeapYear(2014), 'isLeapYear(2014) == F')
    n = n + 1

    print '(71("-"))'

    ! daysInYear

    tests(n) = assert(daysInYear(2014) == 365, 'daysInYear(2014) == 365')
    n = n + 1

    tests(n) = assert(daysInYear(2012) == 366, 'daysInYear(2012) == 366')
    n = n + 1

    tests(n) = assert(daysInYear(2000) == 366, 'daysInYear(2000) == 366')
    n = n + 1

    tests(n) = assert(daysInYear(1900) == 365, 'daysInYear(1900) == 365')
    n = n + 1

    print '(71("-"))'

    ! daysInMonth

    tests(n) = assert(daysInMonth(1, 2014) == 31, 'daysInMonth(1, 2014) == 31')
    n = n + 1

    tests(n) = assert(daysInMonth(2, 2014) == 28, 'daysInMonth(2, 2014) == 28')
    n = n + 1

    tests(n) = assert(daysInMonth(2, 2012) == 29, 'daysInMonth(2, 2012) == 29')
    n = n + 1

    tests(n) = assert(daysInMonth(3, 2014) == 31, 'daysInMonth(3, 2014) == 31')
    n = n + 1

    tests(n) = assert(daysInMonth(4, 2014) == 30, 'daysInMonth(4, 2014) == 30')
    n = n + 1

    tests(n) = assert(daysInMonth(5, 2014) == 31, 'daysInMonth(5, 2014) == 31')
    n = n + 1

    tests(n) = assert(daysInMonth(6, 2014) == 30, 'daysInMonth(6, 2014) == 30')
    n = n + 1

    tests(n) = assert(daysInMonth(7, 2014) == 31, 'daysInMonth(7, 2014) == 31')
    n = n + 1

    tests(n) = assert(daysInMonth(8, 2014) == 31, 'daysInMonth(8, 2014) == 31')
    n = n + 1

    tests(n) = assert(daysInMonth(9, 2014) == 30, 'daysInMonth(9, 2014) == 30')
    n = n + 1

    tests(n) = assert(daysInMonth(10, 2014) == 31, 'daysInMonth(10, 2014) == 31')
    n = n + 1

    tests(n) = assert(daysInMonth(11, 2014) == 30, 'daysInMonth(11, 2014) == 30')
    n = n + 1

    tests(n) = assert(daysInMonth(12, 2014) == 31, 'daysInMonth(12, 2014) == 31')
    n = n + 1

    print '(71("-"))'

    ! clock tests

    ! Initialize a clock with an hourly tick interval
    c = clock(startTime    = datetime(2014, 1, 1, 0, 0, 0), &
              stopTime     = datetime(2014, 1, 2, 0, 0, 0), &
              currentTime  = datetime(2014, 1, 1, 0, 0, 0), &
              tickInterval = timedelta(hours=1))

    tests(n) = assert(.not. c % started, 'clock % started == F (before tick)')
    n = n + 1

    call c % tick()

    tests(n) = assert(c % started, 'clock % started == T (after 1 tick)')
    n = n + 1

    tests(n) = assert(.not. c % stopped, 'clock % stopped == F (after 1 tick)')
    n = n + 1

    ! Tick 23 times
    do i = 1, 23
      call c % tick()
    end do

    tests(n) = assert(c % currentTime == c % stopTime, &
                      'clock % currentTime == clock % stopTime (after 24 ticks)')
    n = n + 1

    tests(n) = assert(c % stopped, 'clock % stopped == T (after 24 ticks)')
    n = n + 1

    ! Reset clock
    call c % reset()

    tests(n) = assert(.not. c % started, 'clock % started == F (after reset)')
    n = n + 1

    tests(n) = assert(.not. c % started, 'clock % stopped == F (after reset)')
    n = n + 1

    tests(n) = assert(c % currentTime == c % startTime, &
                      'clock % currentTime == clock % startTime (after reset)')
    n = n + 1

    print '(71("-"))'

    test_failed = .false.

    call report_tests(tests, test_failed)

    if (test_failed) stop 1

  end subroutine test_datetime

end module datetime_tests


program run_tests
  use datetime_tests, only: test_datetime
  call test_datetime()
end program run_tests
