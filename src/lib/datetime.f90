!
! datetime-fortran - A Fortran library for date and time manipulation
! Copyright (c) 2013-2017, Wavebit Scientific LLC
! All rights reserved.
! 
! Licensed under the BSD-3 clause license. See LICENSE for details.
!
module datetime_module
!=======================================================================
!
! Description: A Fortran module that provides time and date manipulation 
!              facilities. Conforms to Fortran 2003 standard.
!
! Contains:
!
!     types: 
!
!         datetime  - Main datetime object
!         timedelta - Time difference object
!         clock     - A generic clock container
!         tm_struct - For compatibility with C/C++ procedures 
!
!     datetime methods:
!
!         procedure :: getYear
!         procedure :: getMonth
!         procedure :: getDay
!         procedure :: getHour
!         procedure :: getMinute
!         procedure :: getSecond
!         procedure :: getMillisecond
!         procedure :: isocalendar
!         procedure :: isoformat
!         procedure :: isoweekday
!         procedure :: isoweekdayShort
!         procedure :: isoweekdayLong
!         procedure :: isValid
!         procedure :: now
!         procedure :: secondsSinceEpoch
!         procedure :: strftime
!         procedure :: tm
!         procedure :: tzOffset
!         procedure :: utc
!         procedure :: weekday
!         procedure :: weekdayShort
!         procedure :: weekdayLong
!         procedure :: yearday
!
!     timedelta methods:
!
!         procedure :: getDays
!         procedure :: getHours
!         procedure :: getMinutes
!         procedure :: getSeconds
!         procedure :: getMilliseconds
!         procedure :: total_seconds
!
!     clock methods:
!
!         procedure :: reset
!         procedure :: tick
!
!     public procedures:
!
!         function c_strftime
!         function c_strptime
!         function date2num
!         function datetimeRange
!         function daysInMonth
!         function daysInYear
!         function isLeapYear
!         function num2date
!         function strptime
!         function tm2date
!
!=======================================================================

use mod_datetime
use mod_timedelta
use mod_clock
use mod_strftime

!=======================================================================
endmodule datetime_module
