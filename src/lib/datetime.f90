!
! datetime-fortran - A Fortran library for date and time manipulation
! Copyright (c) 2013-2016, Wavebit Scientific LLC
! All rights reserved.
! 
! Licensed under the BSD-3 clause license. See LICENSE for details.
!
module datetime_module
!=======================================================================
!
! Version: 1.4.0
!
! Last update: 2016-01-26
!
! Author: Milan Curcic <mcurcic@wavebitscientific.com>
!         Wavebit Scientific LLC
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

use mod_datetime, only:datetime,date2num,datetimeRange,daysInMonth,&
                       daysInYear,isLeapYear,num2date,strptime,tm2date
use mod_timedelta,only:timedelta
use mod_clock,    only:clock
use mod_strftime, only:tm_struct,c_strftime,c_strptime

!=======================================================================
endmodule datetime_module
