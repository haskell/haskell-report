module Time (
	ClockTime, 
	Month(January,February,March,April,May,June,
	      July,August,September,October,November,December),
	Day(Sunday,Monday,Tuesday,Wednesday,Thursday,Friday,Saturday),
	CalendarTime(CalendarTime, ctYear, ctMonth, ctDay, ctHour, ctMin,
		     ctSec, ctPicosec, ctWDay, ctYDay, 
                     ctTZName, ctTZ, ctIsDST),
	TimeDiff(TimeDiff, tdYear, tdMonth, tdDay, tdHour,
		 tdMin, tdSec, tdPicosec),
	getClockTime, addToClockTime, diffClockTimes,
        toCalendarTime, toUTCTime, toClockTime,
        calendarTimeToString, formatCalendarTime ) where

import Ix(Ix)

data ClockTime = ...			-- Implementation-dependent
instance Ord  ClockTime where ...
instance Eq   ClockTime where ...

data Month =  January   | February | March    | April
           |  May       | June     | July     | August
           |  September | October  | November | December
           deriving (Eq, Ord, Enum, Bounded, Ix, Read, Show)

data Day   =  Sunday | Monday  | Tuesday  | Wednesday | Thursday 
           |  Friday | Saturday
           deriving (Eq, Ord, Enum, Bounded, Ix, Read, Show)

data CalendarTime = CalendarTime {
		ctYear  			:: Int,
	        ctMonth 			:: Month,
	        ctDay, ctHour, ctMin, ctSec 	:: Int,
		ctPicosec			:: Integer,
		ctWDay   			:: Day,
		ctYDay		     		:: Int,
		ctTZName	 		:: String,
		ctTZ         			:: Int,
		ctIsDST				:: Bool
	} deriving (Eq, Ord, Read, Show)

data TimeDiff = TimeDiff {
		tdYear, tdMonth, tdDay, tdHour, tdMin, tdSec :: Int,
		tdPicosec				     :: Integer
	} deriving (Eq, Ord, Read, Show)
