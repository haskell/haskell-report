-- Functions on times
getClockTime         :: IO ClockTime
		     
addToClockTime       :: TimeDiff  -> ClockTime -> ClockTime
diffClockTimes       :: ClockTime -> ClockTime -> TimeDiff
		     
toCalendarTime       :: ClockTime    -> IO CalendarTime
toUTCTime            :: ClockTime    -> CalendarTime
toClockTime          :: CalendarTime -> ClockTime
calendarTimeToString :: CalendarTime -> String
formatCalendarTime   :: TimeLocale -> String -> CalendarTime -> String
