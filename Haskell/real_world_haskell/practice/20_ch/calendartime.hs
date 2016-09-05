-- Example of CalendarTime from 'System.Time'

data CalendarTime = CalendarTime
  { ctYear    :: Int,         -- Year (post-Gregorian)
    ctMonth   :: Month,
    ctDay     :: Int,          -- Day of the month (1 to 31)
    ctHour    :: Int,         -- Hour of the day (0 to 23)
    ctMin     :: Int,          -- Minutes (0 to 59)
    ctSec     :: Int,          -- Seconds (0 to 61, allowing for leap seconds)
    ctPicosec :: Integer,  -- Picoseconds
    ctWDay    :: Day,         -- Day of the week
    ctYDay    :: Int,         -- Day of the year (0 to 364 or 365)
    ctTZName  :: String,    -- Name of timezone
    ctTZ      :: Int,           -- Variation from UTC in seconds
    ctIsDST   :: Bool        -- True if Daylight Saving Time in effect
  }

data Month = January | February | March | April | May | June
             | July | August | September | October | November | December

data Day = Sunday | Monday | Tuesday | Wednesday
           | Thursday | Friday | Saturday
