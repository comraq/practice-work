{-# LANGUAGE DeriveDataTypeable #-}

import Data.Dynamic
import qualified Control.Exception as E

data SqlError = SqlError {
  seState       :: String
, seNativeError :: Int
, seErrorMsg    :: String
} deriving (Eq, Show, Read, Typeable)

instance E.Exception SqlError

{-
 - Note: We need to define our own 'catchSql' and 'handleSql' functions that
 -       can be used ot catch an exception that is of type 'SqlError'. This is
 -       because regular 'catch' and 'handle' function scannot catch 'SqlError',
 -       because it is not a type of 'Exception'
 -}

{-
 - Execute the given IO action. If it raises a 'SqlError', then execute the
 - supplied handler and return its return value. Otherwise, proceed as
 - normal.
 -}
catchSql :: IO a -> (SqlError -> IO a) -> IO a
catchSql = E.catch

-- Like 'catchSql', with the order of arguments reversed
handleSql :: (SqlError -> IO a) -> IO a -> IO a
handleSql = flip catchSql

{-
 - Catches 'SqlError's, and re-raises them as IO errors with fail.
 - This is useful is you don't care to catch SQL errors, but want to see a
 - sane error message if one happens. One would often use this as a
 - high0level wrapper around SQL calls.
 -}
handleSqlError :: IO a -> IO a
handleSqlError action = catchSql action handler
  where handler e = fail ("SQL error: " ++ show e)

throwSqlError :: String -> Int -> String -> a
throwSqlError state nativeerror errormsg =
  E.throw $ SqlError state nativeerror errormsg

throwSqlErrorIO :: String -> Int -> String -> IO a
throwSqlErrorIO state nativeerror errormsg =
  E.evaluate $ throwSqlError state nativeerror errormsg
