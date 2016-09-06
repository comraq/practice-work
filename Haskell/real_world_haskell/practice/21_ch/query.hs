import DbInfo (dbInfo)

import Database.HDBC
import Database.HDBC.MySQL

{-
 - Define a function that takes an integer representing the maximum id value
 - to look up. This will fetch all matching rows from the test database and
 - print them to the screen in a friendly format.
 -}
query :: Int -> IO ()
query maxId = do
    -- Connect to the database
    conn <- connectMySQL dbInfo

    -- Run the query and store the results in r
    r <- quickQuery' conn
         " SELECT id, description from test where id <= ? ORDER BY id, description"
         [toSql maxId]

    -- Convert each row into a String
    let stringRows = map convRow r

    -- Print the rows out
    mapM_ putStrLn stringRows

    -- Disconnect from the database
    disconnect conn

  {-
   - Note that since the 'id' column cannot be null, we can safely read an
   - Integer value. However, the 'description' column does not have this
   - constraint and thus we must extract out the value from 'Maybe'
   -}
  where convRow :: [SqlValue] -> String
        convRow [sqlId, sqlDesc] = show intid ++ ": " ++ desc
          where intid = (fromSql sqlId) :: Integer
                desc  = maybe "NULL" id $ fromSql sqlDesc

        convRow x                = fail $ "Unexpected result: " ++ show x

{-
 - Reading from the database can also be done lazily (using the functions
 - without the "'" suffix. However, note that the entire data set must be
 - finished reading before closing the connection or executing a new query.
 - Managing lazy reading can be quite complex and the strict versions are
 - usually safer/easier to use.
 -}
