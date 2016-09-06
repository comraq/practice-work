module DbInfo where

import Database.HDBC
import Database.HDBC.MySQL

{-
 - To establish a connection:
 -
 - > connectMySQL :: MySQLConnectInfo -> IO Connection
 - > conn <- connectMySQL dbInfo
 -}
dbInfo :: MySQLConnectInfo
dbInfo = defaultMySQLConnectInfo {
  mysqlHost     = "127.0.0.1"
, mysqlUser     = "adam"
, mysqlPassword = "password"
, mysqlDatabase = "test"
}
