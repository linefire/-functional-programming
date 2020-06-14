module Main where

import Indiv
import Database.HDBC
import Database.HDBC.ODBC


main :: IO ()
main = do
  putStrLn "\nИндивидуальная работа кафедры со студентами\n"
  
  -- подключение к базе данных
  --conn <- connectODBC "DSN=indiv"
  conn <- connectODBC "Driver={MySQL ODBC 8.0 Unicode Driver};Server=localhost;Port=3306;Uid=root;Pwd=;Database=indiv;Charset=utf8"
  
  menu conn
  
  disconnect conn
