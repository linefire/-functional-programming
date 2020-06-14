module Main where

import Gauss
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filename] -> do
      m <- load_matrix filename
      if null m 
        then putStrLn $ "Ошибка при загрузке файла " ++ show filename
        else do
          result <- solve m
          if null result
            then putStrLn "Нет решения"
            else putStrLn $ "Решение:" ++ show result
    _ -> putStrLn "Требуются параметр: <входной файл>"
