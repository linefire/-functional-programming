module Gauss
    (
    load_matrix,
    makezero,
    findmax,
    solve
    ) where

import System.IO
import Data.List
import Control.Concurrent
import Control.Monad.Cont
import Control.Exception
import Debug.Trace

-- строка (список)
type Row = [Float]
-- матрица (список строк)
type Matrix = [Row]

-- точность (при сравнении с нулем)
eps :: Float
eps = 0.00000001

-- чтение целого числа
hGetInt :: Handle -> IO Int
hGetInt h = do  
  s <- hGetLine h
  return (read s)

-- чтение числа c плавающей запятой
hGetFloat :: Handle -> IO Float
hGetFloat h = do  
  s <- hGetLine h
  return (read s)

-- загрузить систему уравнений из файла
load_matrix :: String -> IO Matrix
load_matrix filename =
  catch
    (do 
      -- открыть файл
      h <- openFile filename ReadMode
      catch
        (do 
           -- прочитать количество уравнений
           n <- hGetInt h
           -- прочитать коэффициенты и записать из в список списков (Matrix)
           m <- (mapM (\x -> 
                 (mapM (\x -> hGetFloat h ) [0..n]) :: IO Row) 
                 [0..n-1]) :: IO Matrix
           hClose h
           return m)
        -- в случае ошибки закрыть файл и вернуть пустой список
        (\e -> do
          let _ = (e :: IOException)
          hClose h
          return [])
        )
    -- в случае ошибки закрыть файл и вернуть пустой список
    (\e -> do 
       let _ = (e :: IOException)
       return []
       )

-- получить 0 в строке row1 в колонке i
makezero :: Row -> Row -> Int -> Row
makezero row1 row2 i =
  zipWith (\x y -> x - y*k) row1 row2
  where k = (row1!!i)
  
-- поток, связанный со строкой матрицы  
rowThread :: Row -> Int -> [MVar Float] -> MVar (Maybe Int) -> MVar Row 
       -> [MVar (Maybe Float)] -> QSem -> QSemN -> QSemN -> IO ()
rowThread row irow col_vars piv_ind_var piv_var x_vars in_sync out_sync stop_sync = do 
  let 
    -- количество строк матрицы (уравнений)
    n = length row - 1
    
    -- прямой ход метода Гаусса
    reduce :: Row -> Int -> IO ()
    reduce row1 icol = do
      -- ожидание входного семафора
      waitQSem in_sync
    
      -- передать элемент из столбца col
      putMVar (col_vars!!irow) (row1!!icol)

      -- чтение номер ведущей строки
      piv_ind <- readMVar piv_ind_var
      case piv_ind of
        -- нет ведущей строки - прекратить вычисления
        Nothing -> do
          return ()

        Just ind ->
          if ind == irow
            -- если данная строка ведущая - получить единицу в колонке ico
            then pivot row1 icol
            
            -- иначе получить 0 в колонке icol
            else do
              piv_row <- readMVar piv_var
              let row2 = makezero row1 piv_row icol
              -- освобождение выходного семафора
              signalQSemN out_sync 1
              reduce row2 (icol+1)

    -- обработка ведущей строки
    pivot :: Row -> Int -> IO ()
    pivot row1 icol = do
      -- получить единицу в колонке icol
      let row2 = map (\x -> x / (row1!!icol)) row1
      -- передать значения ведущей строки
      putMVar piv_var row2

      -- вычислить значение переменной с индексом icol (с ожиданием требуемых переменных)
      xi <- calcxi row2 icol
      -- передача знанения переменной всем потокам
      putMVar (x_vars!!icol) xi
      
      return ()
          
    -- обратный ход метода Гаусса: вычислить значение переменной 
    -- с индексом icol (с ожиданием требуемых переменных)
    calcxi :: Row -> Int -> IO (Maybe Float)
    calcxi row2 icol =
      calcxi2 (row2!!n) 0
      where calcxi2 s j
              | j == n = return (Just s)
              | j < n = do
                 let a = row2!!j
                 -- не нулевой коэффициент в строке (не в ведущем столбце)
                 if (j /= icol && abs(a) > eps)
                    then do 
                      -- ожидание переменной xj
                      mxj <- readMVar $ x_vars!!j
                      case mxj of
                        -- нет решения - прекратить вычисления
                        Nothing -> 
                          return Nothing
                        Just xj ->
                          -- подставить переменную xj в уравнение
                          calcxi2 (s - a * xj) (j+1)
                    else 
                      calcxi2 s (j+1)
               | otherwise = return $ Just 0
    
  reduce row 0
  signalQSemN stop_sync 1
  return ()
  
-- поиск максимального по модулю числа в списке и возврат соотвествующего индекса
-- параметры: список чисел и список их индексов  
findmax :: [Float] -> [Int] -> (Int, Float)
findmax (x:lst) (i:ilst) =
  findmax2 x i lst ilst
  where
    findmax2 mx mi [] _ = (mi, mx)
    findmax2 mx mi _ [] = (mi, mx)
    findmax2 mx mi (x2:lst2) (i2:ilst2)
      | (abs x2) > (abs mx) = findmax2 x2 i2 lst2 ilst2  -- число больше по модулю
      | otherwise = findmax2 mx mi lst2 ilst2
findmax _ _ = (-1, 0)      


-- решение системы уравнений методом Гаусса
solve :: Matrix -> IO Row
solve matr = do
    -- количество строк матрицы (уравнений)
    let n = length matr

    -- переменные и семафоры для взаимодействия потоков:
    -- элементы ведущего столбца 
    col_vars <- replicateM n $ newEmptyMVar
    -- индекст ведущей строки
    piv_ind_var <- newEmptyMVar
    -- ведущая строка (список значений)
    piv_var <- newEmptyMVar
    -- семафоры начала итерации прямого хода метода Гаусса
    in_syncs <- replicateM n $ newQSem 1
    -- семафоры завершения итерации прямого хода метода Гаусса
    out_sync <- newQSemN 0
    -- значения переменных xi
    x_vars <- replicateM n $ newEmptyMVar
    -- семафор для ожидания завершения потоков
    stop_sync <- newQSemN 0

    -- запук потоков для каждой строки матрицы
    forM_ [0..n-1] $ \i -> forkIO $ rowThread (matr!!i) i
                       col_vars piv_ind_var piv_var x_vars
                       (in_syncs!!i) out_sync stop_sync
    
    let
      gauss :: Int -> [Int] -> IO Bool
      gauss i unused
        | i == n = return True
        | i < n = do
            -- получить i-й столбец матрицы (только неиспользованные ранее строки)
            col <- mapM (\x -> takeMVar $ col_vars!!x) unused
            let (mi, mx) = findmax col unused
                -- удалить строку из списка неиспользованных
                unused2 = delete mi unused
                
            if (abs(mx) < eps)
              then do
                -- вырожденная система - нет решения
                putMVar piv_ind_var Nothing
                -- передача знанения невычисленных переменных всем потокам (передается Nothing)
                mapM_ (\x -> putMVar (x_vars!!x) Nothing) unused
                return False
              else do
                -- передать потокам номер ведущей строки
                putMVar piv_ind_var (Just mi)
                 
                -- ожидание семафора потоков (заверешния итерации)
                waitQSemN out_sync $ length unused2
                    
                _ <- takeMVar piv_ind_var
                _ <- takeMVar piv_var
                    
                -- освобождение семафоров (начало следующей итерации)
                mapM_ (\x -> signalQSem $ in_syncs!!x) unused2
                
                gauss (i+1) unused2
        | otherwise = return False
                
    -- решение системы уравнений методом Гаусса
    solved <- gauss 0 [0..n-1]
    -- анализ результата
    result <- if (solved) 
      -- если решено - сформировать список [x0, x1, ...]
      then mapM (\x -> do
          mxi <- takeMVar $ x_vars!!x
          return $ case mxi of
            Nothing -> 0
            Just xi -> xi
          ) [0..n-1]
      -- нет решения - вернуть пустой список
      else 
        return []

    -- ожидание завершения потоков
    waitQSemN stop_sync n
    
    return result
        
