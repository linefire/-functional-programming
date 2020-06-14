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

-- ������ (������)
type Row = [Float]
-- ������� (������ �����)
type Matrix = [Row]

-- �������� (��� ��������� � �����)
eps :: Float
eps = 0.00000001

-- ������ ������ �����
hGetInt :: Handle -> IO Int
hGetInt h = do  
  s <- hGetLine h
  return (read s)

-- ������ ����� c ��������� �������
hGetFloat :: Handle -> IO Float
hGetFloat h = do  
  s <- hGetLine h
  return (read s)

-- ��������� ������� ��������� �� �����
load_matrix :: String -> IO Matrix
load_matrix filename =
  catch
    (do 
      -- ������� ����
      h <- openFile filename ReadMode
      catch
        (do 
           -- ��������� ���������� ���������
           n <- hGetInt h
           -- ��������� ������������ � �������� �� � ������ ������� (Matrix)
           m <- (mapM (\x -> 
                 (mapM (\x -> hGetFloat h ) [0..n]) :: IO Row) 
                 [0..n-1]) :: IO Matrix
           hClose h
           return m)
        -- � ������ ������ ������� ���� � ������� ������ ������
        (\e -> do
          let _ = (e :: IOException)
          hClose h
          return [])
        )
    -- � ������ ������ ������� ���� � ������� ������ ������
    (\e -> do 
       let _ = (e :: IOException)
       return []
       )

-- �������� 0 � ������ row1 � ������� i
makezero :: Row -> Row -> Int -> Row
makezero row1 row2 i =
  zipWith (\x y -> x - y*k) row1 row2
  where k = (row1!!i)
  
-- �����, ��������� �� ������� �������  
rowThread :: Row -> Int -> [MVar Float] -> MVar (Maybe Int) -> MVar Row 
       -> [MVar (Maybe Float)] -> QSem -> QSemN -> QSemN -> IO ()
rowThread row irow col_vars piv_ind_var piv_var x_vars in_sync out_sync stop_sync = do 
  let 
    -- ���������� ����� ������� (���������)
    n = length row - 1
    
    -- ������ ��� ������ ������
    reduce :: Row -> Int -> IO ()
    reduce row1 icol = do
      -- �������� �������� ��������
      waitQSem in_sync
    
      -- �������� ������� �� ������� col
      putMVar (col_vars!!irow) (row1!!icol)

      -- ������ ����� ������� ������
      piv_ind <- readMVar piv_ind_var
      case piv_ind of
        -- ��� ������� ������ - ���������� ����������
        Nothing -> do
          return ()

        Just ind ->
          if ind == irow
            -- ���� ������ ������ ������� - �������� ������� � ������� ico
            then pivot row1 icol
            
            -- ����� �������� 0 � ������� icol
            else do
              piv_row <- readMVar piv_var
              let row2 = makezero row1 piv_row icol
              -- ������������ ��������� ��������
              signalQSemN out_sync 1
              reduce row2 (icol+1)

    -- ��������� ������� ������
    pivot :: Row -> Int -> IO ()
    pivot row1 icol = do
      -- �������� ������� � ������� icol
      let row2 = map (\x -> x / (row1!!icol)) row1
      -- �������� �������� ������� ������
      putMVar piv_var row2

      -- ��������� �������� ���������� � �������� icol (� ��������� ��������� ����������)
      xi <- calcxi row2 icol
      -- �������� �������� ���������� ���� �������
      putMVar (x_vars!!icol) xi
      
      return ()
          
    -- �������� ��� ������ ������: ��������� �������� ���������� 
    -- � �������� icol (� ��������� ��������� ����������)
    calcxi :: Row -> Int -> IO (Maybe Float)
    calcxi row2 icol =
      calcxi2 (row2!!n) 0
      where calcxi2 s j
              | j == n = return (Just s)
              | j < n = do
                 let a = row2!!j
                 -- �� ������� ����������� � ������ (�� � ������� �������)
                 if (j /= icol && abs(a) > eps)
                    then do 
                      -- �������� ���������� xj
                      mxj <- readMVar $ x_vars!!j
                      case mxj of
                        -- ��� ������� - ���������� ����������
                        Nothing -> 
                          return Nothing
                        Just xj ->
                          -- ���������� ���������� xj � ���������
                          calcxi2 (s - a * xj) (j+1)
                    else 
                      calcxi2 s (j+1)
               | otherwise = return $ Just 0
    
  reduce row 0
  signalQSemN stop_sync 1
  return ()
  
-- ����� ������������� �� ������ ����� � ������ � ������� ��������������� �������
-- ���������: ������ ����� � ������ �� ��������  
findmax :: [Float] -> [Int] -> (Int, Float)
findmax (x:lst) (i:ilst) =
  findmax2 x i lst ilst
  where
    findmax2 mx mi [] _ = (mi, mx)
    findmax2 mx mi _ [] = (mi, mx)
    findmax2 mx mi (x2:lst2) (i2:ilst2)
      | (abs x2) > (abs mx) = findmax2 x2 i2 lst2 ilst2  -- ����� ������ �� ������
      | otherwise = findmax2 mx mi lst2 ilst2
findmax _ _ = (-1, 0)      


-- ������� ������� ��������� ������� ������
solve :: Matrix -> IO Row
solve matr = do
    -- ���������� ����� ������� (���������)
    let n = length matr

    -- ���������� � �������� ��� �������������� �������:
    -- �������� �������� ������� 
    col_vars <- replicateM n $ newEmptyMVar
    -- ������� ������� ������
    piv_ind_var <- newEmptyMVar
    -- ������� ������ (������ ��������)
    piv_var <- newEmptyMVar
    -- �������� ������ �������� ������� ���� ������ ������
    in_syncs <- replicateM n $ newQSem 1
    -- �������� ���������� �������� ������� ���� ������ ������
    out_sync <- newQSemN 0
    -- �������� ���������� xi
    x_vars <- replicateM n $ newEmptyMVar
    -- ������� ��� �������� ���������� �������
    stop_sync <- newQSemN 0

    -- ����� ������� ��� ������ ������ �������
    forM_ [0..n-1] $ \i -> forkIO $ rowThread (matr!!i) i
                       col_vars piv_ind_var piv_var x_vars
                       (in_syncs!!i) out_sync stop_sync
    
    let
      gauss :: Int -> [Int] -> IO Bool
      gauss i unused
        | i == n = return True
        | i < n = do
            -- �������� i-� ������� ������� (������ ���������������� ����� ������)
            col <- mapM (\x -> takeMVar $ col_vars!!x) unused
            let (mi, mx) = findmax col unused
                -- ������� ������ �� ������ ����������������
                unused2 = delete mi unused
                
            if (abs(mx) < eps)
              then do
                -- ����������� ������� - ��� �������
                putMVar piv_ind_var Nothing
                -- �������� �������� ������������� ���������� ���� ������� (���������� Nothing)
                mapM_ (\x -> putMVar (x_vars!!x) Nothing) unused
                return False
              else do
                -- �������� ������� ����� ������� ������
                putMVar piv_ind_var (Just mi)
                 
                -- �������� �������� ������� (���������� ��������)
                waitQSemN out_sync $ length unused2
                    
                _ <- takeMVar piv_ind_var
                _ <- takeMVar piv_var
                    
                -- ������������ ��������� (������ ��������� ��������)
                mapM_ (\x -> signalQSem $ in_syncs!!x) unused2
                
                gauss (i+1) unused2
        | otherwise = return False
                
    -- ������� ������� ��������� ������� ������
    solved <- gauss 0 [0..n-1]
    -- ������ ����������
    result <- if (solved) 
      -- ���� ������ - ������������ ������ [x0, x1, ...]
      then mapM (\x -> do
          mxi <- takeMVar $ x_vars!!x
          return $ case mxi of
            Nothing -> 0
            Just xi -> xi
          ) [0..n-1]
      -- ��� ������� - ������� ������ ������
      else 
        return []

    -- �������� ���������� �������
    waitQSemN stop_sync n
    
    return result
        
