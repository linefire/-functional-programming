import Test.HUnit
import Gauss

main :: IO Counts
main = runTestTT $ TestList [

          TestCase $ assertEqual "makezero" 
            [4.4, 0.0, -7.8, -15.9]
            (makezero [4.4, 4, 2.2, 0.1] [0, 1, 2.5, 4.0] 1),
            
          TestCase $ assertEqual "findmax" 
            (3, -6.0)
            (findmax [5, 4, -6, 2, 0] [0, 1, 3, 4, 5]),
            
          TestCase ( do 
              x <- solve [[1.0, 2, -1, -4], [2, 3, -1, -11], [-2, 0, -3, 22]]
              assertEqual "solve" [-8.0,1.0,-2.0] x ),

          TestCase ( do 
              x <- solve [[1,2,-1,-4], [2,3,-1,-11], [2,3,-1,-11]]
              assertEqual "solve - no solution 1" [] x ),

          TestCase ( do 
              x <- solve [[2,3,-1,-11], [2,3,-1,-11], [2,3,-1,-11]]
              assertEqual "solve - no solution 2" [] x ),

          TestCase ( do 
              x <- load_matrix "test1.txt"
              assertEqual "load_matrix - test 1" 
                [[1.0, 2, -1, -4], [2, 3, -1, -11], [-2, 0, -3, 22]] 
                x ),

          TestCase ( do 
              x <- load_matrix "test2.txt"
              assertEqual "load_matrix - test 2" 
                [[1,2,-1,-4], [2,3.5,-1,-11], [2,3.5,-1,-11]] 
                x ),
              
          TestCase ( do 
              x <- load_matrix "test3.txt"
              assertEqual "load_matrix - test 3" 
                [] 
                x ),

          TestCase ( do 
              x <- load_matrix "test4.txt"
              assertEqual "load_matrix - test 4" 
                [] 
                x )
            
          ]

