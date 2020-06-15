module Lib
    ( someFunc,
      firsttask,
      makeCombinations
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- задача 1
-- генеруємо всі можливі комбінації
-- фільтруємо лише ті, що підходять
firsttask n = filter filterPairings (makeCombinations n)


-- згенерувати всі комбінації потрібної довжини
makeCombinations 0 = return []
makeCombinations length = do
    x <- [1..3]
    xs <- makeCombinations (length-1)
    return (x : xs)

-- True якщо сума кожних сусідніх елементів не більша 4
filterPairings:: [Int] -> Bool
filterPairings [] = True
filterPairings (x:[]) = True
filterPairings (x:y:xs) | x+y > 4 = False
                        | otherwise = filterPairings (y:xs)




-- задача 2
-- тіло функції звісно буде складнішим, але задачею було описати тип цієї функції
checkCond :: [a] -> (a -> Bool) -> Bool
checkCond x f = f (x!!0)
                {-
                тіло  функції у псевдокоді
                for (a : x)
                    if (f a)
                        return True
                return False
                -}


