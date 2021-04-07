{-# LANGUAGE FlexibleContexts #-}
esPar :: Int -> Bool
esPar n = n `mod` 2 == 0

posicPrimerPar :: (Int, Int, Int) -> Int
posicPrimerPar (x, y, z) | mod x 2 == 0 = 1
                         | mod y 2 == 0 = 2
                         | mod z 2 == 0 = 3
                         |otherwise = 4