{-# LANGUAGE FlexibleContexts #-}
isPar :: Int -> Bool
isPar n = n `mod` 2 == 0

posicPrimerPar :: (Int, Int, Int) -> Int
posicPrimerPar (x, y, z) | mod x 2 == 0 = 1
                         | mod y 2 == 0 = 2
                         | mod z 2 == 0 = 3
                         |otherwise = 4

isRelated :: Float -> Float -> Bool
isRelated x y = (x <= 3 && y <= 3) || (x > 3 && x <= 7 && y > 3 && y <= 7)