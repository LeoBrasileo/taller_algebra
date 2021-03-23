{-# LANGUAGE FlexibleContexts #-}
g x y z = x + y + z * z
doble x = 2 * x
suma x y = x + y
normaVectorial x1 x2 = sqrt (x1^2 + x2^2)

funcionRara :: Float -> Float -> Bool -> Bool 
funcionRara x y z = (x >= y) || z
f n | n == 0 n = 1
    | n /= 0 n = 0