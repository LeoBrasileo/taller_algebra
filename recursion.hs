{-# LANGUAGE FlexibleContexts #-}
potencia :: Int -> Int -> Int
potencia n 0 = 1
potencia n k = n * (potencia n (k-1))