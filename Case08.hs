{-# LANGUAGE FlexibleContexts #-}
--listasOrdenadas 2 [1,2,3] -> [[1,2],[2,3],[1,3]]

listasOrdenadas :: Int -> [Int] -> Set ([Int])
listasOrdenadas 0 _ = [[]]
listasOrdenadas k xs
    | length (xs) < k = undefined
    | length (xs) == k = [xs]
    | otherwise = union (listasOrdenadas k (sacarUltimo xs)) (listasOrdenadas (k-1) sacarUltimo)

sacarUltimo :: [Int] -> [Int]
sacarUltimo xs = reverse (tail (reverse xs))