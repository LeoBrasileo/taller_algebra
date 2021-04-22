{-# LANGUAGE FlexibleContexts #-}
menorDivisorDesde :: Int -> Int -> Int
menorDivisorDesde n k | n `mod` k == 0 = k
                      | otherwise = menorDivisorDesde n (k+1)

menorDivisor :: Int -> Int 
menorDivisor n = menorDivisorDesde n 2

esPrimo :: Int -> Bool
esPrimo n | n <= 1 = error "No se puede clacular primos de n menor que 1"
          | (n == menorDivisor n) = True
          | otherwise = False 

ultimoPrimoAnterior :: Int -> Int 
--ultimoPrimoAnterior 1 = 1
ultimoPrimoAnterior n | esPrimo n = n
                      | otherwise = ultimoPrimoAnterior (n - 1)

esPar :: Int -> Bool
esPar n = n `mod` 2 == 0

nEsParYMayor :: Int -> Bool
nEsParYMayor n = esPar n && n > 3

cumpleGoldbachParaEsePrimo :: Int -> Int -> Bool
cumpleGoldbachParaEsePrimo n k = esPrimo(n-k)

cumpleGoldbach :: Int -> Int -> Bool
cumpleGoldbach n 1 = False 
cumpleGoldbach n k | cumpleGoldbachParaEsePrimo n k = True 
                   | otherwise = cumpleGoldbach n ultPrimo
                   where (ultPrimo) = (ultimoPrimoAnterior(k))

satisfaceGoldbach :: Integer -> Bool
satisfaceGoldbach n | nEsParYMayor nInt == False = False
                    | otherwise = cumpleGoldbach nInt (ultimoPrimoAnterior(nInt-2))
                    where (nInt) = fromInteger n