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
nEsParYMayor n = esPar n && n > 2

--k + h = n
--k y h son los numeros primos y n es el numero par resultado de la suma
--teniendo en cuenta esa formula n - k o h debe ser primo si o si
esSumaDePrimosParaEsePrimo :: Int -> Int -> Bool
esSumaDePrimosParaEsePrimo n k = esPrimo(n-k)

esSumaDePrimos :: Int -> Int -> Bool
esSumaDePrimos n 1 = False 
esSumaDePrimos n k | esSumaDePrimosParaEsePrimo n k = True 
                   | otherwise = esSumaDePrimos n ultPrimo
                   where (ultPrimo) = (ultimoPrimoAnterior(k))

satisfaceGoldbach :: Integer -> Bool
satisfaceGoldbach n = nEsParYMayor nInt && esSumaDePrimos nInt (ultimoPrimoAnterior(nInt-2))
                    where (nInt) = fromInteger n

verificarConjeturaHasta :: Integer -> Bool
verificarConjeturaHasta 4 = True
verificarConjeturaHasta n | satisfaceGoldbach(n) == False = False
                          | otherwise = verificarConjeturaHasta (n - 2)