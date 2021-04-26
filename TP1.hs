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
ultimoPrimoAnterior n | esPrimo n = n
                      | otherwise = ultimoPrimoAnterior (n - 1)
ultimoPrimoSiguiente :: Int -> Int 
ultimoPrimoSiguiente n | esPrimo n = n
                       | otherwise = ultimoPrimoSiguiente (n + 1)

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

esSumaDePrimosInt :: Int -> Int -> Int
esSumaDePrimosInt n 1 = 0 
esSumaDePrimosInt n k | esSumaDePrimosParaEsePrimo n k = k 
                      | otherwise = esSumaDePrimosInt n ultPrimo
                      where (ultPrimo) = (ultimoPrimoAnterior(k))

satisfaceGoldbach :: Integer -> Bool
satisfaceGoldbach n = nEsParYMayor nInt && esSumaDePrimos nInt (ultimoPrimoAnterior(nInt-2))
                    where (nInt) = fromInteger n

verificarConjeturaHasta :: Integer -> Bool
verificarConjeturaHasta 4 = True
verificarConjeturaHasta n | satisfaceGoldbach(n) == False = False
                          | otherwise = verificarConjeturaHasta (n - 2)

descomposicionEnPrimos :: Integer -> (Integer, Integer)
descomposicionEnPrimos n | satisfaceGoldbach n = (toInteger primoSumante, toInteger (n- toInteger primoSumante))
                         | otherwise = error "El numero no cumple con las condiciones de la conjetura"
                        where (nInt, ultPrimo, primoSumante) = (fromInteger n, ultimoPrimoAnterior(nInt-2), esSumaDePrimosInt nInt ultPrimo)

--Esta funcion me dice la cantidad de primos que hay desde k hasta n
cantidadPrimos :: Int -> Int -> Int -> Int  
cantidadPrimos n k i | n <= k = i
                     | otherwise = cantidadPrimos n sigPrimo i+1
                    where (sigPrimo) = (ultimoPrimoSiguiente (k+1))

