{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

type Posicion = [Int]
type Jugada = (Int,Int)
type Binario = [Int]

jugar :: Posicion -> Jugada -> Posicion
jugar (p:ps) (i,j) | i == 1 && j >= p = ps
                   | i == 1 && j < p = p-j : ps
                   | otherwise = p : jugar ps (i-1,j)

-- pila -> valor en pila -> [Jugada]
posiblesJugadasPorPila :: Int -> Int -> [Jugada] -> [Jugada]
posiblesJugadasPorPila p v list | v >= 1 = (p,v) : list ++ posiblesJugadasPorPila p (v-1) list
                                | otherwise = list

posiblesJugadasConIndice :: Posicion -> Int -> [Jugada] -> [Jugada]
posiblesJugadasConIndice pos i jugadas | length pos >= i = jugadas ++ posiblesJugadasPorPila i (pos!!(i-1)) [] ++ posiblesJugadasConIndice pos (i+1) jugadas
                                       | otherwise = jugadas

posiblesJugadas :: Posicion -> [Jugada]
posiblesJugadas p = posiblesJugadasConIndice p 1 []

esGanadora :: Posicion -> [Jugada] -> Bool
esGanadora ps [] = False
esGanadora ps (j:js) | not (esGanadora (jugar ps j) (posiblesJugadas (jugar ps j))) = True
                     | otherwise = esGanadora ps js

esPosicionGanadora :: Posicion -> Bool
esPosicionGanadora ps = esGanadora ps (posiblesJugadas ps)

jugadaGanadoraConPosiblesJugadas :: Posicion -> [Jugada] -> Jugada
jugadaGanadoraConPosiblesJugadas ps (j:js) | not (esPosicionGanadora (jugar ps j)) = j
                                           | otherwise = jugadaGanadoraConPosiblesJugadas ps js

jugadaGanadora :: Posicion -> Jugada
jugadaGanadora ps | not (esPosicionGanadora ps) = error "No existen posibles jugadas ganadoras"
                  | otherwise = jugadaGanadoraConPosiblesJugadas ps (posiblesJugadas ps)

--busco la cantidad de jugadas que hay que dejan al adversario en una posicion perdedora
jugadaGanadoraCount :: Posicion -> [Jugada] -> Int
jugadaGanadoraCount ps [] = 0
jugadaGanadoraCount ps (j:js) | not (esPosicionGanadora (jugar ps j)) = 1 + jugadaGanadoraCount ps js
                              | otherwise = jugadaGanadoraCount ps js

numeroDeJugadasGanadoras :: Posicion -> Int
numeroDeJugadasGanadoras ps = jugadaGanadoraCount ps (posiblesJugadas ps)

--Funciones recuperatorio

aBinario :: Int -> Binario
aBinario 0 = []
aBinario n | n `mod` 2 == 1 = aBinario (n `div` 2) ++ [1]
           | n `mod` 2 == 0 = aBinario (n `div` 2) ++ [0]

aDecimal :: Binario -> Int
aDecimal [] = 0
aDecimal bs = (last bs) + 2 * aDecimal (init bs)

mismaLongitud :: Binario -> Binario -> (Binario, Binario)
mismaLongitud ns ms | length ns < length ms = mismaLongitud ([0] ++ ns) ms
                    | length ns > length ms = mismaLongitud ns ([0] ++ ms)
                    | otherwise = (ns, ms)

xorbin :: Binario -> Binario -> Binario
xorbin [] [] = []
xorbin (n:ns) (m:ms) | n /= m = [1] ++ xorbin ns ms
                     | otherwise = [0] ++ xorbin ns ms

xor :: Int -> Int -> Int
xor n m = aDecimal (xorbin nF mF)
        where (nF,mF) = mismaLongitud (aBinario n) (aBinario m)

xorLista :: [Int] -> Int
xorLista [] = 0
xorLista (x:xs) = xor x (xorLista xs)

esPosicionGanadoraViaFormulaCerrada :: Posicion -> Bool
esPosicionGanadoraViaFormulaCerrada ps = xorLista ps /= 0
