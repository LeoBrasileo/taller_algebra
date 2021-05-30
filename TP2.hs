{-# LANGUAGE FlexibleContexts #-}

type Posicion = [Int]
type Jugada = (Int,Int)

seleccionarEnLista :: [Int] -> Int -> Int
seleccionarEnLista xs p = xs!!p

seleccionarPila :: Jugada -> Int
seleccionarPila (a,b) = a - 1
seleccionarFichas :: Jugada -> Int
seleccionarFichas (a,b) = b

quitarPila :: Posicion -> Int -> Posicion
quitarPila [] n = []
quitarPila l n | n == head l = tail l
               | otherwise = head l : quitarPila (tail l) n

jugar :: Posicion -> Jugada -> Posicion
jugar p j | valorEnPila - fichasASacar <= 0 = quitarPila p valorEnPila 
          | otherwise = take pilaAModificar p ++ [valorEnPila - fichasASacar] ++ drop (pilaAModificar + 1) p
        where (valorEnPila, pilaAModificar, fichasASacar) = (seleccionarEnLista p pilaAModificar, seleccionarPila j, seleccionarFichas j)
