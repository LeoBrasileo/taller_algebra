{-# LANGUAGE FlexibleContexts #-}

type Posicion = [Int]
type Jugada = (Int,Int)

seleccionarEnLista :: [Int] -> Int -> Int
seleccionarEnLista xs p = xs!!p
seleccionarEnJugadas :: [Jugada] -> Int -> Jugada
seleccionarEnJugadas xs p = xs!!p

seleccionarPila :: Jugada -> Int
seleccionarPila (a,b) = a - 1
seleccionarFichas :: Jugada -> Int
seleccionarFichas (a,b) = b

quitarPila :: Posicion -> Int -> Posicion
quitarPila [] n = []
quitarPila l n | n == head l = tail l
               | otherwise = head l : quitarPila (tail l) n

jugar :: Posicion -> Jugada -> Posicion
jugar p j | valorEnPila - fichasASacar <= 0 = quitarPila p valorEnPila --Resta las fichas de la pila
          | otherwise = take pilaAModificar p ++ [valorEnPila - fichasASacar] ++ drop (pilaAModificar + 1) p --Elimina la pila de la lista
        where (valorEnPila, pilaAModificar, fichasASacar) = (seleccionarEnLista p pilaAModificar, seleccionarPila j, seleccionarFichas j)

-- pila -> valor en pila -> [Jugada]
posiblesJugadasPorPila :: Int -> Int -> [Jugada] -> [Jugada]
posiblesJugadasPorPila p v list | v >= 1 = (p,v) : list ++ posiblesJugadasPorPila p (v-1) list
                                | otherwise = list

posiblesJugadasConIndice :: Posicion -> Int -> [Jugada] -> [Jugada]
posiblesJugadasConIndice pos i jugadas | length pos >= i = jugadas ++ posiblesJugadasPorPila i valorEnPila [] ++ posiblesJugadasConIndice pos (i+1) jugadas
                                       | otherwise = jugadas
                                    where (valorEnPila) = (seleccionarEnLista pos (i-1))

posiblesJugadas :: Posicion -> [Jugada]
posiblesJugadas p = posiblesJugadasConIndice p 1 []

--la jugada perdedora por default es [n,n] con n cualquier natural
--[n,n] siempre es perdedora
sePuedeLlevarAPosPerdedora :: Posicion -> Bool -> Bool
sePuedeLlevarAPosPerdedora ps e | length ps > 2 = sePuedeLlevarAPosPerdedora (jugar ps jugadaMasGrande) (not e)
                                | length ps == 2 && seleccionarEnLista ps 0 == seleccionarEnLista ps 1 = e
                                | otherwise = sePuedeLlevarAPosPerdedora (llevarAPosPerdedoraFinal ps) (not e)
                        where (jugadaMasGrande) = (jugadaMasGrandeEnPila ps 1)

--cuando la posicion ya es [n1,n2]
llevarAPosPerdedoraFinal :: Posicion -> Posicion
llevarAPosPerdedoraFinal ps | primerValor > segundoValor = jugar ps (1, primerValor - segundoValor)
                            | otherwise = jugar ps (2, segundoValor - primerValor)
                          where (primerValor,segundoValor) = (seleccionarEnLista ps 0, seleccionarEnLista ps 1)

--si queda una sola pila podes asegurarte ganar
--si no hay jugadas posibles es perdedora
--si hay 2 o mas pilas se puede probar que hay una jugada que garantiza ganar, esto se hace llegando a una posicion perdedora para el otro jugador
esPosicionGanadora :: Posicion -> Bool  
esPosicionGanadora ps | length ps == 1 = True 
                      | null (posiblesJugadas ps) = False
                      | length ps >= 2 = sePuedeLlevarAPosPerdedora ps False
                      | otherwise = esPosicionGanadora (jugar ps (1,1))

--Posicion -> pila -> valor mas grande en pila
jugadaMasGrandeEnPila :: Posicion -> Int -> Jugada
jugadaMasGrandeEnPila ps p = (p,seleccionarEnLista ps (p-1))
