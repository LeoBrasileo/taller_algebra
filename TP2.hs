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

esPar :: Int -> Bool
esPar n = n `mod` 2 == 0

pilaMasGrande :: Posicion -> Int
pilaMasGrande ps = head (filter ((== maximum ps) . (ps !!)) [0..]) + 1

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
                                    where valorEnPila = seleccionarEnLista pos (i-1)

posiblesJugadas :: Posicion -> [Jugada]
posiblesJugadas p = posiblesJugadasConIndice p 1 []

--la jugada perdedora por default es [n,n] con n cualquier natural
--[n,n] siempre es perdedora
sePuedeLlevarAPosPerdedora :: Posicion -> Bool -> Bool
sePuedeLlevarAPosPerdedora ps e | length ps > 2 = sePuedeLlevarAPosPerdedora (jugar ps jugadaMasGrande) (not e)
                                | length ps == 2 && lasPilasSonIguales ps = e
                                | otherwise = sePuedeLlevarAPosPerdedora (llevarAPosPerdedoraFinal ps) (not e)
                        where jugadaMasGrande = jugadaMasGrandeEnPila ps 1

--cuando la posicion ya es [n1,n2]
llevarAPosPerdedoraFinal :: Posicion -> Posicion
llevarAPosPerdedoraFinal ps | primerValor > segundoValor = jugar ps (1, primerValor - segundoValor)
                            | otherwise = jugar ps (2, segundoValor - primerValor)
                          where (primerValor,segundoValor) = (seleccionarEnLista ps 0, seleccionarEnLista ps 1)

lasPilasSonIguales :: Posicion -> Bool
lasPilasSonIguales ps = seleccionarEnLista ps 0 == seleccionarEnLista ps 1

--si queda una sola pila podes asegurarte ganar
--si no hay jugadas posibles es perdedora
--si hay 2 o mas pilas se puede probar que hay una jugada que garantiza ganar, esto se hace llegando a una posicion perdedora para el otro jugador
--si hay un numero impar de pilas con cantidades diferentes es una posicion perdedora
esPosicionGanadora :: Posicion -> Bool
esPosicionGanadora ps | length ps == 1 = True
                      | null (posiblesJugadas ps) = False
                      | length ps >= 2 = sePuedeLlevarAPosPerdedora ps False

--Posicion -> pila -> valor mas grande en pila
jugadaMasGrandeEnPila :: Posicion -> Int -> Jugada
jugadaMasGrandeEnPila ps p = (p,seleccionarEnLista ps (p-1))

--Busco la jugada ganadora cuando quedan solo dos pilas
jugarAPosPerdedoraFinal :: Posicion -> Jugada
jugarAPosPerdedoraFinal ps | lasPilasSonIguales ps = error "La jugada es perdedora"
                           | primerValor > segundoValor = (1,primerValor - segundoValor)
                           | primerValor < segundoValor = (2,segundoValor - primerValor)
                          where (primerValor,segundoValor) = (seleccionarEnLista ps 0, seleccionarEnLista ps 1)

jugadaParaSacarPilaDiferente :: Posicion -> Jugada
jugadaParaSacarPilaDiferente ps | length ps > 3 = jugadaParaSacarPilaDiferente (jugar ps (1,primerValor)) -- se hay mas de 3 pilas, configuro una posicion solo con 3
                                | primerValor == segundoValor && segundoValor == tercerValor = (1,primerValor)
                                | primerValor == segundoValor = (3, tercerValor)
                                | primerValor == tercerValor = (2, segundoValor)
                                | segundoValor == tercerValor = (1, primerValor)
                              where (primerValor,segundoValor, tercerValor) = (seleccionarEnLista ps 0, seleccionarEnLista ps 1, seleccionarEnLista ps 2)

--cuando queda 1 pila la jugada gandora es agarrar todas las fichas
--cuando quedan 2 pilas la jugada ganadora es llevar al otro jugadar a un posicion perddedora [n,n]
--caundo quedan mas de dos pilas:
--con un numero impar de pilas (3), la jugada ganadora es sacar todas las fichas en la pila diferente
--si hay un numero par de pilas, la jugada ganadora es sacar todas las fichas en la pila más grande
jugadaGanadora :: Posicion -> Jugada
jugadaGanadora ps | length ps == 1 = (1,head ps)
                  | length ps == 2 = jugarAPosPerdedoraFinal ps
                  | length ps > 2 && esPosicionGanadora ps && esPar (length ps) = (pilaAModificar, seleccionarEnLista ps (pilaAModificar-1))
                  | length ps > 2 && esPosicionGanadora ps && not(esPar (length ps)) = jugadaParaSacarPilaDiferente ps
                  | otherwise = error "la posicion no es ganadora"
                  where pilaAModificar = pilaMasGrande ps
