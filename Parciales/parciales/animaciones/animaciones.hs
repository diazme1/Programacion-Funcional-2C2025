data Accion a = Paso a | SatarArriba a | SaltarAdelante a | Girar a 
    deriving Show 
type Tiempo = Int -- el instante en el que sucede un movimiento 
type Duracion = Int -- cantidad de tiempo que dura el movimiento 
data Animacion a =  Espera Duracion                   -- durante la duracion dada no hay espera
                    | Mov Duracion (Accion a)         -- un cierto movimiento con una duracion dada
                    | Sec (Animacion a) (Animacion a) -- secuencia (la segunda empieza al terminar la primera)
                    | Par (Animacion a) (Animacion a) -- paralelo (arrancan juntas y dura lo que la mas larga)
    deriving Show 

type Frame a = [Accion a] -- acciones simultaneas en un tiempo especifico 
type Simulador a = Tiempo -> Frame a  -- funcion que da las acciones que ocurren en un tiempo dado 


-- EJERCICIO 1 
-- Defirnir la sigueinte furncion de forma tal que cada elemento de cada lista sea considerado exactamente una vez
-- (o sea, se debe realizar un unico recorrido sobre cada lista).

-- Dadas dos listas de numeros sin repetidos, ordenadas de forma creciente, describe una lista ordenada sin repetidos que contiene los 
-- elementos de ambas listas. 
-- AYUDA: Es imprecindible tener en cuenta el orden de los elementos de las listas para poder cumplir con la exigencia de un unico recorrido.
combinarSinDuplicados :: [Int] -> [Int] -> [Int]
combinarSinDuplicados []     ys     = ys 
combinarSinDuplicados xs     []     = xs 
combinarSinDuplicados (x:xs) (y:ys) = if x == y 
                                        then x : combinarSinDuplicados xs ys 
                                        else if x < y 
                                                then x : combinarSinDuplicados xs (y:ys)
                                                else y : combinarSinDuplicados (x:xs) ys 


-- EJERCICIO 2 
-- a. Descibe la duracion total de la animacion (considerando que las acciones en paralelo terminan cuando termina la mas larga). 
duracion :: Animacion a -> Int 
duracion = foldA id (\d _ -> d) (+) (max)

-- b. Describe una animacion donde la duracion de cada accion esta multiplicada por un factor con respecta a la duracion original.
alargar :: Int -> Animacion a -> Animacion a 
alargar fac = foldA (\d     -> Espera (d * fac))
                    (\d acc -> Mov (d * fac) acc)
                    Sec 
                    Par

-- c. Describe una lista con un frame por cada tiempo de la duracion de la animacion. 
-- Cada frame es una lista con las acciones que ocurren simultaneamente en el tiempo indicado (una lista vacia indica que el personaje esta quieto).
-- AYUDA: puede usarse la funcion replicate :: Int -> a -> [a], que dado un numero n y un elemento, devuelve la lista con n copias de ese elemento.
simular :: Animacion a -> [Frame a]
simular = foldA (\d -> replicate d [])
                (\d acc -> replicate d [acc])
                (++)
                zipLong 

zipLong :: [[a]] -> [[a]] -> [[a]] 
zipLong = foldr (\xs f -> \yss -> case yss of 
                                    []        -> xs : f [] 
                                    (ys:yss') -> (xs ++ ys) : f yss')
                id

-- d. Describe una lista de los tiempos de la animacion donde no hay ninguna accion.
-- AYUDA 1: utilizar la funcion del ejercicio 1 y definir contarHasta :: Int -> [Int] que descibe la lista de numeros desde el 1 hasta el dado. 
-- AYUDA 2: al armar los tiempos de la secuencia, tener en cuenta que los tiempos de la 2da componente de la secuencia van despues de la 1era. 
tiempoDeEspera :: Animacion a -> [Tiempo]
tiempoDeEspera = recA contarHasta 
                        (\_ _ -> [])
                        (\ts1 a1 ts2 _ -> ts1 ++ map (+ (duracion a1)) ts2)
                        (\ts1 _ ts2 _ -> combinarSinDuplicados ts1 ts2)

foldN :: a -> (Int -> a -> a) -> Int -> a
foldN z f 0 = z 
foldN z f n = f n (foldN z f (n-1))

contarHasta :: Int -> [Int]
contarHasta = foldN [] (\n ns -> ns ++ [n])


-- EJERCICIO 4 
-- Dar los tipos y escribir los esquemas de recursion estructural y primitiva para Animacion.
foldA :: (Duracion -> b) -> (Duracion -> (Accion a) -> b) -> (b -> b -> b) -> (b -> b -> b) -> Animacion a -> b 
foldA e m s p (Espera d)  = e d 
foldA e m s p (Mov d acc) = m d acc  
foldA e m s p (Sec a1 a2) = s (foldA e m s p a1) (foldA e m s p a2) 
foldA e m s p (Par a1 a2) = p (foldA e m s p a1) (foldA e m s p a2) 

recA :: (Duracion -> b) -> (Duracion -> (Accion a) -> b) -> (b -> Animacion a -> b -> Animacion a -> b) -> (b -> Animacion a -> b -> Animacion a -> b) -> Animacion a -> b 
recA e m s p (Espera d)  = e d 
recA e m s p (Mov d acc) = m d acc 
recA e m s p (Sec a1 a2) = s (recA e m s p a1) a1 (recA e m s p a2) a2 
recA e m s p (Par a1 a2) = p (recA e m s p a1) a1 (recA e m s p a2) a2 


-- EJERCICIO 6

-- Escribir las siguientes funciones utilizando esquemas. 

-- a. Dada una animacion, describe un simulador en el que la animacion se repite infinitamente. 
ciclar :: Animacion a -> Simulador a 
ciclar a = let frames = simular a 
            in \n -> ciclarAux (mod n (length frames)) frames

ciclarAux :: Int -> [Frame a] -> Frame a 
ciclarAux = flip (foldr (\fr frs n -> if n == 0 then fr else frs (n-1))
                        (const []))

-- b. Dadas dos listas con la misma longitud, describe una Animacion que sonsiste en lña secuencia tal que que la animacion 
-- de cada posicion de una lista ocurre en simultaneo con la correspondiente en otra lista.  
combinar :: [Animacion a] -> [Animacion a] -> Animacion a
combinar xs ys = secuenciar (aplicarPar (zip xs ys))

aplicarPar :: [(Animacion a, Animacion a)] -> [Animacion a]
aplicarPar = map (\(a1, a2) -> Par a1 a2)

secuenciar :: [Animacion a] -> Animacion a 
secuenciar = foldr (\a ar -> Sec a ar) (Espera 0)

-- c. Describe una lista de frames con la duracion dada, donde en cada tiempo se observa la superposicion de los simuladores 
-- en la lista de entrda. 
mezclar :: [Simulador a] -> Duracion -> [Frame a]
mezclar ss d = map (\t -> fusionar t ss) (contarHasta d)

fusionar :: Int -> [Simulador a] -> Frame a
fusionar t ss = foldr (\s r -> s t ++ r) [] ss