data N = Z | S N
-- Z = 0 
-- S (S ( S N)) = 3 

data MSExp a =  Empty | 
                Entry N a | -- n es la cantidad de apariciones de a 
                AddAll N (MSExp a) | -- agrega n apariciones 
                Union (MSExp a) (MSExp a) -- une las apariciones de los elementos del primero con los del segundo  


-- EJERCICIO 1
-- Definir por recursion explicita las siguientes funciones:

-- Retorna el significado del multiset dado. 
evalMSE :: Eq a => MSExp a -> a -> N 
evalMSE Empty           y = Z 
evalMSE (Entry n x)     y = if x == y then n else Z 
evalMSE (AddAll n ms)   y = addN n (evalMSE ms y) 
evalMSE (Union ms1 ms2) y = addN (evalMSE ms1 y) (evalMSE ms2 y)

addN :: N -> N -> N  
addN Z     m = m 
addN (S n) m = S (addN n m) 


-- Dada una funcion de actualziacion de numeros naturales y un multioset naturalizado, describe el multiset que resulta de actualizar
-- todas las ocurrencias de los elementos actuales segund  la funcion dada.
-- Por ejemplo, si la funcion incrementa el natural en 1, es equivalente a que todos los elementos existentes incorporan una ocurrencia mas.
-- {{a, a, b}} = {{a, a, a, b, b}}
updateOcurrsMSE :: (N -> N) -> MSExp a -> MSExp a 
updateOcurrsMSE f Empty           = Empty
updateOcurrsMSE f (Entry n x)     = Entry (f n) x 
updateOcurrsMSE f (Union ms1 ms2) = Union (updateOcurrsMSE f ms1) (updateOcurrsMSE f ms2)


-- Describe el resultado de la normalizacion del multiset dado.
-- Un multiset esta normalizado si no contiene ocurrencias del contrstructor AddAll, 
-- y si ningun constructor Union tiene como argumento un constructor Empty. 
normMSE :: MSExp a -> MSExp a 
normMSE Empty           = Empty
normMSE (Entry n x)     = Entry n x 
normMSE (AddAll n ms)   = updateOcurrsMSE (addN n) (normMSE ms)
--                      = normAddAll n (normMSE ms)
normMSE (Union ms1 ms2) = normUnion (normMSE ms1) (normMSE ms2)

-- normAddAll :: N -> MSExp a -> MSExp a 
-- normAddAll m Empty           = Empty
-- normAddAll m (Entry n x)     = Entry (addN m n) x 
-- normAddAll m (Union ms1 ms2) = Union (normAddAll f ms1) (normAddAll f ms2)

normUnion :: MSExp a -> MSExp a -> MSExp a 
normUnion Empty ms2 = ms2 
normUnion ms1 Empty = ms1 
normUnion ms1 ms2   = Union ms1 ms2 


-- Describe la cantidad de "fallas" que tiene el multiset dado para estar normalziado, 
-- (o sea, la cantidad de constructores AddAll ademas de la cantidad de constructores 
-- Empty que son argumento de una Union). Observar que si cantFallas m = 0, entonces m esta normalizado.
cantFallas :: MSExp a -> Int 
cantFallas Empty           = 0
cantFallas (Entry n x)     = 0 
cantFallas (AddAll n ms)   = 1 + cantFallas ms 
cantFallas (Union ms1 ms2) = unoSiHayEmpty ms1 ms2 + cantFallas ms1 + cantFallas ms2 
 
unoSiHayEmpty :: MSExp a -> MSExp a -> Int 
unoSiHayEmpty Empty _ = 1 
unoSiHayEmpty _ Empty = 1 
unoSiHayEmpty _ _     = 0 


-- EJERCICIO 2
foldMSE :: b -> (N -> a -> b) -> (N -> b -> b) -> (b -> b -> b) -> MSExp a -> b 
foldMSE z e a u Empty           = z 
foldMSE z e a u (Entry n x)     = e n x 
foldMSE z e a u (AddAll n ms)   = a n (foldMSE z e a u ms) 
foldMSE z e a u (Union ms1 ms2) = u (foldMSE z e a u ms1) (foldMSE z e a u ms2)

recMSE :: b -> (N -> a -> b) -> (N -> b -> MSExp a -> b) -> (b -> MSExp a ->  b -> MSExp a -> b) -> MSExp a -> b 
recMSE z e a u Empty           = z 
recMSE z e a u (Entry n x)     = e n x 
recMSE z e a u (AddAll n ms)   = a n (recMSE z e a u ms) ms 
recMSE z e a u (Union ms1 ms2) = u (recMSE z e a u ms1) ms1 (recMSE z e a u ms2) ms2 


-- EJERCICIO 3 
-- Escribir las versiones del ejecicio 1 sin ulizar recursion explicita. 
 
evalMSE' :: Eq a => MSExp a -> a -> N 
evalMSE' = foldMSE  (const Z) 
                    (\n x -> \y -> if x == y then n else Z)
                    (\n f -> \y -> addN n (f y))
                    (\f g -> \y -> addN (f y) (g y))

updateOcurrsMSE' :: (N -> N) -> MSExp a -> MSExp a 
updateOcurrsMSE' = flip (foldMSE    (const Empty)
                                    (\n x -> \f -> Entry (f n) x)
                                    (\_ _ -> \_ -> error "...")
                                    (\g h -> \f -> Union (g f) (h f)))

normMSE' :: MSExp a -> MSExp a 
normMSE' = foldMSE Empty Entry (updateOcurrsMSE . addN) normUnion

cantFallas' :: MSExp a -> Int 
cantFallas' = recMSE 0 
                    (\_ _ -> 0) 
                    (\_ m _ -> 1 + m) 
                    (\n1 ms1 n2 ms2 -> unoSiHayEmpty ms1 ms2 + n1 + n2)