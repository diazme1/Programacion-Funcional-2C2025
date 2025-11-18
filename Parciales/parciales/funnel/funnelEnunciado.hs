-- Tupla de elementos que cumplen o no un criterio respectivamente
type Partition a = ([a], [a]) 
-- Un criterio que, de cumplirse un predicado, aplica la primer funcion o la segunda en caso contrario
data Criteria a b = C (a -> Bool) (a -> b) (a -> b)
-- Una estructura linel no vacia que representa los criterios a utilizar que se aplican desde el ultimo al primero:
-- Ej: Step c3 (Step c2 (Initial c1)) aplica primero c1, luego c2, por ultimo c3
data Funnel a b = Initial (Criteria a b) | Step (Criteria a b) (Funnel a b) 


-- Devuelve una tupla cuya primera componente es partition, compuesto por los elementos que cumplen y no cumplen el criterio dado, 
-- y la segunda componente son los elementos que cumplen luego de aplicarse la funcion del criterio. 
partition :: Criteria a b -> [a] -> (Partition a, [b]) -- (([a], [a]), [b]) 
partition (C p f g) []     = (([],[]), [])
partition (C p f g) (x:xs) = let ((ts, fs), ys) = partition (C p f g) xs -- El resultado de la recursion 
                                in if p x -- si se cumple, proceso el primer elemento 
                                    then ((x:ts, fs), f x : ys)
                                    else ((ts, x:fs), g x : ys)

-- Aplica el nuevo criterio a los elementos que no cumplieron criterios anteriores (la parte de la partición que quedó sin procesar), 
-- y luego combina los resultados nuevos con los previos usando la función combinadora.
step :: Criteria a b -> ([b] -> b) -> (Partition a, [b]) -> (Partition a, [b])
step c f ((ts, fs), ys) = let ((ts', fs'), ys') = partition c fs -- los elementos de fs procesados con el nuevo criterio 
                            in ((ts ++ ts', fs'), f ys' : ys)

-- Para un elemento a, primero evalúa el primer criterio y luego, sobre el resultado b obtenido, 
-- aplica el segundo criterio para finalmente obtener c.
composeC :: Criteria a b -> Criteria b c -> Criteria a c
composeC (C p1 f1 g1) (C p2 f2 g2) = C (\x -> p1 x && p2 (f1 x)) (f2 . f1) (g2 . g1)


-- EJERCICIO 1 
-- Definir por recursion explicita las siguientes funciones:

-- a. Dado un funnel, una función que "reduce" (foldr) una lista de resultados, y una lista de tipo [a], 
-- retorna la particion de elementos a tras aplicar el funnel. 
appF :: Funnel a b -> ([b] -> b) -> [a] -> (Partition a, [b])

-- b. Computa el complemento de un funnel que filtra y transforma infomacion con los criterios del funnel dado negados. 
-- Tener en cuenta que al negra un criterio, los predicados de transformacion deben intercambiarse adecuadamente.    
complementF :: Funnel a b -> Funnel a b

-- c. Dado un funnel, retorna uno donde los criterios se aplican al reves. 
-- Pensar la solucion como una lista 
reverseF :: Funnel a b -> Funnel a b

-- d. Dado un funnel y una funcion b -> c, lo retorna mapeando sus funciones de a -> b por a -> c
mapF :: (b -> c) -> Funnel a b -> Funnel a c

-- e. Une dos funnel, paso a paso. Cada paso del primer funnel alimenta de datos al segundo, y si hay mas pasos en alguno de los dos, 
-- se ignoran. 
zipF :: Funnel a b -> Funnel b c -> Funnel a c


-- EJERCICIO 2 
-- Definir foldFunnel y recFunnel.


-- EJERCICIO 3
-- Escribir las versiones del ejecicio 1 sin ulizar recursion explicita. 


-- EJERCICIO 4 
-- ¿para todo fn. para todo f. para todo xs. appF fn f xs = appF (complementF (complementF fn)) f xs?