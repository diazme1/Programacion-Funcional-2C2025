data (Ord a) => PrefixT a = Nil                           -- un árbol vacío
                          | Node a                        -- Un árbol con un prefijo a.
                                (PrefixT a)               -- Un subárbol con prefijos menores que a.
                                (PrefixT a)               -- Un subárbol con prefijos cuyo primer elemento es a.
                                (PrefixT a) deriving Show -- Un subárbol con prefijos mayores que a. 

-- a) 
defined :: Ord a => PrefixT a -> [a] -> Bool
-- Que retorna verdadero si la cadena dada es un prefijo definido en el árbol (note que la cadena vacía es prefijo de cualquier cadena). 
defined Nil xs               = case xs of 
                                [] -> True
                                _  -> False
defined (Node a p1 p2 p3) xs = case xs of
                                []      -> True
                                (x:xs') -> if x==a then defined p2 xs' else if x<a then defined p1 xs else defined p3 xs

-- b)
insert :: Ord a => PrefixT a -> [a] -> PrefixT a
-- Que inserta una nueva cadena al árbol (definiendo todos sus prefijos).
insert Nil xs               = case xs of 
                                []      -> Nil
                                (x:xs') -> Node x Nil (insert Nil xs') Nil 
insert (Node a p1 p2 p3) xs = case xs of
                                []      -> Node a p1 p2 p3
                                (x:xs') -> if x==a then Node a p1 (insert p2 xs') p3 else if x<a then Node a (insert p1 (x:xs')) p2 p3 else Node a p1 p2 (insert p3 (x:xs'))     

-- c)
join :: Ord a => PrefixT a -> PrefixT a -> PrefixT a
-- Que une dos arboles de prefijos, es decir, el arbol resultado contiene todos los prefijos definidos en los argumentos.
join pre1 pre2 = insertAll' pre1 (toList pre2)

insertAll' :: Ord a => PrefixT a -> [[a]] -> PrefixT a
insertAll' p []     = p
insertAll' p (x:xs) = insertAll' (insert p x) xs

toList :: Ord a => PrefixT a -> [[a]]
toList Nil               = []
toList (Node a p1 p2 p3) = [a] : (agregarATodos a (toList p2)) ++ toList p1 ++ toList p3

agregarATodos :: a -> [[a]] -> [[a]]
-- Describe la lista dada pero con el elemento dado agregado en cada lista de la lista dada.
agregarATodos x []       = []
agregarATodos x (xs:xss) = (x:xs) : agregarATodos x xss

-- d)
delete :: Ord a => PrefixT a -> [a] -> PrefixT a
-- Que elimina del arbol de prefijos todos los elementos con el prefijo dado.
delete Nil xs               = Nil
delete (Node a p1 p2 p3) xs = case xs of
                                []      -> Nil
                                (x:xs') -> if x==a 
                                            then case xs' of
                                                    [] -> (join p1 p3)
                                                    _  -> Node a p1 (delete p2 xs') p3
                                            else if x<a 
                                                then Node a (delete p1 xs) p2 p3
                                                else Node a p1 p2 (delete p3 xs) 



-- Ejercicio 2

foldPrefixT :: Ord a => b -> (a -> b -> b -> b -> b) -> PrefixT a -> b
foldPrefixT ni no Nil               = ni
foldPrefixT ni no (Node a p1 p2 p3) = no a (foldPrefixT ni no p1) (foldPrefixT ni no p2) (foldPrefixT ni no p3)

-- a) 
flatten :: Ord a => PrefixT a -> [[a]]
-- Que retorna todos los prefijos en el arbol en orden lexicográfico y sin repetidos.
flatten = foldPrefixT [] (\x xss1 xss2 xss3 -> ([x] : (agregarATodos x xss2)) ++ xss1 ++ xss3)


-- b)
insertAll :: Ord a => [[a]] -> PrefixT a
-- Que dada una lista de cadenas construye el árbol con todas las cadenas y sus prefijos definidos.
insertAll = foldr (\xs pr -> insert pr xs) Nil


-- c)
union :: Ord a => [PrefixT a] -> PrefixT a
-- Que dada una lista de árboles retorna un árbol que tiene definido los prefijos de cada árbol en la secuencia.
union = foldr (\p r -> join p r) Nil


-- d)
intersect :: Ord a => PrefixT a -> PrefixT a -> PrefixT a
-- Que dados dos árboles retorna un árbol con los prefijos que estan en ambos árboles.
intersect = foldPrefixT (\_ -> Nil) (\a f1 f2 f3 -> \pre2 -> case pre2 of
                                                                    Nil               -> Nil
                                                                    (Node x t1 t2 t3) -> if x==a 
                                                                                            then Node x (f1 t1) (f2 t2) (f3 t3) 
                                                                                            else Nil)


-- e)
palindromes :: Ord a => PrefixT a -> PrefixT a
-- Que construye un árbol con todos los prefijos que son un palindromo de algun prefijo definido en el arbol de
-- entrada (una cadena es un palindromo de otra cuando al invertirla resulta igual, por ejemplo "neuquen" y "anana" (sin considerar las tildes).
palindromes p = insertAll (filter isPalindrome (flatten p))  

isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = xs == reverse xs


-- f)
trim :: Ord a => [[a]] -> [[a]]
-- Que dada una lista de cadenas retorna una de longitud menor o igual donde las cadenas prefijas de otras han
-- sido eliminadas. Por ejemplo, ["prefijo", "pre", "cadena", "cadena"] debería retornar ["prefijo", "cadena"], pues
-- "pre" es prefijo de "prefijo" y "cadena" es prefijo de "cadena".
trim = foldl' (\rss xs -> if esPrefijoEn xs rss then rss else rss ++ [xs]) []

esPrefijoEn :: Ord a => [a] -> [[a]] -> Bool
esPrefijoEn xs = any (esPrefijo xs)

esPrefijo :: Ord a => [a] -> [a] -> Bool
esPrefijo xs ys = xs == take (length xs) ys

foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' f z []     = z
foldl' f z (x:xs) = foldl' f (f z x) xs


-- g)
longest :: Ord a => PrefixT a -> Int
-- Que retorna la longitud del camino más largo entre dos nodos cualesquiera.
longest = foldPrefixT 0 (\_ n1 n2 n3 -> max (1 + n2) (max n1 n3))



-- Ejercicio 3

-- ¿uncurry couple . decouple = id?
-- Por ppio. de extensionalidad.
-- ¿Para todo ab. (uncurry couple . decouple) ab = id ab?
-- Por def. de (.), es equivalente.
-- ¿Para todo ab. uncurry couple (decouple ab) = id ab?
-- Por def. de id, es equivalente.
-- ¿Para todo ab. uncurry couple (decouple ab) = ab?
-- Sea a :: AB a b un tree cualquiera. Por ppio. de inducción sobre la estructura de a.

-- Caso Base, a = Leaf b.
--      ¿uncurry couple (decouple (Leaf b)) = Leaf b?

-- Caso Inductivo, a = NodeAB x ab1 ab2
--      HI1: ¡uncurry couple (decouple ab1) = ab1!
--      HI2: ¡uncurry couple (decouple ab2) = ab2!
--      TI: ¿uncurry couple (decouple (NodeAB x ab1 ab2)) = NodeAB x ab1 ab2?


-- Dem: Caso Base.
-- LI.
--      uncurry couple (decouple (Leaf b))
--                     -------------------
--  =                                                       (Por def. de decouple)
--      uncurry couple (skeleton (Leaf b), extract (Leaf b))
--                      -----------------
--  =                                                       (skeleton.1)
--      uncurry couple (EmptyT, extract (Leaf b))
--                              ----------------
--  =                                                       (extract.1)
--      uncurry couple (EmptyT, Tip b)
--      ------------------------------
--  =                                                       (Por def. de uncurry)
--      couple EmptyT (Tip b)
--      ---------------------
--  =                                                       (couple.1)
--      Leaf b

-- Como LI y LD son iguales, queda demostrado para el caso base.


-- Dem: Caso Inductivo.
-- LI.
--      uncurry couple (decouple (NodeAB x ab1 ab2))
--                     -----------------------------
--  =                                                                                                       (Por def. de decouple)
--      uncurry couple (skeleton (NodeAB x ab1 ab2), extract (NodeAB x ab1 ab2))
--                     ----------------------------
--  =                                                                                                       (skeleton.2)
--      uncurry couple (NodeT x (skeleton ab1) (skeleton ab2), extract (NodeAB x ab1 ab2))
--                                                             --------------------------
--  =                                                                                                       (extract.2)
--      uncurry couple (NodeT x (skeleton ab1) (skeleton ab2), Join (extract ab1) (extract ab2))
--      ----------------------------------------------------------------------------------------
--  =                                                                                                       (Por def. de uncurry)
--      couple (NodeT x (skeleton ab1) (skeleton ab2)) (Join (extract ab1) (extract ab2))
--      ---------------------------------------------------------------------------------
--  =                                                                                                       (couple.2)
--      NodeAB x (couple (skeleton ab1) (extract ab1)) (couple (skeleton ab2) (extract ab2))
--               ------------------------------------- -------------------------------------
--  =                                                                                                       (Por def. de uncurry)
--      NodeAB x (uncurry couple (skeleton ab1, extract ab1)) (uncurry couple (skeleton ab2, extract ab2)) 
--                               ---------------------------                  ---------------------------
--  =                                                                                                       (Por def. de decouple, 2 veces)
--      Node AB x (uncurry couple (decouple ab1)) (uncurry couple (decouple ab2))
--                ------------------------------- -------------------------------
--  =                                                                                                       (HI1 y HI2)
--      Node AB x ab1 ab2

-- Como LI y LD son iguales, queda demostrado para el caso inductivo. 