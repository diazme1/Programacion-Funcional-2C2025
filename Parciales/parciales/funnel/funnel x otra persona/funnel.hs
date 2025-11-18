type Partition a = ([a], [a]) -- tupla de elementos que cumplen o no un criterio respectivamente
data Criteria a b = C (a -> Bool) (a -> b) (a -> b) -- un criterio que, de cumplirse un predicado, aplica la primer funcion; la segunda en caso contrario
data Funnel a b = Initial (Criteria a b) | Step (Criteria a b) (Funnel a b) {-- 
    una estructura linear no vacia que representa los criterios a utilizar que se aplican desde el ultimo: -}
    -- Ej: Step c3 (Step c2 (Initial c1)) aplica primero c1, luego c2, por ultimo c3

    -- Funnel Int Int =  Step (C esPrimo multPorDos multPor3)  (Initial (C esPar sumaleUno sumaleDos))

    -- partition (C esPrimo multPorDos multPor3) sum [1,2,6]
    -- = (([1,2], [6]), [2,4,18])

partition :: Criteria a b -> [a] -> (Partition a, [b])
partition (C p f g) [] = (([], []), [])
partition (C p f g) (x:xs) = let ((ss, ns), bs) = partition (C p f g) xs
                                in if p x
                                    then ((x:ss, ns), (f x):bs)
                                    else ((ss, x:ns), (g x):bs)

step :: Criteria a b -> ([b] -> b) -> (Partition a, [b]) -> (Partition a, [b])
step c f ((ss, ns), bs) = let ((ss', ns'), bs') = partition c ss 
                            in ((ss', ns ++ ns'), f bs' : bs)

composeC :: Criteria a b -> Criteria b c -> Criteria a c
composeC (C p f g) (C q h k) = C (\a -> p a && q (f a)) (h . f) (k . g)

-- 1) Definir con RE
appF :: Funnel a b -> ([b] -> b) -> [a] -> (Partition a, [b])
-- dado un funnel, una función que "reduce" una lista de resultados, y una lista de tipo [a],
--  retorna la particion de elementos a tras aplicar el funnel
appF (Initial c) f xs = let (p, bs) = partition c xs in 
                            (p, [f bs])
appF (Step c fun) f xs = step c f (appF fun f xs) 

complementF :: Funnel a b -> Funnel a b
-- que retorna al funnel donde todos los criterios se reemplazan por sus complementos
complementF (Initial c)  = Initial (complementC c)
complementF (Step c fun) = Step (complementC c) (complementF fun)

complementC :: Criteria a b -> Criteria a b 
complementC (C f g h) = C (not . f) g h

reverseF :: Funnel a b -> Funnel a b
-- dado un funnel, retorna uno donde los criterios se aplican al reves
reverseF (Initial c)  = Initial c
reverseF (Step c fun) = pushEnd c (reverseF fun)

pushEnd :: Criteria a b -> Funnel a b -> Funnel a b 
pushEnd c (Initial c') = Step c (Initial c')
pushEnd c (Step c' fun) = Step c' (pushEnd c fun)

mapF :: (b -> c) -> Funnel a b -> Funnel a c
-- dado un funnel y una funcion b -> c, lo retorna mapeando sus funciones de a -> b por a -> c
mapF g (Initial c) = Initial (mapC g c)
mapF g (Step c fun) = Step (mapC g c) (mapF g fun)

mapC :: (b -> c) -> Criteria a b -> Criteria a c 
mapC j (C f g h) = C f (j . g) (j . h)

zipF :: Funnel a b -> Funnel b c -> Funnel a c
-- zippea dos funnels, combinando los criteria
zipF (Initial c) fun = case fun of 
                        (Initial c') -> Initial (composeC c c')
                        (Step c' fun') -> Initial (composeC c c')
zipF (Step c fun) fun' = case fun' of 
                            (Initial c') -> Initial (composeC c c')
                            (Step c' fun'') -> Step (composC c c') (zipF fun fun'') 

-- 2) Demostrar que:
-- ¿para todo fn'. para todo f'. para todo xs'.
--      appF fn' f' xs' = appF (complementF (complementF fn')) f' xs'?

{-
    Sean fn :: Funnel a b, f :: ([b] -> b), xs :: [a]
    Por ppio. de ind. sobre la estructura de fn, se verá que

    CB.) fn = Initial c) 
        ¿appF (Initial c) f xs = appF (complementF (complementF (Initial c))) f xs?


-}

-- 3) Implementar foldF y recF, en base a la estructura de Funnel a b
foldF :: (Criteria a b -> c -> c) -> (Criteria a b -> c) -> Funnel a b -> c
foldF s i (Initial c)  = i c 
foldF s i (Step c fun) = s c (foldF s i fun)

recF :: (Criteria a b -> c) -> (Criteria a b -> c -> Funnel a b -> c) -> Funnel a b -> c 
recF i s (Initial c) = i c 
recF i s (Step c fun) = s c (recF i s fun) fun 

-- 4) Implementar el punto 1 usando foldF o recF según convenga
appF' :: Funnel a b -> ([b] -> b) -> [a] -> (Partition a, [b])
appF' = foldF (\c pab f xs -> step c f (pab f xs)) (\c f xs -> let (p, ys) = partition c xs in (p, [f ys]))

complementF' :: Funnel a b -> Funnel a b
complementF' = foldF (\c fun -> Step (complementC c) fun) (Initial . complementC)

reverseF' :: Funnel a b -> Funnel a b
reverseF' = foldF pushEnd' Initial

pushEnd' :: Criteria a b -> Funnel a b -> Funnel a b 
pushEnd' c f = foldF (\c' fun cri -> Step c' (fun cri))
                     (\c' cri -> Step c' (Initial cri))
                     f c

mapF' :: (b -> c) -> Funnel a b -> Funnel a c
mapF' f = foldF (\c fun -> Step (mapC f c) fun)
                (Initial . (mapC f))

zipF' :: Funnel a b -> Funnel b c -> Funnel a c
zipF' = foldF (\c fun fun' -> case fun' of 
                            (Initial c') -> Initial (composeC c c')
                            (Step c' fun'') -> Step (composC c c') (fun fun''))
              (\c fun -> case fun of 
                        (Initial c') -> Initial (composeC c c')
                        (Step c' fun') -> Initial (composeC c c'))