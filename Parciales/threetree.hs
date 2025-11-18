data ThreeT a = Leaf a | Branch a (ThreeT a) (ThreeT a) (ThreeT a)
    deriving Show

sumarUno :: Int -> Int
sumarUno n = n+1

esUno :: Int -> Bool
esUno n = n==1

ej :: ThreeT Int
ej = Branch 1   (Branch 2
                    (Leaf 3)
                    (Leaf 4)
                    (Leaf 5))
                (Leaf 6)
                (Leaf 7)

ej1 :: ThreeT (Int, String)
ej1 = Leaf (1,"Hola")
--Ejercicio 1)
--a)
sizeTT :: ThreeT a -> Int
sizeTT (Leaf _)            = 1
sizeTT (Branch _ t1 t2 t3) = 1 + sizeTT t1 + sizeTT t2 + sizeTT t3

--b)
sumTT :: ThreeT Int -> Int
sumTT (Leaf n)            = n
sumTT (Branch n t1 t2 t3) = n + sumTT t1 + sumTT t2 + sumTT t3

--c)
leavesTT :: ThreeT a -> [a]
leavesTT (Leaf a)            = [a]
leavesTT (Branch a t1 t2 t3) = a : (leavesTT t1 ++ leavesTT t2 ++ leavesTT t3)

--d)
mapTT :: (a -> b) -> ThreeT a -> ThreeT b
mapTT f (Leaf a)            = Leaf (f a)
mapTT f (Branch a t1 t2 t3) = Branch (f a) (mapTT f t1) (mapTT f t2) (mapTT f t3)

--e)
maxTT :: Ord a => ThreeT a -> a
maxTT (Leaf a)            = a
maxTT (Branch a t1 t2 t3) = maximum (a: [maxTT t1] ++ [maxTT t2] ++ [maxTT t3])

maxTT' :: Ord a => ThreeT a -> a
maxTT' (Leaf a)            = a
maxTT' (Branch a t1 t2 t3) = max a (max (maxTT t1) (max (maxTT t2) (maxTT t3)))

--f)
findTT :: Eq a => (a -> Bool) -> ThreeT (a,b) -> Maybe b
findTT f (Leaf tp)            = if f (fst tp) then Just (snd tp) else Nothing
findTT f (Branch tp t1 t2 t3) = if f (fst tp) then Just (snd tp) else isPresentEn (findTT f t1) (findTT f t2) (findTT f t3)

isPresentEn :: Maybe b -> Maybe b -> Maybe b -> Maybe b
isPresentEn  a b c = case (a,b,c) of
                        (Just e, _, _) -> Just e
                        (_,Just e, _)  -> Just e
                        (_, _, Just e) -> Just e
                        (_,_,_)        -> Nothing

--g)
levelNTT :: Int -> ThreeT a -> [a]
levelNTT 0 (Leaf a)            = [a]
levelNTT _ (Leaf _)            = []
levelNTT 0 (Branch a _ _ _)    = [a]
levelNTT n (Branch _ t1 t2 t3) = levelNTT (n-1) t1 ++ levelNTT (n-1) t2 ++ levelNTT (n-1) t3

--h)
listPerLevelTT :: ThreeT a -> [[a]]
listPerLevelTT (Leaf a)            = [[a]]
listPerLevelTT (Branch a t1 t2 t3) = [a] : zipTresListas (listPerLevelTT t1) (listPerLevelTT t2) (listPerLevelTT t3)

zipTresListas :: [[a]] -> [[a]] -> [[a]] -> [[a]]
zipTresListas []       ls         []     = ls
zipTresListas ls        []        []     = ls
zipTresListas []        []        ls     = ls
zipTresListas (l1:ls1) (l2:ls2) (l3:ls3) = (l1 ++ l2 ++ l3) : zipTresListas ls1 ls2 ls3


--Ejercicio 2)
foldTT :: (a -> b) -> (a -> b -> b -> b -> b) -> ThreeT a -> b
foldTT fl fb (Leaf a)            = fl a
foldTT fl fb (Branch a t1 t2 t3) = fb a (foldTT fl fb t1) (foldTT fl fb t2) (foldTT fl fb t3)

--Ejercicio 3) 
sizeTT' :: ThreeT a -> Int
sizeTT' = foldTT (\_ -> 1) (\_ r1 r2 r3 -> 1 + r1 + r2 + r3)

sumTT' :: ThreeT Int -> Int
sumTT' = foldTT id (\a r1 r2 r3 -> a + r1 + r2 + r3)

leavesTT' :: ThreeT a -> [a]
leavesTT' = foldTT (\a -> [a]) (\a r1 r2 r3 -> a : (r1++r2++r3))

mapTT' :: (a -> b) -> ThreeT a -> ThreeT b
mapTT' f  = foldTT (\a -> Leaf (f a)) (\a r1 r2 r3 -> Branch (f a) r1 r2 r3)

mapTT'' :: (a -> b) -> ThreeT a -> ThreeT b
mapTT'' f  = foldTT (Leaf . f) (Branch . f)

maxTT'' :: Ord a => ThreeT a -> a
maxTT'' = foldTT id (\a r1 r2 r3 -> max a (max r1 (max r2 r3)))

findTT' :: Eq a => (a -> Bool) -> ThreeT (a,b) -> Maybe b
findTT' f = foldTT (\a -> if f (fst a) then Just (snd a) else Nothing) (\a r1 r2 r3 -> if f (fst a) then Just (snd a) else isPresentEn r1 r2 r3)

levelNTT' :: Int -> ThreeT a -> [a]
levelNTT' n t = (foldTT fl fb t) n
                    where
                        fl a n = if n == 0 then [a] else []
                        fb a r1 r2 r3 n = if n == 0
                                then [a]
                                else r1 (n - 1) ++ r2 (n - 1) ++ r3 (n - 1)

listPerLevelTT' :: ThreeT a -> [[a]]
listPerLevelTT' = foldTT (\a -> [[a]]) (\a r1 r2 r3 -> [a]: zipTresListas r1 r2 r3)

--Ejercicio 4)
--a)
{-
sizeTT = sumTT . mapTT (const 1)

Para todo t,
        ¿ sizeTT t = sumTT . mapTT (const 1) t ?
    Sea t un árbol ThreeT cualquiera,

    Caso base, t = Leaf a
        ¿ sizeTT (Leaf a) = sumTT . mapTT (const 1) (Leaf a) ?

    Caso inductivo, t = Branch a t1 t2 t3
        HI.1) sizeTT t1 = sumTT . mapTT (const 1) t1
        HI.2) sizeTT t2 = sumTT . mapTT (const 1) t2
        HI.3) sizeTT t3 = sumTT . mapTT (const 1) t3
        ¿ sizeTT (Branch a t1 t2 t3) = sumTT . mapTT (const 1) (Branch a t1 t2 t3) ?

    Caso base:
        --lado izq:
        sizeTT (Leaf a)
        -- def. sizeTT.1
        1

        --lado der:
        sumTT . mapTT (const 1) (Leaf a)
        -- def (.)
        sumTT (mapTT (const 1)) (Leaf a)
        -- def. mapTT.1
        sumTT (Leaf (const 1 a))
        -- def. const
        sumTT (Leaf 1)
        -- def. sumTT.1
        1

    Caso inductivo:
        --lado izq:
        sizeTT (Branch a t1 t2 t3)
        -- def. sizeTT.2
        1 + sizeTT t1 + sizeTT t2 + sizeTT t3


        --lado der:
        sumTT . mapTT (const 1) (Branch a t1 t2 t3)
        -- def. (.)
        sumTT (mapTT (const 1) (Branch a t1 t2 t3))
        -- def. mapTT.2 
        sumTT (Branch (const 1) a (mapTT (const 1) t1) (mapTT (const 1) t2) (mapTT (const 1) t3))
        -- def. sumTT.2
        const 1 a + sumTT (mapTT (const 1) t1) + sumTT (mapTT (const 1) t2) + sumTT (mapTT (const 1) t3)
        -- por HI.1, HI.2 y HI.3: sumTT (mapTT (const 1) t) = sizeTT t
        const 1 a + sizeTT t1 + sizeTT t2 + sizeTT t3
        -- def. const
        1 + sizeTT t1 + sizeTT t2 + sizeTT t3
-}

--b)
{-
sum . leavesTT = sumTT

Para todo t,
    ¿ sum . leavesTT t = sumTT t?

    Sea t un ThreeT a cualquiera, se demuestra por inducción estructural sobre t,

    Caso base, t = Leaf a
        ¿ sum . leavesTT  (Leaf a) = sumTT (Leaf a) ?

    Caso inductivo, t = Branch a t1 t2 t3
        HI.1) sum . leavesTT t1 = sumTT t1
        HI.1) sum . leavesTT t2 = sumTT t2
        HI.1) sum . leavesTT t3 = sumTT t3
        ¿ sum . leavesTT (Branch a t1 t2 t3) = sumTT (Branch a t1 t2 t3) ?

    Case base:
        --lado izq
        sum . leavesTT (Leaf a)
        -- def. (.)
        sum (leavesTT (Leaf a))
        -- def. leavesTT.1
        sum [a]
        -- def. sum
        a

        --lado der
        sumTT (Leaf a)
        -- def. sumTT.1
        a

    Caso inductivo: 
        --lado izq
        sum . leavesTT (Branch a t1 t2 t3) 
        --def. (.)
        sum (leavesTT (Branch a t1 t2 t3)) 
        --def. leavesTT.2
        sum (a : (leavesTT t1 ++ leavesTT t2 ++ leavesTT t3))

        --lado der
        sumTT (Branch a t1 t2 t3)
        --def. sumTT.2
        a + (sumTT t1) + (sumTT t2) + (sumTT t3)
        --por HI.1, HI.2 y HI.3
        a + (leavesTT )

-}