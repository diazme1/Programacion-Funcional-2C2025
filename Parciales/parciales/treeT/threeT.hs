data ThreeT a = Leaf a | Branch a (ThreeT a) (ThreeT a) (ThreeT a)

sizeTT :: ThreeT a -> Int 
sizeTT (Leaf x) = 1
sizeTT (Branch x tt1 tt2 tt3) = 1 + sizeTT tt1 + sizeTT tt2 + sizeTT tt3 

sumTT :: ThreeT Int -> Int 
sumTT (Leaf n) = n 
sumTT (Branch n tt1 tt2 tt3) = n + sumTT tt1 + sumTT tt2 + sumTT tt3

-- devuelve los elementos que estan en las hojas
leavesTT :: ThreeT a -> [a]
leavesTT (Leaf x) = [x]
leavesTT (Branch x tt1 tt2 tt3) = x : (leavesTT tt1 ++ leavesTT tt2 ++ leavesTT tt3)

mapTT :: (a -> b) -> ThreeT a -> ThreeT b
mapTT f (Leaf x) = Leaf (f x)
mapTT f (Branch x tt1 tt2 tt3) = Branch (f x) (mapTT f tt1) (mapTT f tt2) (mapTT f tt3)

maxTT :: Ord a => ThreeT a -> a 
maxTT (Leaf x) = x 
maxTT (Branch x tt1 tt2 tt3) = max x (max (maxTT tt1 (maxTT tt2 tt3)))
--    let max' = maxTT tt1 (maxTT tt2 tt3)) in 
--        if x > max' then x else max' 

findTT :: Eq a => (a -> Bool) -> ThreeT (a, b) -> Maybe b 
findTT f (Leaf x) = if f (fst x) then Just (snd x) else Nothing
findTT f (Branch x tt1 tt2 tt3) = 
    if f (fst x)
        then Just (snd x)
        else fromJust3 (findTT tt1) (findTT tt2) (findTT tt3)

fromJust3 :: Maybe b -> Maybe b -> Maybe b -> Maybe b 
fromJust3 (Just x) _ _ = x 
fromJust3 _ (Just x) _ = x 
fromJust3 _ _ (Just x) = x
fromJust3 _ _ _ = Nothing 

levelNTT :: Int -> ThreeT a -> [a]
levelNTT 0 (Leaf x) = [x]
levelNTT 0 (Branch x tt1 tt2 tt3) = [x]
levelNTT n (Leaf x) = []
levelNTT n (Branch x tt1 tt2 tt3) = levelNTT (n-1) tt1 ++ 
                                    levelNTT (n-1) tt2 ++ 
                                    levelNTT (n-1) tt3 
listPerLevelTT :: ThreeT a -> [[a]]
listPerLevelTT (Leaf x) = [[x]]
listPerLevelTT (Branch x tt1 tt2 tt3) = [x] : 
    (mergeLevels3 (listPerLevelTT tt1) (listPerLevelTT tt2) (listPerLevelTT tt3))

mergeLevels3 :: [[a]] -> [[a]] -> [[a]] -> [[a]]
mergeLevels3 xss [] [] = xss 
mergeLevels3 [] yss [] = yss 
mergeLevels3 [] [] zss = zss
mergeLevels3 (xs:xss) (ys:yss) (zs:zss) = xs ++ ys ++ zs :
        (mergeLevels3 xss yss zss)


-- EJERCICIO 2
foldTT :: (a -> b -> b -> b -> b) -> (a -> b) -> ThreeT a -> b 
foldTT f z (Leaf x) = z x 
foldTT f z (Branch x tt1 tt2 tt3) = f x (foldTT f z tt1) (foldTT f z tt2) (foldTT f z tt3)

-- EJERCICIO 3
sizeTT :: ThreeT a -> Int 
sizeTT = foldTT (\x n1 n2 n3 -> 1 + n1 + n2 + n3) (const 1)

sumTT :: ThreeT Int -> Int 
sumTT = foldTT (\x n1 n2 n3 -> x + n1 + n2 + n3) id 

leavesTT :: ThreeT a -> [a]
leavesTT = foldTT (\x x1 x2 x3 -> x : (x1 ++ (x2 ++ x3))) (:[])

mapTT :: (a -> b) -> ThreeT a -> ThreeT b 
mapTT f = foldTT (Branch . f) (Leaf . f)

maxTT :: Ord a => ThreeT a -> a 
maxTT = foldTT (\x e1 e2 e3 -> x 'max' e1 'max' e2 'max' e3) id

findTT :: Eq a => (a -> Bool) -> ThreeT (a, b) -> Maybe b 
findTT f = foldTT (\x res1 res2 res3 -> if f (fst x) then Just (snd x) else fromJust3 res1 res2 res3) 
                  (\x -> if f (fst x) then Just (snd x) else Nothing) 

levelNTT :: Int -> ThreeT a -> [a]
levelNTT = flip (foldTT (\x t1 t2 t3 -> n ->
                if n == 0 then [x] 
                    else t1 (n-1) ++ t2 (n-1) ++ t3 (n-1))
                (\x -> n -> if n == 0 then [x] else []))

-- TODO: REVISAR
listPerLevelTT :: ThreeT a -> [[a]]
listPerLevelTT = foldTT (\x t1 t2 t3 -> [x] : ) (:[] . :[])

-- EJERCICIO 4.
{-
    a) sizeTT = sumTT . mapTT (const 1)
        Prop.) ¿sizeTT = sumTT . mapTT (const 1)?
        Dem. ) Por ppio de ext. es equivalente demostrar:
            Para todo tt', 
                ¿sizeTT tt' = sumTT . mapTT (const 1) tt'?
            
            Sea tt :: ThreeT a, por ppio. de ind. sobre la estructura
            de tt, se verá que
        
        CB. tt = (Leaf x):
                ¿sizeTT (Leaf x) = sumTT . mapTT (const 1) (Leaf x)?
        
        CI. tt = (Branch x tt1 tt2 tt3):
            TI.1) ¡sizeTT tt1 = sumTT . mapTT (const 1) tt1!
            TI.2) ¡sizeTT tt2 = sumTT . mapTT (const 1) tt2!
            TI.3) ¡sizeTT tt3 = sumTT . mapTT (const 1) tt3!
            HI) ¿sizeTT (Branch x tt1 tt2 tt3) = sumTT . mapTT (const 1) (Branch x tt1 tt2 tt3)?
        

        Dem. CB)
            Lado izq.:
                sizeTT (Leaf x)
            ->  --------------                  (sizeTT.1)
                1
            
            Lado der.:
                sumTT . mapTT (const 1) (Leaf x)
            ->  -------------------------------- (compose)
                sumTT (mapTT (const 1) (Leaf x))
            ->         ------------------------  (mapTT.1)
                sumTT (Leaf (const 1 x))
            ->              ----------           (const)
                sumTT (Leaf 1)
            ->  -------------                    (sumTT.1)
                1
            
        Vale la propiedad.

        Dem. CI)
            Lado izq.:
                sizeTT (Branch x tt1 tt2 tt3)
            ->  ----------------------------    (sizeTT.2)
                1 + sizeTT tt1 + sizeTT tt2 + sizeTT tt3
            

            Lado der.:
                sumTT . mapTT (const 1) (Branch x tt1 tt2 tt3)
            ->  
                sumTT (mapTT (const 1) (Branch x tt1 tt2 tt3))
            ->        ----------------------------------------
                sumTT (Branch (const 1 x) (mapTT (const 1) tt1) (mapTT (const 1) tt2) (mapTT (const 1) tt3))
            ->                 ---------
                sumTT (Branch 1 (mapTT (const 1) tt1) (mapTT (const 1) tt2) (mapTT (const 1) tt3))
            ->  ----------------------------------------------------------------------------------
                1 + (sumTT (mapTT (const 1) tt1)) + (sumTT (mapTT (const 1) tt2)) + (sumTT (mapTT (const 1) tt3)))
            -> (compose)
                1 + sumTT . mapTT (const 1) tt1 + sumTT . mapTT (const 1) tt2 + sumTT . mapTT (const 1) tt3
            -> TI.1 TI.2 TI.3
                1 + sizeTT tt1 + sizeTT tt2 + sizeTT tt3

        Vale la propiedad.
-}


{-
    b) sum . leavesTT = sumTT

-}
