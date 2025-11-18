{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}

sumF :: [Int] -> Int
sumF = foldr (\x rs -> x + rs) 0

lengthF :: [a] -> Int
lengthF = foldr (\x rs -> 1 + rs) 0

mapF :: (a -> b) -> [a] -> [b]
mapF f = foldr (\x rs -> (f x : rs)) []

-- filter :: (a -> Bool) -> [a] -> [b]
-- filter f = foldr (\x rs -> if f x then x:rs else rs) []

find :: (a -> Bool) -> [a] -> Maybe a
find f = foldr (\x rs -> if f x then Just x else rs) Nothing

esUno :: Int -> Bool
esUno x = x == 1

any :: (a -> Bool) -> [a] -> Bool
any f = foldr (\x rs -> f x || rs) False

all :: (a -> Bool) -> [a] -> Bool
all f = foldr (\x rs -> f x && rs) True

countBy :: (a -> Bool) -> [a] -> Int
countBy f = foldr (\x rs -> if f x then 1+rs else rs) 0

partition :: (a -> Bool) -> [a] -> ([a], [a])
partition f = foldr (\x rs -> if f x then (x:fst rs, snd rs) else (fst rs, x:snd rs)) ([],[])

zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith f = foldr (\x h -> \ys -> case ys of
                                    [] -> error "..."
                                    (y:ys) -> f x y : (h ys)) (\ys -> if null ys 
                                                                         then []
                                                                         else error "...")

--scanr :: (a -> b -> b) -> b -> [a] -> [b]

takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile f = foldr (\x rs -> if f x then x:rs else []) []

-- take :: Int -> [a] -> [a]
-- take n = foldr (\x r k -> if k > 0 then x : r (k - 1) else []) []

-- drop :: Int -> [a] -> [a]

-- elemAt :: Int -> [a] -> a

