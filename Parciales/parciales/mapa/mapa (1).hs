data Dir = LeftM | RightM | StraightM
data Mapa a = Cofre [a] | Nada (Mapa a) | Bifurcacion [a] (Mapa a) (Mapa a)

objects :: Mapa a -> [a]
objects (Cofre xs)             = xs
objects (Nada m)               = objects m 
objects (Bifurcacion xs ml mr) = xs ++ (objects ml) ++ (objects mr)

mapMp :: (a -> b) -> Mapa a -> Mapa b 
mapMp f (Cofre xs) = Cofre (map f xs)
mapMp f (Nada m)   = mapMp f m 
mapMp f (Bifurcacion xs ml mr) = Bifurcacion (map f xs) (mapMp f ml) (mapMp f mr)

hasObjectAt :: (a -> Bool) -> Mapa a -> [Dir] -> Bool 
hasObjectAt f (Cofre xs) [] = any f xs
hasObjectAt f (Nada m)  (StraightM:xs) = hasObjectAt f m xs 
hasObjectAt f (Bifurcacion xs ml mr) [] = any f xs 
hasObjectAt f (Bifurcacion xs ml mr) (LeftM:xs) = hasObjectAt f ml xs 
hasObjectAt f (Bifurcacion xs ml mr) (RightM:xs) = hasObjectAt f mr xs
hasObjectAt _ _ _ = False

longestPath :: Mapa a -> [Dir]
longestPath (Cofre xs) = []
longestPath (Nada m)   = StraightM : (longestPath m) 
longestPath (Bifurcacion xs ml mr) = 
    let (lPath, rPath) = ((longestPath ml), longestPath mr) in
    if length lPath > length rPath
        then LeftM : lPath
        else RightM : rPath

heightM :: Mapa a -> Int 
heightM (Cofre xs) = 1
heightM (Nada m)   = 1 + heightM m 
heightM (Bifurcacion xs ml mr) = 1 + (max (heightM ml) (heightM mr)) 

allPaths :: Mapa a -> [[Dir]]
allPaths (Cofre xs) = [[]]
allPaths (Nada m)   = map (StraightM:) (allPaths m)
allPaths (Bifurcacion xs ml mr) = (map (LeftM:) (allPaths ml)) ++ (map (RightM:) (allPaths mr))

-- Tipar y definir foldM y recM
foldM :: ([a] -> b) 
      -> (b -> b) 
      -> ([a] -> b -> b -> b) 
      -> Mapa a 
      -> b
foldM c n b (Cofre xs) = c xs 
foldM c n b (Nada m)   = n (foldM c n b m) 
foldM c n b (Bifurcacion xs ml mr) = b xs (foldM c n b ml) (foldM c n b mr)

recM :: ([a] -> b) 
     -> (b -> Mapa a -> b)
     -> ([a] -> b -> b -> Mapa a -> Mapa a -> b)
     -> Mapa a 
     -> b 
recM c n b (Cofre xs) = c xs 
recM c n b (Nada m)   = n (recM c n b m) m 
recM c n b (Bifurcacion xs ml mr) = b xs (recM c n b ml) (recM c n b mr) ml mr 

-- Redefinir funciones utilizando foldM o recM
objects' :: Mapa a -> [a]
objects' = foldM id 
                 id
                 ((++) . (++))

mapMp' :: (a -> b) -> Mapa a -> Mapa b 
mapMp' = foldM (Cofre . map f)
               
