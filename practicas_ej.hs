import Data.Sequence (Seq(Empty))
data Dir = Izq | Der
    deriving (Show, Eq)

data Tree a = EmptyT | NodeT a (Tree a) (Tree a)
    deriving (Show, Eq)

caminoMasCortoHasta :: Eq a => a -> Tree a -> Maybe [Dir]
caminoMasCortoHasta e EmptyT          = Nothing
caminoMasCortoHasta e (NodeT a t1 t2) = if e == a 
                                        then Just [] 
                                        else elMasCorto (appendDir Izq (caminoMasCortoHasta e t1)) (appendDir Der (caminoMasCortoHasta e t2))
                                            

appendDir :: Dir -> Maybe [Dir] -> Maybe [Dir]
appendDir dir xs = case xs of
                    Just ls -> Just (dir:ls)
                    Nothing -> Nothing

elMasCorto :: Maybe [Dir] -> Maybe [Dir] -> Maybe [Dir]
elMasCorto Nothing x = x
elMasCorto x Nothing = x
elMasCorto (Just xs) (Just ys) = if length xs > length ys then Just ys else Just xs

foldTree :: b -> (a -> b -> b -> b) -> Tree a -> b
foldTree fe fn EmptyT          = fe 
foldTree fe fn (NodeT a t1 t2) = fn a (foldTree fe fn t2) (foldTree fe fn t2)

recTree :: b -> (a -> Tree a -> Tree a -> b -> b -> b) -> Tree a -> b
recTree fe fn EmptyT          = fe
recTree fe fn (NodeT a t1 t2) = fn a t1 t2 (recTree fe fn t1) (recTree fe fn t2)

estaEnElCamino :: [Dir] -> Tree a -> Bool
estaEnElCamino  [x]    EmptyT         = False
estaEnElCamino  []    _               = True
estaEnElCamino (d:ds) (NodeT _ t1 t2) = case d of
                                            Izq -> estaEnElCamino ds t1     
                                            Der -> estaEnElCamino ds t2

estaEnElCamino' :: [Dir] -> Tree a -> Bool
estaEnElCamino' xs  EmptyT         = null xs
estaEnElCamino' xs (NodeT _ t1 t2) = case xs of
                                        [] -> True
                                        (Izq:dirs) -> estaEnElCamino' dirs t1
                                        (Der:dirs) -> estaEnElCamino' dirs t2

estaEnElCaminoF :: [Dir] -> Tree a -> Bool
estaEnElCaminoF xs = foldTree (null xs) (\_ r1 r2 -> case xs of
                                                        [] -> True
                                                        (Izq:dirs) -> r1
                                                        (Der:dirs) -> r2)