import System.FilePath (isValid)
data MSExp a = EmptyMS | AddMS a (MSExp a) | RemoveMS a (MSExp a) | UnionMS (MSExp a) (MSExp a) | MapMS (a -> a) (MSExp a)
    --deriving Show

m = AddMS 3 (AddMS 2 (AddMS 3 EmptyMS))
m1 = RemoveMS 5 (AddMS 3 EmptyMS)
m2 = MapMS (\x->x+1) (AddMS 2 EmptyMS)

bool2Int :: Bool -> Int
bool2Int True  = 1
bool2Int False = 0

--Ejercicio 1)
--a)
-- occursMSE :: Eq a => a -> MSExp a -> Int
-- occursMSE x EmptyMS             = 0
-- occursMSE x (AddMS y ms)        = if x==y then 1 + occursMSE x ms else occursMSE x ms
-- occursMSE x (UnionMS ms1 ms2)   = occursMSE x ms1 + occursMSE x ms2
-- occursMSE x (RemoveMS y ms)     = if x==y then max 0 (occursMSE x ms - 1) else occursMSE x ms
-- occursMSE x (MapMS f ms)        = occursMSEWith x f ms 

occursMSEWith :: Eq a => a -> (a -> a) -> MSExp a -> Int
occursMSEWith x f EmptyMS           = 0
occursMSEWith x f (AddMS y ms)      = let r = occursMSEWith x f ms in
                                        bool2Int (x == f y) + r 
occursMSEWith x f (RemoveMS y ms)   = let r = occursMSEWith x f ms in 
                                        max 0 (r - bool2Int (x == f y))
occursMSEWith x f (UnionMS ms1 ms2) = occursMSEWith x f ms1 + occursMSEWith x f ms2
occursMSEWith x f (MapMS h ms)      = occursMSEWith x (f . h) ms --hay que acumular las funciones 
                                                    --si uso id en la primer acumulada toma la f del map

--occursMSEWith es lo mismo que occursMSE, solo que cambia la funcion por parametro en el caso de MapMS
occursMSE :: Eq a => a -> MSExp a -> Int
occursMSE x ms = occursMSEWith x id ms

--b)
filterMSE :: (a -> Bool) -> MSExp a -> MSExp a
filterMSE f EmptyMS           = EmptyMS
filterMSE f (AddMS y ms)      = let r = filterMSE f ms in
                                    if f y then AddMS y r else r
filterMSE f (RemoveMS y ms)   = let r = filterMSE f ms in
                                    if f y then RemoveMS y r else r
filterMSE f (UnionMS ms1 ms2) = UnionMS (filterMSE f ms1) (filterMSE f ms2)
filterMSE f (MapMS h ms)      = MapMS h (filterMSE (f . h) ms) --mismo criterio de acumular funciones


--c)
{-
Version costosa, podria intentar con una estructura [(k,v)] 
e ir acumulando occurs y ver si alguno quedo negativo
-}
isValidMSE :: MSExp a -> Bool
isValidMSE EmptyMS           = True
isValidMSE (RemoveMS x ms)   = 1 + cantRemoveMS x ms < cantAddMS x ms
isValidMSE (AddMS x ms)      = isValidMSE ms
isValidMSE (UnionMS ms1 ms3) = isValidMSE ms1 && isValidMSE ms2
-- isValidMSE (MapMS f ms)      = isValidMSE ()

cantRemoveMS :: Eq a => a -> MSExp a -> Int
cantRemoveMS _  EmptyMS          = 0
cantRemoveMS x (RemoveMS y ms)   = bool2Int (x==y) + cantRemoveMS x ms
cantRemoveMS x (AddMS _ ms)      = cantRemoveMS x ms
cantRemoveMS x (UnionMS ms1 ms2) = cantRemoveMS x ms1 + cantRemoveMS x ms2
cantRemoveMS x ()

