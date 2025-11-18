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
isValidMSE :: MSExp a -> Bool
isValidMSE EmptyMS           = True
isValidMSE (RemoveMS x ms)   = occursMSE x ms > 0 && isValidMSE ms
isValidMSE (AddMS x ms)      = isValidMSE ms
isValidMSE (UnionMS ms1 ms3) = isValidMSE ms1 && isValidMSE ms2
isValidMSE (MapMS f ms)      = isValidMSE ms

--d)
evalMSE :: Eq a => MSExp a -> [a]
evalMSE EmptyMS             = []
evalMSE (RemoveMS x ms)     = remove x (evalMSE ms) 
evalMSE (AddMS x ms)        = x : evalMSE ms
evalMSE (UnionMS ms1 ms2)   = evalMSE ++ evalMSE ms2
evalMSE (MapMS f ms)        = map f (evalMSE ms)

--e)
simpMSE :: MSExp a -> MSExp a
simpMSE EmptyMS             = EmptyMS
simpMSE (AddMS x ms)        = AddMS x (simpMSE ms)
simpMSE (UnionMS ms1 ms2)   = case (ms1, ms2) of
                                    (EmptyMS, ms) -> simpMSE ms
                                    (ms, EmptyMS) -> simpMSE ms
simpMSE (MapMS f ms)        = simpMap f (simpMSE ms)
simpMSE (RemoveMS x ms)     = simpRemove x (simpMSE ms)

simpRemove :: Eq a => a -> MSExp a -> MSExp a
simpRemove x (AddMS y ms) = if x==y then ms else RemoveMS x (AddMS y ms)
simpRemove x ms           = ms

simpMap :: (a -> a) -> MSExp a -> MSExp a
simpMap f EmptyMS = EmptyMS
simpMap f ms      = MapMS f ms

--Ejercicio 2)
{-
Para todo ms, 
    ¿ evalMSE . simpMSE ms = evalMSE ?

    Sea ms::MSExp cualquiera, 

    Caso base, ms = EmptyMS
        ¿ evalMSE . simpMSE EmptyMS = evalMSE EmptyMS ?

    Caso inductivo 1, ms = AddMS x ms
        HI.1) evalMSE . simpMSE ms = evalMSE ms
        ¿ evalMSE . simpMSE (AddMS x ms) = evalMSE (AddMS x ms) ?

    Caso inductivo 2, ms = RemoveMS x ms
        HI.1) evalMSE . simpMSE ms = evalMSE ms
        ¿ evalMSE . simpMSE (RemoveMS x ms) = evalMSE (RemoveMS x ms) ?

    Caso inductivo 3, ms = UnionMS ms1 ms2
        HI.1) evalMSE . simpMSE ms1 = evalMSE ms1
        HI.2) evalMSE . simpMSE ms2 = evalMSE ms2
        ¿ evalMSE . simpMSE (UnionMS ms1 ms2) = evalMSE (UnionMS ms1 ms2) ?

    Caso inductivo 3, ms = MapMS f ms
        HI.1) evalMSE . simpMSE ms = evalMSE ms
        ¿ evalMSE . simpMSE (MapMS f ms) = evalMSE (MapMS f ms) ?

    Caso base:
    --lado izq
    evalMSE . simpMSE EmptyMS
    = (def. (.))
    evalMSE (simpMSE EmptyMS)
    = (def. simpMSE.1)
    evalMSE EmptyMS
    = (def. evalMSE.1)
    []

    --lado der
    evalMSE EmptyMS
    = (def. evalMSE.1)
    []

    Caso inductivo 1:
    --lado izq
    evalMSE . simpMSE (AddMS x ms)
    = (def. (.))
    evalMSE (simpMSE (AddMS x ms))
    = (def. simpMSE.2)
    evalMSE (AddMS x (simpMSE ms))
    = (def. evalMSE.3)
    x : (evalMSE (simpMSE ms))
    = (por HI)
    x : (evalMSE ms)

    --lado der
    evalMSE (AddMS x ms)
    = (def. evalMSE.3)
    x : (evalMSE ms)

    [Faltan casos inductivos 2 y 3 (utilizan lemas)]
-}

--Ejercicio 3)