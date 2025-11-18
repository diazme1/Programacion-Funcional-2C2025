-- Los multisets son una estructura de datos que permiten saber cuántas veces fue ingresado un elemento. 
-- Para expresar sus operaciones, utilizaremos el lenguaje expresado por el tipo MSExp.

data MSExp a
    = EmptyMS                     -- un multiset vacío
    | AddMS a (MSExp a)           -- agrega una ocurrencia del elemento
    | RemoveMS a (MSExp a)        -- quita una ocurrencia del elemento (si hay alguna)
    | UnionMS (MSExp a) (MSExp a) -- combina dos multisets, sumando ocurrencias
    | MapMS (a -> a) (MSExp a)    -- transforma todos los elementos con una función


-- EJERCICIO 1 
-- Definir por recursión explícita las siguientes funciones

-- a. Describe la cantidad de ocurrencias del elemento dado en el multiset.
-- AYUDA 1: como la función pedida NO se puede hacer por recursión estructural, considerar definir como función auxiliar 
-- occursMESWith :: a-> (a->a)-> MSExp a-> Int por recursión estructural e invocarla con los argumentos correspondientes
-- AYUDA 2: el número retornado NO puede ser negativo. O sea, los RemoveMS que no encuentran un AddMS correspondiente no deben tener efecto.
occursMSE :: Eq a => a -> MSExp a -> Int
occursMSE e ms = occursMESWith' e id ms

occursMESWith :: Eq a => a -> (a -> a) -> MSExp a -> Int
occursMESWith e f EmptyMS           = 0 
occursMESWith e f (AddMS x ms)      = unoSi (e == f x) + occursMESWith e f ms 
occursMESWith e f (RemoveMS x ms)   = if occursMESWith e f ms > 0 then occursMESWith e f ms - unoSi (e == f x) else occursMESWith e f ms  
occursMESWith e f (UnionMS ms1 ms2) = occursMESWith e f ms1 + occursMESWith e f ms2 
occursMESWith e f (MapMS g ms)      = occursMESWith e (f . g) ms 

unoSi:: Bool -> Int 
unoSi True  = 1 
unoSi False = 0

occursMESWith' :: Eq a => a -> (a -> a) -> MSExp a -> Int
occursMESWith' e f ms = (foldMS (\_ _ -> 0) -- const(const 0)
                                (\x n e' f' -> unoSi (e' == f' x) + n e' f')
                                (\x n e' f' -> if n e' f' > 0 then n e' f' - unoSi (e' == f' x) else n e' f')
                                (\n m e' f' -> n e' f' + m e' f')
                                (\g n e' f' -> n e' (f' . g))
                        ) ms e f 

-- b. Describe el multiset resultante de eniminar todas las ocurrencias de los elementos que no cumplen con el predicado dado.
-- AYUDA: tener en cuenta que en el caso de MapMS, el elemento que se debe analizar es el procesado por la función dada en ese MapMS, 
-- y no el elemento sin procesar.
filterMSE :: (a -> Bool) -> MSExp a -> MSExp a
filterMSE p EmptyMS           = EmptyMS 
filterMSE p (AddMS x ms)      = if p x then AddMS x (filterMSE p ms) else (filterMSE p ms)
filterMSE p (RemoveMS x ms)   = RemoveMS x (filterMSE p ms)  
filterMSE p (UnionMS ms1 ms2) = UnionMS (filterMSE p ms1) (filterMSE p ms2)
filterMSE p (MapMS f ms)      = MapMS f (filterMSE (p . f) ms)

filterMSE' :: (a -> Bool) -> MSExp a -> MSExp a
filterMSE' = flip (foldMS   (const EmptyMS)
                            (\x ms p    -> if p x then AddMS x (ms p) else ms p)
                            (\x ms p    -> RemoveMS x (ms p))
                            (\ms1 ms2 p -> UnionMS (ms1 p) (ms2 p))
                            (\f ms p    -> MapMS f (ms (p . f))))

-- c. Indica si la expresión de multiset dada es válida. 
-- Una expresión de multiset es inválida si tiene más RemoveMS que AddMS para un elemento determinado; en el caso de una UnionMS, 
-- cada parte se considera por separado. 
isValidMSE :: Eq a => MSExp a -> Bool
isValidMSE = isValidMSEWith id

-- Función auxiliar con transformación acumulada (para MapMS) 
isValidMSEWith :: Eq a => (a -> a) -> MSExp a -> Bool
isValidMSEWith f EmptyMS           = True
isValidMSEWith f (AddMS x ms)      = isValidMSEWith f ms
isValidMSEWith f (RemoveMS x ms)   = occursMESWith (f x) f ms > 0 && isValidMSEWith f ms
isValidMSEWith f (UnionMS ms1 ms2) = isValidMSEWith f ms1 && isValidMSEWith f ms2
isValidMSEWith f (MapMS g ms)      = isValidMSEWith (f . g) ms

isValidMSEWith' :: Eq a => (a -> a) -> MSExp a -> Bool
isValidMSEWith' = flip (recMS   (const True)
                                (\x b _     f -> b f)
                                (\x b ms    f -> occursMESWith (f x) f ms > 0 && b f)
                                (\b1 _ b2 _ f -> b1 f && b2 f)
                                (\g b _     f -> b (f . g)))

-- d. Describe la lista de todas las ocurrencias de los elementos del multiset dado.
evalMSE :: Eq a => MSExp a -> [a]
evalMSE EmptyMS           = []
evalMSE (AddMS x ms)      = x : evalMSE ms 
evalMSE (RemoveMS x ms)   = remove x (evalMSE ms)
evalMSE (UnionMS ms1 ms2) = evalMSE ms1 ++ evalMSE ms2 
evalMSE (MapMS f ms)      = map f (evalMSE ms) 

remove :: Eq a => a -> [a] -> [a]
remove x []     = []
remove x (y:ys) = if x == y then ys else y : remove x ys 

evalMSE' :: Eq  a => MSExp a -> [a] 
evalMSE' = foldMS [] (:) remove (++) map 

remove' :: Eq a => a -> [a] -> [a]
remove' = flip (recr    (const []) 
                        (\y ys zs x -> if x == y then ys else y : zs x))

recr :: b -> (a -> [a] -> b -> b) -> [a] -> b
recr z f []     = z
recr z f (x:xs) = f x xs (recr z f xs)

-- Suponiendo que recibe una expresión de multiset válida y describe al multiset resultante de simplificar el dado, 
-- aplicando las siguientes reglas en todos los lugares posibles:
-- UnionMS EmptyMS e      ⇒ e
-- UnionMS e EmptyMS      ⇒ e
-- MapMS f EmptyMS        ⇒ EmptyMS
-- RemoveMS x (AddMS x e) ⇒ e
simpMSE' ::  Eq a => MSExp a -> MSExp a
simpMSE' = foldMS EmptyMS AddMS simpRemoveMS simpUnionMS simpMapMS 

simpRemoveMS :: Eq a => a -> MSExp a -> MSExp a 
simpRemoveMS x (AddMS y ms) = if x == y then ms else RemoveMS x (AddMS y ms)
simpRemoveMS x ms           = RemoveMS x ms 

simpUnionMS :: MSExp a -> MSExp a -> MSExp a 
simpUnionMS ms1 EmptyMS = ms1 
simpUnionMS EmptyMS ms2 = ms2 
simpUnionMS ms1 ms2     = UnionMS ms1 ms2 

simpMapMS :: (a -> a) -> MSExp a -> MSExp a 
simpMapMS f EmptyMS = EmptyMS
simpMapMS f ms      = MapMS f ms 


-- EJERCICIO 3 
foldMS :: b -> (a -> b -> b) -> (a -> b -> b) -> (b -> b -> b) -> ((a -> a) -> b -> b) -> MSExp a -> b 
foldMS e a r u m EmptyMS           = e 
foldMS e a r u m (AddMS x ms)      = a x (foldMS e a r u m ms) 
foldMS e a r u m (RemoveMS x ms)   = r x (foldMS e a r u m ms) 
foldMS e a r u m (UnionMS ms1 ms2) = u (foldMS e a r u m ms1) (foldMS e a r u m ms2) 
foldMS e a r u m (MapMS f ms)      = m f (foldMS e a r u m ms)

recMS :: b -> (a -> b -> MSExp a -> b) -> (a -> b -> MSExp a -> b) -> (b -> MSExp a -> b -> MSExp a -> b) -> 
    ((a -> a) -> b -> MSExp a -> b) -> MSExp a -> b 
recMS e a r u m EmptyMS           = e 
recMS e a r u m (AddMS x ms)      = a x (recMS e a r u m ms) ms
recMS e a r u m (RemoveMS x ms)   = r x (recMS e a r u m ms) ms 
recMS e a r u m (UnionMS ms1 ms2) = u (recMS e a r u m ms1) ms1 (recMS e a r u m ms2) ms2
recMS e a r u m (MapMS f ms)      = m f (recMS e a r u m ms) ms


-- EJEMPLOS
ejemplo :: MSExp Int
ejemplo =
  MapMS (2*) (
    UnionMS
      (RemoveMS 2 (AddMS 1 (AddMS 2 (AddMS 2 (AddMS 3 (AddMS 3 (AddMS 3 EmptyMS)))))))
      (AddMS 1 (AddMS 4 EmptyMS))
  )


instance Show a => Show (MSExp a) where
  show EmptyMS = "EmptyMS"
  show (AddMS x ms) = "AddMS " ++ show x ++ " (" ++ show ms ++ ")"
  show (RemoveMS x ms) = "RemoveMS " ++ show x ++ " (" ++ show ms ++ ")"
  show (UnionMS ms1 ms2) = "UnionMS (" ++ show ms1 ++ ") (" ++ show ms2 ++ ")"
  show (MapMS _ ms) = "MapMS <func> (" ++ show ms ++ ")"
