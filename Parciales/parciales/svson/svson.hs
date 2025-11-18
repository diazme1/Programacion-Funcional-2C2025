data Value = Vint Int | Vstring String | Vbool Bool | Vlist [Value]
    deriving Show
type Field = String
data Svson i = Empty | Obj i (Field -> Maybe Value) (Svson i) (Svson i)


-- La estructura Svson i representa un árbol binario de búsqueda (BST) donde cada nodo contiene:
-- Un índice de tipo i (que debe ser ordenable),
-- Una función que mapea nombres de campos a valores opcionales,
-- Dos subárboles izquierdo y derecho

-- Obtiene los valores de una lista de fields de un objeto
valuesOf :: [Field] -> (Field -> Maybe Value) -> [Value]
valuesOf = foldr (\k f -> \g -> case g k of 
                                    Nothing -> f g
                                    Just v  -> v : f g)
                (const [])

-- Obtiene pares (campo, valor) para los fields que existen
valuesWithFields :: [Field] -> (Field -> Maybe Value) -> [(Field, Value)]
valuesWithFields = foldr (\k f -> \g -> case g k of 
                                            Nothing -> f g 
                                            Just v  -> (k,v) : f g)
                        (const [])

-- Restringe un objeto (representado por una funcion) a solo los fields especificados. 
-- ks es la lista de campos permitidos, f la funcion original. 
-- Si el campo que llega esta en f, entonces f k, sino devuelve Nothing
only :: [Field] -> (Field -> Maybe Value) -> Field -> Maybe Value
only ks f = \k -> if elem k ks then f k else Nothing 

-- Actualiza o agrega un field con un valor en un objeto (representado por una funcion).
-- k es el campo a actualizar, v es su valor y f es la funcion original.  
-- Si el campo que llega es igual al que hay que actualizar, se develve el valor. Sino f k.
update :: Field -> Value -> (Field -> Maybe Value) -> Field -> Maybe Value
update k v f = \k' -> if k == k' then Just v else f k 

-- Crea un objeto (representado por una funcion) con un solo campo y valor.
singleton :: Field -> Value -> Field -> Maybe Value
singleton k v = \k' -> if k == k' then Just v else Nothing


-- EJERCICIOS 1

-- Devuelve una lista con todos los índices presentes en la estructura, en cualquier orden.
indices :: Svson i -> [i]
indices = foldSvson [] (\j _ xs ys -> j : xs ++ ys)                    

-- Indica si un índice dado existe en la estructura. Debe aprovechar la propiedad de BST para ser eficiente.
belongs :: Ord i => i -> Svson i -> Bool 
belongs = flip (foldSvson (const False) 
                          (\j _ g1 g2 -> \i' -> if i' == j 
                                                  then True
                                                  else if i' < j 
                                                    then g1 i' 
                                                    else g2 i'))

-- Busca un índice específico y devuelve los valores de los campos solicitados para ese índice. Si el índice no existe, 
-- devuelve una lista vacía.
lookupProjecting :: Ord i => i -> [Field] -> Svson i -> [Value] 
lookupProjecting i' = flip (foldSvson (const []) 
                                    (\j f g1 g2 -> \ks -> if null ks 
                                                            then []
                                                            else if i' == j
                                                                    then valuesOf ks f
                                                                    else if i' < j 
                                                                        then g1 ks 
                                                                        else g2 ks))

-- Actualiza o inserta un campo en el objeto con el índice dado. Si el índice no existe, crea un nuevo nodo en la posición correcta del BST.
upsert :: Ord i => i -> Field -> Value -> Svson i -> Svson i
upsert i' k v = recSvson (Obj i' (singleton k v) Empty Empty)
                        (\j f sr1 s1 sr2 s2 -> if i' == j 
                                                then Obj j (update k v f) s1 s2
                                                else if i' < j 
                                                    then Obj j f sr1 s2
                                                    else Obj j f s1 sr2)

-- Realiza la intersección entre dos estructuras, combinando los objetos que tienen índices en común.
mkObj :: Ord i => Svson i -> Svson i -> Svson i
mkObj s1 s2 = toSvson (intersect (toList s1) (toList s2))

intersect :: Eq i => [(i, Field -> Maybe Value)] -> [(i, Field -> Maybe Value)] -> [(i, Field -> Maybe Value)] 
intersect = foldr (\(i,fv) g -> \ifvs' -> if null ifvs' 
                                            then []
                                            else if elemI i ifvs' 
                                                then (i,fv) : g ifvs' 
                                                else g ifvs')
                    (const [])

elemI :: Eq i => i -> [(i, Field -> Maybe Value)] -> Bool 
elemI = flip (foldr (\(i, fv) g -> \i' -> i' == i || g i')
                    (const False))

toList :: Svson i -> [(i, Field -> Maybe Value)]
toList = foldSvson [] (\i fv ifvs1 ifvs2 -> (i, fv) : ifvs1 ++ ifvs2) 

toSvson :: Ord i => [(i, Field -> Maybe Value)] -> Svson i 
toSvson = foldr (\(i,fv) s -> insert i fv s)  
                Empty
-- toSvson = foldr (uncurry insert) Empty

insert :: Ord i => i -> (Field -> Maybe Value) -> Svson i -> Svson i 
insert i fv = recSvson (Obj i fv Empty Empty)
                    (\j f sr1 s1 sr2 s2 -> if i == j 
                                            then Obj j f s1 s2 
                                            else if i < j 
                                                    then Obj j f sr1 s2 
                                                    else Obj j f s1 sr2) 

-- Devuelve hasta n objetos que cumplan la condición dada, junto con el número real de objetos encontrados. 
-- Debe priorizar los índices menores (recorrido in-order del BST).
takeUpSatisfying :: Ord i => Int -> (i -> Bool) -> Svson i -> (Svson i, Int)
takeUpSatisfying _ _ Empty = (Empty, 0)
takeUpSatisfying n p (Obj i fv s1 s2) = let (s1', k1) = takeUpSatisfying n p s1
                                          in if k1 == n 
                                            then (s1', k1)
                                            else if p i
                                                  then let (s2', k2) = takeUpSatisfying (n - k1 - 1) p s2
                                                        in (Obj i fv s1' s2', k1 + 1 + k2)
                                                  else let (s2', k2) = takeUpSatisfying (n - k1) p s2
                                                        in (merge s1' s2', k1 + k2)

takeUpSatisfying' :: Ord i => Int -> (i -> Bool) -> Svson i -> (Svson i, Int)
takeUpSatisfying' n p s = foldSvson 
                                (const (Empty, 0))
                                (\i fv sr1 sr2 -> \m -> let (s1', k1) = sr1 m
                                                          in if k1 == m 
                                                              then (s1', k1) 
                                                              else if p i
                                                                    then let (s2', k2) = sr2 (m - k1 - 1)
                                                                          in (Obj i fv s1' s2', k1 + 1 + k2)
                                                                    else let (s2', k2) = sr2 (m - k1)
                                                                          in (merge s1' s2', k1 + k2)) s n 
-- Notar que sr1 y sr2 :: Int -> (i -> Bool) -> (Svson i, Int), el tipo cambia porque paso s n al fold

merge :: Ord i => Svson i -> Svson i -> Svson i
merge s1 s2 = toSvson (toList s1 ++ toList s2)


-- EJERCICIO 2
-- Definir foldSvson y recSvson 
foldSvson :: b -> (i -> (Field -> Maybe Value) -> b -> b -> b) -> Svson i -> b 
foldSvson z f Empty            = z 
foldSvson z f (Obj i' g s1 s2) = f i' g (foldSvson z f s1) (foldSvson z f s2)

recSvson :: b -> (i -> (Field -> Maybe Value) -> b -> Svson i -> b -> Svson i -> b) -> Svson i -> b 
recSvson z f Empty            = z 
recSvson z f (Obj i' g s1 s2) = f i' g (recSvson z f s1) s1 (recSvson z f s2) s2 



 




esPar :: Int -> Bool
esPar n = n `mod` 2 == 0



-- EJEMPLO 
ejemplo :: Svson Int
ejemplo = 
  Obj 15 (singleton "x" (Vint 15))
    (Obj 13 (singleton "x" (Vint 13))
      (Obj 10 (singleton "x" (Vint 10))
        (Obj 5 (singleton "x" (Vint 5))
          (Obj 3 (singleton "x" (Vint 3)) Empty Empty)
          (Obj 6 (singleton "x" (Vint 6)) Empty Empty))
        (Obj 12 (singleton "x" (Vint 12)) Empty Empty))
      (Obj 14 (singleton "x" (Vint 14)) Empty Empty))
    (Obj 20 (singleton "x" (Vint 20)) Empty Empty)

ejemplo2 :: Svson Int
ejemplo2 = Obj 15 (singleton "x" (Vint 15)) Empty (Obj 20 (singleton "x" (Vint 20)) Empty Empty)

instance Show i => Show (Svson i) where
  show Empty = "Empty"
  show (Obj i _ l r) =
    "Obj " ++ show i ++ " <func> (" ++ show l ++ ") (" ++ show r ++ ")"
