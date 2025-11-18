esUno :: Int -> Bool
esUno n = n==1

--1)

--Describe la cantidad de elementos de la lista.
lengthRE :: [a] -> Int
lengthRE []     = 0
lengthRE (x:xs) = 1 + lengthRE xs

--Describe la suma de todos los elementos de la lista.
sumRE :: [Int] -> Int
sumRE []     = 0
sumRE (x:xs) = x + sumRE xs

--Describe el producto entre todos los elementos de la lista.
productRE :: [Int] -> Int
productRE []     = 1
productRE (x:xs) = x * productRE xs

--Describe la lista resultante de concatenar todas las listas que son elementos de la dada.
concatRE :: [[a]] -> [a]
concatRE []     = []
concatRE (x:xs) = x ++ concatRE xs

--Indica si el elemento dado pertenece a la lista.
elemRE :: Eq a => a -> [a] -> Bool
elemRE _ []     = False
elemRE e (x:xs) = e==x || elemRE e xs

--Indica si todos los elementos de la lista cumplen el predicado dado.
allRE :: (a -> Bool) -> [a] -> Bool
allRE _ []     = True
allRE f (x:xs) = f x && allRE f xs

--Indica si algún elemento de la lista cumple el predicado dado.
anyRE :: (a -> Bool) -> [a] -> Bool
anyRE _ []     = False
anyRE f (x:xs) = f x || anyRE f xs


--Describe la cantidad de elementos de la lista que cumplen el predicado dado.
count :: (a -> Bool) -> [a] -> Int
count _ [] = 0
count f (x:xs) = if f x then 1 + count f xs
                        else count f xs

--Indica si todos los elementos de la primera lista se encuentran en la segunda.
subset :: Eq a => [a] -> [a] -> Bool
subset [] _      = True
subset (x:xs) ys = elemRE x ys && subset xs ys

--Describe el resultado de agregar los elementos de la primera lista adelante de los elementos de la segunda.
(+-+) :: [a] -> [a] -> [a]
(+-+) [] ys     = ys
(+-+) (x:xs) ys = x : (+-+) xs ys

--Describe la lista que tiene los elementos en el orden inverso a la lista dada.
reverseRE :: [a] -> [a]
reverseRE []     = []
reverseRE (x:xs) = reverseRE xs ++ [x]

--Describe la lista resultante de juntar de a pares los elementos de ambas listas, según la posición que comparten en cada una.
zipRE :: [a] -> [b] -> [(a,b)] 
zipRE (x:xs) (y:ys) = (x,y) : zipRE xs ys
zipRE   _      _    = []

--Describe el par de listas que resulta de desarmar la lista dada; la primera componente del resultado se corresponde con las primeras componentes de los pares dados, y la segunda componente con las segundas componentes de dichos pares.
unzipRE :: [(a,b)] -> ([a],[b])
unzipRE []     = ([],[]) 
unzipRE (x:xs) = let (ys, rs) = unzipRE xs 
                    in (fst x : ys, snd x : rs)


--------------- Sección II ---------------
data N = Z | S N
    deriving Show

uno = S Z
dos = S (S Z)

--Describe el número representado por el elemento dado.
evalN :: N -> Int
evalN Z   = 0
evalN (S n) = 1+ evalN n

--Describe la representación unaria de la suma de los números representados por los argumentos.
--La resolución debe ser exclusivamente simbólica, o sea, SIN calcular cuáles son esos números.
addN :: N -> N -> N
addN Z n = n
addN (S n) n1 = S (addN n n1)

--Describe la representación unaria del producto de los números representados por los argumentos. 
--La resolución debe ser exclusivamente simbólica.
--2*3 = 2+2+2
prodN :: N -> N -> N
prodN _   Z    = Z 
prodN n (S n1) = addN  n (prodN n n1)

--Describe la representación unaria del número dado usando el tipo N.
int2N :: Int -> N
int2N 0 = Z
int2N n = S (int2N (n-1))

type NU = [()]

--Describe el número representado por el elemento dado.
evalNU :: NU -> Int
evalNU []      = 0
evalNU (():ns) = 1 + evalNU ns

--Describe la representación unaria del resultado de sumarle uno al número representado por el argumento.
--La resolución debe ser exclusivamente simbólica.
succNU :: NU -> NU
succNU ns = ():ns

--Describe la representación unaria de la suma de los números representados por los argumentos.
addNU :: NU -> NU -> NU
addNU ns ms = ns ++ ms

--Describe la representación unaria dada por el tipo Ncorrespondiente al número representado por el argumento.
nu2n :: NU -> N
nu2n  []     = Z 
nu2n (():ns) = S (nu2n ns) 

nu2nSinRE :: NU -> N
nu2nSinRE nu = int2N (evalNU nu)

nu2nLen :: NU -> N
nu2nLen nu = int2N (length nu)


--Describe la representación unaria dada por el tipo NUcorrespondiente al número representado por el argumento
n2nu :: N -> NU
n2nu  Z    = []  
n2nu (S n) = (): n2nu n


--------------- Sección III ---------------

data ExpA = Cte Int | Suma ExpA ExpA | Prod ExpA ExpA

--Describe el número que resulta de evaluar la cuenta representada por la expresión aritmética dada.
evalExpA :: ExpA -> Int
evalExpA (Cte n)      = n
evalExpA (Prod e1 e2) = evalExpA e1 * evalExpA e2
evalExpA (Suma e1 e2) = evalExpA e1 + evalExpA e2

--Describe una expresión aritmética con el mismo significado que la dada, pero que
--no tiene sumas del número 0, ni multiplicaciones por 1 o por 0. 
simplificarExpA :: ExpA -> ExpA
simplificarExpA (Cte n) = Cte n
simplificarExpA (Suma e1 e2) =
  case (simplificarExpA e1, simplificarExpA e2) of
    (Cte 0, e2') -> e2'               
    (e1', Cte 0) -> e1'               
    (e1', e2')   -> Suma e1' e2'      
simplificarExpA (Prod e1 e2) =
  case (simplificarExpA e1, simplificarExpA e2) of
    (Cte 0, _)   -> Cte 0             
    (_, Cte 0)   -> Cte 0             
    (Cte 1, e2') -> e2'               
    (e1', Cte 1) -> e1'               
    (e1', e2')   -> Prod e1' e2'      

--Describe la cantidad de veces que aparece suma cero en la expresión aritmética dada.
cantidadDeSumaCero :: ExpA -> Int
cantidadDeSumaCero (Cte _)      = 0
cantidadDeSumaCero (Prod e1 e2) = cantidadDeSumaCero e1 + cantidadDeSumaCero e2
cantidadDeSumaCero (Suma e1 e2) = let actual = case (e1,e2) of
                                                  ((Cte 0), (Cte 0)) -> 2
                                                  ((Cte 0), _)       -> 1
                                                  (_, (Cte 0))       -> 1
                                  in actual + cantidadDeSumaCero e1 + cantidadDeSumaCero e2

                                  
