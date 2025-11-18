zipListas :: [[a]] -> [[a]] -> [[a]]
zipListas []       ls       = ls
zipListas ls        []      = ls
zipListas (l1:ls1) (l2:ls2) = (l1 ++ l2) : zipListas ls1 ls2
-- Expresiones en EA
ea1 = Const 5
ea2 = BOp Sum (Const 2) (Const 3)
ea3 = BOp Mul (Const 4) (Const 5)
ea4 = BOp Mul (BOp Sum (Const 1) (Const 2)) (Const 3)
ea5 = BOp Sum (Const 0) (BOp Mul (Const 2) (Const 3))

-- Expresiones equivalentes en ExpA
exp1 = Cte 5
exp2 = Suma (Cte 2) (Cte 3)
exp3 = Prod (Cte 4) (Cte 5)
exp4 = Prod (Suma (Cte 1) (Cte 2)) (Cte 3)
exp5 = Suma (Cte 0) (Prod (Cte 2) (Cte 3))

data ExpA = Cte Int | Suma ExpA ExpA | Prod ExpA ExpA
    deriving Show
data EA = Const Int | BOp BinOp EA EA
    deriving Show
data BinOp = Sum | Mul
    deriving Show

--Describe el número que resulta de evaluar la cuenta representada por la expresión aritmética dada.
evalEA :: EA -> Int
evalEA (Const n)       = n
evalEA (BOp bop e1 e2) = case bop of
                            Sum -> evalEA e1 + evalEA e2
                            Mul -> evalEA e1 * evalEA e2

--Describe una expresión aritmética representada con el tipo ExpA, cuya estructura y significado son los
--mismos que la dada.
ea2ExpA :: EA -> ExpA
ea2ExpA (Const n)       = Cte n
ea2ExpA (BOp bop e1 e2) = case bop of
                            Sum -> Suma (ea2ExpA e1) (ea2ExpA e2)
                            Mul -> Prod (ea2ExpA e1) (ea2ExpA e2)

--Describe una expresión aritmética representada con el tipo EA, cuya estructura y significado son los
--mismos que la dada.
expA2ea :: ExpA -> EA
expA2ea (Cte n) = Const n
expA2ea (Suma e1 e2) = BOp Sum (expA2ea e1) (expA2ea e2)
expA2ea (Prod e1 e2) = BOp Mul (expA2ea e1) (expA2ea e2)


data Arbol a b = Hoja b | Nodo a (Arbol a b) (Arbol a b)
    deriving Show

--Describe la cantidad de hojas en el árbol dado.
cantidadDeHojas :: Arbol a b -> Int
cantidadDeHojas (Hoja _)           = 1
cantidadDeHojas (Nodo _ aIzq aDer) = cantidadDeHojas aIzq + cantidadDeHojas aDer

--Describe la cantidad de nodos en el árbol dado.
cantidadDeNodos :: Arbol a b -> Int
cantidadDeNodos (Hoja _)           = 0
cantidadDeNodos (Nodo _ aIzq aDer) = 1 + cantidadDeNodos aIzq + cantidadDeNodos aDer

--Describe la cantidad de constructores en el árbol dado.
cantidadDeConstructores :: Arbol a b -> Int
cantidadDeConstructores (Hoja _)           = 1
cantidadDeConstructores (Nodo _ aIzq aDer) = 1 + cantidadDeConstructores aIzq + cantidadDeConstructores aDer

--Describe la representación como elemento del tipo Arbol BinOp Int de la expresión aritmética dada.
ea2Arbol :: EA -> Arbol BinOp Int
ea2Arbol (Const n)       = Hoja n 
ea2Arbol (BOp bop e1 e2) = Nodo bop (ea2Arbol e1) (ea2Arbol e2)


data Tree a = EmptyT | NodeT a (Tree a) (Tree a)
    deriving Show

esUno :: Int -> Bool
esUno n = n==1

arbol1 :: Tree Int
arbol1 = NodeT 5 EmptyT EmptyT
arbol2 :: Tree Int
arbol2 = NodeT 10 (NodeT 4 EmptyT EmptyT)
                  (NodeT 7 EmptyT EmptyT)
arbol3 :: Tree Int                  
arbol3 =NodeT 8
            (NodeT 3
                (NodeT 1 EmptyT EmptyT)
                (NodeT 6 
                    (NodeT 4 EmptyT EmptyT) 
                    (NodeT 7 EmptyT EmptyT)))
            (NodeT 10 EmptyT 
                    (NodeT 14 
                        (NodeT 13 EmptyT EmptyT) 
                        EmptyT))

--Describe el número resultante de sumar todos los números en el árbol dado.
sumarT :: Tree Int -> Int
sumarT EmptyT          = 0
sumarT (NodeT n t1 t2) = n + sumarT t1 + sumarT t2

--Describe la cantidad de elementos en el árbol dado.
sizeT :: Tree a -> Int
sizeT EmptyT          = 0
sizeT (NodeT _ t1 t2) = 1 + sizeT t1 + sizeT t2

--Indica si en el árbol dado hay al menos un elemento que cumple con el predicado dado.
anyT :: (a -> Bool) -> Tree a -> Bool
anyT f EmptyT          = False
anyT f (NodeT a t1 t2) = f a || anyT f t1 || anyT f t2

--Describe la cantidad de elementos en el árbol dado que cumplen con el predicado dado.
countT :: (a -> Bool) -> Tree a -> Int
countT f EmptyT = 0
countT f (NodeT a t1 t2) = if f a 
                            then 1 + countT f t1 + countT f t2
                            else countT f t1 + countT f t2

--Describe la cantidad de hojas del árbol dado.
countLeaves :: Tree a -> Int
countLeaves EmptyT                 = 0
countLeaves (NodeT _ EmptyT EmptyT) = 1
countLeaves (NodeT _ t1 t2)        = countLeaves t1 + countLeaves t2

--Describe la altura del árbol dado.
heightT :: Tree a -> Int
heightT EmptyT = 0
heightT (NodeT _ t1 t2) = 1 + max (heightT t1) (heightT t2)

--Describe la lista in order con los elementos del árbol dado.
inOrder :: Tree a -> [a]
inOrder EmptyT          = []
inOrder (NodeT a t1 t2) = a : (inOrder t1 ++ inOrder t2)

--Describe la lista donde cada elemento es una lista con los elementos de un nivel del árbol dado.
listPerLevel :: Tree a -> [[a]]
listPerLevel EmptyT          = []
listPerLevel (NodeT a t1 t2) = [a] : zipListas (listPerLevel t1) (listPerLevel t2)

--Describe un árbol con los mismos elemento que el árbol dado pero en orden inverso.
mirrorT :: Tree a -> Tree a
mirrorT EmptyT = EmptyT
mirrorT (NodeT a t1 t2) = NodeT a (mirrorT t2) (mirrorT t1)

--Describe la lista con los elementos del nivel dado en el árbol dado.
levelN :: Int -> Tree a -> [a]
levelN _ EmptyT          = [] 
levelN 0 (NodeT a _ _)   = [a]
levelN n (NodeT _ t1 t2) = levelN (n-1) t1 ++ levelN (n-1) t2

--Describe la lista con los elementos de la rama más larga del árbol.
ramaMasLarga :: Tree a -> [a]
ramaMasLarga EmptyT = []
ramaMasLarga (NodeT a t1 t2) = if heightT t1 > heightT t2 
                                  then a : ramaMasLarga t1
                                  else a : ramaMasLarga t2

--Describe la lista con todos los caminos existentes en el árbol dado.
todosLosCaminos :: Tree a -> [[a]]
todosLosCaminos EmptyT          = []
todosLosCaminos (NodeT a t1 t2) = [a] : agregarPrimero a (todosLosCaminos t1 ++ todosLosCaminos t2)

agregarPrimero :: a -> [[a]] -> [[a]]
agregarPrimero _ []     = []
agregarPrimero a (l:ls) = (a:l) : agregarPrimero a ls



data AppList a = Single a | Append (AppList a) (AppList a)
    deriving Show

lista1 :: AppList Int
lista1 = Single 5
lista2 :: AppList Int
lista2 = Append (Single 1) (Single 2)
lista3 :: AppList Int
lista3 = Append (Append (Single 1) (Single 2)) (Single 3)

--Describe la cantidad de elementos de la lista.
lenAL :: AppList a -> Int
lenAL (Single a)     = 1
lenAL (Append a1 a2) = lenAL a1 + lenAL a2

--Describe la lista resultante de agregar el elemento dado al principio de la lista dada.
consAL :: a -> AppList a -> AppList a
consAL a al = Append (Single a) al

--Describe el primer elemento de la lista dada.
headAL :: AppList a -> a
headAL (Single a)    = a
headAL (Append a1 _) = headAL a1

--Describe la lista resultante de quitar el primer elemento de la lista dada.
tailAL :: AppList a -> AppList a
tailAL (Single a)     =  error "appList no puede ser vacía"
tailAL (Append a1 a2) = case a1 of 
                            Single a -> a2
                            _        -> Append (tailAL a1) a2  

--Describe la lista resultante de agregar el elemento dado al final de la lista dada.
snocAL :: AppList a -> a -> AppList a
snocAL (Single a) a1 = Append (Single a) (Single a1)
snocAL al a          = Append al (Single a)

--Describe el último elemento de la lista dada.
lastAL :: AppList a -> a
lastAL (Single a)    = a
lastAL (Append _ a2) = lastAL a2 

--Describe la lista dada sin su último elemento.
initAL :: AppList a -> AppList a
initAL (Single a)     = error "Una appList no puede ser vacía"
initAL (Append a1 a2) = case a2 of
                            Single a -> a1
                            _        -> Append a1 (initAL a2)  

--Describe la lista dada con sus elementos en orden inverso.
reverseAL :: AppList a -> AppList a
reverseAL (Single a)     = Single a
reverseAL (Append a1 a2) = Append (reverseAL a2) (reverseAL a1)

--Indica si el elemento dado se encuentra en la lista dada.
elemAL :: Eq a => a -> AppList a -> Bool
elemAL e (Single a) = a==e
elemAL e (Append a1 a2) = elemAL e a1 || elemAL e a2 

--Describe el resultado de agregar los elementos de la primera lista adelante de los elementos de la segunda.
--NOTA: buscar la manera más eficiente de hacerlo.
appendAL :: AppList a -> AppList a -> AppList a
appendAL a1 a2 = Append a1 a2

--Describe la representación lineal de la lista dada.
appListToList :: AppList a -> [a]
appListToList (Single a)     = [a]
appListToList (Append a1 a2) = appListToList a1 ++ appListToList a2


data QuadTree a = LeafQ a | NodeQ (QuadTree a) (QuadTree a)
                                  (QuadTree a) (QuadTree a)
    deriving Show

data Color = RGB Int Int Int
    deriving Show

type Image = QuadTree Color

imagen1 = LeafQ (RGB 255 255 255)
imagen2 = NodeQ
            (LeafQ (RGB 255 0 0))     
            (LeafQ (RGB 0 255 0))     
            (LeafQ (RGB 0 0 255))     
            (LeafQ (RGB 255 255 0))   
        
imagen3 = NodeQ
            (LeafQ (RGB 0 0 0)) 
            (NodeQ              
                (LeafQ (RGB 255 0 0))
                (LeafQ (RGB 0 255 0))
                (LeafQ (RGB 0 0 255))
                (LeafQ (RGB 255 255 255)))
            (LeafQ (RGB 128 128 128)) 
            (LeafQ (RGB 255 255 0))   

--Describe la altura del árbol dado.
heightQT :: QuadTree a -> Int
heightQT (LeafQ _)               = 1
heightQT (NodeQ qt1 qt2 qt3 qt4) = 1 + maximum [heightQT qt1, heightQT qt2, heightQT qt3, heightQT qt4]

--Describe la cantidad de hojas del árbol dado.
countLeavesQT :: QuadTree a -> Int
countLeavesQT (LeafQ _)               = 1
countLeavesQT (NodeQ qt1 qt2 qt3 qt4) = countLeavesQT qt1 + countLeavesQT qt2 + countLeavesQT qt3 + countLeavesQT qt4

--Describe la cantidad de constructores del árbol dado.
sizeQT :: QuadTree a -> Int
sizeQT (LeafQ _)               = 1
sizeQT (NodeQ qt1 qt2 qt3 qt4) = 1 + sizeQT qt1 + sizeQT qt2 + sizeQT qt3 + sizeQT qt4

--Describe el árbol resultante de transformar en hoja todos aquellos nodos 
--para los que se cumpla que todas sus hojas tengan el mismo valor.
--compress :: QuadTree a -> QuadTree a


--Describe el árbol resultante de transformar en nodo (manteniendo el dato de la
--hoja correspondiente) todas aquellas hojas que no se encuentren en el nivel de la altura del árbol.
--uncompress :: QuadTree a -> QuadTree a

--Describe la imagen dada en el tamaño dado.
--Precondición: el tamaño dado es potencia de 4 y es mayor o igual a la altura del árbol dado elevado a la 4ta potencia.
--NOTA: Una imagen tiene tamaño t cuando todas las hojas se encuentran en el nivel log4(t).
--render :: Image -> Int -> Image