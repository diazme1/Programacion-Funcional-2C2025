data Nim = Empty -- juego vacío
            | Heap Int Nim -- pila con n fichas (n>0)
            deriving Show
type Move = (Int,Int) -- jugada: de la fila i remover k fichas
data GameTree = Nil -- árbol vacío
                | Node (Move, GameTree) -- (jugada, hijos - jugadas del contrincante)
                        GameTree -- hermanos (otras jugadas propias)
                deriving Show

nimEj = Heap 2 (Heap 1 Empty)
nimEj1 = Heap 4 (Heap 3 Empty)
gameEj = Node ((0,1), Node ((0,1), Nil) 
                     (Node ((1,1), Nil) 
                      Nil))
            --(0,1)--hijo--  
        (Node ((0,2), 
                Node ((1,1), Nil) 
                Nil)
        (Node ((1,1), 
                Node ((0,1), Nil) 
                (Node ((0,2), Nil) Nil)) 
                Nil)
        )

--Ejercicio 1)
foldNim :: b -> (Int -> b -> b) -> Nim -> b
foldNim fb fr Empty        = fb
foldNim fb fr (Heap n nim) = fr n (foldNim fb fr nim)

recNim :: b -> (Int -> Nim -> b -> b) -> Nim -> b
recNim fb fr Empty        = fb 
recNim fb fr (Heap n nim) = fr n nim (recNim fb fr nim)

foldGame :: b -> ((Move, GameTree) -> b -> b -> b) -> GameTree -> b
foldGame fb fr Nil           = fb
foldGame fb fr (Node x game) = fr x (foldGame fb fr (snd x)) (foldGame fb fr game)

recGame :: b -> ((Move, GameTree) -> GameTree -> b -> b) -> GameTree -> b
recGame fb fr Nil           = fb
recGame fb fr (Node x game) = fr x game (recGame fb fr game)

--Ejercicio 2)
heaps :: Nim -> Int
heaps = foldNim 0 (\_ r -> 1 + r)

chips :: Nim -> Int
chips = foldNim 0 (+)

maxHeap :: Nim -> Int
maxHeap = foldNim 0 max

alongside :: Nim -> Nim -> Nim
alongside nim1 nim2 = recNim nim2 (\n _ r -> Heap n r) nim1

-- alongside nim1 Empty = nim1 --> caso recursivo 
-- alongside Empty nim2 = nim2 --> caso base
-- alongside (Heap n nim1) nim2  = Heap n (alongside nim1 nim2) 
                                --nim1  n rec  

gameHeight :: GameTree -> Int
gameHeight = foldGame 0 (\x rch rsib -> max (1+ rch) rsib)

-- gameHeight Nil = 0
-- gameHeight (Node (_, jugadas) gt) = max (1 + gameHeight jugadas) (gameHeight gt)

branches :: GameTree -> [[Move]]
branches = foldGame [[]] (\x rch rsib -> addFirst (fst x) rch ++ rsib )

addFirst :: Move -> [[Move]] -> [[Move]]
addFirst _   []     = []
addFirst m (mv:mvs) = (m:mv) : addFirst m mvs

--Ejercicio 3)