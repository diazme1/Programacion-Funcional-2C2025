data Nim = Empty          -- juego vacío
           | Heap Int Nim -- pila con n fichas (n>0)

type Move = (Int,Int)                 -- jugada: de la fila i remover k fichas

data GameTree = Nil                     -- árbol vacío
                | Node (Move, GameTree) -- (jugada, hijos - jugadas del contrincante)
                       GameTree         -- hermanos (otras jugadas propias)

-- Ej 1)

foldNim :: b -> (Int -> b -> b) -> Nim -> b
foldNim f h Empty      = f
foldNim f h (Heap x n) = h x (foldNim f h n)

recNim :: b -> (Int -> b -> Nim -> b) -> Nim -> b
recNim f h Empty      = f
recNim f h (Heap x n) = h x (recNim f h n) n

foldGame :: b -> ((Move, b) -> b -> b) -> GameTree -> b
foldGame f g Nil               = f
foldGame f g (Node (m, jo) jp) = g (m , (foldGame f g jo)) (foldGame f g jp)

recGame :: b -> ((Move, b) -> b -> GameTree -> GameTree -> b) -> GameTree -> b
recGame f g Nil               = f
recGame f g (Node (m, jo) jp) = g (m , (recGame f g jo)) (recGame f g jp) jo jp 

-- Ej 2)

heaps :: Nim -> Int   -- indica la cantidad de pilas en un juego de Nim dado.
heaps = foldNim 0 (const (+1))

chips :: Nim -> Int   -- que indica la cantidad de fichas en un juego de Nim dado.
chips = foldNim 0 (+)

maxHeap :: Nim -> Int -- que computa la cantidad de fichas en el heap más grande.
maxHeap = foldNim 0 max

alongside :: Nim -> Nim -> Nim 
alongside = foldNim id (\x r n2 -> Heap x (r n2))

gameHeight :: GameTree -> Int
gameHeight = foldGame 0 (\(m,jo) jp -> 1 + max jo jp)

branches :: GameTree -> [[Move]]
branches = foldGame [[]] (\(m,jo) jp -> map (m:) (jo ++ jp)) 

-- Ej 3)

--turn :: Nim -> Move -> Maybe Nim 
---- ERROR: g Int -> Maybe ((a,Int) -> Nim) -> Nim -> (a,Int) -> Maybe Nim
--turn = recNim f g               
--     where f              _     = Nothing 
--           g x r        n (0,k) = case compare k x of GT -> Nothing 
--                                                      EQ -> Just n
--                                                      LT -> Just (Heap (x-k) n)
--           g x Nothing  n _     = Nothing
--           g x (Just r) n (i,k) = Just (Heap x (r (i-1,k)))

turn :: Nim -> Move -> Maybe Nim 
turn = recNim f g               
     where f       _     = Nothing 
           g x r n (0,k) = case compare k x of GT -> Nothing 
                                               EQ -> Just n
                                               LT -> Just (Heap (x-k) n)
           g x r n (i,k) = case r (i-1,k) of Just ni -> Just (Heap x ni)
                                             Nothing -> Nothing           

moves :: Nim -> [Move] -- retorna la lista de jugadas válidas del jugador actual
moves = foldNim [] (\x r -> listaDeJugdasHeap x ++ incrementarIndices r) --error me olvide de cambiar rec por fold

listaDeJugdasHeap :: Int -> [Move]
listaDeJugdasHeap 0 = [] 
listaDeJugdasHeap x = (0, x) : listaDeJugdasHeap (x-1)

incrementarIndices :: [Move] -> [Move] 
incrementarIndices = map sumarAlPrimero

sumarAlPrimero :: (Int,Int) -> (Int,Int)
sumarAlPrimero (x, y) = (x+1, y)



