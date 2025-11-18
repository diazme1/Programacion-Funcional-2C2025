data Point = P Float Float
    deriving Show
data Pen = NoColour | Colour Float Float Float
    deriving Show

type Angle = Float
type Distance = Float

data Turtle = T Pen Angle Point
    deriving Show
data TCommand = Go Distance | Turn Angle | GrabPen Pen | TCommand :#: TCommand -- :#: es un constructor INFIJO
    deriving Show

data Line = Line Pen Point Point
    deriving Show
-- Dibuja una línea del 1er punto hasta el 2do, con el color dado
data LineAssembly = LA [Line]
    deriving Show

endPoint :: Point -> Angle -> Distance -> Point
endPoint (P x y) ang d =
    let rad = ang * pi / 180
    in P (x + d * cos rad)
         (y + d * sin rad)
-- Calcula el punto final al arrancar el dibujo en el punto dado,
-- apuntando en el ángulo dado, y moverse la distancia dada

compileT :: TCommand -> Turtle -> (LineAssembly, Turtle)
compileT (Go d) (T pen a p)      = let endp = endPoint p a d in
                                        if isColour pen then (LA [Line pen p endp], T pen a endp)
                                                        else (LA [], T pen a endp)
compileT (Turn a) (T pen ang p)  = (LA [], (T pen (ang+a) p))
compileT (GrabPen pen) (T _ a p) = (LA [], (T pen a p))
compileT (c :#: cs) t = let compileC = compileT c t 
                            compileRs = compileT cs (snd compileC) in
                            (combineLA (fst compileC) (fst compileRs), snd compileRs)

isColour :: Pen -> Bool
isColour NoColour = False
isColour _        = True

combineLA :: LineAssembly -> LineAssembly -> LineAssembly
combineLA (LA l1) (LA l2) = LA (l1++l2)

cmd = Go 10 :#: Turn 90 :#: Go 20
t0 = T NoColour 0 (P 0 0)

--Go 10 --> LA [], T NoColour 0 (P 10.0 0.0)
--Turn 90 --> LA [], T NoColour 90.0 (P 10.0 0.0)
--Go 20 --> 