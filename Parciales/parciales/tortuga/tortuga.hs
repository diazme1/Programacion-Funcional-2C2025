data   Point      =   P   Float   Float   deriving Show
data   Pen        =   NoColour   |   Colour   Float   Float   Float   deriving Show
type   Angle      =   Float   
type   Distance   =   Float   
data   Turtle     =   T   Pen   Angle   Point   deriving Show
data   TCommand   =   Go   Distance   |   Turn   Angle   |   GrabPen   Pen   |   TCommand :#: TCommand   --   :#:   es   un   constructor   INFIJO   
    deriving Show

-- EJERCICIO 1                
-- Dar el tipo y escribir el esquema de recursión primitiva de TCommand.
recTC :: (Distance -> b) -> (Angle -> b) -> (Pen -> b) -> (b -> TCommand -> b -> TCommand -> b) -> TCommand -> b 
recTC g t gp h (Go d)        = g d 
recTC g t gp h (Turn a)      = t a  
recTC g t gp h (GrabPen p)   = gp p 
recTC g t gp h (tc1 :#: tc2) = h (recTC g t gp h tc1) tc1 (recTC g t gp h tc2) tc2

-- Dar el tipo y escribir la función que expresa el esquema de recursión estructural sobre TCommand, sin utilizar recursión explícita.
foldTC :: (Distance -> b) -> (Angle -> b) -> (Pen -> b) -> (b -> b -> b) -> TCommand -> b
foldTC g t gp h = recTC g t gp (\tcr1 _ tcr2 _ -> h tcr1 tcr2) 


-- EJERCICIO 2
-- Definir sin usar recursión explícita:

-- Dado un programa de gráficos de tortuga, describe la cantidad de constructores (:#:) del mismo.
cantSeq :: TCommand -> Int
cantSeq = foldTC (const 0) (const 0) (const 0) (\n m -> 1 + n + m) 

-- Dado un programa de gráficos de tortuga, describe la cantidad total de constructores (:#:) del mismo que están en algún argumento izquierdo de un (:#:)
cantSeqsALaIzqDeSeq :: TCommand -> Int
cantSeqsALaIzqDeSeq = recTC (const 0) (const 0) (const 0) (\n tc1 m tc2 -> unoIfExist tc1 + n + m)

unoIfExist :: TCommand -> Int 
unoIfExist (_ :#: _) = 1 
unoIfExist _         = 0


-- Dado un programa de gráficos de tortuga, describe uno equivalente que no tiene argumentos a la izquierda de un (:#:) 
-- que estén formados por (:#:).
-- SUGERENCIA: Considerar definir una función auxiliar para el caso inductivo.
assocDer :: TCommand -> TCommand
assocDer (Go d)        = Go d
assocDer (Turn a)      = Turn a 
assocDer (GrabPen p)   = GrabPen p 
assocDer (tc1 :#: tc2) = assocDerAux tc1 (assocDer tc2)

assocDerAux :: TCommand -> TCommand -> TCommand
assocDerAux (tc1' :#: tc2') tc2 = assocDerAux tc1' (assocDerAux tc2' tc2) 
assocDerAux tc1             tc2 = tc1 :#: tc2 

assocDer' :: TCommand -> TCommand
assocDer' = recTC Go Turn GrabPen (\_ tc1 tcr2 _ -> assocDerAux tc1 tcr2)

-- Escala cada comando Go de su argumento según un factor dado. Por ejemplo, scaleTC(Go 10 :#: Go 30) 2 = (Go 20 :#: Go 60)
scaleTC :: TCommand -> Float -> TCommand
scaleTC = foldTC    (\d fac -> Go (d * fac)) 
                    (\a fac -> Turn a)  
                    (\p fac -> GrabPen p)
                    (\f h fac -> f fac :#: h fac)

-- scaleTC  tc fac = (foldTC (\d -> Go (d * fac)) Turn GrabPen (:#:)) tc 


-- EJERCICIO 3 
-- Dar una versión de la función cantSeqs dada a continuación, sin utilizar recursión explicita
cantSeqs :: TCommand -> (Int, Int)
cantSeqs (c1 :#: c2) = let (cs1, csis1) = cantSeqs c1 
                           (cs2, csis2) = cantSeqs c2 
                        in (1 + cs1 + cs2, cs1 + csis1 + csis2) 
cantSeqs c           = (0,0)

cantSeqs' :: TCommand -> (Int, Int)
cantSeqs' = foldTC  (const (0,0)) 
                    (const (0,0)) 
                    (const (0,0)) 
                    (\(cs1, csis1) (cs2, csis2) -> (1 + cs1 + cs2, cs1 + csis1 + csis2))

-- EJEMPLOS
triangle :: TCommand   
triangle = Go 30 :#: Turn 120 :#: Go 30 :#: Turn 120 :#: Go 30 :#: Turn 120