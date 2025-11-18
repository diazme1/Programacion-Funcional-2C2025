data NExp = Var Variable | NCte Int | NBOp NBinOp NExp NExp
    deriving Show
data NBinOp = Add | Sub | Mul | Div | Mod | Pow
    deriving Show
type Variable = String

----------TAD: Memoria----------
data Memoria = M [(Variable, Int)]
  deriving (Show, Eq)

-- memoria vacía
enBlanco :: Memoria
enBlanco = M []

-- busca el valor de una variable
cuantoVale :: Variable -> Memoria -> Maybe Int
cuantoVale v (M m) = lookup v m

-- registra o actualiza una variable
recordar :: Variable -> Int -> Memoria -> Memoria
recordar v n (M m) =
  M ((v, n) : filter ((/= v) . fst) m)
-- agrega el par (v,n) y elimina versiones anteriores

-- lista de variables registradas
variables :: Memoria -> [Variable]
variables (M m) = map fst m



--Describe el número resultante de evaluar la expresión dada a partir de la memoria dada.
evalNExp :: NExp -> Memoria -> Int
evalNExp (NCte n) _        = n
evalNExp (Var v) m         = case cuantoVale v m of
                                Just n -> n
                                Nothing -> error "Variable no encontrada en memoria."
evalNExp (NBOp op e1 e2) m = evalOp op (evalNExp e1 m) (evalNExp e2 m)

evalOp :: NBinOp -> Int -> Int -> Int
evalOp Add = (+)
evalOp Sub = (-)
evalOp Mul = (*)  
evalOp Div = div
evalOp Mod = mod
evalOp Pow = (^)

--Describe una expresión con el mismo significado que la dada, pero simplificada y reemplazando las
--subexpresiones que no dependan de la memoria por su expresión más sencilla. 
--La resolución debe ser exclusivamente simbólica.
cfNExp :: NExp -> NExp
cfNExp (NBOp op e1 e2) = cfOp op (cfNExp e1) (cfNExp e2)
cfNExp nexp         = nexp

cfOp :: NBinOp -> NExp -> NExp -> NExp
cfOp op (NCte n1) (NCte n2) = NCte (evalOp op n1 n2)
cfOp op e1 e2               = NBOp op e1 e2

--Demostrar propiedad:
--evalNExp . cfNExp = evalNExp

-----------Ejercicio 2-----------

data BExp = BCte Bool | Not BExp | And BExp BExp | Or BExp BExp | ROp RelOp NExp NExp
    deriving Show
data RelOp = Eq | NEq | Gt | GEq | Lt | LEq
    deriving Show

--Describe el booleano que resulta de evaluar la expresión dada a partir de la memoria dada.
evalBExp :: BExp -> Memoria -> Bool
evalBExp (BCte b)        m = b
evalBExp (Not b)         m = not (evalBExp b m)
evalBExp (And b1 b2)     m = evalBExp b1 m && evalBExp b2 m
evalBExp (Or b1 b2)      m = evalBExp b1 m || evalBExp b2 m
evalBExp (ROp rop b1 b2) m = evalROp rop (evalNExp b1 m) (evalNExp b2 m)

evalROp :: RelOp -> Int -> Int -> Bool
evalROp Eq  = (==)
evalROp NEq = (/=)
evalROp Gt  = (>)
evalROp GEq = (>=)
evalROp Lt  = (<)
evalROp LEq = (<=)

--Describe una expresión con el mismo significado que la dada, pero reemplazando las
--subexpresiones que no dependan de la memoria por su expresión más sencilla. 
--La resolución debe ser exclusivamente simbólica.
--cfBExp :: BExp -> BExp


-----------Ejercicio 3-----------
data Programa = Prog Bloque
    deriving Show
type Bloque = [Comando]
type Nombre = String
data Comando = Assing Nombre NExp | If BExp Bloque Bloque | While BExp Bloque
    deriving Show 

--Describe la memoria resultante de evaluar el programa dado a partir de la memoria dada.
evalProg :: Programa -> Memoria -> Memoria
evalProg 

--Describe la memoria resultante de evaluar el bloque dado a partir de la memoria dada.
evalBlq :: Bloque -> Memoria -> Memoria

--Describe la memoria resultante de evaluar el comando dado a partir de la memoria dada.
evalCom :: Comando -> Memoria -> Memoria

--Describe un programa con el mismo significado que el dado, pero aplicando constant folding sobre las expresiones y descartando los comandos
--que no serán ejecutados.
optimizeCF :: Programa -> Programa
