data Fila a = Fin
            | Celda Int a (Fila a)
--sawady.faso@gmail.com

-- f Fin = ...
-- f (Celda i a fil) = .. f fil

-- cuenta la cantidad de veces que aparece un elemento que cumple el predicado
countF :: (a -> Bool) -> Fila a -> Int
countF p Fin             = 0
countF p (Celda i a fil) = fromEnum (p a) + (countF p fil)

-- le suma N a los elementos donde el predicado da verdadero
sumarN :: (a -> Bool) -> Int -> Fila a -> Fila a
sumarN f n Fin             = Fin
sumarN f n (Celda i a fil) = Celda (sumarSiCumple f a i n) a (sumarN f n fil)


sumarSiCumple:: (a -> Bool) -> a -> Int -> Int  -> Int
sumarSiCumple f a i n = if f a then i+n else i


-- Junta dos filas manteniendo el orden de los elementos
concatenarF :: Fila a -> Fila a -> Fila a
concatenarF Fin             f2 = f2
concatenarF (Celda i a fil) f2 = Celda i a (concatenarF fil f2)


-- transforma cada elemento aplicando una función a los mismos
mapF :: (a -> b) -> Fila a -> Fila b
mapF f Fin             = Fin
mapF f (Celda i a fil) = Celda i (f a) (mapF f fil)

-- transforma una fila de filas en una fila
aplanar :: Fila (Fila a) -> Fila a 


-- los elementos iguales los colapsa en una misma posición (sean contiguos o no)
comprimir :: Fila a -> Fila a

-- denota la composición de las funciones manteniendo el orden de aparición y aplicandolas las veces que aparezca
componer :: Fila (a -> a) -> (a -> a)
componer Fin             = id
componer (Celda i f fil) = many i f . (componer fil)


data Functions a = Id
                 | F (a -> a) (Functions a)
                 | B (a -> Bool) (Functions a) (Functions a)

evalF :: Functions a -> (a -> a)
manyF :: Int -> (a -> a) -> Functions a
toFila :: Functions a -> Fila a



data Pizza = Prepizza | Capa Ingrediente Pizza

data Ingrediente = Aceitunas Int | Jamón | Queso | Salsa | MezclaRara

f Prepizza = ...
f (Capa i pz) = ... f pz

cantidadDeCapas :: Pizza -> Int
cantidadDeCapas Prepizza    = 0
cantidadDeCapas (Capa i pz) = 1 + cantidadDeCapas pz

cantidadDeAceitunas :: Pizza -> Int
cantidadDeAceitunas Prepizza    = 0
cantidadDeAceitunas (Capa i pz) = aceitunas i + cantidadDeAceitunas pz

aceitunas (Aceitunas n) = n
aceitunas _             = 0

duplicarAceitunas :: Pizza -> Pizza
duplicarAceitunas Prepizza    = Prepizza
duplicarAceitunas (Capa i pz) = Capa (duplicarSiAceitunas i) (duplicarAceitunas pz)

duplicarSiAceitunas (Aceitunas n) = Aceitunas (n*2)
duplicarSiAceitunas i             = i

sinLactosa :: Pizza -> Pizza
sinLactosa Prepizza    = Prepizza
sinLactosa (Capa i pz) = 
    if tieneLactosa i
       then sinLactosa pz
       else Capa i (sinLactosa pz)

tieneLactosa Queso = True
tieneLactosa _     = False

aptaIntolerantesLactosa :: Pizza -> Bool
aptaIntolerantesLactosa Prepizza    = True
aptaIntolerantesLactosa (Capa i pz) = not (tieneLactosa i) && aptaIntolerantesLactosa pz

conDescripcionMejorada :: Pizza -> Pizza
conDescripcionMejorada Prepizza    = Prepizza
conDescripcionMejorada (Capa i pz) = juntarAceitunas i (conDescripcionMejorada pz)

juntarAceitunas (Aceitunas n) (Capa (Aceitunas m) pz) = Capa (Aceituna (n+m)) pz
juntarAceitunas i             pz                      = Capa i pz