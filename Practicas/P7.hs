import Language.Haskell.TH (condE)
import Data.Binary.Get (Decoder(Fail))
data Pizza = Prepizza | Capa Ingrediente Pizza
    deriving Show
data Ingrediente = Aceitunas Int | Anchoas | Cebolla | Jamón | Queso | Salsa
    deriving Show

pizza = Capa Anchoas (Capa Cebolla Prepizza)
pizzaConAceitunas = Capa Anchoas (Capa (Aceitunas 1) (Capa (Aceitunas 2) Prepizza))
pizzaConQueso = Capa Anchoas (Capa Queso (Capa (Aceitunas 2) Prepizza))

--3)
cantidadDeCapas :: Pizza -> Int
cantidadDeCapas Prepizza   = 0
cantidadDeCapas (Capa i p) = 1 + cantidadDeCapas p

cantidadDeAceitunas :: Pizza -> Int
cantidadDeAceitunas Prepizza               = 0
cantidadDeAceitunas (Capa (Aceitunas n) p) = n + cantidadDeAceitunas p
cantidadDeAceitunas (Capa _ p)             = cantidadDeAceitunas p

duplicarAceitunas :: Pizza -> Pizza
duplicarAceitunas Prepizza               = Prepizza
duplicarAceitunas (Capa (Aceitunas n) p) = Capa (Aceitunas (n*2)) (duplicarAceitunas p)
duplicarAceitunas (Capa i p)             = Capa i (duplicarAceitunas p)

-- cantidadDeAceitunas (duplicarAceitunas p ) = 2 * cantidadDeAceitunas p
{-
Para todo p,
    ¿ cantidadDeAceitunas (duplicarAceitunas p ) = 2 * cantidadDeAceitunas p ?
Sea p una pizza cualquiera, voy a demostrar por inducción estructural sobre p

Caso base: p = Prepizza
    ¿ cantidadDeAceitunas (duplicarAceitunas Prepizza) = 2 * cantidadDeAceitunas Prepizza ?

Caso inductivo 1, p = Capa (Aceitunas n) p1
    ¿ cantidadDeAceitunas (duplicarAceitunas (Capa (Aceitunas n) p1)) = 2 * cantidadDeAceitunas (Capa (Aceitunas n) p1) ?

Caso inductivo 2, p = Capa i p1
    ¿ cantidadDeAceitunas (duplicarAceitunas (Capa i p1)) = 2 * cantidadDeAceitunas (Capa i p1) ?

Caso base:
    -- lado izq
    cantidadDeAceitunas (duplicarAceitunas Prepizza)
    -- def duplicarAceitunas.1
    cantidadDeAceitunas Prepizza
    -- def cantidadDeAceitunas.1
    0

    -- lado der
    2 * cantidadDeAceitunas Prepizza
    -- def cantidadDeAceitunas.1
    2 * 0
    -- aritmetica
    0

Caso inductivo 1:
    -- lado izq
    cantidadDeAceitunas (duplicarAceitunas (Capa (Aceitunas n) p1))
    -- def duplicarAceitunas.2
    cantidadDeAceitunas (Capa (Aceitunas (n*2)) (duplicarAceitunas p2))
    -- def cantidadDeAceitunas.2
    (n*2) + cantidadDeAceitunas (duplicarAceitunas p2)
    -- por HI
    (n * 2) + 2 * cantidadDeAceitunas p2)

    -- lado der
    2 * cantidadDeAceitunas (Capa (Aceitunas n) p1)
    -- def cantidadDeAceitunas.2
    2 * (n + cantidadDeAceitunas p2)
    -- distributiva
    (2 * n) + (2 * cantidadDeAceitunas p2)

Caso inductivo 2:
    -- lado izq
    cantidadDeAceitunas (duplicarAceitunas (Capa i p1))
    -- def duplicarAceitunas.3
    cantidadDeAceitunas (Capa i (duplicarAceitunas p1))
    -- def cantidadDeAceitunas.3 
    cantidadDeAceitunas (duplicarAceitunas p1)
    -- por HI
    2 * cantidadDeAceitunas p1
    

    -- lado der
    2 * cantidadDeAceitunas (Capa i p1)
    -- def cantidadDeAceitunas.3
    2 * cantidadDeAceitunas p1
-}

sinLactosa :: Pizza -> Pizza
sinLactosa Prepizza       = Prepizza
sinLactosa (Capa Queso p) = sinLactosa p 
sinLactosa (Capa i p)     = Capa i (sinLactosa p)

aptaIntoleranteLactosa :: Pizza -> Bool
aptaIntoleranteLactosa Prepizza       = True 
aptaIntoleranteLactosa (Capa Queso p) = False
aptaIntoleranteLactosa (Capa i p)     = aptaIntoleranteLactosa p

conDescripcionMejorada :: Pizza -> Pizza
conDescripcionMejorada Prepizza                                    = Prepizza
conDescripcionMejorada (Capa (Aceitunas n) (Capa (Aceitunas m) p)) = Capa (Aceitunas (n+m)) (conDescripcionMejorada p)
conDescripcionMejorada (Capa i p)                                  = Capa i (conDescripcionMejorada p)


------------------- Sección II -------------------
type Nombre = String
data Planilla = Fin | Registro Nombre Planilla
    deriving Show
data Equipo = Becario Nombre | Investigador Nombre Equipo Equipo Equipo
    deriving Show

planilla = Registro "Emilia" (Registro "Amalia" Fin)
planilla1 = Registro "Guadalupe" (Registro "Carolina" Fin)
equipo = Investigador "Emilia" (Becario "Abril") 
                               (Becario "Carolina")
                               (Becario "Alejandra")
equipo1 = Investigador "Emilia" (Investigador "Abril" (Becario "Tomas")
                                                      (Becario "Fabian")
                                                      (Becario "Tobias")) 
                               (Becario "Carolina")
                               (Becario "Alejandra")

--3)
--Describe la cantidad de nombres en una planilla dada.
largoDePlanilla :: Planilla -> Int
largoDePlanilla Fin          = 0
largoDePlanilla (Registro n p) = 1 + largoDePlanilla p

--Toma un nombre y una planilla e indica si en la planilla dada está el nombre dado.
esta :: Nombre -> Planilla -> Bool
esta n Fin = False
esta n (Registro np p) = n == np || esta n p

--Toma dos planillas y genera una única planilla con los registros de ambas planillas.
juntarPlanillas :: Planilla -> Planilla -> Planilla
juntarPlanillas Fin p2             = p2
juntarPlanillas (Registro n p1) p2 = Registro n (juntarPlanillas p1 p2)


--Describe la cantidad de niveles jerárquicos de un equipo dado.
nivelesJerarquicos :: Equipo -> Int
nivelesJerarquicos (Becario n) = 1
nivelesJerarquicos (Investigador n e e1 e2) = 1 + maximum [nivelesJerarquicos e, nivelesJerarquicos e1, nivelesJerarquicos e2]

--Describe la cantidad de integrantes de un equipo dado.
cantidadDeIntegrantes :: Equipo -> Int
cantidadDeIntegrantes (Becario n) = 1
cantidadDeIntegrantes (Investigador n e e1 e2) = 1 + cantidadDeIntegrantes e + cantidadDeIntegrantes e1 + cantidadDeIntegrantes e2


--Describe la planilla de integrantes de un equipo dado.
planillaDeIntegrantes :: Equipo -> Planilla
planillaDeIntegrantes (Becario n) = Registro n Fin
planillaDeIntegrantes (Investigador n e e1 e2) = Registro n (juntarPlanillas3 (planillaDeIntegrantes e) (planillaDeIntegrantes e1) (planillaDeIntegrantes e2))

juntarPlanillas3 :: Planilla -> Planilla -> Planilla -> Planilla
juntarPlanillas3 Fin p2 p3              = juntarPlanillas p2 p3
juntarPlanillas3 (Registro n p1) p2 p3  = Registro n (juntarPlanillas3 p1 p2 p3)


------------------- Sección III -------------------
--3)
data Dungeon a = Habitacion a | Pasaje (Maybe a) (Dungeon a) | Bifurcacion (Maybe a) (Dungeon a) (Dungeon a)
    deriving Show

dungeon1 =
  Pasaje (Just "Puerta rota")
    (Pasaje Nothing
      (Pasaje (Just "Trampa oculta")
        (Habitacion "Tesoro final")))

dungeon2 =
  Bifurcacion (Just "Entrada principal")
    (Pasaje (Just "Pasillo izquierdo")
      (Bifurcacion Nothing
        (Habitacion "Cámara de los murciélagos")
        (Habitacion "Cámara de los esqueletos")))
    (Pasaje (Just "Pasillo derecho")
      (Pasaje (Just "Puerta secreta")
        (Habitacion "Sala del dragón")))

llenoDe1 =
  Pasaje (Just 1)
    (Pasaje (Just 1)
      (Pasaje (Just 1)
        (Habitacion 1)))

--Describe la cantidad de bifurcaciones de un dungeon dado.
cantidadDeBifurcaciones :: Dungeon a -> Int
cantidadDeBifurcaciones (Habitacion a)        = 0 
cantidadDeBifurcaciones (Bifurcacion _ d1 d2) = 1 + cantidadDeBifurcaciones d1 + cantidadDeBifurcaciones d2
cantidadDeBifurcaciones (Pasaje _ d)          = cantidadDeBifurcaciones d


--Describe la cantidad de puntos interesantes de un dungeon dado. Los puntos interesantes son los lugares donde puede aparecer un elemento.
cantidadDePuntosInteresantes :: Dungeon a -> Int
cantidadDePuntosInteresantes (Habitacion a)        = 1
cantidadDePuntosInteresantes (Pasaje a d)          = 1 + cantidadDePuntosInteresantes d
cantidadDePuntosInteresantes (Bifurcacion a d1 d2) = 1 + cantidadDePuntosInteresantes d1 + cantidadDePuntosInteresantes d2



--Describe la cantidad de puntos interesantes del dungeon dado en las que no hay ningún elemento.
cantidadDePuntosVacios :: Dungeon a -> Int
cantidadDePuntosVacios (Habitacion a)        = 0
cantidadDePuntosVacios (Pasaje a d)          = case a of 
                                                Just a -> cantidadDePuntosVacios d
                                                Nothing -> 1 + cantidadDePuntosVacios d
cantidadDePuntosVacios (Bifurcacion a d1 d2) = case a of 
                                                Just a -> cantidadDePuntosVacios d1 + cantidadDePuntosVacios d2
                                                Nothing -> 1 + cantidadDePuntosVacios d1 + cantidadDePuntosVacios d2

--Dado un elemento y un dungeon, describe la cantidad de puntos interesantes del dungeon en las que se encuentra el elemento dado.
cantidadDePuntosCon :: Eq a => a -> Dungeon a -> Int
cantidadDePuntosCon z (Habitacion e)        = unoSiEs0Sino z e 
cantidadDePuntosCon z (Pasaje e d)          = unoSiEs0SinoMaybe z e + cantidadDePuntosCon z d
cantidadDePuntosCon z (Bifurcacion e d1 d2) = unoSiEs0SinoMaybe z e + cantidadDePuntosCon z d1 + cantidadDePuntosCon z d2

unoSiEs0Sino :: Eq a => a -> a -> Int
unoSiEs0Sino a b = if a == b then 1 else 0
                   
unoSiEs0SinoMaybe :: Eq a => a -> Maybe a -> Int
unoSiEs0SinoMaybe a b = case b of
                          Just b -> if a == b then 1 else 0
                          Nothing -> 0


--Indica si no hay bifurcaciones en un dungeon dado. 
esLineal :: Dungeon a -> Bool
esLineal (Habitacion _)      = True
esLineal (Pasaje _ d)        = esLineal d
esLineal (Bifurcacion _ _ _) = False


--Dado un elemento y un dungeon, indica si el elemento se encuentra en todas las posiciones del dungeon.
llenoDe :: Eq a => a -> Dungeon a -> Bool
llenoDe e1 (Habitacion e2) = e1 == e2
llenoDe e1 (Pasaje e2 d)   = case e2 of
                               Just e2 -> e1==e2 && llenoDe e1 d
                               Nothing -> False
llenoDe e1 (Bifurcacion e2 d1 d2) = case e2 of
                                      Just e2 -> e1==e2 && llenoDe e1 d1 && llenoDe e1 d2
                                      Nothing -> False

