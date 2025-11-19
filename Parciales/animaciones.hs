zipListas :: [[a]] -> [[a]] -> [[a]]
zipListas []       ls       = ls
zipListas ls        []      = ls
zipListas (l1:ls1) (l2:ls2) = (l1 ++ l2) : zipListas ls1 ls2

data Accion a = Paso a | SaltoArriba a | SaltoAdelante a | Giro a
    deriving Show
type Tiempo = Int
type Duracion = Int
data Animacion a = Espera Duracion 
                 | Mov Duracion (Accion a) 
                 | Sec (Animacion a) (Animacion a) 
                 | Par (Animacion a) (Animacion a)
                 deriving Show

type Frame a = [Accion a]
type Simulador a = Tiempo -> Frame a
                 
ej = Sec (Par (Sec (Sec (Espera 1) (Mov 3 (Paso "Bob")))
                (Sec (Mov 1 (Giro "Bob")) (Mov 2 (SaltoAdelante "Bob"))))
              (Sec (Mov 2 (SaltoAdelante "Ana"))
                (Sec (Mov 1 (Giro "Ana")) (Sec (Mov 3 (Paso "Ana")) (Espera 1)))))
         (Espera 1)

fsEJ = [
        [SaltoAdelante "Ana"] , 
        [Paso "Bob", SaltoAdelante "Ana"] ,
        [Paso "Bob" , Paso "Ana"] , 
        [Giro "Ana" , Paso "Bob"] ,
        [Giro "Bob" , Paso "Ana"] , 
        [SaltoAdelante "Bob" , Paso "Ana"] ,
        [SaltoAdelante "Bob"] , 
        []
       ]

--Ejercicio 1)
--combinarSinDuplicados :: [Int] -> [Int] -> [Int]

--Ejercicio 2)
duracion :: Animacion a -> Int
duracion (Espera n) = n
duracion (Mov n _)  = n
duracion (Sec a1 a2) = duracion a1 + duracion a2
duracion (Par a1 a2) = let r1= duracion a1 
                           r2 = duracion a2 in
                        max r1 r2

alargar :: Int -> Animacion a -> Animacion a
alargar n (Espera m)  = Espera (n*m)
alargar n (Mov m a)   = Mov (n*m) a
alargar n (Sec a1 a2) = Sec (alargar n a1) (alargar n a2)
alargar n (Par a1 a2) = Par (alargar n a1) (alargar n a2)

simular :: Animacion a -> [Frame a]
simular (Espera m)  = replicate m []
simular (Mov m acc) = replicate m [acc]
simular (Sec a1 a2) = simular a1 ++ simular a2
simular (Par a1 a2) = zipListas (simular a1) (simular a2)

tiemposDeEspera :: Animacion a -> [Tiempo]
tiemposDeEspera a = tiemposMuertosDe (contarHasta (duracion a)) a

tiemposMuertosDe :: [Int] -> Animacion a -> [Tiempo]
tiemposMuertosDe tps (Espera d)  = take d tps
tiemposMuertosDe tps (Mov d _)   = []
tiemposMuertosDe tps (Sec a1 a2) = let tpsSec = separar (duracion a1) tps in
                                      tiemposMuertosDe (fst tpsSec) a1 ++ tiemposMuertosDe (snd tpsSec) a2
tiemposMuertosDe tps (Par a1 a2) = intersect (tiemposMuertosDe tps a1) (tiemposMuertosDe tps a2)

intersect :: [Int] -> [Int] -> [Int]
intersect [] _ = []
intersect (x:xs) ys = if elem x ys
                        then x : intersect xs ys
                        else     intersect xs ys

contarHasta :: Int -> [Int]
contarHasta 0 = []
contarHasta n = contarHasta (n-1) ++ [n]

separar :: Int -> [Int] -> ([Int], [Int])
separar 0 xs     = ([], xs) 
separar _ []     = ([], [])
separar n (x:xs) = let r = separar (n-1) xs in 
                    (x: fst r, snd r)


--Ejercicio 3)
{-
Para todo k, k >= 0 
  ¿ duracion . (alargar k) = (k*) . duracion ?

  Caso base 1, Espera d
    ¿ duracion . (alargar k) (Espera d) = (k*) . duracion (Espera d) ?

  Caso base 2, Mov d acc
    ¿ duracion . (alargar k) (Mov d acc) = (k*) . duracion (Mov d acc) ?

  Caso inductivo 1, Sec a1 a2
    HI.1) duracion . (alargar k) a1 = (k*) . duracion a1
    HI.2) duracion . (alargar k) a2 = (k*) . duracion a2
    ¿ duracion . (alargar k) (Sec a1 a2) = (k*) . duracion (Sec a1 a2) ?

  Caso inductivo 2, Par a1 a2
    HI.1) duracion . (alargar k) a1 = (k*) . duracion a1
    HI.2) duracion . (alargar k) a2 = (k*) . duracion a2
    ¿ duracion . (alargar k) (Par a1 a2) = (k*) . duracion (Par a1 a2) ?

  Caso base 1:
    --lado izq
    duracion . (alargar k) (Espera d)
    = (def. (.))
    duracion (alargar k (Espera d))
    = (def. alargar.1)
    duracion (Espera k*d)
    = (def. duracion.1)
    k*d

    --lado der
    (k*) . duracion (Espera d)
    = (def. (.))
    k* (duracion (Espera d))
    = (def. duracion.1)
    k*d

  Caso base 2:
    --lado izq
    duracion . (alargar k) (Mov d acc)
    = (def. (.))
    duracion (alargar k (Mov d acc))
    = (def. alargar.2)
    duracion (Mov (d*k) acc)
    = (def. duracion.2)
    k*d

    --lado der
    (k*) . duracion (Mov d acc)
    = (def. (.))
    k* (duracion (Mov d acc))
    = (def. duracion.2)
    k*d

    Caso inductivo 1:
    --lado izq
    duracion . (alargar k) (Sec a1 a2)
    = (def. (.))
    duracion (alargar k (Sec a1 a2))
    = (def. alargar.3)
    duracion (Sec (alargar k a1) (alargar k a2))
    = (def. duracion.3)
    duracion (alargar k a1) + duracion (alargar k a2)
    = por HI.1 y HI.2
    k* (duracion a1) + k* (duracion a2)

    --lado der
    (k*) . duracion (Sec a1 a2)
    = (def. (.))
    k* (duracion (Sec a1 a2))
    = (def. duracion.3)
    k* (duracion a1 + duracion a2)
    = distribución de la multiplicación respecto de la suma
    k* (duracion a1) + k* (duracion a2)

    Caso inductivo 2:
    --lado izq
    duracion . (alargar k) (Par a1 a2)
    = (def. (.))
    duracion (alargar k (Par a1 a2))
    = (def. alargar.4)
    duracion (Par (alargar k a1) (alargar k a2))
    = (def. duracion.4)
    max (duracion (alargar k a1)) (duracion (alargar k a2))
    = por HI.1 y HI.2
    max (k* (duracion a1)) (k* (duracion a2))
    = lema max (k*n) (k*m) = k* (max n m)
    k* (max (duracion a1) (duracion a2))
    
    --lado der
    (k*) . duracion (Par a1 a2)
    = (def. (.))
    k* (duracion (Par a1 a2))
    = (def. duracion.4)
    k* (max (duracion a1) (duracion a2))

    Lema k >= 0, n :: Int, m :: Int,
      ¿ max (k*n) (k*m) = k* (max n m) ?
      --lado izq
      max (k*n) (k*m)
      = (def. max)
      if (k*n)>(k*m) then (k*n) else (k*m)
      = (aritm.)
      if n>m then (k*n) else (k*m)
      = (distrb. del if)
      k* if n>m then n else m
      = (def. max)
      k* (max n m)

-}

--Ejercicio 4)
foldA :: (Duracion -> b) -> (Duracion -> Accion a -> b) -> (b -> b -> b) -> (b -> b -> b) -> Animacion a -> b
foldA fe fm fs fp (Espera d)  = fe d
foldA fe fm fs fp (Mov d acc) = fm d acc 
foldA fe fm fs fp (Sec a1 a2) = fs (foldA fe fm fs fp a1) (foldA fe fm fs fp a2)
foldA fe fm fs fp (Par a1 a2) = fp (foldA fe fm fs fp a1) (foldA fe fm fs fp a2) 

recA :: (Duracion -> b) -> (Duracion -> Accion a -> b) -> (b -> Animacion a -> b -> Animacion a -> b) -> (b -> Animacion a -> b -> Animacion a -> b) -> Animacion a -> b
recA fe fm fs fp (Espera d)  = fe d
recA fe fm fs fp (Mov d acc) = fm d acc 
recA fe fm fs fp (Sec a1 a2) = fs (recA fe fm fs fp a1) a1 (recA fe fm fs fp a2) a2
recA fe fm fs fp (Par a1 a2) = fp (recA fe fm fs fp a1) a1 (recA fe fm fs fp a2) a2

--Ejercicio 5)
duracionF :: Animacion a -> Int
duracionF = foldA id (\d _ -> d) (+) max

alargarF :: Int -> Animacion a -> Animacion a 
alargarF n = foldA (\d -> Espera (d*n)) (\d acc -> Mov (d*n) acc) (\r1 r2 -> Sec r1 r2) (\r1 r2 -> Par r1 r2)


simularF :: Animacion a -> [Frame a]
simularF = foldA (\d -> replicate d []) (\d acc -> replicate d [acc]) (++) zipListas

--Ejercicio 6)
