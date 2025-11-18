-- EJERCICIO 4
-- para todo sp :: Spaceship. componentes (replace f sp) = map f (componentes sp)
-- Sea f' :: Compoenente -> Componente y sp' :: Spaceship.
-- Por principio de induccion estructural sobre sp': 
-- Caso base, sp' = Plug 
-- ¿componentes (replace f Plug) = map f (componentes Plug)?
-- Caso ind, sp' = Module c s1 s2 
-- HI1) ¡componentes (replace f s1) = map f (componentes s1)!
-- HI2) ¡componentes (replace f s2) = map f (componentes s2)!
-- TI) ¿componentes (replace f (Module c s1 s2)) = map f (componentes (Module c s1 s2))?

-- Caso base
-- Lado izq 
  componentes (replace f Plug)
=              -------------- (replace.1)
  componentes Plug 
= ----------------            (componentes.1)
  []

-- Lado der 
  map f (componentes Plug)
=        ----------------     (componentes.1)
  map f []
= --------                    (map.1)
  []
-- Vale el caso base 

-- Caso ind 
-- Lado izq 
  componentes (replace f (Module c s1 s2))
=              --------------------------   (replace.2)
  componentes (Module (f c) (replace f s1) (replace f s2))
= ------------------------------------------------------- (componentes.2)
  componentes (replace f s1) ++ [f c] ++ componentes (replace f s2)
= -------------------------             (HI1)
  map f (componentes s1) ++ [f c] ++ componentes (replace f s2)
=                                    -------------------------- (HI2)
  map f (componentes s1) ++ [f c] ++ map f (componentes s2)

-- Lado der 
  map f (componentes (Module c s1 s2))
=        ---------------------------- (componentes.2)
  map f (components s1 ++ [c] ++ components s2)
= --------------------------------------------- LEMA: map f (xs ++ ys) = map f xs ++ map f ys
  map f (componentes s1) ++ map f ([c] ++ componentes s2) 
=                           ----------------------------  LEMA: map f (xs ++ ys) = map f xs ++ map f ys
  map f (componentes s1) ++ map f [c] ++ map f (componentes s2)
=                           ---------           (map.2)
  map f (componentes s1) ++ [f c] ++ map f (componentes s2)
-- Vale el caso inductivo 


-- LEMA: map f (xs ++ ys) = map f xs ++ map f ys
-- Sea f' :: a -> b, xs', ys' :: [a]. 
-- Por principio de induccion estructural sobre xs': 
-- Caso base, xs' = []
-- ¿map f' ([] ++ ys') = map f [] ++ map f' ys'?
-- Caso ind, xs' = (x:xs)
-- HI) ¡map f' (xs ++ ys') = map f' xs ++ map f' ys'! 
-- TI) ¿map f' ((x:xs) ++ ys') = map f (x:xs) ++ map f' ys'?

-- Caso base
-- Lado izq                   -- Lado der 
  map f' ([] ++ ys')            map f [] ++ map f' ys'
=         --------- (++.1)    = --------              (map.1)
  map f' ys'                    [] ++ amap f' ys' 
                              = -----------------     (++.1)
                                map f' ys' 
-- Vale el caso base

-- Caso ind 
-- Lado izq                       -- Lado der
  map f' ((x:xs) ++ ys')            map f' (x:xs) ++ map f' ys'
=         ------------- (++.2)    = ------------              (map.2)
  map f' (x : xs ++ ys')            f' x : map f' xs ++ map f' ys' 
= --------------------- (map.2)   =        -----------------------  (HI)
  f' x : map f' (xs ++ ys)          f' x : map f' (xs ++ ys')
-- Vale el caso ind