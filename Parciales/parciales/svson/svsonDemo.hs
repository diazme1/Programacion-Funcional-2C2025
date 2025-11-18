-- EJERCICIO 4
-- para todo i. belongs i = elem i . indices 
-- Por principio de extensionalidad. ¿para todo s. para todo i. belongs i s = (elem i . indices) s?
-- Por defincion de (.), es equivalente: ¿para todo s. para todo i. belongs i s = elem i (indices s)?
-- Sea s' :: Svnson i, i' :: i. Se demouestra por principio de induccion estructural sobre s': 
-- Caso base, s' = Empty 
-- ¿belongs i' Empty = elem i' (indices Empty)?
-- Caso inductivo, s' = Obj i f s1 s2 
-- HI1) ¡belongs i' s1 = elem i' (indices s1)!
-- HI2) ¡belongs i' s2 = elem i' (indices s2)!
-- TI)  ¿belongs i' (Obj i f s1 s2) = elem i' (indices (Obj i f s1 s2))?

-- Caso base
-- Lado izq                         -- Lado der 
  belongs i' Empty                    elem i' (indices Empty)
= ----------------  (belongs.1)     =          -------------  (indices.1)
  False                               elem i' []
                                    = ----------  (elem.1)
                                      False 
-- Como el lado izq y el lado der son iguales, queda demostrado el caso base

-- Caso ind
-- Lado izq
  belongs i' (Obj i f s1 s2)
= --------------------------  (belongs.2)
  if i' == i then True else if i' < i then belongs i' s1 else belongs i' s2 
=                                          -------------  (HI1)
  if i' == i then True else if i' < i then elem i' (indices s1) else belongs i' s2 
=                                                                    -------------  (HI2)
  if i' == i then True else if i' < i then elem i' (indices s1) else elem i' (indices s2)
  ---------------------------------------------------------------------------------------

-- Lado der 
  elem i' (indices (Obj i f s1 s2))
=          -----------------------  (indices.2)
  elem i' (i : indices s1 ++ indices s2)
= ------------------------------------- (elem.2)
  i' == i || elem i' (indices s1 ++ indices s2) 
=            ---------------------------------  (LEMA: elem i (xs ++ ys) = elem i xs || elem i ys)
  i' == i || elem i' (indices s1) || elem i' (indices s2)
  -------------------------------------------------------

-- Parto en casos sobre i e i' 

-- C1, i' == i 
-- Lado izq
  if i' == i then True else if i' < i then elem i' (indices s1) else elem i' (indices s2)
= --------------------------------------------------------------------------------------- (C1)
  True 

-- Lado der
  i' == i || elem i' (indices s1) || elem i' (indices s2)
= -------                                   (C1)
  True || elem i' (indices s1) || elem i' (indices s2)
= --------------------------------------    (True || b = True)
  True 
-- Lado izq y lado der son iguales, vale C2

-- C2, i' < i 
-- Lado izq 
  if i' == i then True else if i' < i then elem i' (indices s1) else elem i' (indices s2)
= --------------------------------------------------------------------------------------- (C2)
  elem i' (indices s2)

-- Lado der
  i' == i || elem i' (indices s1) || elem i' (indices s2)
= -------                                   (C2)
  False || elem i' (indices s1) || elem i' (indices s2) 
=                                  --------------------   (Por inv. BST y C2, i' < indices s1)
  False || elem i' (indices s1) || False 
=          -----------------------------  (b || False = b)
  False || elem i' (indices s1)
= -----------------------------           (False || b = b)
  elem i' (indices s1)
-- Lado izq y lado der son iguales, vale C2

-- C3, i' > i 
-- Lado izq 
  if i' == i then True else if i' < i then elem i' (indices s1) else elem i' (indices s2)
= --------------------------------------------------------------------------------------- (C3)
  elem i' (indices s2)

-- Lado der 
  i' == i || elem i' (indices s1) || elem i' (indices s2)
= -------                                                 (C3)
  False || elem i' (indices s1) || elem i' (indices s2)
=          -------------------                            (Por inv. BST y C2, i' < indices s1)
  False || False || elem i' (indices s2)
=          -----------------------------                  (Flase || b = b)
  False || elem i' (indices s2)
= ----------------------------                            (Flase || b = b)
  elem i' (indices s2)
-- Lado izq y lado der son iguales, vale C3 

-- Como valen C1, C2 y C3, vale el caso ind. 


-- LEMA: elem i (xs ++ ys) = elem i xs || elem i ys
-- Sea i' :: a, xs', ys' :: [a]. Se demostrara por principio de induccion estructural sobre xs': 
-- Caso base, xs' = []
-- ¿elem i' ([] ++ ys') = elem i' [] || elem i' ys'?
-- Caso ind, xs' = x:xs
-- HI) ¡elem i' (xs ++ ys') = elem i' xs || elem i' ys'!
-- TI) ¿elem i' ((x:xs) ++ ys') = elem i' (x:xs) || elem i' ys'?

-- Caso base
-- Lado izq                   -- Lado der
  elem i' ([] ++ ys')           elem i' [] || elem i' ys'
=          --------- (++.1)   = ----------                (elem.1)
  elem i' ys'                   Flase || elem i' ys'
                              = -------------------       (False || b = b)
                                elem i' ys'
-- Lado izq y lado der son iguales, vale el caso base 

-- Caso ind
-- Lado izq 
  elem i' ((x:xs) ++ ys')
=          -------------  (++.2)
  elem i' (x : (xs ++ ys'))
= ------------------------  (elem.2)
  i' == x || elem i' (xs ++ ys')
=            ------------------ (HI)
  i' == x || elem i' xs || elem i' ys'

-- Lado der 
  elem i' (x:xs) || elem i' ys'
= --------------                (elem.2)
  (i' == x || elem i' xs) || elem i' ys'
= ------------------------------------- (asociatividad del ||)
  i' == x || elem i' xs || elem i' ys'
-- Lado izq y lado der son iguales, vale el caso ind 
