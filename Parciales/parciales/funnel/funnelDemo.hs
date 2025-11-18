-- EJERCICIO 4 
-- ¿para todo fn. para todo f. para todo xs. appF fn f xs = appF (complementF (complementF fn)) f xs?
-- Por LEMA: fn = complementF (complementF fn), es equivalente: ¿para todo fn. para todo f. para todo xs. appF fn f xs = appF fn f xs?
-- Lado izq y lado der son iguales, queda demostrado.

-- LEMA: fn = complementF (complementF fn)
-- Sea fn' :: Funnel a b. Se demostrara por induccion estructural sobre la estructura fn':
-- Caso base, fn' = Initial c 
-- ¿Initial c = complementF (complementF (Initial c))?
-- Caso ind, fn' = Step c fn 
-- HI) ¡Step c fn = complementF (complementF fn)!
-- TI) ¿Step c fn = complementF (complementF (Step c fn))?

-- Caso base
-- Lado der 
  complementF (complementF (Initial c))
=              -----------------------  (complementF.1)
  complementF (Initial (complementC c))
= ------------------------------------  (complement.1)
  Initial (complementC (complementC c))
=          ---------------------------  (LEMA: c = complementC (complementC c))
  Initial c 
-- Lado izq y lado der son iguales, vale el caso base 

-- Caso ind
-- Lado der 
  complementF (complementF (Step c fn))
=              ----------------------- (complementF.2)
  complementF (Step (complementC c) complementF fn)
= ------------------------------------------------- (complemnetF.2)
  Step (complementC (complementC c)) (complementF (complementF fn))
=      ---------------------------- (LEMA: c = complementC (complementC c))
  Step c (complementF (complementF fn))
=        ------------------------------ (HI)
  Step c fn 
-- Lado izq y lado der son iguales, vale el caso ind 


-- LEMA: para todo c :: Criteria a b. c = complementC (complementC c)
-- Sea c':: Criteria a b, se demostrará por casos sobre la estructura de c': 

-- Caso1: c' = C p f g 
-- ¿C p f g = complementC (complementC (C p f g))?

-- Lado izq 
  C p f g
-- Lado der
  complementC (complementC (C p f g))
=              ---------------------  (complementC)
  complementC (C (not p) g f)
= ----------------------------        (complementC)
  C (not (not p)) f g 
=   ------------                      (prop. not (not p) = p)
  C p f g 
-- Lado izq y lado der son iguales, vale la demostracion.     