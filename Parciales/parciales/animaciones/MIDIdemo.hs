-- 3) DEMOSTRAR
-- para todo k >=0. duracion. (alargar k) = (k*) . duracion ?
-- Por principio de extensionalidad. para todo c. para todo k >= 0. (duracion . (alargar k)) c = ((k*) . duracion) c ?
-- Por definicion de (.). para todo c. para todo k >= 0. duracion ((alargar k) c) = (k*) (duracion c)?
-- Sea k' :: Int, >= 0 y sea c':: Comp. Por principio de induccion estructural sobre c':

-- Caso base 1, c' = Silencio d 
-- duracion ((alargar k') (Silencio d)) = k'* (duracion (Silencio d)) ?

-- Caso base 2, c' = Figura not d 
-- duracion ((alargar k') (Figura not d)) = k'* (duracion (Figura not d)) ?

-- Caso ind 1, c' = Apregio c1 c2
-- HI11) duracion ((alargar k') c1) = k'* (duracion c1) !
-- HI12) duracion ((alargar k') c2) = k'* (duracion c2) !
-- TI1)  duracion ((alargar k') (Apregio c1 c2)) = k'* (duracion (Apregio c1 c2)) ?

-- Caso ind 2, c' = Acorde c1 c2
-- HI21) duracion ((alargar k') c1) = k'* (duracion c1) !
-- HI22) duracion ((alargar k') c2) = k'* (duracion c2) !
-- TI2)  duracion ((alargar k') (Acorde c1 c2)) = k'* (duracion (Acorde c1 c2)) ?


-- Caso base 1
-- Lado izq. 
    duracion ((alargar k') (Silencio d))
=             ------------------------      (alargar.1)
    duracion (Silencio (k' * d)) 
=   ---------------------------             (duracion.1)
    k' * d

-- Lado der.
    k'* (duracion (Silencio d))
=          ---------------------            (duracion.1)
    k' * d 
-- como ambos lados son equivalentes, queda caso base.1 demostrado


-- Caso base 2 
-- Lado izq. 
    duracion ((alargar k') (Figura not d))
=              --------------------------   (alargar.2)
    duracion (Figura not (k' * d))
=   -----------------------------           (duracion.2)
    k' * d

-- Lado der 
    k' * (duracion (Figura not d))
=         -----------------------           (duracion.2)
    k' * d 
-- como ambos lados son equivalentes, queda caso base.2 demostrado


-- Caso ind 1 
-- Lado izq.
    duracion ((alargar k') (Apregio c1 c2))
=             ----------------------------  (alargar.3)
    duracion (Apregio (alargar k' c1) (alargar k' c2))
=   -------------------------------------------------   (duracion.3)
    duracion (alargar k' c1) + duracion (alargar k' c2)
=   -----------------------                             (HI11)
    k'* (duracion c1) + duracion (alargar k' c2)
=                       -----------------------         (HI12)
    k'* (duracion c1) + k'* (duracion c2)

-- Lado der. 
    k'* (duracion (Apregio c1 c2)) 
=       -------------------------   (duracion.3)
    k' * (duracion c1 + duracion c2)
=   -------------------------------- (distribucion del producto)
    k'* (duracion c1) + k'* (duracion c2)
-- como ambos lados son equivalentes, queda demostrado el caso ind.1


-- Caso ind 2 
-- Lado izq.
    duracion ((alargar k') (Acorde c1 c2))
=            ----------------------------       (alargar.4)
    duracion (Acorde (alargar k' c1) (alargar k' c2))
=   ------------------------------------------------    (duracion.4)
    max (duracion (alargar k' c1)) (duracion (alargar k' c2))
=       --------------------------                              (HI21)                        
    max (k'* (duracion c1)) (duracion (alargar k' c2))
=                           -------------------------       (HI22)
    max (k'* (duracion c1)) (k'* (duracion c1))
=   -------------------------------------------             (LEMA)
    k' * (max (duracion c1) (duracion c2))

-- Lado der
    k' * (duracion (Acorde c1 c2)) 
=        ----------------------         (duracion.4)
    k' * (max (duracion c1) (duracion c2)) 
-- como ambos lados son equivalentes, queda demostrado el caso ind.1

-- LEMA 
-- para todo k > 0, n, m :: Int. max (k * n) (k * m) = k * (max n m)
-- DEMOSTRACION
    max (k * n) (k * m)
=                           (max)
    if k * n > k * m then k * n else k * m 
=                           (aritm)
    if n > m then k * n else k * m 
=                           (distrib. del if)
    k * if n > m then n else m 
=                           (max)
    k * max n m 