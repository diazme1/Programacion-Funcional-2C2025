-- EJERCICIO 4
-- ¿toGT . toAG = id?

-- Por principio de extensionalidad. ¿para todo gm. (toGT . toAG) gm = id gm?
-- Por definicion de (.), es equivalente. ¿para todo gm. toGT (toAG gm) = id gm?
-- Por definicion de id, es equivalente. ¿para todo gm. toGT (toAG gm) = gm?
-- Sea gm' :: GameTree. Por principio de induccion estructural sobre la estructura gm':
-- Caso base, gm' = Nil 
-- ¿toGT (toAG Nil) = Nil?
-- Caso ind, gm' = Node (mov, gm1) gm2
-- HI1) ¡toGT (toAG gm1) = gm1!
-- HI2) ¡toGT (toAG gm2) = gm2!
-- TI)  ¿toGT (toAG (Node (mov, gm1) gm2)) = (Node (mov, gm1) gm2)?

-- Caso base 
    toGT (toAG Nil)
=         --------  (toGA.1)
    toGT []
=   -------         (toGT.1)
    Nil
-- Vale el caso base 

-- Caso ind 
    toGT (toAG (Node (mov, gm1) gm2))
=         --------------------------    (toAG.2)
    toGT (NodeAG mov (toAG gm1) : toAG gm2)
=   --------------------------------------- (toGT.2)
    Node (mov, toGT (toAG gm1)) (toGT (toAG gm2))
=                                --------------- (HI2)
    Node (mov, toGT (toAG gm1)) gm2 
=              ---------------         (HI1)
    Node (mov, gm1) gm2
-- Vale el caso ind