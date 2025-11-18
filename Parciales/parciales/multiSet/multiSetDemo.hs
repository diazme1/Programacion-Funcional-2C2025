-- EJERCICIO 2 
-- Demostrar la sigueinte propiedad: evalMSE . simpMSE = evalMSE
-- Por principio de extensionalidad. ¿para todo ms :: MSExp a. (evalMSE . simpMSE) ms = evalMSE ms?
-- Por defincion de (.), es quivalente: ¿para todo ms :: MSExp a. evalMSE (simpMSE ms) = evalMSE ms?
-- Por principio de induccion estructural sobre la estructura de ms': 
-- Caso base, ms' = EmptyMS 
-- ¿evalMSE (simpMSE EmptyMS) = evalMSE EmptyMS?
-- Caso ind 1, ms' = AddMS x ms 
-- HI1) ¡evalMSE (simpMSE ms) = evalMSE ms!
-- TI1) ¿evalMSE (simpMSE (AddMS x ms)) = evalMSE (AddMS x ms)?
-- Caso ind 2, ms' = RemoveMS x ms 
-- HI2) ¡evalMSE (simpMSE ms) = evalMSE ms!
-- TI2) ¿evalMSE (simpMSE (RemoveMS x ms)) = evalMSE (RemoveMS x ms)?
-- Caso ind 3, ms' = UnionMS ms1 ms2 
-- HI3.1) ¡evalMSE (simpMSE ms1) = evalMSE ms1!
-- HI3.2) ¡evalMSE (simpMSE ms2) = evalMSE ms2!
-- TI3)   ¿evalMSE (simpMSE (UnionMS ms1 ms2)) = evalMSE (UnionMS ms1 ms2)?
-- Caso ind 4, ms' = MapMS f ms 
-- HI4) ¡evalMSE (simpMSE ms) = evalMSE ms!
-- TI4) ¿evalMSE (simpMSE (MapMS f ms)) = evalMSE (MapMS f ms)?

-- Caso base
    evalMSE (simpMSE EmptyMS)
=                               (simpMSE.1)
    evalMSE EmptyMS
-- Vale el caso base 

-- Caso ind 1 
    evalMSE (simpMSE (AddMS x ms))
=                                   (simpMSE.2)
    evalMSE (AddMS x (simpMSE ms))
=                                   (evalMSE.2)
    x : (evalMSE (simpMSE ms))
=                                   (HI2)
    x : evalMSE ms                  
=                                   (evalMSE.2)
    evalMSE (AddMS x ms)
-- Vale el caso ind 1 

-- Caso ind 2 
    evalMSE (simpMSE (RemoveMS x ms))
=                                   (simpMSE.3)
    evalMSE (simpRemoveMS x (simpMSE ms)) 
=                                   (LEMA 1)   
    remove x (evalMSE (simpMSE ms))
=                                   (HI3)
    remove x (evalMSE ms) 
=                                   (evalMSE.3)
    evalMSE (RemoveMS x ms)
-- Vale el caso ind 2 

-- Caso ind 3 
    evalMSE (simpMSE (UnionMS ms1 ms2))
=                                   (simpMSE.4)
    evalMSE (simpUnionMS (simpMSE ms1) (simpMSE ms2))
=                                   (LEMA 2)
    evalMSE (simpMSE ms1) ++ evalMSE (simpMSE ms2)
=                                   (HI3.1 y HI3.2)
    evalMSE ms1 ++ evalMSE ms2 
=                                   (evalMSE.4)
    evalMSE (UnionMS ms1 ms2)
-- Vale el caso ind 3

-- Caso ind 4
    evalMSE (simpMSE (MapMS f ms))
=                                   (simpMSE.5)
    evalMSE (simpMapMS f (simpMSE ms))
=                                   (LEMA 3)
    map f (evalMSE (simpMSE ms))
=                                   (HI4)
    map f (evalMSE ms)
=                                   (evalMSE.5)
    evalMSE (MapMS f ms)
-- Vale el caso ind 4

-----------------------------------------------------------------------------------------------

-- LEMA 1: 
-- para todo x. para todo ms. evalMSE (simpRemoveMS x ms) = remove x (evalMSE ms)
-- Sea ms' :: MSExp a, z :: a. 
-- Se demostrara por casos sobre ms':
-- Caso 1, ms' = AddMS x ms 
-- ¿evalMSE (simpRemoveMS z (AddMS x ms)) = remove z (evalMSE (AddMS x ms))?
-- Caso 2: ms' /= AddMS x ms.
-- ¿evalMSE (simpRemoveMS z ms') = remove z (evalMSE ms')?

-- Caso 1:
-- LI.
    evalMSE (simpRemoveMS z (AddMS x ms))
=                                   (simpRemoveMS.1)
    evalMSE (if z == x then ms else RemoveMS z (Add x ms))
=

-- LD 
    remove z (evalMSE (AddMS x ms))
=                                   (evalMSE.2)
    remove z (x : evalMSE ms)
=                                   (remove.2)
    if z == x then evalMSE ms else x : remove z (evalMSE ms)
=                                   


        -- Parto en casos sobre el if.
        --  Subcaso 1: z == x.
        --  LI.
            evalMSE (if z == x then ms else RemoveMS z (Add x ms))
        =                                  (C1)
            evalMSE ms 
        -- LD.
            if z == x then evalMSE ms else x : remove z (evalMSE ms)
        =                                  (C1)
            evalMSE ms
        -- Vale C1 

        -- Subcaso : z /= x
        -- LI 
            evalMSE (if z == x then ms else RemoveMS z (Add x ms))
        =                                   (z /= x)
            evalMSE (RemoveMS z (Add x ms))
        =                                   (evalMSE.3)
            remove z (evalMSE (Add x ms))
        =                                   (evalMSE.2)
            remove z (x : evalMSE ms)
        =                                   (remove.2)
            if z == x then evalMSE ms else x : remove z (evalMSE ms) 
        -- Vale C2 


-- Caso 2 
    evalMSE (simpRemoveMS z ms') 
=                                   (simpRemoveMS.1)
    evalMSE (RemoveMS z ms')
=                                   (evalMSE.3)
    remove z (evalMSE ms')
-- Vale el caso 2

-----------------------------------------------------------------------------------------------


-- LEMA 2: 
-- para todo ms1. para todo ms2. evalMSE (simpUnionMS ms1 ms2) = evalMSE ms1 ++ evalMSE ms2
-- Sean m1, m2 :: MSExp a. 
-- Se demostrara por casos sobre m1 y m2:
-- Caso 1, m1 = EmptyMS, m2 = cualquier caso 
-- ¿evalMSE (simpUnionMS EmptyMS m2) = evalMSE EmptyMS ++ evalMSE m2?
-- Caso 2, m1 /= EmptyMS, m2 = EmptyMS
-- ¿evalMSE (simpUnionMS m1 EmptyMS) = evalMSE m1 ++ evalMSE EmptyMS?
-- Caso 3, m1 /= EmptyMS, m2 /= EmptevalMSEyMS
-- ¿evalMSE (simpUnionMS m1 m2) =  m1 ++ evalMSE m2?

-- Caso 1 
-- LI 
    evalMSE (simpUnionMS (simpMSE Empty) (simpMSE m2))
=                               (simpMSE.1)
    evalMSE (simpUnionMS Empty m2)
=                               (simpUnionMS.1)
    evalMSE m2 
-- LD 
    evalMSE Empty ++ evalMSE m2
=                               (evalMSE.1)
    [] ++ evalMSE m2 
=                               (++.1)
    evalMSE m2 
-- Vale el caso 1 

-- Caso 2
-- Igual que el caso 1 pero con los parametros invertidos
-- Vale el caso 2 

-- Caso 3 
-- LI 
    evalMSE (simpUnionMS m1 m2)
=                               (simpUnionMS.3)
    evalMSE (UnionMS m1 m2)
=
    evalMSE m1 ++ evalMSE m2 
-- Vale el caso 3 

-----------------------------------------------------------------------------------------------

-- LEMA 3: 
-- para todo f. para todo ms. evalMSE (simpMapMS f ms) = map f (evalMSE ms)
-- Sea ms' :: MSExp a, f :: a -> a. 
-- Se demostrara por casos sobre ms':
-- Caso 1, ms' = EmptyMS
-- ¿evalMSE (simpMapMS f EmtyMS) = map f (evalMSE EmptyMS)?
-- Caso 2: ms' /= EmptyMS.
-- ¿evalMSE (simpMapMS f ms') = map f (evalMSE ms')?

-- Caso 1
-- LI
    evalMSE (simpMapMS f EmtyMS)
=                       (simpMapMS.1)
    evalMSE EmptyMS 
=                       (evalMSE.1)
    []
-- LD 
    map f (evalMSE EmptyMS)
=                       (evalMSE.1)
    map f []
=                       (map.1)
-- Vale el caso 1 

-- Caso 2 
    evalMSE (simpMapMS f ms') 
=                       (simpMapMS)
    evalMSE (MapMS f ms')
=                       (evalMSE.5)
    map f (evalMSE ms')
-- Vale el caso 2 