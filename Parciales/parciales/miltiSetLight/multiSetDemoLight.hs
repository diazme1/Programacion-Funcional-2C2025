-- EJERCICIO 2
-- Demostrar la siguiente propiedad: cantFallas . normMSE = const 0 

-- Por principio de extensionalidad es equivalente a ¿para todo ms :: MSExp a. (cantFallas . normMSE) ms = const 0 ms?
-- Por definicion de (.), es equivalente a ¿para todo ms. cantFallas (normMSE ms) = const 0 ms?
-- Por definicion de const, es equivalente: ¿para todo ms. cantFallas (normMSE ms) = 0?
-- Sea ms' :: MSExp a. Por principio de inducción estructural sobre la la estructura de ms': 
-- Caso base 1, ms' = Empty 
-- ¿cantFallas (normMSE Empty) = 0?
-- Caso base 2, ms' = Entry n x 
-- ¿cantFallas (normMSE (Entry n x)) = 0?
-- Caso ind 1, ms' = AddAll n ms 
-- HI1) ¡cantFallas (normMSE ms) = 0!
-- TI1) ¿cantFallas (normMSE (AddAll n ms)) = 0?
-- ¿cantFallas (normMSE (Entry n x)) = 0?
-- Caso ind 2, ms' = Union ms1 ms2
-- HI1) ¡cantFallas (normMSE ms1) = 0!
-- HI1) ¡cantFallas (normMSE ms2) = 0!
-- TI1) ¿cantFallas (normMSE (Union ms1 ms2)) = 0?

-- Caso base 1 
    cantFallas (normMSE Empty)
=                               (normMSE.1)
    cantFallas Empty 
=                               (cantFallas.1)
    0 
-- Vale el caso base 1 

-- Caso base 2 
    cantFallas (normMSE (Entry n x))
=                               (normMSE.2)
    cantFallas (Entry n x)
=                               (cantFallas.2)
    0 
-- Vale el caso base 2 

-- Caso ind 1
    cantFallas (normMSE (AddAll n ms))
=                               (normMSE.3)
    cantFallas (updateOcurrsMSE (addN n) (normMSE ms))
=                               (LEMA 1)
    cantFallas (normMSE ms)
=                               (HI1)
    0 
-- Vale el caso ind 1 

-- Caso ind 2
    cantFallas (normMSE (Union ms1 ms2))
=                               (normMSE.4)
    cantFallas (normUnion (normMSE ms1) (normMSE ms2))
=                               (LEMA 2)
    cantFallas (normMSE ms1) + cantFallas (normMSE ms2) 
=                               (HI1 y HI2)
    0 + 0 
=                               (aritm)
    0
-- Vale el caso ind 2 


-- LEMA 1: 
-- ¿para todo ms :: MESxp a normalizado. para todo f :: N -> N. cantFallas (updateOcurrsMSE f ms) = cantFallas ms?
-- Sea ms' :: MSExp a normalizado, f' :: N -> N. Por principio de induccion estructural sobre la estructura de ms': 
-- Caso base 1, ms' = Empty 
-- ¿cantFallas (updateOcurrsMSE f Empty) = cantFallas Empty?
-- Caso base 2, ms' = Entry n x  
-- ¿cantFallas (updateOcurrsMSE f (Entry n x)) = cantFallas (Entry n x)?
-- Caso ind 1, ms' = Union ms1 ms2  
-- HI2) ¡cantFallas (updateOcurrsMSE f ms1) = cantFallas ms1!
-- HI2) ¡cantFallas (updateOcurrsMSE f ms2) = cantFallas ms2!
-- TI)  ¿cantFallas (updateOcurrsMSE f (Union ms1 ms2)) = cantFallas (Union ms1 ms2)?

-- Caso base 1
    cantFallas (updateOcurrsMSE f Empty)
=                               (updateOcurrsMSE.1)
    cantFallas Empty 
-- Vale el caso base 1 

-- Caso base 2 
-- LI
    cantFallas (updateOcurrsMSE f (Entry n x))
=                               (updateOcurrsMSE.2) 
    cantFallas (Entry (f n) x) 
=                               (cantFallas.2)
    0 
-- LD 
    cantFallas (Entry n x)
=                               (cantFallas.2)
    0
-- Vale el caso base 2

-- Caso ind 3 
-- LI
    cantFallas (updateOcurrsMSE f (Union ms1 ms2))
=                               (updateOcurrsMSE.3)
    cantFallas (Union (updateOcurrsMSE f ms1) (updateOcurrsMSE f ms2))
=                               (cantFallas.4)
    unoSiHayEmpty ms1 ms2 + cantFallas (updateOcurrsMSE f ms1) + cantFallas (updateOcurrsMSE f ms1)
=                               (HI1 y HI2)
    unoSiHayEmpty ms1 ms2 + cantFallas ms1 + cantFallas ms2
-- LD 
    cantFallas (Union ms1 ms2)
=                               (cantFallas.4)
    unoSiHayEmpty ms1 ms2 + cantFallas ms1 + cantFallas ms2


-- LEMA 2: cantFallas (normUnion ms1 md2) = cantFallas ms1 + cantFallas ms2
-- ¿para todo ms1, ms2 :: MESxp a normalizado. cantFallas (normUnion ms1 md2) = cantFallas ms1 + cantFallas ms2?
-- Sean ms1', ms2' :: MSExp a, normalizados. Se demostará por casos sobre ms1' y ms2': 
-- Caso 1, ms1' = Empty y ms2' = cualquier caso  
-- ¿cantFallas (normUnion Empty md2') = cantFallas Empty + cantFallas ms2'?
-- Caso 2, ms1' != Empty y ms2' = Empty 
-- ¿cantFallas (normUnion md1' Empty) = cantFallas ms1' + cantFallas Empty?
-- Caso 3, ms1' != Empty y ms2' != Empty 
-- ¿cantFallas (normUnion ms1' md2') = cantFallas ms1' + cantFallas ms2'?

-- Caso 1 
-- LI 
    cantFallas (normUnion Empty md2')
=                               (normUnion.1)
    cantFallas ms2' 
-- LD 
    cantFallas Empty + cantFallas ms2'
=                               (cantFallas.1)
    0 + cantFallas ms2'     
=                               (aritm)
    cantFallas' ms2' 
-- Vale el caso 1 

-- Caso 2
-- LI 
    cantFallas (normUnion md1' Empty)
=                               (normUnion.2)
    cantFallas ms1' 
-- LD 
    cantFallas ms1' + cantFallas Empty
=                               (cantFallas.1)
    cantFallas ms1' + 0
=                               (aritm)
    cantFallas' ms1' 

-- Caso 3 
-- LI
    cantFallas (normUnion ms1' md2')
=                               (normUnion.3)
    cantFallas (Union ms1' ms2')
=                               (cantFallas.4)
    unoSiHayEmpty ms1' ms2' + cantFallas ms1' + cantFallas ms2' 
=                               (ms1' y ms2' no son Empty)
    0 + cantFallas ms1' + cantFallas ms2'
=                               (aritm)
    cantFallas ms1' + cantFallas ms2'
-- LD 
    cantFallas ms1' + cantFallas ms2'
-- Vale el caso 3 