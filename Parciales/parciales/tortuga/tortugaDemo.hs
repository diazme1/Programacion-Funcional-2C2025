-- EJERCICIO 4 
-- Demostrar que la versión dada de cantSeqs en el ejercicio anterior, efectivamente lo es.
-- Por principio de extensionalidad. ¿para todo tc :: TCommand. cantSeqs tc = cantSeqs' tc?
-- Sea tc' :: TCommand. Por principio de inducción estructural sobre la estructura de tc': 
-- Caso base 1, tc' = Go d 
-- ¿cantSeqs (Go d) = cantSeqs' (Go d)?
-- Caso base 2, tc' = Turn a 
-- ¿cantSeqs (Turn a) = cantSeqs' (Turn a)?
-- Caso base 3, tc' = GrabPen p
-- ¿cantSeqs (GrabPen p) = cantSeqs' (GrabPen p)?
-- Caso ind, tc' = tc1 :#: tc2 
-- HI1) ¡cantSeqs tc1 = cantSeqs' tc1!
-- HI2) ¡cantSeqs tc2 = cantSeqs' tc2!
-- TI) ¿cantSeqs (tc1 :#: tc2) = cantSeqs' (tc1 :#: tc2)?

-- Caso base 1
-- LI                           
    cantSeqs (Go d)             
=               (cantSeqs.2)
    (0,0)

-- LD
    cantSeqs' (Go d)
=               (cantSeqs')
    foldTC (const (0,0)) (const (0,0)) (const (0,0)) (\(cs1, csis1) (cs2, csis2) -> (1 + cs1 + cs2, cs1 + csis1 + csis2)) (Go d)
=               (foldTC)
    recTC (const (0,0)) (const (0,0)) (const (0,0)) (\(cs1, csis1) _ (cs2, csis2) _ -> (1 + cs1 + cs2, cs1 + csis1 + csis2)) (Go d)
=               (recT.1)
    const (0,0) d 
=               (const)
    (0,0)
-- Vale el caso base 1


-- Caso ind 
-- LI 
    cantSeqs (tc1 :#: tc2) 
=               (cantSeqs.1)
    let (cs1, csis1) = cantSeqs c1 
        (cs2, csis2) = cantSeqs c2 
    in (1 + cs1 + cs2, cs1 + csis1 + csis2)


-- LD 
    cantSeqs' (tc1 :#: tc2)
=               (cantSeqs')
    foldTC (const (0,0)) (const (0,0)) (const (0,0)) (\(cs1, csis1) (cs2, csis2) -> (1 + cs1 + cs2, cs1 + csis1 + csis2)) (tc1 :#: tc2)
=               (foldT)
    recTC (const (0,0)) (const (0,0)) (const (0,0)) (\(cs1, csis1) _ (cs2, csis2) _ -> (1 + cs1 + cs2, cs1 + csis1 + csis2)) (tc1 :#: tc2)
=               (recT.4)
    (\(cs1, csis1) _ (cs2, csis2) _ -> (1 + cs1 + cs2, cs1 + csis1 + csis2)) 
        (recTC (const (0,0)) (const (0,0)) (const (0,0)) (\(cs1, csis1) _ (cs2, csis2) _ -> (1 + cs1 + cs2, cs1 + csis1 + csis2)) tc1) 
            tc1 
                (recTC (const (0,0)) (const (0,0)) (const (0,0)) (\(cs1, csis1) _ (cs2, csis2) _ -> (1 + cs1 + cs2, cs1 + csis1 + csis2)) tc2)
                    tc2
=               (foldTC) 
    (\(cs1, csis1) _ (cs2, csis2) _ -> (1 + cs1 + cs2, cs1 + csis1 + csis2)) 
        (foldTC (const (0,0)) (const (0,0)) (const (0,0)) (\(cs1, csis1) (cs2, csis2) -> (1 + cs1 + cs2, cs1 + csis1 + csis2)) tc1) 
            tc1 
                (foldTC (const (0,0)) (const (0,0)) (const (0,0)) (\(cs1, csis1) (cs2, csis2) -> (1 + cs1 + cs2, cs1 + csis1 + csis2)) tc2)
                    tc2
=               (cantSeqs')         
    (\(cs1, csis1) _ (cs2, csis2) _ -> (1 + cs1 + cs2, cs1 + csis1 + csis2)) (cantSeqs' tc1) tc1 (cantSeqs' tc2) tc2
=               (HI1 y HI2)
    (\(cs1, csis1) _ (cs2, csis2) _ -> (1 + cs1 + cs2, cs1 + csis1 + csis2)) (cantSeqs tc1) tc1 (cantSeqs tc2) tc2  
=               (cantSeqs)



-- LI
    cantSeqs (tc1 :#: tc2)
=               (cantSeqs.1)
    let (cs1, csis1) = cantSeqs tc1
        (cs2, csis2) = cantSeqs tc2
    in (1 + cs1 + cs2, cs1 + csis1 + csis2)


-- LD
    cantSeqs' (tc1 :#: tc2)
=               (cantSeqs')
    foldTC (const (0,0)) (const (0,0)) (const (0,0))
           (\(cs1, csis1) (cs2, csis2) ->
              (1 + cs1 + cs2, cs1 + csis1 + csis2))
           (tc1 :#: tc2)
=               (foldTC)
    recTC (const (0,0)) (const (0,0)) (const (0,0))
          (\(cs1, csis1) _ (cs2, csis2) _ ->
              (1 + cs1 + cs2, cs1 + csis1 + csis2))
          (tc1 :#: tc2)
=               (recTC.4)
    (\(cs1, csis1) _ (cs2, csis2) _ ->
        (1 + cs1 + cs2, cs1 + csis1 + csis2))
        (recTC ... tc1) tc1 (recTC ... tc2) tc2
=               (foldTC)
    (\(cs1, csis1) _ (cs2, csis2) _ ->
        (1 + cs1 + cs2, cs1 + csis1 + csis2))
        (foldTC ... tc1) tc1 (foldTC ... tc2) tc2
=               (cantSeqs')
    (\(cs1, csis1) _ (cs2, csis2) _ ->
        (1 + cs1 + cs2, cs1 + csis1 + csis2))
        (cantSeqs' tc1) tc1 (cantSeqs' tc2) tc2
=               (HI1 y HI2)
    (\(cs1, csis1) _ (cs2, csis2) _ ->
        (1 + cs1 + cs2, cs1 + csis1 + csis2))
        (cantSeqs tc1) tc1 (cantSeqs tc2) tc2
=               (β-reducción)
    let (cs1, csis1) = cantSeqs tc1
        (cs2, csis2) = cantSeqs tc2
    in (1 + cs1 + cs2, cs1 + csis1 + csis2)