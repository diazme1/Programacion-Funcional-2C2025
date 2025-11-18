-- Dar el tipo de la expresion compose12 twice compose dadas las siguientes definiciones:
twice :: (a -> a) -> a -> a 
twice f x = f (f x)

compose :: (b -> c) -> (a -> b) -> a -> c 
compose f g x = f (g x)

compose12 :: (d -> c) -> (a -> b -> d) -> a -> b -> c 
compose12 f g x y = f (g x y)
compose12 = \f g x y -> f (g x y)
compose12 f g = \x y -> f (g x y)


compose12       :: (d -> c) -> (a -> b -> d) -> a -> b -> c
twice           :: (e -> e) -> e -> e
------------------------------------------------------------
compose12 twice :: (a -> b -> e -> e) -> a -> b -> e -> e

compose12 twice         :: (a -> b -> (e -> e)) -> a -> b -> e -> e
compose                 :: (b' -> c') -> (a' -> b') -> (a' -> c')
-------------------------------------------------------------------
compose12 twice compose ::  (b' -> c') -> (a' -> b') -> (a' -> c')
                            (b' -> d) -> (d -> b') -> (d -> d)      
                            (a -> b) -> (b -> a) -> (b -> b)


-- Dar una funcion anonima que solamente utilice variables y sea equivalente a compose12 twice compose

    compose12 twice compose 
->  -----------------------         (def. de compose12, con f <- twice, g <- compose)
    twice (compose x y) 
->  -------------------             (def. de twice, con f <- compose x y, x <- z)
    compose x y ((compose x y) z)
->               ---------------    (def. de compose, con f <- x, y <- g, x <- z)
    compose x y (x (y z))
->  --------------------            (def. de compose, con f <- x, g <- y, x <- z)
    x (y (x (y z)))
        

    compose12 twice compose
->  -----------------------                     (def. de compose12, con f <- twice, g <- compose)
    \x y -> twice (compose x y) 
->          -------------------                 (def. de twice, con f <- compose x y, x <- z)
    \x y z -> compose x y ((compose x y) z)
->            -----------------------------     (def. de compose, con f <- x, y <- g, x <- (compose x y) z)
    \x y z -> x (y ((compose x y) z))
->                  ----------------            (def. de compose, con f <- x, g <- y, x <- z)
    \x y z -> x (y (x (y z))) 