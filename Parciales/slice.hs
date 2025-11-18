import Distribution.Verbosity (normal)
import Distribution.SPDX (SimpleLicenseExpression)
data SliceExp a = Base [a] 
                    | Take Int (SliceExp a)
                    | Drop Int (SliceExp a) 
    deriving (Show)

ej :: SliceExp Int 
ej = Take 3 (Drop 2 (Take 3 (Drop 1 (Base [1..10]))))
ejNormalized = Take 4 (Drop 4 (Base [1..10]))


-- EJERCICIO 1 
-- Definir por recursion explicita las siguientes funciones:

-- a. Devuelve la lista final, luego de evaluar todas las operaciones.
materialize :: SliceExp a -> [a]
materialize (Base xs)     = xs 
materialize (Take n sexp) = takeN n (materialize sexp)
materialize (Drop n sexp) = dropN n (materialize sexp)

takeN :: Int -> [a] -> [a]
takeN _ []     = []  
takeN 0 _      = []
takeN n (x:xs) =  x: takeN (n-1) xs 
                 
dropN :: Int -> [a] -> [a]
dropN 0 xs     = xs
dropN n []     = []  
dropN n (x:xs) = dropN (n-1) xs 


-- b. Devuelve la longitud final de un SliceExp. 
-- lenS :: SliceExp a -> Int
-- lenS = length (materialize)

lenS :: SliceExp a -> Int
lenS (Base xs)     = length xs
lenS (Take n sexp) = min (lenS sexp) (lenS sexp + n)
lenS (Drop n sexp) = min (lenS sexp) (lenS sexp - n)


lenSCompose :: SliceExp a -> Int
lenSCompose = length . materialize

-- c. Simplifica expresiones de manera tal que no hay dos Take ni dos Drop seguidos, y solo numeros positivos. 
normalize :: SliceExp a -> SliceExp a
normalize (Base xs)     = Base xs  
normalize (Take n sexp) = if n>0 then case sexp of 
                                        (Take m sexp1) -> Take (n+m) (normalize sexp1)
                                        _              -> Take n (normalize sexp)
                                else normalize sexp
normalize (Drop n sexp) =if n>0 then case sexp of
                                        (Drop m sexp1) -> Drop (n+m) (normalize sexp1)
                                        _              -> Drop n (normalize sexp)
                                else normalize sexp


-- d. Aplica take a una expresión SliceExp ya normalizada, sin evaluarla completamente. La expresion que devuelve 
-- no inicia con el contrsuctor Take. 
takeS :: Int -> SliceExp a -> SliceExp a
takeS n sexp = case sexp of 
                    (Take m sexp1) -> takeSExp (n+m) sexp1
                    _              -> takeSExp n sexp

takeSExp :: Int -> SliceExp a -> SliceExp a
takeSExp n (Base xs)  = Base (takeN n xs)
takeSExp n (Drop m sexp) = Drop m (takeSExp n sexp)
takeSExp n (Take m sexp) = Take m (takeSExp n sexp)


-- EJERCICIO 2 
-- Demostrar: lenS . normalize = lenS

{-
Para todo sexp, 
        ¿ lenS . normalize sexp = lenS sexp ?
-}


-- EJERCICIO 3
-- Definir el esquema primitivo y recursivo de SliceExp a. 

foldS :: ([a] -> b) -> (Int -> b -> b) -> SliceExp a -> b
foldS fb fr (Base xs)     = fb xs
foldS fb fr (Take n sexp) = fr n (foldS fb fr sexp)
foldS fb fr (Drop n sexp) = fr n (foldS fb fr sexp)


-- EJERCICIO 4
-- Escribir las versiones del ejecicio 1 sin ulizar recursion explicita.
