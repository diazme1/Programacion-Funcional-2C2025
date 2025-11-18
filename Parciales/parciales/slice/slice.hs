data SliceExp a = Base [a] 
                    | Take Int (SliceExp a)
                    | Drop Int (SliceExp a) deriving (Show)

ej :: SliceExp Int 
ej = Take 3 (Take 2 (Drop 3 (Drop 1 (Base [1..10]))))


-- EJERCICIO 1 
-- Definir por recursion explicita las siguientes funciones:

-- a. Devuelve la lista final, luego de evaluar todas las operaciones.
materialize :: SliceExp a -> [a]

-- b. Devuelve la longitud final de un SliceExp. 
lenS :: SliceExp a -> Int

-- c. Simplifica expresiones de manera tal que no hay dos Take ni dos Drop seguidos, y solo numeros positivos. 
normalize :: SliceExp a -> SliceExp a

-- d. Aplica take a una expresión SliceExp ya normalizada, sin evaluarla completamente. La expresion que devuelve 
-- no inicia con el contrsuctor Take. 
takeS :: Int -> SliceExp a -> SliceExp a


-- EJERCICIO 2 
-- Demostrar: lenS . normalize = lenS


-- EJERCICIO 3
-- Definir el esquema primitivo y recursivo de SliceExp a. 


-- EJERCICIO 4
-- Escribir las versiones del ejecicio 1 sin ulizar recursion explicita. 