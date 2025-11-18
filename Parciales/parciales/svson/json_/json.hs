data Value = Vint Int | Vstring String | Vbool Bool | Vlist [Value]
type Field = String
data Svson i = Empty | Obj i (Field -> Maybe Value) (Svson i) (Svson i)

-- La estructura Svson i representa un árbol binario de búsqueda (BST) donde cada nodo contiene:
-- Un índice de tipo i (que debe ser ordenable)
-- Una función que mapea nombres de campos a valores opcionales
-- Dos subárboles izquierdo y derecho

-- Funciones auxiliares implementadas
valuesOf :: [Field] -> (Field -> Maybe Value) -> [Value]
-- Obtiene los valores de una lista de campos de un objeto

valuesWithFields :: [Field] -> (Field -> Maybe Value) -> [(Field, Value)]
-- Obtiene pares (campo, valor) para los campos que existen

only :: [Field] -> (Field -> Maybe Value) -> (Field -> Maybe Value)
-- Restringe un objeto a solo los campos especificados

update :: Field -> Value -> (Field -> Maybe Value) -> (Field -> Maybe Value)
-- Actualiza o agrega un campo con un valor en un objeto

singleton :: Field -> Value -> (Field -> Maybe Value)
-- Crea un objeto con un solo campo y valor

-- Implementar
-- devuelve una lista con todos los índices presentes en la estructura, en cualquier orden
indices :: Svson i -> [i]
indices Empty = []
indices (Obj i f sv1 sv1) = i : (indices sv1 ++ indices sv2)

-- indica si un índice dado existe en la estructura. Debe aprovechar la propiedad de BST para ser eficiente.
belongs :: Ord i => i -> Svson i -> Bool
belongs x Empty = False 
belongs x (Obj i f sv1 sv2) = if i == x then True 
                                else if i < x then belongs x sv1 
                                              else belongs x sv2

-- busca un índice específico y devuelve los valores de los campos solicitados para ese índice. Si el índice no existe, devuelve una lista vacía.
lookupProjecting :: Ord i => i -> [Field] -> Svson i -> [Value]
lookupProjecting x fs Empty = []
lookupProjecting x fs (Obj i f sv1 sv2) = if x == i then valuesOf fs f 
                                                    else if x < i 
                                                        then lookupProjecting x fs sv1 
                                                        else lookupProjecting x fs sv2

-- actualiza o inserta un campo en el objeto con el índice dado. Si el índice no existe, crea un nuevo nodo en la posición correcta del BST.
upsert :: Ord i => i -> Field -> Value -> Svson i -> Svson i
upsert x f v Empty = Obj x (singleton f v) Empty Empty
upsert x f v (Obj i sv1 sv2) = if x == i then Obj (update f v i) sv1 sv2 
                                else if 

