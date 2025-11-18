data Component = Cargo | Engine | Shield | Cannon 
    deriving Show
data Spaceship = Module Component Spaceship Spaceship | Plug
    deriving Show

data Direction = Larboard | Starboard
    deriving Show 
data Size = Small | Big | Torpedo
    deriving Show 
type Hazard = (Direction, Int, Size)


-- EJERCICIO 1 

-- a. Inidica si la nave posee al menos un generador de campos de fuerza. 
shielded :: Spaceship -> Bool 
shielded = foldSs False (\c b1 b2 -> isShield c || b1 || b2)

isShield :: Component -> Bool 
isShield Shield = True 
isShield _      = False 

-- b. Indica si la nave posee al menos un cañon. 
armed :: Spaceship -> Bool 
armed = foldSs False (\c b1 b2 -> isCannon c || b1 || b2)

isCannon :: Component -> Bool 
isCannon Cannon = True 
isCannon _      = False

-- c. Redorna el poder de propulsion de la nave. 
thrust :: Spaceship -> Int 
thrust = foldSs 0 (\c n1 n2 -> if isEngine c then 1 + n1 + n2 else n1 + n2)

isEngine :: Component -> Bool 
isEngine Engine = True 
isEngine _      = False

-- d. Deveulve la nave resultante de despender los modulos dependientes del modulo donde se recibe el impacto
-- (se asume que se produce el impacto). 
wreck :: Hazard -> Spaceship -> Spaceship
wreck = flip (recSs (const Plug)
                    (\c f1 s1 f2 s2 -> \(d,n,s) -> if n == 0 
                                                    then error "..."
                                                    else if n == 1
                                                        then case d of 
                                                            Larboard -> case impactar s s1 of 
                                                                            Plug -> Plug 
                                                                            s1' -> Module c s1' s2
                                                            Starboard -> case impactar s s2 of
                                                                            Plug -> Plug 
                                                                            s2' -> Module c s1 s2'
                                                        else case d of 
                                                            Larboard -> Module c (f1 (d,n-1,s)) s2
                                                            Starboard -> Module c s1 (f2 (d,n-1,s))))

impactar :: Size -> Spaceship -> Spaceship 
impactar Small   s = if shielded s then s else Plug
impactar Big     s = if armed s then impactar Small s else s 
impactar Torpedo s = Plug                                                     


-- EJERCICIO 2 

foldSs :: b -> (Component -> b -> b -> b) -> Spaceship -> b
foldSs p m  Plug            = p 
foldSs p m (Module c s1 s2) = m c (foldSs p m s1) (foldSs p m s2)

recSs :: b -> (Component -> b -> Spaceship -> b -> Spaceship -> b) -> Spaceship -> b
recSs p m  Plug            = p 
recSs p m (Module c s1 s2) = m c (recSs p m s1) s1 (recSs p m s2) s2 


-- EJERCICIO 3

-- a. Retorna la capacidad de la nave, donde cada modulo de carga aporta una unidad de capacidad.
capacity :: Spaceship -> Int 
capacity = foldSs 0 (\c n1 n2 -> if isCargo c then 1 + n1 + n2 else n1 + n2)

isCargo :: Component -> Bool 
isCargo Cargo = True 
isCargo _     = False 

-- b. Dada una lista de naves, retorna la de capacidad maxima. 
largest :: [Spaceship] -> Spaceship 
largest = foldr1 (\s s' -> if capacity s > capacity s' then s else s')

-- c. Dada una nave, retorna su alto y ancho (pensando el alto como la cantidad de componentes de la rama mas larga
-- y el ancho como como la cantidad de componentes del nivel mas ancho).
dimensions :: Spaceship -> (Int, Int) 
dimensions s = (heightSs s, widthSs s)

heightSs :: Spaceship -> Int 
heightSs = foldSs 0 (\_ h1 h2 -> 1 + max h1 h2)

widthSs :: Spaceship -> Int 
widthSs = maximum . widthPerLevel 

widthPerLevel :: Spaceship -> [Int]
widthPerLevel = foldSs [0] (\_ ws1 ws2 -> 1 : addLevels ws1 ws2)

addLevels :: [Int] -> [Int] -> [Int]
addLevels = foldr (\n ns1 -> \ns2 -> case ns2 of 
                                        [] -> n : ns1 []
                                        (n':ns2') -> n + n' : ns1 ns2')
                    id 

-- d. Simula el resultado de maniobrar una nave a traves de una serie de peligros.
-- Si se encuentra un objeto pequeño y la nave esta escudada, no se produce impacto. 
-- Si el objeto es grande y la nave esta armada, entonces se transforma en un objeto pequeño. 
-- Si es un torpedo, no se puede evitar el impacto. 
manoeuvre :: Spaceship -> [Hazard] -> Spaceship
manoeuvre = flip (foldr (\h f -> \s -> f (wreck h s))
                        id) 

-- e. Dadas una lista de naves y una lista de peligros, retorna la lista de naves que sovrervien los peligros,
-- es decir, las naves con motores funcionales luego de navegar a traves de los meteoros. 
test :: [Spaceship] -> [Hazard] -> [Spaceship]
test = foldr (\s fs -> \hs -> let s' = manoeuvre s hs
                                in if thrust s' > 0 then s' : fs hs else fs hs)
                (const [])
-- test ss hs = filter (\s -> thrust s > 0) (map (\s -> manoeuvre'' s hs) ss)


-- AUXILIARES
components :: Spaceship -> [Component]
components Plug = []
components (Module c s1 s2) = components s1 ++ [c] ++ components s2

replace :: (Component -> Component) -> Spaceship -> Spaceship
replace f Plug = Plug
replace f (Module c s1 s2) = Module (f c) (replace f s1) (replace f s2)


-- EJEMPLO 
spaceShip1 :: Spaceship 
spaceShip1 = Module Cannon (Module Cargo (Module Shield (Module Shield Plug Plug) Plug) Plug) 
                        (Module Engine (Module Engine (Module Shield Plug Plug) Plug) Plug)

spaceShip2 :: Spaceship 
spaceShip2 = Module Cannon (Module Cargo (Module Shield (Module Cargo Plug Plug) Plug) Plug) 
                        Plug

spaceShip3 :: Spaceship 
spaceShip3 = Module Cannon (Module Cargo (Module Shield Plug Plug) Plug) 
                        (Module Engine (Module Cargo (Module Shield Plug Plug) Plug) Plug)

spaceShips :: [Spaceship]
spaceShips = [spaceShip1, spaceShip2, spaceShip3]