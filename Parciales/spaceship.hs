import Distribution.Compat.Lens (_1)
import Text.XHtml (height, width, size)
import Distribution.InstalledPackageInfo (InstalledPackageInfo(hsLibraries))
data Component = Cargo | Engine | Shield | Cannon
    deriving Show

data Spaceship = Module Component Spaceship Spaceship | Plug
    deriving Show

              --izq         der
data Direction = Larboard | Starboard
    deriving Show

data Size = Small | Big | Torpedo
    deriving Show

type Hazard = (Direction, Int, Size)

ej = Module Cannon  (Module Cargo Plug Plug) 
                    (Module Engine Plug 
                                   (Module Shield Plug Plug))
ej1 = Module Shield (Module Engine  (Module Cannon  (Module Engine Plug Plug)
                                                    (Module Engine Plug Plug)) 
                                    (Module Cannon  (Module Engine Plug Plug)
                                                    (Module Engine Plug Plug))) 
                    (Module Engine  Plug 
                                    (Module Cannon  (Module Engine Plug Plug)
                                                    (Module Engine Plug Plug)))

ej2 = Module Cargo (Module Cargo Plug Plug) (Module Cargo Plug Plug)

--Ejercicio 1)
--a)
shielded :: Spaceship -> Bool
shielded Plug             = False
shielded (Module c s1 s2) = isShield c || shielded s1 || shielded s2

isShield :: Component -> Bool
isShield Shield = True
isShield _      = False

--b)
armed :: Spaceship -> Bool
armed Plug             = False
armed (Module c s1 s2) = isCannon c || armed s1 || armed s2 

isCannon :: Component -> Bool
isCannon Cannon = True
isCannon _      = False

--c)
thrust :: Spaceship -> Int
thrust Plug = 0
thrust (Module c s1 s2) = oneIfEngine c + thrust s1 + thrust s2

oneIfEngine :: Component -> Int
oneIfEngine Engine = 1
oneIfEngine _      = 0

--d)
wreck :: Hazard -> Spaceship -> Spaceship
wreck (Larboard,n,sz) s = wreckSpaceshipFromL n s
wreck (Starboard,n,sz) s = wreckSpaceshipFromS n s

wreckSpaceshipFromL :: Int -> Spaceship -> Spaceship
wreckSpaceshipFromL _ Plug             = Plug
wreckSpaceshipFromL 0 (Module c s1 s2) = Plug
wreckSpaceshipFromL n (Module c s1 s2) = Module c (wreckSpaceshipFromL (n-1) s1) s2

wreckSpaceshipFromS :: Int -> Spaceship -> Spaceship
wreckSpaceshipFromS _ Plug             = Plug
wreckSpaceshipFromS 0 (Module c s1 s2) = Plug
wreckSpaceshipFromS n (Module c s1 s2) = Module c s1 (wreckSpaceshipFromS (n-1) s2) 

--Ejercicio 2)
foldSS :: b -> (Component -> b -> b -> b) -> Spaceship -> b
foldSS fp fm Plug             = fp  
foldSS fp fm (Module c s1 s2) = fm c (foldSS fp fm s1) (foldSS fp fm s2)

--Ejercicio 3)
--a)
capacity :: Spaceship -> Int
capacity = foldSS 0 (\c r1 r2 -> oneIfCargo c + r1 + r2)

oneIfCargo :: Component -> Int
oneIfCargo Cargo = 1
oneIfCargo _     = 0

--b)
largest :: [Spaceship] -> Spaceship
largest = foldrS (\s rs -> if capacity s > capacity rs then s else rs)

foldrS :: (Spaceship -> Spaceship -> Spaceship) -> [Spaceship] -> Spaceship
foldrS f [s]    = s 
foldrS f (s:ss) = f s (foldrS f ss)

{-
Con RE:
largest' :: [Spaceship] -> Spaceship
largest' []     = error "lista vacis"
largest' [s]    = s
largest' (s:ss) = let rs = largest' ss in 
                    if capacity s > capacity rs then s else rs
-}
--c)
dimensions :: Spaceship -> (Int,Int)
dimensions s = (heightS s, widthS s)

heightS :: Spaceship -> Int
heightS = foldSS 0 (\_ r1 r2 -> 1 + max r1 r2)

widthS :: Spaceship -> Int
widthS s = let levels = listPerLevelS s in
                foldr (\s rs -> max (length s) rs) 0 levels

listPerLevelS :: Spaceship -> [[Component]]
listPerLevelS = foldSS [] (\m r1 r2 -> [m]: zipListas r1 r2)

zipListas :: [[a]] -> [[a]] -> [[a]]
zipListas []       ls       = ls
zipListas ls        []      = ls
zipListas (l1:ls1) (l2:ls2) = (l1 ++ l2) : zipListas ls1 ls2

manoeuvre :: Spaceship -> [Hazard] -> Spaceship
manoeuvre s hs = foldl (\sh h-> (recibirImpacto h sh)) s hs

{-
Con RE:
manoeuvre' :: Spaceship -> [Hazard] -> Spaceship
manoeuvre' s []     = s
manoeuvre' s (h:hs) = manoeuvre' (recibirImpacto h s) hs
-}

recibirImpacto :: Hazard -> Spaceship -> Spaceship
recibirImpacto h s = case sizeH h of 
                        Small   -> if shielded s then s else wreck h s
                        Big     -> if armed s then s else wreck h s
                        Torpedo -> wreck h s

sizeH :: Hazard -> Size
sizeH (_,_,sz) = sz

--Ejercicio 4)
{-
components :: Spaceship -> [Component]
components Plug = []
components (Module c s1 s2) = components s1 ++ [c] ++ components s2
replace :: (Component -> Component) -> Spaceship -> Spaceship
replace f Plug = Plug
replace f (Module c s1 s2) = Module (f c) (replace f s1) (replace f s2)
Demuestre que: ∀sp:Spaceship. componentes (replace f sp) = map f (componentes sp)

Para todo sp,
    ¿ components (replace f sp) = map f (components sp) ?

    Sea sp una Spaceship cualquiera

    Caso base, sp = Plug
        ¿ components (replace f Plug) = map f (components Plug) ?

    Caso inductivo, sp = Module c sp1 sp2
        HI.1) components (replace f sp1) = map f (components sp1)
        HI.2) components (replace f sp2) = map f (components sp2)
        ¿ components (replace f (Module c sp1 sp2)) = map f (components (Module c sp1 sp2)) ?

    Caso base:
        --lado izq:
        components (replace f Plug)
        --def. replace.1
        components Plug
        --def. components.1
        []

        --lado der:
        map f (components Plug)
        --def. components.1
        map f [] 
        --def. map
        []

    Caso inductivo:
        --lado izq
        components (replace f (Module c sp1 sp2))
        --def. replace.2
        components (Module (f c) (replace f sp1) (replace f sp2))
        --def. components.2
        components (replace f sp1) ++ [f c] ++ components (replace f sp2)

        --lado der
        map f (components (Module c sp1 sp2))
        --def. components.2
        map f (components sp1 ++ [c] ++ components sp2)
        --lema map f (xs ++ ys) = map f xs ++ map f ys
        map f (components sp1) ++ map f ([c] ++ components sp2)
        --lema map f (xs ++ ys) = map f xs ++ map f ys
        map f (components sp1) ++ [f c] ++ map f (components sp2)
        --por HI.1 e HI.2
        components (replace f sp1) ++ [f c] ++ components (replace f sp2)

        Demostración lema: map f (xs ++ ys) = map f xs ++ map f ys
        
        Caso base, xs = []
            ¿ map f ([]++ys) = map f [] ++ map f ys ?

        Caso inductivo, xs = x:xs
            HI) map f (xs++ys) = map f xs ++ map f ys
            ¿ map f ((x:xs)++ys) = map f (x:xs) ++ map f ys ?

        Caso base:
        --lado izq:
        map f ([]++ys)
        --def. (++)
        map f ys 

        --lado der
        map f [] ++ map f ys
        --def. map.1
        [] : map f ys
        --def. (++)
        map f ys

        Caso inductivo:
        --lado izq
        map f ((x:xs)++ys)
        --def. map
        f x ++ map f (xs++ys)
        --por HI
        f x : (map f xs ++ map f ys)

        --lado der
        map f (x:xs) ++ map f ys
        --def. map
        f x : (map f xs ++ map f ys)
-}