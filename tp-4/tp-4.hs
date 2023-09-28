{-- 1) Pizzas --}

data Pizza = Prepizza
            | Capa Ingrediente Pizza
  deriving Show
data Ingrediente = Salsa
                   | Queso
                   | Jamon
                   | Aceitunas Int
    deriving Show

pizza1 = (Capa Queso (Capa Salsa Prepizza))
pizza2 = Capa Jamon (Capa Salsa (Capa Queso Prepizza))

unoSi :: Bool -> Int
unoSi True  = 1
unoSi False = 0
pertenece :: Eq a => a -> [a] -> Bool
pertenece _ [] = False
pertenece e (x:xs) = e == x || pertenece e xs
{---------- Ejercicio 1 ---------------------------------------------}

cantidadDeCapas :: Pizza -> Int
cantidadDeCapas Prepizza   = 0
cantidadDeCapas (Capa _ p) = 1 + cantidadDeCapas p --arreglado

esIngrediente :: Ingrediente -> Bool
esIngrediente Salsa         = True
esIngrediente Queso         = True
esIngrediente Jamon         = True
esIngrediente (Aceitunas _) = True
esIngrediente _             = False

{---------- Ejercicio 2 ---------------------------------------------}

armarPizza :: [Ingrediente] -> Pizza
armarPizza []     = Prepizza
armarPizza (i:is) = Capa i (armarPizza is)

{---------- Ejercicio 3 ---------------------------------------------}

sacarJamon :: Pizza -> Pizza
sacarJamon Prepizza   = Prepizza
sacarJamon (Capa i p) = if esJamon i
                        then sacarJamon p --ahora sigue con la recursion
                        else Capa i (sacarJamon p)

esJamon :: Ingrediente -> Bool
esJamon Jamon = True
esJamon _     = False

{---------- Ejercicio 4 ---------------------------------------------}

tieneSoloSalsaYQueso :: Pizza -> Bool
tieneSoloSalsaYQueso Prepizza = True
tieneSoloSalsaYQueso (Capa i p) = tieneSalsaOQueso i && tieneSoloSalsaYQueso p

tieneSalsaOQueso :: Ingrediente -> Bool
tieneSalsaOQueso Salsa = True
tieneSalsaOQueso Queso = True
tieneSalsaOQueso _     = False

{---------- Ejercicio 5 ---------------------------------------------}

duplicarAceitunas :: Pizza -> Pizza
duplicarAceitunas Prepizza               = Prepizza
duplicarAceitunas (Capa i p)             = (Capa (siEsAceitunaDuplicar i) (duplicarAceitunas p))

siEsAceitunaDuplicar :: Ingrediente -> Ingrediente
siEsAceitunaDuplicar (Aceitunas cant) = Aceitunas (cant*2)
siEsAceitunaDuplicar ig = ig

{---------- Ejercicio 6 ---------------------------------------------}

cantCapasPorPizza :: [Pizza] -> [(Int, Pizza)]
cantCapasPorPizza []     = []
cantCapasPorPizza (p:ps) = (cantidadDeCapas p, p) : cantCapasPorPizza ps

{-- 2) Mapa de tesoros (con bifurcaciones) --}

data Dir = Izq | Der
    deriving Show
data Objeto = Tesoro | Chatarra
    deriving Show
data Cofre = Cofre [Objeto]
    deriving Show   
data Mapa = Fin Cofre
            | Bifurcacion Cofre Mapa Mapa
    deriving Show

mapa1 = Bifurcacion (Cofre [Chatarra]) 
            (Bifurcacion (Cofre []) 
                (Fin (Cofre [])) 
                (Fin (Cofre [Chatarra]))
            ) 
            (Fin (Cofre [])
        )

{---------- Ejercicio 1 ---------------------------------------------}

hayTesoro :: Mapa -> Bool
hayTesoro (Fin c)               = tieneTesoro c
hayTesoro (Bifurcacion c m1 m2) = tieneTesoro c || hayTesoro m1 || hayTesoro m2

tieneTesoro :: Cofre -> Bool
tieneTesoro (Cofre []) = False
tieneTesoro (Cofre c)  = tieneTesoro' c

tieneTesoro' :: [Objeto] -> Bool
tieneTesoro' (o:os) = esTesoro o || tieneTesoro' os

esTesoro :: Objeto -> Bool
esTesoro Tesoro = True
esTesoro _      = False

{---------- Ejercicio 2 ---------------------------------------------}

hayTesoroEn :: [Dir] -> Mapa -> Bool
hayTesoroEn [] (Fin c) = tieneTesoro c
hayTesoroEn [] (Bifurcacion c _ _) = tieneTesoro c
hayTesoroEn (d:ds) (Bifurcacion c m1 m2) = if esIzq d
                                            then hayTesoroEn ds m1
                                            else hayTesoroEn ds m2

esIzq :: Dir -> Bool
esIzq Izq = True
esIzq _ = False

{---------- Ejercicio 3 ---------------------------------------------}

caminoAlTesoro :: Mapa -> [Dir]
--Indica el camino al tesoro. Precondición: existe un tesoro y es único.
caminoAlTesoro (Fin _) = [] 
caminoAlTesoro (Bifurcacion c m1 m2) = if tieneTesoro c
                                       then []
                                       else 
                                           if hayTesoro m1
                                           then Izq : caminoAlTesoro m1
                                           else Der : caminoAlTesoro m2

{---------- Ejercicio 4 ---------------------------------------------}
ejMapa2 = Bifurcacion (Cofre [Chatarra])
                    (Bifurcacion (Cofre [Chatarra,Chatarra]) 
                        (Bifurcacion (Cofre [Chatarra,Chatarra,Chatarra])
                            (Fin (Cofre [Chatarra, Chatarra, Chatarra, Chatarra]))
                            (Fin (Cofre [Chatarra, Chatarra, Chatarra, Chatarra]))
                        )
                        (Bifurcacion (Cofre [Chatarra,Chatarra,Chatarra])
                            (Fin (Cofre [Chatarra, Chatarra, Chatarra, Chatarra]))
                            (Fin (Cofre [Chatarra, Chatarra, Chatarra, Chatarra]))
                        )
                    )
                    (Bifurcacion (Cofre [Chatarra,Chatarra]) 
                        (Bifurcacion (Cofre [Chatarra,Chatarra,Chatarra])
                            (Fin (Cofre [Chatarra, Chatarra, Tesoro, Chatarra]))
                            (Fin (Cofre [Chatarra, Chatarra, Chatarra, Chatarra]))
                        )
                        (Bifurcacion (Cofre [Chatarra,Chatarra,Chatarra])
                            (Fin (Cofre [Chatarra, Chatarra, Chatarra, Chatarra]))
                            (Fin (Cofre [Chatarra, Chatarra, Chatarra, Chatarra]))
                        )
                    )

ejMapa3 = Bifurcacion (Cofre [Tesoro])                                                                                
                    (Bifurcacion (Cofre [Tesoro,Chatarra]) 
                        (Bifurcacion (Cofre [Chatarra,Chatarra,Chatarra])
                            (Bifurcacion (Cofre [Chatarra, Chatarra, Chatarra, Chatarra])
                                (Fin (Cofre []))
                                (Fin (Cofre [])))
                            (Fin (Cofre [Chatarra, Chatarra, Chatarra, Chatarra]))
                        )
                        (Bifurcacion (Cofre [Chatarra,Chatarra,Chatarra])
                            (Fin (Cofre [Chatarra, Chatarra, Chatarra, Chatarra]))
                            (Fin (Cofre [Chatarra, Chatarra, Chatarra, Chatarra]))
                        )
                    )
                    (Bifurcacion (Cofre [Tesoro,Chatarra]) 
                        (Bifurcacion (Cofre [Chatarra,Chatarra,Chatarra])
                            (Fin (Cofre [Chatarra, Chatarra, Tesoro, Chatarra]))
                            (Fin (Cofre [Chatarra, Chatarra, Chatarra, Chatarra]))
                        )
                        (Bifurcacion (Cofre [Chatarra,Chatarra,Chatarra])
                            (Fin (Cofre [Chatarra, Chatarra, Tesoro, Chatarra]))
                            (Fin (Cofre [Chatarra, Chatarra, Chatarra, Chatarra]))
                        )
                    )
                    

caminoDeLaRamaMasLarga :: Mapa -> [Dir]
--Indica el camino de la rama más larga.
caminoDeLaRamaMasLarga (Fin _) = []
caminoDeLaRamaMasLarga (Bifurcacion _ m1 m2) = direccionDe m1  ++ caminoDeLaRamaMasLarga m2 ++ caminoDeLaRamaMasLarga m1
                                                 
direccionDe :: Mapa -> [Dir]
direccionDe (Fin _) = []
direccionDe (Bifurcacion _ m1 m2) = if not (esFin m1) 
                                    then [Izq]
                                    else [Der]
            
esFin :: Mapa -> Bool
esFin (Fin _) = True
esFin _ = False

{---------- Ejercicio 5 ---------------------------------------------}

tesorosPorNivel :: Mapa -> [[Objeto]]
--Devuelve los tesoros separados por nivel en el árbol.
tesorosPorNivel (Fin c) = [tesorosEn c]
tesorosPorNivel (Bifurcacion c m1 m2 ) = 
    tesorosEn c : zipListas (tesorosPorNivel m1) (tesorosPorNivel m2) 

zipListas :: [[a]] -> [[a]] -> [[a]]
zipListas [] yss = yss 
zipListas xss [] = xss
zipListas (xs:xss) (ys:yss) = (xs++ys) : zipListas xss yss 

tesorosEn :: Cofre -> [Objeto]
tesorosEn (Cofre []) = []
tesorosEn (Cofre os) = losTesoros os

losTesoros :: [Objeto] -> [Objeto]
losTesoros (o:os) = if esTesoro o then o : losTesoros os
                                  else losTesoros os 

{---------- Ejercicio 6 ---------------------------------------------}
todosLosCaminos :: Mapa -> [[Dir]]
--Devuelve todos lo caminos en el mapa.
todosLosCaminos (Fin _) = []
todosLosCaminos (Bifurcacion _ m1 m2) = direccionDe m1 : consACada Izq (todosLosCaminos m1)
                                     ++ consACada Der (todosLosCaminos m2)


consACada :: a -> [[a]] -> [[a]]
consACada x [] = []
consACada x (xs:xss) = (x:xs) : consACada x xss


{-- 3) Nave Espacial --}
data Componente = LanzaTorpedos | Motor Int | Almacen [Barril]
    deriving Show
data Barril = Comida | Oxigeno | Torpedo | Combustible
    deriving Show
data Sector = S SectorId [Componente] [Tripulante]
    deriving Show

type SectorId = String
type Tripulante = String

data Tree a = EmptyT | NodeT a (Tree a) (Tree a)
    deriving Show
data Nave = N (Tree Sector)
    deriving Show

idSector :: Sector -> SectorId
idSector (S id _ _) = id

sector1 = S "sector1" [LanzaTorpedos, (Almacen [Oxigeno, Comida, Torpedo])] ["tilin", "el ben"]
sector2 = S "sector2" [(Motor 10), (Motor 30), (Almacen [Combustible, Oxigeno])] ["el gongy", "el corky"]

nave = N (NodeT sector1 (NodeT sector2 EmptyT EmptyT) (EmptyT))

{---------- Ejercicio 1 ---------------------------------------------}

sectores :: Nave -> [SectorId]
sectores (N EmptyT) = []
sectores (N s)      = sectores' s

sectores' :: Tree Sector -> [SectorId]
sectores' EmptyT          = []
sectores' (NodeT s n1 n2) = sectorId s : sectores' n1 ++ sectores' n2

sectorId :: Sector -> SectorId
sectorId (S i _ _) = i

{---------- Ejercicio 2 ---------------------------------------------}

poderDePropulsion :: Nave -> Int
poderDePropulsion (N EmptyT) = 0
poderDePropulsion (N s)      = poderDePropulsion' s

poderDePropulsion' :: Tree Sector -> Int
poderDePropulsion' EmptyT          = 0
poderDePropulsion' (NodeT s n1 n2) = sumarPoderes (componentes s) + poderDePropulsion' n1 + poderDePropulsion' n2

sumarPoderes :: [Componente] -> Int
sumarPoderes []     = 0
sumarPoderes (c:cs) = potencia c + sumarPoderes cs

potencia :: Componente -> Int
potencia (Motor p) = p
potencia _         = 0

componentes :: Sector -> [Componente]
componentes (S _ c _) = c

{---------- Ejercicio 3 ---------------------------------------------}

barriles :: Nave -> [Barril]
barriles (N EmptyT) = []
barriles (N s)      = barriles' s

barriles' :: Tree Sector -> [Barril]
barriles' EmptyT          = []
barriles' (NodeT s n1 n2) = barrilesDelSector (componentes s) ++ barriles' n1 ++ barriles' n2

barrilesDelSector :: [Componente] -> [Barril]
barrilesDelSector []     = []
barrilesDelSector (c:cs) = barrilesDelAlmacen c ++ barrilesDelSector cs

barrilesDelAlmacen :: Componente -> [Barril]
barrilesDelAlmacen (Almacen b) = b
barrilesDelAlmacen _           = []

{---------- Ejercicio 4 ---------------------------------------------}
esMismoSectorId :: SectorId -> SectorId -> Bool
esMismoSectorId id1 id2 = id1 == id2

agregarComponentes :: [Componente] -> Sector -> Sector
agregarComponentes cs1 (S id cs2 ts) = S id (cs1 ++ cs2) ts

agregarASectorS :: [Componente] -> SectorId -> Sector -> Sector
agregarASectorS cs1 id1 s = if esMismoSectorId id1 (idSector s)
                                            then agregarComponentes cs1 s
                                            else s

agregarASectorT :: [Componente] -> SectorId -> Tree Sector -> Tree Sector
agregarASectorT cs id EmptyT = EmptyT
agregarASectorT cs id (NodeT s t1 t2) = NodeT (agregarASectorS cs id s) (agregarASectorT cs id t1) (agregarASectorT cs id t2)

agregarASector :: [Componente] -> SectorId -> Nave -> Nave
-- En caso de no existir el sector en la nave, no añade la lista
agregarASector [] _ n = n
agregarASector cs id (N t) = N (agregarASectorT cs id t)

{---------- Ejercicio 5 ---------------------------------------------}
asignarTripulanteA :: Tripulante -> [SectorId] -> Nave -> Nave
-- PRECOND: Todos los id de la lista pertenecen a la Nave
asignarTripulanteA tp [] (N t) = N t
asignarTripulanteA tp sts (N t) = N (asignarTripulanteAT tp sts t)

asignarTripulanteAS :: Tripulante -> Sector -> Sector
asignarTripulanteAS tp (S id cps tps) = S id cps (tp:tps)

asignarTripulanteAT :: Tripulante -> [SectorId] -> Tree Sector -> Tree Sector
asignarTripulanteAT _ _ EmptyT = EmptyT
asignarTripulanteAT tp [] (NodeT s t1 t2) = NodeT s (asignarTripulanteAT tp [] t1) (asignarTripulanteAT tp [] t2)
asignarTripulanteAT tp (st:sts) (NodeT s t1 t2) = if esMismoSectorId st (idSector s) 
                                                    then NodeT (asignarTripulanteAS tp s) (asignarTripulanteAT tp sts t1) (asignarTripulanteAT tp sts t2)
                                                    else NodeT s (asignarTripulanteAT tp sts t1) (asignarTripulanteAT tp sts t2)

{---------- Ejercicio 6 ---------------------------------------------}

sectoresAsignados :: Tripulante -> Nave -> [SectorId]
sectoresAsignados tp (N t) = sectoresAsignadosT tp t

esTripulanteAsignadoASector :: Tripulante -> Sector -> Bool
esTripulanteAsignadoASector tp (S id cps tps) = pertenece tp tps

sectoresAsignadosT :: Tripulante -> Tree Sector -> [SectorId]
sectoresAsignadosT tp EmptyT = []
sectoresAsignadosT tp (NodeT s t1 t2) = if esTripulanteAsignadoASector tp s 
                                            then idSector s : sectoresAsignadosT tp t1 ++ sectoresAsignadosT tp t2
                                            else sectoresAsignadosT tp t1 ++ sectoresAsignadosT tp t2

{---------- Ejercicio 7 ---------------------------------------------}
tripulantes :: Nave -> [Tripulante]
tripulantes (N t) = sinRepetir(tripulantesT t)

sinRepetir :: Eq a => [a] -> [a]
sinRepetir [] = []
sinRepetir (x:xs) = if pertenece x xs
                        then xs
                        else x:xs

tripulantesS :: Sector -> [Tripulante]
tripulantesS (S id cps tps) = tps

tripulantesT :: Tree Sector -> [Tripulante]
tripulantesT EmptyT = []
tripulantesT (NodeT s t1 t2) = tripulantesS s ++ tripulantesT t1 ++ tripulantesT t2

------------------------------------Manada de Lobos------------------------------------

type Presa = String
type Territorio = String
type Nombre = String
data Lobo = Cazador Nombre [Presa] Lobo Lobo Lobo
            | Explorador Nombre [Territorio] Lobo Lobo
            | Cria Nombre
            deriving Show
data Manada = M Lobo
            deriving Show

esCria :: Lobo -> Bool
esCria (Cria _) = True
esCria _ = False

unoSiEsCriaSinoCantDeCrias :: Lobo -> Int
unoSiEsCriaSinoCantDeCrias (Cria _) = 1
unoSiEsCriaSinoCantDeCrias l = cantidadDeCriasL l

cantidadDeCriasL :: Lobo -> Int
cantidadDeCriasL (Explorador _ _ l1 l2) = unoSiEsCriaSinoCantDeCrias l1 + unoSiEsCriaSinoCantDeCrias l2
cantidadDeCriasL (Cazador _ _ l1 l2 l3) = unoSiEsCriaSinoCantDeCrias l1 + unoSiEsCriaSinoCantDeCrias l2 + unoSiEsCriaSinoCantDeCrias l3

cantidadDeCrias :: Manada -> Int
cantidadDeCrias (M l) = unoSiEsCriaSinoCantDeCrias l

cantidadDeAlimentoL :: Lobo -> Int
cantidadDeAlimentoL (Cria _) = 0
cantidadDeAlimentoL (Explorador _ _ l1 l2) = cantidadDeAlimentoL l1 + cantidadDeAlimentoL l2
cantidadDeAlimentoL (Cazador _ ps l1 l2 l3) = length ps + cantidadDeAlimentoL l1 + cantidadDeAlimentoL l2 + cantidadDeAlimentoL l3

cantidadDeAlimento :: Manada -> Int
cantidadDeAlimento (M l) = cantidadDeAlimentoL l

buenaCaza :: Manada -> Bool
buenaCaza m = cantidadDeAlimento m > cantidadDeCrias m

--

elegirEntre :: (Nombre, Int) -> (Nombre, Int) -> (Nombre, Int)
elegirEntre (n1, c1) (n2, c2) = if (c1>=c2) then (n1, c1)
                                                else (n2, c2)

elegir :: [ (Nombre, Int) ] -> (Nombre, Int)
-- PRECOND: la lista no es vacía
elegir (nc : [])  = nc
elegir (nc : ncs) = elegirEntre nc (elegir ncs)

elAlfaL :: Lobo -> (Nombre, Int)
elAlfaL (Cazador n presas l1 l2 l3) = elegir [ (n, length presas)
                                                , elAlfaL l1
                                                , elAlfaL l2
                                                , elAlfaL l3
                                                ]
elAlfaL (Explorador n _ l1 l2)      = elegir [ elAlfaL l1
                                                , elAlfaL l2
                                                , (n, 0)
                                                ]
elAlfaL (Cria n)                    = (n, 0)

elAlfa :: Manada -> (Nombre, Int)
elAlfa (M l) = elAlfaL l

--

losQueExploraronL :: Territorio -> Lobo -> [Nombre]
losQueExploraronL t (Cria _) = []
losQueExploraronL t (Cazador n ps l1 l2 l3) = losQueExploraronL t l1 ++ losQueExploraronL t l2 ++ losQueExploraronL t l3
losQueExploraronL t (Explorador n ts l1 l2) = if pertenece t ts 
                                                then n : losQueExploraronL t l1 ++ losQueExploraronL t l2
                                                else losQueExploraronL t l1 ++ losQueExploraronL t l2

losQueExploraron :: Territorio -> Manada -> [Nombre]
losQueExploraron t (M l) = losQueExploraronL t l

--

estaEnLosTerritorios :: Territorio -> [Territorio] -> Bool
estaEnLosTerritorios _ [] = False
estaEnLosTerritorios t1 (t2:t2s) = if t1 == t2
                                    then True
                                    else False || estaEnLosTerritorios t1 t2s

sinTerritoriosRepetidos :: [Territorio] -> [Territorio]
sinTerritoriosRepetidos [] = []
sinTerritoriosRepetidos (t:ts) = if estaEnLosTerritorios t ts
                                    then sinTerritoriosRepetidos ts
                                    else t : sinTerritoriosRepetidos ts

territoriosL :: Lobo -> [Territorio]
territoriosL (Cria _) = []
territoriosL (Explorador _ ts l1 l2) = ts ++ territoriosL l1 ++ territoriosL l2
territoriosL (Cazador _ _ l1 l2 l3) = territoriosL l1 ++ territoriosL l2 ++ territoriosL l3

territorios :: Manada -> [Territorio]
territorios (M l) = sinTerritoriosRepetidos(territoriosL l)

exploradoresPorTerritorioDeLista :: [Territorio] -> Manada -> [(Territorio, [Nombre])]
exploradoresPorTerritorioDeLista [] _ = []
exploradoresPorTerritorioDeLista (t:ts) m = (t, losQueExploraron t m) : exploradoresPorTerritorioDeLista ts m

exploradoresPorTerritorio :: Manada -> [(Territorio, [Nombre])]
exploradoresPorTerritorio m = exploradoresPorTerritorioDeLista (territorios m) m

--

estaVacia :: [a] -> Bool
estaVacia [] = True
estaVacia _ = False

agregarSi :: a -> [a] -> Bool -> [a]
agregarSi x xs c = if c then (x:xs) else xs

nombreDe :: Lobo -> Nombre
nombreDe (Cria n) = n
nombreDe (Cazador n _ _ _ _) = n
nombreDe (Explorador n _ _ _) = n

esCazador :: Lobo -> Bool
esCazador (Cazador _ _ _ _ _) = True
esCazador _ = False

nombresDe :: [Lobo] -> [Nombre]
nombresDe [] = []
nombresDe (l:ls) = nombreDe l : nombresDe ls

soloCazadores :: [Lobo] -> [Lobo]
soloCazadores [] = []
soloCazadores (l:ls) = agregarSi l (soloCazadores ls) (esCazador l)

todosLosSuperiores :: Nombre -> Lobo -> [Lobo]
-- COMENTARIO: Se utiliza unión de listas para evaluar si el cazador buscado se encuentra entre los descendientes, ya que si no está alli, las listas son vacías.
todosLosSuperiores n (Cria n2) = []
todosLosSuperiores n (Explorador n2 ts l1 l2) = 
    let descendientes = todosLosSuperiores n l1 ++ todosLosSuperiores n l2
        exploradorActual = (Explorador n2 ts l1 l2)
    in if nombreDe l1 == n || nombreDe l2 == n
            then [exploradorActual]
            else if not(estaVacia (descendientes))
                then exploradorActual : descendientes
                else []
todosLosSuperiores n (Cazador n2 ps l1 l2 l3) =
    let descendientes = todosLosSuperiores n l1 ++ todosLosSuperiores n l2 ++ todosLosSuperiores n l3
        cazadorActual = (Cazador n2 ps l1 l2 l3)
    in if nombreDe l1 == n || nombreDe l2 == n || nombreDe l3 == n
            then [cazadorActual]
                else if not(estaVacia (descendientes))
                    then cazadorActual : descendientes
                    else []

superioresDelCazador :: Nombre -> Manada -> [Nombre]
--PRECOND: hay un cazador con dicho nombre y es único.
superioresDelCazador n (M l) = nombresDe(soloCazadores(todosLosSuperiores n l))
