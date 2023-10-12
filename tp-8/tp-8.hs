import Map 

data Componente = LanzaTorpedos | Motor Int | Almacen [Barril]
data Barril = Comida | Oxigeno | Torpedo | Combustible

type SectorId = String
type Nombre = String
type Rango = String

module Nave 
(N, construir, ingresarT, sectoresAsignados, datosDeSector, tripulantesN, agregarASector, asignarASector )
where 
    data Nave = N (Map SectorId Sector) (Map Nombre Tripulante) (MaxHeap Tripulante)
    {- 
        INV: cada tripulante del map debe estar en el MaxHeap
        Un tripulante no puede estar asignado a un Sector que no exista en la Nave
    -}
construir :: [SectorId] -> Nave
construir (s:ss) = let newS = crearS s
                   in N( assocM s newS (construir ss)) emptyM emptyH 

ingresarT :: Nombre -> Rango -> Nave -> Nave
ingresarT no r (N m m2 h) = case (lookupM no m2) of
                            Nothing -> let newT = crearT no r 
                                        in  N m (assocM no newT m2) (insertH newT h)
                            Just t -> N m m2 h    

sectoresAsignados :: Nombre -> Nave -> Set SectorId
sectoresAsignados n (N ms mt h) = let t = lookupM n mt
                                 in sectoresT t  {- abrir la estructura es O(1), lookupM es O(log t) 
                                                    y sectoresT O(1) por lo que queda O(log t) -}     

datosDeSector :: SectorId -> Nave -> (Set Nombre, [Componente])
datosDeSector s (N ms mt h) = let s = lookupM s ms
                             in (tripulantesS s, componentesS s)
{-Abrir la estructura de nave es O(1), lookupM O(log s) y tripulantesS y componentesS son O(1) -}

tripulantesN :: Nave -> [Tripulante]
tripulantesN (N ms mt h) = case (isEmptyH h) of
                          False -> let m = maxH h 
                                   in m : tripulantesN (N ms (deleteM (nombre m)) (deleteMaxH h))
                          True -> N(ms mt h)

agregarASector :: [Componente] -> SectorId -> Nave -> Nave
agregarASector cs id (N ms mt h) = case (lookupM id ms) of
                                      Just s -> let newS = agregarComponentes cs id 
                                                in N(assocM id newS ms) mt mh 
                                      Nothing ->  N( ms mt h)  

agregarComponentes :: [Componentes] -> Sector -> Sector 
agregarComponentes [] s     = s 
agregarComponentes (c:cs) s = agregarC c (agregarComponentes cs s)  

asignarASector :: Nombre -> SectorId -> Nave -> Nave

{- 
h) asignarASector :: Nombre -> SectorId -> Nave -> Nave
Propósito: Asigna un sector a un tripulante.
Nota: No importa si el tripulante ya tiene asignado dicho sector.
Precondición: El tripulante y el sector existen.
Eficiencia: O(log S + log T + T log T)

-}

