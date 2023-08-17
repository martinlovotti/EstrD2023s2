{-
1 Números enteros
1. Defina las siguientes funciones:
a) sucesor :: Int -> Int
Dado un número devuelve su sucesor
-}
sucesor :: Int -> Int
sucesor n = n+1 --Recibe un numero n y le agrega 1

{-
b) sumar :: Int -> Int -> Int
Dados dos números devuelve su suma utilizando la operación +.-}
suma :: Int -> Int -> Int
suma a b = a+b 

{-
c) divisionYResto :: Int -> Int -> (Int, Int)
Dado dos números, devuelve un par donde la primera componente es la división del
primero por el segundo, y la segunda componente es el resto de dicha división. Nota:
para obtener el resto de la división utilizar la función mod :: Int -> Int -> Int,
provista por Haskell.-}

divisionYResto :: Int -> Int -> (Int, Int)
divisionYResto x y = ( div x y , mod x y)

{-
d) maxDelPar :: (Int,Int) -> Int
Dado un par de números devuelve el mayor de estos
-}
maxDelPar :: (Int,Int) -> Int
maxDelPar (a,b) = if a > b 
                  then a
                  else b 

{-
De 4 ejemplos de expresiones diferentes que denoten el número 10, utilizando en cada expresión a todas las funciones del punto anterior.
maxDelPar (divisionYResto (suma 10 10) (sucesor 1))
10
Main> maxDelPar (divisionYResto (suma 15 15) (sucesor 2))
10
Main> maxDelPar (divisionYResto (suma 20 20) (sucesor 3))
10
Main> maxDelPar (divisionYResto (suma 25 25) (sucesor 4))
10
-}

{-
a) opuesto :: Dir -> Dir
Dada una dirección devuelve su opuesta.
-}
data Dir = Norte | Este | Sur | Oeste

opuesto :: Dir -> Dir
opuesto x =
    case x of
        Norte -> Sur
        Este -> Oeste
        Sur -> Norte
        Oeste -> Este
{-
b) iguales :: Dir -> Dir -> Bool
Dadas dos direcciones, indica si son la misma. Nota: utilizar pattern matching y no ==.
-}
iguales :: Dir -> Dir -> Bool
iguales Norte Norte = True
iguales Sur Sur = True
iguales Este Este = True
iguales Oeste Oeste = True
iguales _ _ = False

{-
c) siguiente :: Dir -> Dir
Dada una dirección devuelve su siguiente, en sentido horario, y suponiendo que no existe
la siguiente dirección a Oeste. ¾Posee una precondición esta función? ¾Es una función
total o parcial? ¾Por qué?
-}
siguiente :: Dir -> Dir
siguiente d =
    case d of
        Norte -> Este
        Este -> Sur
        Sur -> Oeste
        Oeste -> Norte

{-
Definir el tipo de dato DiaDeSemana, con las alternativas Lunes, Martes, Miércoles, Jueves,
Viernes, Sabado y Domingo. Supongamos que el primer día de la semana es lunes, y el último
es domingo. Luego implementar las siguientes funciones:
a) primeroYUltimoDia :: (DiaDeSemana, DiaDeSemana)
Devuelve un par donde la primera componente es el primer día de la semana, y la
segunda componente es el último día de la semana. Considerar definir subtareas útiles
que puedan servir después.-}

data DiaDeSemana = Lunes | Martes | Miercoles | Jueves | Viernes | Sabado | Domingo

primeroYUltimoDia :: (DiaDeSemana, DiaDeSemana)
primeroYUltimoDia = (Lunes, Domingo)

{-
b) empiezaConM :: DiaDeSemana -> Bool
Dado un día de la semana indica si comienza con la letra M.
-}
empiezaConM :: DiaDeSemana -> Bool
empiezaConM dia = case dia of
    Martes -> True
    Miercoles -> True
    _ -> False

{-
c) vieneDespues :: DiaDeSemana -> DiaDeSemana -> Bool
Dado dos días de semana, indica si el primero viene después que el segundo. Analizar
la calidad de la solución respecto de la cantidad de casos analizados (entre los casos
analizados en esta y cualquier subtarea, deberían ser no más de 9 casos).-}

vieneDespues :: DiaDeSemana -> DiaDeSemana -> Bool
vieneDespues Lunes Domingo = True
vieneDespues Martes Lunes = True
vieneDespues Miercoles Martes = True
vieneDespues Jueves Miercoles = True
vieneDespues Viernes Jueves = True
vieneDespues Sabado Viernes = True
vieneDespues Domingo Sabado = True
vieneDespues _ _ = False

{-
d) estaEnElMedio :: DiaDeSemana -> Bool
Dado un día de la semana indica si no es ni el primer ni el ultimo dia
-}
estaEnElMedio :: DiaDeSemana -> Bool
estaEnElMedio Lunes = False
estaEnElMedio Domingo = False
estaEnElMedio _ = True

{-
3. Los booleanos también son un tipo de enumerativo. Un booleano es True o False. Dfina
las siguientes funciones utilizando pattern matching (no usar las funciones sobre booleanos
ya definidas en Haskell):
a) negar :: Bool -> Bool
Dado un booleano, si es True devuelve False, y si es False devuelve True.
En Haskell ya está definida como not.-}

negar :: Bool -> Bool
negar True = False
negar False = True

{-
b) implica :: Bool -> Bool -> Bool
Dados dos booleanos, si el primero es True y el segundo es False, devuelve False, sino
devuelve True.
Esta función NO debe realizar doble pattern matching.
Nota: no viene implementada en Haskell.-}
implica :: Bool -> Bool -> Bool
implica True False = False
implica _ _ = True

{-
c) yTambien :: Bool -> Bool -> Bool
Dados dos booleanos si ambos son True devuelve True, sino devuelve False.
Esta función NO debe realizar doble pattern matching.
En Haskell ya está definida como \&\&.-}
yTambien :: Bool -> Bool -> Bool
yTambien True True = True
iTambien _ _ = False

{-
d) oBien :: Bool -> Bool -> Bool
Dados dos booleanos si alguno de ellos es True devuelve True, sino devuelve False.
Esta función NO debe realizar doble pattern matching.
En Haskell ya está definida como ||
-}

oBien :: Bool -> Bool -> Bool
oBien True _ = True
oBien _ True = True
oBien _ _ = False

{-4. Registros
1. Definir el tipo de dato Persona, como un nombre y la edad de la persona. Realizar las
siguientes funciones:
-}
data Persona = P String Int 
 deriving Show
martin = P "Martin" 20
franco = P "Franco" 31

nombre :: Persona -> String
--Devuelve el nombre de una persona
nombre (P n e) = n


edad :: Persona -> Int
--Devuelve la edad de una persona
edad (P n e) = e


crecer :: Persona -> Persona
--Aumenta en uno la edad de la persona.
crecer (P n e) = P n (e+1)


cambioDeNombre :: String -> Persona -> Persona
--Dados un nombre y una persona, devuelve una persona con la edad de la persona y el
--nuevo nombre.
cambioDeNombre nuevoNombre (P n e) = P nuevoNombre e

esMayorQueLaOtra :: Persona -> Persona -> Bool
--Dadas dos personas indica si la primera es mayor que la segunda.
esMayorQueLaOtra per1 per2 = if edad per1 > edad per2
                            then True
                            else False

laQueEsMayor :: Persona -> Persona -> Persona
--Dadas dos personas devuelve a la persona que sea mayor.
laQueEsMayor persona1 persona2 = if edad persona1 > edad persona2
                                  then persona1
                                  else persona2

{-
2. Definir los tipos de datos Pokemon, como un TipoDePokemon (agua, fuego o planta) y un
porcentaje de energía; y Entrenador, como un nombre y dos Pokémon. Luego definir las
siguientes funciones:-}

data TipoDePokemon = Agua | Fuego | Planta deriving (Eq, Show)
 
data Pokemon = Poke TipoDePokemon Int deriving (Eq, Show)
 
data Entrenador = E String Pokemon Pokemon deriving (Eq, Show)
 
tipo :: Pokemon -> TipoDePokemon
tipo (Poke t e) = t

pok1 :: Entrenador -> Pokemon
pok1 (E n p1 p2) = p1

pok2 :: Entrenador -> Pokemon
pok2 (E n p1 p2) = p2

poke1 = Poke Agua 22
poke2 = Poke Fuego 21

yiy = E "Yiyo" poke1 poke2
tito= E "Tito" poke1 poke2
superaA :: Pokemon -> Pokemon -> Bool
--Dados dos Pokémon indica si el primero, en base al tipo, es superior al segundo. Agua
--supera a fuego, fuego a planta y planta a agua. Y cualquier otro caso es falso.
superaA (Poke Agua _) (Poke Fuego _) = True
superaA (Poke Fuego _) (Poke Planta _) = True
superaA (Poke Planta _) (Poke Agua _) = True
superaA _ _ = False

contarTipo :: TipoDePokemon -> Pokemon -> Int
contarTipo tip (Poke t e) =
        if t == tip
            then 1 
            else 0

cantidadDePokemonDe :: TipoDePokemon -> Entrenador -> Int
cantidadDePokemonDe tipoBuscado (E n p p2) =
    contarTipo tipoBuscado p + contarTipo tipoBuscado p2
  
juntarPokemon :: (Entrenador, Entrenador) -> [Pokemon]
--Dado un par de entrenadores, devuelve a sus Pokémon en una lista.
juntarPokemon (E n p1 p2, E n2 p3 p4) = [p1,p2,p3,p4]

{-
5. Funciones polimórcas
1. Dena las siguientes funciones polimórcas:-}
loMismo :: a -> a
--Dado un elemento de algún tipo devuelve ese mismo elemento.
loMismo d = d

siempreSiete :: a -> Int
--Dado un elemento de algún tipo devuelve el número 7.
siempreSiete x = 7

swap :: (a,b) -> (b, a)
--Dadas una tupla, invierte sus componentes.
swap (a,b) = (b,a)
{-¾Por qué existen dos variables de tipo diferentes?
2. Responda la siguiente pregunta: ¾Por qué estas funciones son polimórcas?
   Porque funciona para cualquier tipo de dato que se le ingrese 
-}

{-6. Pattern matching sobre listas
1. Dena las siguientes funciones polimórcas utilizando pattern matching sobre listas (no
utilizar las funciones que ya vienen con Haskell):-}

estaVacia :: [a] -> Bool
--Dada una lista de elementos, si es vacía devuelve True, sino devuelve False.
--Denida en Haskell como null.
estaVacia [] = True
estaVacia (_)  = False

elPrimero :: [a] -> a
--Dada una lista devuelve su primer elemento.
--Denida en Haskell como head.
elPrimero (x:_) = x

sinElPrimero :: [a] -> [a]
--Dada una lista devuelve esa lista menos el primer elemento.
--Denida en Haskell como tail.
sinElPrimero (_:x)= (x)

splitHead :: [a] -> (a, [a])
--Dada una lista devuelve un par, donde la primera componente es el primer elemento de la
--lista, y la segunda componente es esa lista pero sin el primero.
splitHead (x) = (elPrimero (x), sinElPrimero (x))