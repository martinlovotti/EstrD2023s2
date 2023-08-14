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

--Hasta acá por ahora

{-
data Persona = P String Int String

martin = P "Martin" 20 "argentino" 

nombre :: Persona -> String
nombre (P n e d) = n

edad :: Persona -> Int
edad (P n e d) = e

nacionalidad :: Persona -> String
nacionalidad (P n e d) = d

esMayor :: Persona -> Bool
esMayor p = edad p >= 18

esArgentino :: Persona -> Bool
esArgentino p = nacionalidad p == "argentino" 
-}