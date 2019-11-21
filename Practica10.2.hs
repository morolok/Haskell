-- PD- 2019/20
-- Correspondiente a Relaci�n 21 de I1M 2018-19
-- El TAD de las pilas.
-- Departamento de Ciencias de la Computaci�n e I.A.
-- Universidad de Sevilla
-- =====================================================================

-- ---------------------------------------------------------------------
-- Introducci�n                                                       --
-- ---------------------------------------------------------------------

-- El objetivo de esta relaci�n de ejercicios es definir funciones sobre 
-- el TAD de las pilas, utilizando las implementaciones estudiadas en el 
-- tema 14 cuyas transparencias se encuentran en 
--    http://www.cs.us.es/~jalonso/cursos/i1m-17/temas/tema-14.html
-- 
-- Para realizar los ejercicios hay que instalar la librer�a I1M que
-- contiene la implementaci�n de TAD de las pilas. Los pasos para
-- instalarla son los siguientes:
-- + Descargar el paquete I1M desde http://bit.ly/1pbnDqm
-- + Descomprimirlo (y se crea el directorio I1M-master.zip).
-- + Cambiar al directorio I1M-master.
-- + Ejecutar cabal install I1M.cabal
--
-- Otra forma es descargar las implementaciones de las implementaciones
-- de las pilas:
-- + PilaConTipoDeDatoAlgebraico.hs que est� en http://bit.ly/21z3g49
-- + PilaConListas.hs               que est� en http://bit.ly/21z3oAD

-- ---------------------------------------------------------------------
-- Importaci�n de librer�as                                           --
-- ---------------------------------------------------------------------


import Data.List

import Test.QuickCheck

-- Hay que elegir una implementaci�n del TAD pilas.

--import PilaConTipoDeDatoAlgebraico

--import PilaConListas

import I1M.Pila

-- ---------------------------------------------------------------------
-- A lo largo de la relaci�n de ejercicios usaremos los siguientes
-- ejemplos de pilas:
-- ---------------------------------------------------------------------


ejP1, ejP2, ejP3, ejP4, ejP5 :: Pila Int

ejP1 = foldr apila vacia [1..20]

ejP2 = foldr apila vacia [2,5..18]

ejP3 = foldr apila vacia [3..10]

ejP4 = foldr apila vacia [4,-1,7,3,8,10,0,3,3,4]

ejP5 = foldr apila vacia [1..5]


-- ---------------------------------------------------------------------
-- Ejercicio 1: Definir la funci�n
--    filtraPila :: (a -> Bool) -> Pila a -> Pila a
-- tal que (filtraPila p pila) es la pila con los elementos de pila
-- que verifican el predicado p, en el mismo orden. Por ejemplo,
--    ghci> ejP1
--    1|2|3|4|5|6|7|8|9|10|11|12|13|14|15|16|17|18|19|20|-
--    ghci> filtraPila even ejP1
--    2|4|6|8|10|12|14|16|18|20|-

-- ---------------------------------------------------------------------


filtraPila :: (a -> Bool) -> Pila a -> Pila a

filtraPila p pila
    | esVacia pila = vacia
    | p x = apila x $ filtraPila p (desapila pila)
    | otherwise = filtraPila p (desapila pila)
    where x = cima pila


-- ---------------------------------------------------------------------
-- Ejercicio 2: Definir la funci�n
--    mapPila :: (a -> a) -> Pila a -> Pila a
-- tal que (mapPila f pila) es la pila formada con las im�genes por f de
-- los elementos de pila, en el mismo orden. Por ejemplo,
--    ghci> mapPila (+7) ejP1
--    8|9|10|11|12|13|14|15|16|17|18|19|20|21|22|23|24|25|26|27|-
-- ---------------------------------------------------------------------


mapPila :: (a -> a) -> Pila a -> Pila a

mapPila f p
    | esVacia p = vacia
    | otherwise = apila (f x) (mapPila f (resto))
    where x = cima p
          resto = desapila p


-- ---------------------------------------------------------------------
-- Ejercicio 3: Definir la funci�n
--    pertenecePila :: (Eq a) => a -> Pila a -> Bool
-- tal que (pertenecePila y p) se verifica si y s�lo si y es un elemento
-- de la pila p. Por ejemplo,
--    pertenecePila 7 ejP1  == True
--    pertenecePila 70 ejP1 == False
-- ---------------------------------------------------------------------


pertenecePila :: (Eq a) => a -> Pila a -> Bool

pertenecePila y pila
    | esVacia pila = False
    | y == x = True
    | otherwise = pertenecePila y resto
    where x = cima pila
          resto = desapila pila

-- ---------------------------------------------------------------------
-- Ejercicio 4: definir la funci�n
--    contenidaPila :: (Eq a) => Pila a -> Pila a -> Bool
-- tal que (contenidaPila p1 p2) se verifica si y s�lo si todos los
-- elementos de p1 son elementos de p2. Por ejemplo,
--    contenidaPila ejP2 ejP1 == True
--    contenidaPila ejP1 ejP2 == False
-- ---------------------------------------------------------------------


contenidaPila :: (Eq a) => Pila a -> Pila a -> Bool

contenidaPila p1 p2
    | esVacia p1 = True
    | otherwise = pertenecePila x p2 && contenidaPila resto p2
    where x = cima p1
          resto = desapila p1


-- ---------------------------------------------------------------------
-- Ejercicio 4: Defiir la funci�n
--    prefijoPila :: (Eq a) => Pila a -> Pila a -> Bool
-- tal que (prefijoPila p1 p2) se verifica si la pila p1 es justamente
-- un prefijo de la pila p2. Por ejemplo,
--    prefijoPila ejP3 ejP2 == False
--    prefijoPila ejP5 ejP1 == True
-- ---------------------------------------------------------------------


prefijoPila :: (Eq a) => Pila a -> Pila a -> Bool

prefijoPila p1 p2
    | esVacia p1 = True
    | esVacia p2 = False
    | otherwise = (x == y) && (prefijoPila resto1 resto2)
    where x = cima p1
          y = cima p2
          resto1 = desapila p1
          resto2 = desapila p2


-- ---------------------------------------------------------------------
-- Ejercicio 5: Definir la funci�n
--    subPila :: (Eq a) => Pila a -> Pila a -> Bool
-- tal que (subPila p1 p2) se verifica si p1 es una subpila de p2.
-- Por ejemplo, 
--    subPila ejP2 ejP1 == False
--    subPila ejP3 ejP1 == True
-- ---------------------------------------------------------------------


subPila :: (Eq a) => Pila a -> Pila a -> Bool

subPila p1 p2
    | esVacia p1 = True
    | esVacia p2 = False
    | otherwise = (prefijoPila p1 p2) || (subPila p1 resto2)
    where resto2 = desapila p2


-- ---------------------------------------------------------------------
-- Ejercicio 6: Definir la funci�n
--    ordenadaPila :: (Ord a) => Pila a -> Bool
-- tal que (ordenadaPila p) se verifica si los elementos de la pila p
-- est�n ordenados en orden creciente. Por ejemplo,
--    ordenadaPila ejP1 == True
--    ordenadaPila ejP4 == False
-- ---------------------------------------------------------------------


ordenadaPila :: (Ord a) => Pila a -> Bool

ordenadaPila pila = ordenadaPilaAux x resto
    where x = cima pila
          resto = desapila pila

ordenadaPilaAux x pila
    | esVacia pila = True
    | otherwise = (x < y) && (ordenadaPilaAux y resto)
    where y = cima pila
          resto = desapila pila


ordenadaPilaProf :: (Ord a) => Pila a -> Bool

ordenadaPilaProf pila
    | esVacia pila = True
    | esVacia resto = True
    | otherwise = (x < y) && (ordenadaPilaProf resto)
    where x = cima pila
          resto = desapila pila
          y = cima resto


-- ---------------------------------------------------------------------
-- Ejercicio 7.1: Definir una funci�n
--    lista2Pila :: [a] -> Pila a
-- tal que (lista2Pila xs) es una pila formada por los elementos de xs.
-- Por ejemplo,
--    lista2Pila [1..6] == 1|2|3|4|5|6|-
-- ---------------------------------------------------------------------


lista2Pila :: [a] -> Pila a

lista2Pila xs = lista2PilaAux xs (length xs - 1) vacia

lista2PilaAux xs cont pila
    | cont == 0 = res
    | otherwise = lista2PilaAux xs (cont-1) nuevaPila
    where res = apila (xs!!cont) pila
          nuevaPila = apila (xs!!cont) pila


--lista2PilaProf :: [a] -> Pila a



-- ---------------------------------------------------------------------
-- Ejercicio 7.2: Definir una funci�n
--  pila2Lista :: Pila a -> [a]
-- tal que (pila2Lista p) es la lista formada por los elementos de p.
-- Por ejemplo,
--    pila2Lista ejP2 == [2,5,8,11,14,17]
-- ---------------------------------------------------------------------


pila2Lista :: Pila a -> [a]

pila2Lista pila = pila2ListaAux pila []

pila2ListaAux pila res
    | esVacia pila = res
    | otherwise = pila2ListaAux resto (res++[x])
    where x = cima pila
          resto = desapila pila


-- ---------------------------------------------------------------------
-- Ejercicio 7.3: Comprobar con QuickCheck que la funci�n pila2Lista es
-- la inversa de  lista2Pila, y rec�procamente.
-- ---------------------------------------------------------------------


prop_pila2Lista :: Pila Int -> Bool

prop_pila2Lista pila = (lista2Pila (pila2Lista pila)) == pila


-- ghci> quickCheck prop_pila2Lista
-- +++ OK, passed 100 tests.


prop_lista2Pila :: [Int] -> Bool

prop_lista2Pila xs = (pila2Lista (lista2Pila xs)) == xs


-- ghci> quickCheck prop_lista2Pila
-- +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 9.1: Definir la funci�n 
--    ordenaInserPila :: (Ord a) => Pila a -> Pila a
-- tal que (ordenaInserPila p) es una pila con los elementos de la pila
-- p, ordenados por inserci�n. Por ejemplo,
--    ghci> ordenaInserPila ejP4
--    -1|0|3|3|3|4|4|7|8|10|-
-- ---------------------------------------------------------------------

ordenaInserPila :: (Ord a) => Pila a -> Pila a
ordenaInserPila = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 9.2: Comprobar con QuickCheck que la pila 
---    (ordenaInserPila p) 
-- est� ordenada correctamente.

prop_ordenaInserPila :: Pila Int -> Bool
prop_ordenaInserPila p = undefined

-- ghci> quickCheck prop_ordenaInserPila
-- +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 10.1: Definir la funci�n
--    nubPila :: (Eq a) => Pila a -> Pila a
-- tal que (nubPila p) es una pila con los elementos de p sin
-- repeticiones. Por ejemplo,
--    ghci> ejP4
--    4|-1|7|3|8|10|0|3|3|4|-
--    ghci> nubPila ejP4
--    -1|7|8|10|0|3|4|-
-- ---------------------------------------------------------------------


nubPila :: (Eq a) => Pila a -> Pila a

nubPila pila = nubPilaAux pila [] []

nubPilaAux pila ls res
    | esVacia pila = lista2Pila res
    | elem x ls = nubPilaAux resto ls res
    | otherwise = nubPilaAux resto (ls++[x]) (res++[x])
    where x = cima pila
          resto = desapila pila


nubPila2 :: (Eq a) => Pila a -> Pila a

nubPila2 pila
    | esVacia resto = apila x vacia
    | pertenecePila x resto = nubPila2 resto
    | otherwise = apila x (nubPila2 resto)
    where x = cima pila
          resto = desapila pila

-- ---------------------------------------------------------------------
-- Ejercicio 10.2: Definir la propiedad siguiente: "las composici�n de
-- las funciones nub y pila2Lista coincide con la composici�n de las
-- funciones pila2Lista y nubPila", y comprobarla con quickCheck.
-- En caso de ser falsa, redefinir la funci�n nubPila para que se
-- verifique la propiedad.
-- ---------------------------------------------------------------------


-- La propiedad es

prop_nubPila :: Pila Int -> Bool

prop_nubPila p = pila2Lista (nubPilaProf p) == nub (pila2Lista p)


-- La comprobaci�n es

nubPilaProf :: (Eq a) => Pila a -> Pila a

nubPilaProf pila
    | esVacia resto = vacia
    | otherwise = apila x (nubPilaProf restoF)
    where x = cima pila
          resto = desapila pila
          restoF = filtraPila (/=x) resto


-- ---------------------------------------------------------------------
-- Ejercicio 11: Definir la funci�n 
--    maxPila :: (Ord a) => Pila a -> a
-- tal que (maxPila p) sea el mayor de los elementos de la pila p. Por
-- ejemplo, 
--    ghci> ejP4
--    4|-1|7|3|8|10|0|3|3|4|-
--    ghci> maxPila ejP4
--    10
-- ---------------------------------------------------------------------

maxPila :: (Ord a) => Pila a -> a
maxPila = undefined

-- ---------------------------------------------------------------------
-- Generador de pilas                                                 --
-- ---------------------------------------------------------------------

-- genPila es un generador de pilas. Por ejemplo,
--    ghci> sample genPila
--    -
--    0|0|-
--    -
--    -6|4|-3|3|0|-
--    -
--    9|5|-1|-3|0|-8|-5|-7|2|-
--    -3|-10|-3|-12|11|6|1|-2|0|-12|-6|-
--    2|-14|-5|2|-
--    5|9|-
--    -1|-14|5|-
--    6|13|0|17|-12|-7|-8|-19|-14|-5|10|14|3|-18|2|-14|-11|-6|-
genPila :: (Arbitrary a, Num a) => Gen (Pila a)
genPila = do xs <- listOf arbitrary
             return (foldr apila vacia xs)
  
-- El tipo pila es una instancia del arbitrario. 
instance (Arbitrary a, Num a) => Arbitrary (Pila a) where
    arbitrary = genPila


