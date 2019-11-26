-- PD- 2019/20
-- Correspondiente a Relaci�n 22 de I1M 2017-18
-- El TAD de las colas.
-- Departamento de Ciencias de la Computaci�n e I.A.
-- Universidad de Sevilla
-- =====================================================================

-- ---------------------------------------------------------------------
-- Introducci�n                                                       --
-- ---------------------------------------------------------------------

-- El objetivo de esta relaci�n de ejercicios es definir funciones sobre 
-- el TAD de las colas, utilizando las implementaciones estudiadas en el 
-- tema 15 transparencias se encuentran en 
--    http://www.cs.us.es/~jalonso/cursos/i1m-17/temas/tema-15.html
-- 
-- Para realizar los ejercicios hay que instalar la librer�a I1M que
-- contiene la implementaci�n de TAD de las pilas. Los pasos para
-- instalarla son los siguientes:
-- + Descargar el paquete I1M desde http://bit.ly/1pbnDqm
-- + Descomprimirlo (y se crea el directorio I1M-master.zip).
-- + Cambiar al directorio I1M-master.
-- + Ejecutar cabal install I1M.caba
-- 
-- Otra forma es descargar las implementaciones de las implementaciones
-- de las colas:
-- + ColaConListas.hs    que est� en http://bit.ly/21z3wQL
-- + ColaConDosListas.hs que est� en http://bit.ly/21z3AQp

-- ---------------------------------------------------------------------
-- Importaci�n de librer�as                                           --
-- ---------------------------------------------------------------------


import Data.List

import Test.QuickCheck

-- Hay que elegir una implementaci�n del TAD colas:

import ColaConListas

-- import ColaConDosListas

-- import I1M.Cola

    
-- ---------------------------------------------------------------------
-- Nota. A lo largo de la relaci�n de ejercicios usaremos los siguientes
-- ejemplos de colas:


c1, c2, c3, c4, c5, c6 :: Cola Int

c1 = foldr inserta vacia [1..20]

c2 = foldr inserta vacia [2,5..18]

c3 = foldr inserta vacia [3..10]

c4 = foldr inserta vacia [4,-1,7,3,8,10,0,3,3,4]

c5 = foldr inserta vacia [15..20]

c6 = foldr inserta vacia (reverse [1..20])


-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 1: Definir la funci�n
--    ultimoCola :: Cola a -> a
-- tal que (ultimoCola c) es el �ltimo elemento de la cola c. Por
-- ejemplo:
--    ultimoCola c4 == 4
--    ultimoCola c5 == 15
-- ---------------------------------------------------------------------


ultimoCola :: Cola a -> a

ultimoCola c = ultimoColaAux c x
    where x = primero c

ultimoColaAux c prim
    | esVacia c = prim
    | otherwise = ultimoColaAux r x
    where r = resto c
          x = primero c


-- ---------------------------------------------------------------------
-- Ejercicio 2: Definir la funci�n
--    longitudCola :: Cola a -> Int
-- tal que (longitudCola c) es el n�mero de elementos de la cola c. Por
-- ejemplo, 
--     longitudCola c2 == 6
-- ---------------------------------------------------------------------

longitudCola :: Cola a -> Int
longitudCola c = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 3: Definir la funci�n 
--    todosVerifican :: (a -> Bool) -> Cola a -> Bool
-- tal que (todosVerifican p c) se verifica si todos los elementos de la
-- cola c cumplen la propiedad p. Por ejemplo,
--    todosVerifican (>0) c1 == True
--    todosVerifican (>0) c4 == False
-- ---------------------------------------------------------------------

todosVerifican :: (a -> Bool) -> Cola a -> Bool
todosVerifican p c = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 4: Definir la funci�n
--    algunoVerifica :: (a -> Bool) -> Cola a -> Bool
-- tal que (algunoVerifica p c) se verifica si alg�n elemento de la cola
-- c cumple la propiedad p. Por ejemplo,
--   algunoVerifica (<0) c1 == False
--   algunoVerifica (<0) c4 == True
-- ---------------------------------------------------------------------

algunoVerifica :: (a -> Bool) -> Cola a -> Bool
algunoVerifica p c = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 5: Definir la funci�n
--    ponAlaCola :: Cola a -> Cola a -> Cola a
-- tal que (ponAlaCola c1 c2) es la cola que resulta de poner los
-- elementos de c2 a la cola de c1. Por ejemplo,
--    ponAlaCola c2 c3 == C [17,14,11,8,5,2,10,9,8,7,6,5,4,3]
-- ---------------------------------------------------------------------


ponAlaCola :: Cola a -> Cola a -> Cola a

ponAlaCola col1 col2
    | esVacia col2 = col1
    | otherwise = ponAlaCola nuevaC1 r
    where nuevaC1 = inserta x col1
          x = primero col2
          r = resto col2


-- ---------------------------------------------------------------------
-- Ejercicio 6: Definir la funci�n
--    mezclaColas :: Cola a -> Cola a -> Cola a
-- tal que (mezclaColas c1 c2) es la cola formada por los elementos de
-- c1 y c2 colocados en una cola, de forma alternativa, empezando por
-- los elementos de c1. Por ejemplo,
--    mezclaColas c2 c4 == C [17,4,14,3,11,3,8,0,5,10,2,8,3,7,-1,4]
-- ---------------------------------------------------------------------

mezclaColas :: Cola a -> Cola a -> Cola a
mezclaColas c1 c2 = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 7: Definir la funci�n
--    agrupaColas :: [Cola a] -> Cola a
-- tal que (agrupaColas [c1,c2,c3,...,cn]) es la cola formada mezclando
-- las colas de la lista como sigue: mezcla c1 con c2, el resultado con
-- c3, el resultado con c4, y as� sucesivamente. Por ejemplo,
--    ghci> agrupaColas [c3,c3,c4]
--    C [10,4,10,3,9,3,9,0,8,10,8,8,7,3,7,7,6,-1,6,4,5,5,4,4,3,3]
-- ---------------------------------------------------------------------

agrupaColas :: [Cola a] -> Cola a
agrupaColas = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 8: Definir la funci�n
--    perteneceCola :: Eq a => a -> Cola a -> Bool
-- tal que (perteneceCola x c) se verifica si x es un elemento de la
-- cola c. Por ejemplo, 
--    perteneceCola 7 c1  == True
--    perteneceCola 70 c1 == False
-- ---------------------------------------------------------------------

perteneceCola :: Eq a => a -> Cola a -> Bool
perteneceCola y c = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 9: Definir la funci�n
--    contenidaCola :: Eq a => Cola a -> Cola a -> Bool
-- tal que (contenidaCola c1 c2) se verifica si todos los elementos de
-- c1 son elementos de c2. Por ejemplo, 
--    contenidaCola c2 c1 == True
--    contenidaCola c1 c2 == False
-- ---------------------------------------------------------------------

contenidaCola :: Eq a => Cola a -> Cola a -> Bool
contenidaCola c1 c2 = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 10: Definir la funci�n
--    prefijoCola :: Eq a => Cola a -> Cola a -> Bool
-- tal que (prefijoCola c1 c2) se verifica si la cola c1 es un prefijo
-- de la cola c2. Por ejemplo, 
--    prefijoCola c3 c2 == False
--    prefijoCola c5 c1 == True
-- ---------------------------------------------------------------------


prefijoCola :: Eq a => Cola a -> Cola a -> Bool

prefijoCola c1 c2
    | esVacia c1 = True
    | esVacia c2 = False
    | otherwise = (x == y) && (prefijoCola r1 r2)
    where x = primero c1
          y = primero c2
          r1 = resto c1
          r2 = resto c2


-- ---------------------------------------------------------------------
-- Ejercicio 11: Definir la funci�n
--    subCola :: Eq a => Cola a -> Cola a -> Bool
-- tal que (subCola c1 c2) se verifica si c1 es una subcola de c2. Por
-- ejemplo,  
--    subCola c2 c1 == False
--    subCola c3 c1 == True
-- ---------------------------------------------------------------------


subCola :: Eq a => Cola a -> Cola a -> Bool

subCola c1 c2 = undefined


-- ---------------------------------------------------------------------
-- Ejercicio 12: Definir la funci�n
--    ordenadaCola :: Ord a => Cola a -> Bool
-- tal que (ordenadaCola c) se verifica si los elementos de la cola c
-- est�n ordenados en orden creciente. Por ejemplo,
--    ordenadaCola c6 == True
--    ordenadaCola c4 == False
-- ---------------------------------------------------------------------

ordenadaCola :: Ord a => Cola a -> Bool
ordenadaCola c = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 13.1: Definir una funci�n
--    lista2Cola :: [a] -> Cola a
-- tal que (lista2Cola xs) es una cola formada por los elementos de xs.
-- Por ejemplo,
--    lista2Cola [1..6] == C [1,2,3,4,5,6]
-- ---------------------------------------------------------------------


lista2Cola :: [a] -> Cola a

lista2Cola xs = lista2ColaAux xs vacia

lista2ColaAux [] res = res

lista2ColaAux (x:xs) res = lista2ColaAux xs (inserta x res)


lista2ColaP :: [a] -> Cola a

lista2ColaP xs = foldl (\c x -> inserta x c) vacia xs
                    -- flip inserta


-- ---------------------------------------------------------------------
-- Ejercicio 13.2: Definir una funci�n
--    cola2Lista :: Cola a -> [a]
-- tal que (cola2Lista c) es la lista formada por los elementos de p.
-- Por ejemplo,
--    cola2Lista c2 == [17,14,11,8,5,2]
-- ---------------------------------------------------------------------


cola2Lista :: Cola a -> [a]

cola2Lista cola
    | esVacia cola = []
    | otherwise = [x]++(cola2Lista r)
    where x = primero cola
          r = resto cola


-- ---------------------------------------------------------------------
-- Ejercicio 13.3. Comprobar con QuickCheck que la funci�n cola2Lista es
-- la inversa de  lista2Cola, y rec�procamente.
-- ---------------------------------------------------------------------

prop_cola2Lista :: Cola Int -> Bool
prop_cola2Lista c = undefined

-- ghci> quickCheck prop_cola2Lista
-- +++ OK, passed 100 tests.

prop_lista2Cola :: [Int] -> Bool
prop_lista2Cola xs = undefined

-- ghci> quickCheck prop_lista2Cola
-- +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 14: Definir la funci�n 
--    maxCola :: Ord a => Cola a -> a
-- tal que (maxCola c) es el mayor de los elementos de la cola c. Por
-- ejemplo, 
--    maxCola c4 == 10
-- ---------------------------------------------------------------------

maxCola :: Ord a => Cola a -> a
maxCola p = undefined

-- ---------------------------------------------------------------------
-- Generador de colas                                          --
-- ---------------------------------------------------------------------

-- genCola es un generador de colas de enteros. Por ejemplo,
--    ghci> sample genCola
--    C ([],[])
--    C ([],[])
--    C ([],[])
--    C ([],[])
--    C ([7,8,4,3,7],[5,3,3])
--    C ([],[])
--    C ([1],[13])
--    C ([18,28],[12,21,28,28,3,18,14])
--    C ([47],[64,45,7])
--    C ([8],[])
--    C ([42,112,178,175,107],[])
genCola :: (Num a, Arbitrary a) => Gen (Cola a)
genCola = frequency [(1, return vacia),
                     (30, do n <- choose (10,100)
                             xs <- vectorOf n arbitrary
                             return (creaCola xs))]
          where creaCola = foldr inserta vacia

-- El tipo cola es una instancia del arbitrario.
instance (Arbitrary a, Num a) => Arbitrary (Cola a) where
    arbitrary = genCola

