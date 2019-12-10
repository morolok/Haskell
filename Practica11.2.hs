-- PD 2019-18: De las relaciones de programación dinámica de I1M
-- Programación dinámica
-- Departamento de Ciencias de la Computación e I.A.
-- Universidad de Sevilla
-- =====================================================================

-- ---------------------------------------------------------------------
-- § Librerías auxiliares                                             --
-- ---------------------------------------------------------------------


import Data.List (genericLength)

import Data.Matrix


-- ---------------------------------------------------------------------
-- § Caminos en una retícula.                                         --
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 1.1. Se considera una retícula con sus posiciones numeradas,
-- desde el vértice superior izquierdo, hacia la derecha y hacia
-- abajo. Por ejemplo, la retícula de dimensión 3x4 se numera como sigue:
--    |-------+-------+-------+-------|
--    | (1,1) | (1,2) | (1,3) | (1,4) |
--    | (2,1) | (2,2) | (2,3) | (2,4) |
--    | (3,1) | (3,2) | (3,3) | (3,4) |
--    |-------+-------+-------+-------|
--
-- Definir, por recursión, la función
--    caminosR :: (Int,Int) -> [[(Int,Int)]]
-- tal que (caminosR (m,n)) es la lista de los caminos en la retícula de
-- dimensión mxn desde (1,1) hasta (m,n). Por ejemplo,
--    λ> caminosR (2,3)
--    [[(1,1),(1,2),(1,3),(2,3)],
--     [(1,1),(1,2),(2,2),(2,3)],
--     [(1,1),(2,1),(2,2),(2,3)]]
--    λ> mapM_ print (caminosR (3,4))
--    [(1,1),(1,2),(1,3),(1,4),(2,4),(3,4)]
--    [(1,1),(1,2),(1,3),(2,3),(2,4),(3,4)]
--    [(1,1),(1,2),(2,2),(2,3),(2,4),(3,4)]
--    [(1,1),(2,1),(2,2),(2,3),(2,4),(3,4)]
--    [(1,1),(1,2),(1,3),(2,3),(3,3),(3,4)]
--    [(1,1),(1,2),(2,2),(2,3),(3,3),(3,4)]
--    [(1,1),(2,1),(2,2),(2,3),(3,3),(3,4)]
--    [(1,1),(1,2),(2,2),(3,2),(3,3),(3,4)]
--    [(1,1),(2,1),(2,2),(3,2),(3,3),(3,4)]
--    [(1,1),(2,1),(3,1),(3,2),(3,3),(3,4)]
-- ---------------------------------------------------------------------


caminosR :: (Int,Int) -> [[(Int,Int)]]

caminosR (1,y) = [[(1,z) | z <- [y,y-1..1]]]

caminosR (x,1) = [[(z,1) | z <- [x,x-1..1]]]

caminosR (x,y) = [(x,y):cs | cs <- (caminosR (x-1,y) ++ caminosR (x,y-1))]



-- ---------------------------------------------------------------------
-- Ejercicio 1.2. Definir, por recursión, la función
--    caminosPD :: (Int,Int) -> [[(Int,Int)]]
-- tal que (caminosPD (m,n)) es la lista de los caminos en la retícula de
-- dimensión mxn desde (1,1) hasta (m,n).
-- ---------------------------------------------------------------------


caminosPD :: (Int,Int) -> [[(Int,Int)]]

caminosPD (m,n) = q!(m,n)
    where q = matrix m n f
          f (x,1) = [[(z,1) | z <- [1..x]]]
          f (1,y) = [[(1,z) | z <- [1..y]]]
          f (x,y) = [cs ++ [(x,y)] | cs <- (q!(x-1,y) ++ q!(x,y-1))]


-- ---------------------------------------------------------------------
-- Ejercicio 1.3. Comparar la eficiencia calculando el tiempo necesario
-- para evaluar las siguientes expresiones 
--    length (head (caminosR (8,8)))
--    length (head (caminosR (8,8)))
--    maximum (head (caminosR (2000,2000)))
--    maximum (head (caminosPD (2000,2000)))
-- ---------------------------------------------------------------------

-- La comparación es

-- ---------------------------------------------------------------------
-- Ejercicio 2.1. Definir, usando caminosR, la función
--    nCaminosCR :: (Int,Int) -> Integer
-- tal que (nCaminosCR (m,n)) es el número de caminos en la retícula de
-- dimensión mxn desde (1,1) hasta (m,n). Por ejemplo,
--      nCaminosR (2,3)                        ==  3
--      nCaminosR (3,4)                        ==  10
-- ---------------------------------------------------------------------


nCaminosCR :: (Int,Int) -> Integer

nCaminosCR (m,n) = genericLength (caminosR (m,n))


-- ---------------------------------------------------------------------
-- Ejercicio 2.2. Definir, usando caminosPD, la función
--    nCaminosCPD :: (Int,Int) -> Integer
-- tal que (nCaminosCPD (m,n)) es el número de caminos en la retícula de
-- dimensión mxn desde (1,1) hasta (m,n). Por ejemplo,
-- ---------------------------------------------------------------------


nCaminosCPD :: (Int,Int) -> Integer

nCaminosCPD (m,n) = genericLength (caminosPD (m,n))


-- ---------------------------------------------------------------------
-- Ejercicio 2.3. Definir, por recursión, la función
--    nCaminosR :: (Int,Int) -> Integer
-- tal que (nCaminosR (m,n)) es el número de caminos en la retícula de
-- dimensión mxn desde (1,1) hasta (m,n). Por ejemplo,
-- ---------------------------------------------------------------------


nCaminosR :: (Int,Int) -> Integer

nCaminosR (1, _) = 1

nCaminosR (_, 1) = 1

nCaminosR (x, y) = nCaminosR (x-1, y) + nCaminosR (x, y-1)


-- ---------------------------------------------------------------------
-- Ejercicio 2.4. Definir, por programación dinámica, la función
--    nCaminosPD :: (Int,Int) -> Integer
-- tal que (nCaminosPD (m,n)) es el número de caminos en la retícula de
-- dimensión mxn desde (1,1) hasta (m,n). Por ejemplo,
-- ---------------------------------------------------------------------


nCaminosPD :: (Int,Int) -> Integer

nCaminosPD p = q!p
    where q = nCaminosPD2 p

nCaminosPD2 (m,n) = q
    where q = matrix m n f
          f (1, _) = 1
          f (_, 1) = 1
          f (x, y) = q!(x-1, y) + q!(x, y-1)


-- ---------------------------------------------------------------------
-- Ejercicio 2.5. Los caminos desde (1,1) a (m,n) son las permutaciones
-- con repetición de m-1 veces la A (abajo) y n-1 veces la D
-- (derecha). Por tanto, su  número es 
--    ((m-1)+(n-1))! / (m-1)!*(n-1)!
--  
-- Definir, con la fórmula anterior, la función
--    nCaminosF :: (Int,Int) -> Integer
-- tal que (nCaminosF (m,n)) es el número de caminos en la retícula de
-- dimensión mxn desde (1,1) hasta (m,n). Por ejemplo,
-- ---------------------------------------------------------------------


fact :: Int -> Integer

fact n = product [1.. fromIntegral n]


nCaminosF :: (Int,Int) -> Integer

nCaminosF (m, n) = (fact ((m-1)+(n-1))) `div` (fact ((m-1)*(n-1)))

 
-- ---------------------------------------------------------------------
-- Ejercicio 2.6. La fórmula anterior para el cálculo del número de
-- caminos se puede simplificar.
--  
-- Definir, con la fórmula simplificada, la función
--    nCaminosFS :: (Int,Int) -> Integer
-- tal que (nCaminosFS (m,n)) es el número de caminos en la retícula de
-- dimensión mxn desde (1,1) hasta (m,n). Por ejemplo,
-- ---------------------------------------------------------------------

nCaminosFS :: (Int,Int) -> Integer
nCaminosFS = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 2.7. Comparar la eficiencia calculando el tiempo necesario
-- para evaluar las siguientes expresiones
-- :set +s
--    nCaminosCR  (8,8)
--    nCaminosCPD (8,8)
--    nCaminosCR  (12,12)
--    nCaminosCPD (12,12)
--    nCaminosR   (12,12)
--    nCaminosPD  (12,12)
--    length (show (nCaminosPD (1000,1000)))
--    length (show (nCaminosF  (1000,1000)))
--    length (show (nCaminosFS (1000,1000)))
--    length (show (nCaminosF  (2*10^4,2*10^4)))
--    length (show (nCaminosFS (2*10^4,2*10^4)))
-- ---------------------------------------------------------------------

-- La comparación es


-- ---------------------------------------------------------------------
-- § Apilamiento de barriles.                                         --
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- § Introducción.                                                    --
-- ---------------------------------------------------------------------

-- Un montón de barriles se construye apilando unos encima de otros por
-- capas, de forma que en cada capa todos los barriles están apoyados
-- sobre dos de la capa inferior y todos los barriles de una misma capa
-- están pegados unos a otros. Por ejemplo, los siguientes montones son
-- válidos:  
--       _          _   _                   _
--      / \        / \ / \                 / \
--     _\_/_      _\_/_\_/_   _       _   _\_/_   _
--    / \ / \    / \ / \ / \ / \     / \ / \ / \ / \
--    \_/ \_/    \_/ \_/ \_/ \_/     \_/ \_/ \_/ \_/
--
-- y los siguientes no son válidos:
--     _   _          _       _               _   _
--    / \ / \        / \     / \             / \ / \
--    \_/_\_/_      _\_/_   _\_/_       _   _\_/_\_/
--      / \ / \    / \ / \ / \ / \     / \ / \ / \
--      \_/ \_/    \_/ \_/ \_/ \_/     \_/ \_/ \_/
--
-- Se puede comprobar que el número M(n) de formas distintas de
-- construir montones con n barriles en la base viene dado por la
-- siguiente fórmula: 
--               n-1
--              -------
--               \
--                \
--    M(n) = 1 +   )    (n-j) * M(j)
--                /
--               /
--              -------
--               j = 1
--
-- El objetivo es estudiar la transformación de definiciones recursivas 
-- en otras con programación dinámica y comparar su eficiencia.


-- ---------------------------------------------------------------------
-- § Ejercicios                                                       --
-- ---------------------------------------------------------------------

-- --------------------------------------------------------------------- 
-- Ejercicio 3. Definir, por recursión, la función
--    montonesR :: Integer -> Integer
-- tal que (montonesR n) es el número de formas distintas de construir 
-- montones con n barriles en la base. Por ejemplo,
--    montonesR 1   ==  1
--    montonesR 5   ==  34
--    montonesR 10  ==  4181
--    montonesR 15  ==  514229
--    montonesR 20  ==  63245986
-- ---------------------------------------------------------------------

montonesR :: Integer -> Integer
montonesR = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 4. Definir, por programación dinámica, la función
--    montonesPD :: Integer -> Integer
-- tal que (montonesPD n) es el número de formas distintas de construir 
-- montones con n barriles en la base. Por ejemplo,
--    montonesR 1   ==  1
--    montonesR 5   ==  34
--    montonesR 10  ==  4181
--    montonesR 15  ==  514229
--    montonesR 20  ==  63245986
--    length (show (montonesPD 1000))  ==  418
-- ---------------------------------------------------------------------

montonesPD :: Integer -> Integer
montonesPD n = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 5. Comparar la eficiencia calculando el tiempo necesario
-- para evaluar las siguientes expresiones
--    montonesR  23
--    montonesPD 23
-- ---------------------------------------------------------------------

-- La comparación es

-- ---------------------------------------------------------------------
-- Ejercicio 6. Operando con las ecuaciones de M(n) se observa que
--    M(1) = 1                          = 1
--    M(2) = 1 + M(1)                   = M(1) + M(1)   
--    M(3) = 1 + 2*M(1) + M(2)          = M(2) + (M(1) + M(2))
--    M(4) = 1 + 3*M(1) + 2*M(2) + M(3) = M(3) + (M(1) + M(2) + M(3))
-- En general,
--    M(n) = M(n-1) + (M(1) + ... + M(n-1))
--
-- Unsando la ecuación anterior, definir por recursión la función
--    montonesR2 :: Integer -> Integer
-- tal que (montonesR2 n) es el número de formas distintas de construir 
-- montones con n barriles en la base. Por ejemplo,
--    montonesR2 1   ==  1
--    montonesR2 5   ==  34
--    montonesR2 10  ==  4181
--    montonesR2 15  ==  514229
--    montonesR2 20  ==  63245986
-- ---------------------------------------------------------------------

montonesR2 :: Integer -> Integer
montonesR2 = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 7. Comparar la eficiencia calculando el tiempo necesario
-- para evaluar las siguientes expresiones
--    montonesR  23
--    montonesR2 23
--    length (show (montonesPD 1000))
--    length (show (montonesR2 1000))
-- ---------------------------------------------------------------------

-- La comparación es

-- ---------------------------------------------------------------------
-- Ejercicio 8. Usando la ecuación anterior y programación dinámica,
-- definir la función 
--    montonesPD2 :: Integer -> Integer
-- tal que (montonesPD2 n) es el número de formas distintas de construir 
-- montones con n barriles en la base. Por ejemplo,
--    montonesPD2 1   ==  1
--    montonesPD2 5   ==  34
--    montonesPD2 10  ==  4181
--    montonesPD2 15  ==  514229
--    montonesPD2 20  ==  63245986
-- ---------------------------------------------------------------------

montonesPD2 :: Integer -> Integer
montonesPD2 n = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 9. Comparar la eficiencia calculando el tiempo necesario
-- para evaluar las siguientes expresiones
--    length (show (montonesR2  40000))
--    length (show (montonesPD2 40000))
-- ---------------------------------------------------------------------

-- La comparación es

-- ---------------------------------------------------------------------
-- Ejercicio 10. Definir, usando scanl1, la lista
--    sucMontones :: [Integer]
-- cuyos elementos son los números de formas distintas de construir 
-- montones con n barriles en la base, para n = 1, 2, .... Por ejemplo,
--    take 10 sucMontones  ==  [1,2,5,13,34,89,233,610,1597,4181]
-- ---------------------------------------------------------------------

sucMontones :: [Integer]
sucMontones = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 11. Usando la sucesión anterior, definir la función 
--    montonesS :: Integer -> Integer
-- tal que (montonesS n) es el número de formas distintas de construir 
-- montones con n barriles en la base. Por ejemplo,
--    montonesS 1   ==  1
--    montonesS 5   ==  34
--    montonesS 10  ==  4181
--    montonesS 15  ==  514229
--    montonesS 20  ==  63245986
-- ---------------------------------------------------------------------

montonesS :: Int -> Integer
montonesS n = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 12. Comparar la eficiencia calculando el tiempo necesario
-- para evaluar las siguientes expresiones
--    length (show (montonesR2  40000))
--    length (show (montonesPD2 40000))
--    length (show (montonesS   40000))
-- ---------------------------------------------------------------------

-- La comparación es