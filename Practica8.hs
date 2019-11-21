-- PD-Práctica 8  (Tomado de una relación de ejercicios de I1M)
-- Combinatoria.
-- Departamento de Ciencias de la Computación e I.A.
-- Universidad de Sevilla
-- =====================================================================
-- ---------------------------------------------------------------------
-- Introducción                                                       --
-- ---------------------------------------------------------------------
-- El objetivo de esta relación es estudiar la generación y el número de
-- las principales operaciones de la combinatoria. En concreto, se
-- estudia 
--    * Subconjuntos.
--    * Permutaciones.
--    * Combinaciones sin repetición.
--    * Combinaciones con repetición
--    * Variaciones sin repetición.
--    * Variaciones con repetición.
-- ---------------------------------------------------------------------
-- Importación de librerías                                           --
-- ---------------------------------------------------------------------


import Data.List


-- ---------------------------------------------------------------------
-- § Subconjuntos
-- ---------------------------------------------------------------------
-- ---------------------------------------------------------------------
-- Ejercicio 1.1 Definir, por recursión, la función 
--    subconjuntos :: [a] -> [[a]]
-- tal que (subconjuntos xs) es la lista de las subconjuntos de la lista
-- xs. Por ejemplo, 
--    ghci> subconjuntos [2,3,4]
--    [[2,3,4],[2,3],[2,4],[2],[3,4],[3],[4],[]]
--    ghci> subconjuntos [1,2,3,4]
--    [[1,2,3,4],[1,2,3],[1,2,4],[1,2],[1,3,4],[1,3],[1,4],[1],
--       [2,3,4],  [2,3],  [2,4],  [2],  [3,4],  [3],  [4], []]
-- --------------------------------------------------------------------
-- Nota: Se corresponde con 'subsequences' (definido en Data.List) 
-- --------------------------------------------------------------------


subconjuntos :: [a] -> [[a]]

subconjuntos [] = [[]]

subconjuntos (x:xs) = (map (x:) yss) ++ yss
    where yss = subconjuntos xs


-- ---------------------------------------------------------------------
-- Ejercicio 1.2 Definir, usando subconjuntos, la función
--    numSubconjuntos :: Integer -> Integer
-- tal que (numSubconjuntos n) es el número de subconjuntos de un
-- conjunto con n elementos. Por ejemplo,
--    numSubconjuntos 3  ==  8
--    numSubconjuntos 4  ==  16
-- ---------------------------------------------------------------------
-- Nota: Usar genericLength (definido en Data.List)
-- ---------------------------------------------------------------------

numSubconjuntos :: Integer -> Integer
numSubconjuntos = undefined

-- ---------------------------------------------------------------------
-- El número de subconjuntos de un conjunto de n elementos es 
-- igual a  ........
-- -------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 1.3 Definir el predicado
--    contenido :: Eq a => [a] -> [a] -> Bool
-- tal que (contenido xs ys) se verifica si xs es un subconjunto de
-- ys. Por ejemplo,
--    contenido [1,3,2,3] [1,2,3]  ==  True
--    contenido [1,3,4,3] [1,2,3]  ==  False
-- ---------------------------------------------------------------------
 
contenido :: Eq a => [a] -> [a] -> Bool
contenido = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 1.4 Definir el predicado
--       esSubsucesion :: Eq a => [a] -> [a] -> Bool
-- tal que (esSubsucesion xs ys) se verifica si xs es una subsucesión de
-- ys. Por ejemplo:
--  esSubsucesion [2,4] [1,2,3,4,5] == True
--  esSubsucesion [1,3,2] [1,2,3,4,5] == False
-- ---------------------------------------------------------------------
-- Nota: Se corresponde con  'isSubsequenceOf' (definido en Data.List)
-- --------------------------------------------------------------------

esSubsucesion :: Eq a => [a] -> [a] -> Bool
esSubsucesion = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 1.5 Definir el predicado
--       esSublista :: Eq a => [a] -> [a] -> Bool
-- tal que (esSublista xs ys) se verifica si xs es una sublista de
-- ys. Por ejemplo:
--  esSublista [1,2] [1,2,3,4,5] == True
--  esSublista [2,3,4] [1,2,3,4,5] == True
--  esSublista [2,4] [1,2,3,4,5] == False
-- ---------------------------------------------------------------------
-- Nota: Se corresponde con  'isInfixOf' (definido en Data.List)
-- Otros predefinidos relacionados: 'isPrefixOf' 'isSuffixOf' 
-- --------------------------------------------------------------------


esSublista :: Eq a => [a] -> [a] -> Bool

esSublista [] _ = True

esSublista _ [] = False

esSublista xs ys = esPrefijo xs ys || esSublista xs (tail ys)
    where esPrefijo xs ys = elem xs $ inits ys


-- ---------------------------------------------------------------------
-- § Permutaciones
-- ---------------------------------------------------------------------
-- ---------------------------------------------------------------------
-- Ejercicio 2.1 Definir, por recursión, la función
--    intercala :: a -> [a] -> [[a]]
-- tal que (intercala x ys) es la lista de las listas obtenidas
-- intercalando x entre los elementos de ys. Por ejemplo,
--    intercala 1 [2,3]  ==  [[1,2,3],[2,1,3],[2,3,1]]
-- ---------------------------------------------------------------------


intercala :: a -> [a] -> [[a]]

intercala x [] = [[x]]

intercala x (y:ys) = (x:y:ys) : map (y:) (intercala x ys)


-- ---------------------------------------------------------------------
-- Ejercicio 2.2 Definir, por recursión, la función 
--    permutaciones :: [a] -> [[a]]  
-- tal que (permutaciones xs) es la lista de las permutaciones de la
-- lista xs. Por ejemplo,
--    permutaciones "bc"   ==  ["bc","cb"]
--    permutaciones "abc"  ==  ["abc","bac","bca","acb","cab","cba"]
-- Nota: puede ser útil la función anterior y concatMap
-- ---------------------------------------------------------------------

permutaciones :: [a] -> [[a]]
permutaciones = undefined

-- ---------------------------------------------------------------------
-- Nota: Se corresponde con  'permutations' (definido en Data.List)
-- ---------------------------------------------------------------------
-- Ejercicio 2.3 Definir, usando permutaciones, la función
--    numPermutaciones :: Integer -> Integer
-- tal que (numPermutaciones n) es el número de permutaciones de un
-- conjunto con n elementos. Por ejemplo,
--    numPermutaciones 3  ==  6
--    numPermutaciones 4  ==  24
-- ---------------------------------------------------------------------

numPermutaciones :: Integer -> Integer
numPermutaciones = undefined

-- ---------------------------------------------------------------------
-- El número de permutaciones de un conjunto de n elementos es 
-- igual a  ........
-- --------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- § Combinaciones  (el orden importa)        
-- ---------------------------------------------------------------------  
-- ---------------------------------------------------------------------
-- Ejercicio 3.1 Definir la función 
--    combinaciones :: Integer -> [a] -> [[a]]
-- tal que (combinaciones k xs) es la lista de las combinaciones de
-- orden k de los elementos de la lista xs. Por ejemplo,
--    ghci> combinaciones 2 "bcde"
--    ["bc","bd","be","cd","ce","de"]
--    ghci> combinaciones 3 "bcde"
--    ["bcd","bce","bde","cde"]
--    ghci> combinaciones 3 "abcde"
--    ["abc","abd","abe","acd","ace","ade","bcd","bce","bde","cde"]
-- ---------------------------------------------------------------------

combinaciones :: Integer -> [a] -> [[a]]
combinaciones = undefined


-- ---------------------------------------------------------------------
-- Nota: Se corresponde con  'choose' (definido en Math.Combinat.Sets)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 3.2 Definir, usando combinaciones, la función
--    numCombinaciones :: Integer -> Integer -> Integer
-- tal que (numCombinaciones n k) es el número de combinaciones de
-- orden k de un conjunto con n elementos. Por ejemplo,
--    numCombinaciones 4 2  ==  6
--    numCombinaciones 4 3  ==  4
-- ---------------------------------------------------------------------

numCombinaciones :: Integer -> Integer -> Integer
numCombinaciones = undefined

-- ---------------------------------------------------------------------
-- El número de combinaciones de orden k de un conjunto con n elementos 
-- es igual a .....
-- ----------------------------------------------------------------------


-- ---------------------------------------------------------------------
-- § Combinaciones con repetición
-- ---------------------------------------------------------------------
-- ---------------------------------------------------------------------
-- Ejercicio 4.1 Definir la función
--    combinacionesR :: Integer -> [a] -> [[a]]
-- tal que (combinacionesR k xs) es la lista de las combinaciones de orden
-- k de los elementos de xs con repeticiones. Por ejemplo,
--    ghci> combinacionesR 2 "abc"
--    ["aa","ab","ac","bb","bc","cc"]
--    ghci> combinacionesR 3 "bc"
--    ["bbb","bbc","bcc","ccc"]
--    ghci> combinacionesR 3 "abc"
--    ["aaa","aab","aac","abb","abc","acc","bbb","bbc","bcc","ccc"]
-- ---------------------------------------------------------------------

combinacionesR :: Integer -> [a] -> [[a]]
combinacionesR = undefined

-- ---------------------------------------------------------------------
-- Nota: Se corresponde con  'combine' (definido en Math.Combinat.Sets)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 4.2. Definir, usando combinacionesR, la función
--    numCombinacionesR :: Integer -> Integer -> Integer
-- tal que (numCombinacionesR n k) es el número de combinaciones con
-- repetición de orden k de un conjunto con n elementos. Por ejemplo,
--    numCombinacionesR 3 2  ==  6
--    numCombinacionesR 2 3  ==  4
-- ---------------------------------------------------------------------

numCombinacionesR :: Integer -> Integer -> Integer
numCombinacionesR  = undefined

-- ----------------------------------------------------------------------
-- El número de combinaciones con repetición de orden k de un conjunto 
-- con n elementos es igual a ....
-------------------------------------------------------------------------


-- ---------------------------------------------------------------------
-- § Variaciones  (el orden no importa)
-- ---------------------------------------------------------------------
-- ---------------------------------------------------------------------
-- Ejercicio 5.1 Definir la función 
--    variaciones :: Integer -> [a] -> [[a]]
-- tal que (variaciones n xs) es la lista de las variaciones n-arias
-- de la lista xs. Por ejemplo,
--    variaciones 2 "abc"  ==  ["ab","ba","ac","ca","bc","cb"]
-- ---------------------------------------------------------------------

variaciones :: Integer -> [a] -> [[a]]
variaciones = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 5.2. Definir, usando variaciones, la función
--    numVariaciones :: Integer -> Integer -> Integer
-- tal que (numVariaciones n k) es el número de variaciones de orden
-- k de un conjunto con n elementos. Por ejemplo,
--    numVariaciones 4 2  ==  12
--    numVariaciones 4 3  ==  24
-- ---------------------------------------------------------------------

numVariaciones :: Integer -> Integer -> Integer
numVariaciones  = undefined

-- ---------------------------------------------------------------------
-- El número de variaciones de orden k de un conjunto con n elementos es 
-- igual a ......
-- ----------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- § Variaciones con repetición
-- ---------------------------------------------------------------------
-- ---------------------------------------------------------------------
-- Ejercicio 6.1. Definir la función
--    variacionesR :: Integer -> [a] -> [[a]]
-- tal que (variacionesR k xs) es la lista de las variaciones de orden
-- k de los elementos de xs con repeticiones. Por ejemplo,
--    ghci> variacionesR 1 "ab"
--    ["a","b"]
--    ghci> variacionesR 2 "ab"
--    ["aa","ab","ba","bb"]
--    ghci> variacionesR 3 "ab"
--    ["aaa","aab","aba","abb","baa","bab","bba","bbb"]
-- ---------------------------------------------------------------------

variacionesR :: Integer -> [a] -> [[a]]
variacionesR = undefined


-- ---------------------------------------------------------------------
-- Nota: Se corresponde con  'tuplesFromList' (definido en Math.Combinat.Sets)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 6.2. Definir, usando variacionesR, la función
--    numVariacionesR :: Integer -> Integer -> Integer
-- tal que (numVariacionesR n k) es el número de variaciones con
-- repetición de orden k de un conjunto con n elementos. Por ejemplo,
--    numVariacionesR 3 2  ==  9
--    numVariacionesR 2 3  ==  8
-- ---------------------------------------------------------------------

numVariacionesR :: Integer -> Integer -> Integer
numVariacionesR  = undefined

-- -----------------------------------------------------------------------
--  El número de variaciones con repetición de orden k de un conjunto 
--  con n elementos es igual a ...
-- -----------------------------------------------------------------