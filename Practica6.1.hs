-- PD-Practica 6
-- Funciones de orden superior y definiciones por plegados.
-- Departamento de Ciencias de la Computacion e I.A.
-- Universidad de Sevilla
-- =====================================================================
 
-- ---------------------------------------------------------------------
-- Ejercicio 1. Redefinir por recursion la funcion
--    takeWhile :: (a -> Bool) -> [a] -> [a]
-- tal que (takeWhile p xs) es la lista de los elemento de xs hasta el
-- primero que no cumple la propiedad p. Por ejemplo,
--    takeWhile' (<7) [2,3,9,4,5]  ==  [2,3]
-- ---------------------------------------------------------------------
 
takeWhile' :: (a -> Bool) -> [a] -> [a]

takeWhile' _ [] = []

takeWhile' p (x:xs)
           | p x = x:(takeWhile' p xs)
           | otherwise = []
           
 
-- ---------------------------------------------------------------------
-- Ejercicio 2. Redefinir por recursion la funcion
--    dropWhile :: (a -> Bool) -> [a] -> [a]
-- tal que (dropWhile p xs) es la lista de eliminando los elemento de xs
-- hasta el primero que no cumple la propiedad p. Por ejemplo,
--    dropWhile' (<7) [2,3,9,4,5]  ==  [9,4,5]
-- ---------------------------------------------------------------------
 
dropWhile' :: (a -> Bool) -> [a] -> [a]

dropWhile' _ [] = []

dropWhile' p (x:xs)
           | p x = dropWhile' p xs
           | otherwise = (x:xs)
 
-- ---------------------------------------------------------------------
-- Ejercicio 3.1. Redefinir, usando foldr, la funcion concat. Por ejemplo, 
--    concat' [[1,3],[2,4,6],[1,9]]  ==  [1,3,2,4,6,1,9]
-- ---------------------------------------------------------------------
 
concat' :: [[a]] -> [a]
concat' = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 3.2. Comprobar con QuickCheck que la funciones concat',
-- y concat son equivalentes.
-- ---------------------------------------------------------------------

-- La propiedad es
prop_concat :: [[Int]] -> Bool
prop_concat xss = undefined

-- La comprobacion es

-- ---------------------------------------------------------------------
-- Ejercicio 3.3. Comprobar con QuickCheck que la longitud de 
-- (concat' xss) es la suma de las longitudes de los elementos de xss.
-- ---------------------------------------------------------------------

-- La propiedad es
prop_longConcat :: [[Int]] -> Bool
prop_longConcat xss = undefined

-- La comprobacion es

-- ---------------------------------------------------------------------
-- Ejercicio 4. Definir la funcion segmentos con su signatura
-- tal que (segmentos p xs) es la lista de los segmentos de xs cuyos
-- elementos verifican la propiedad p. Por ejemplo,
--    segmentos even [1,2,0,4,9,6,4,5,7,2]  ==  [[2,0,4],[6,4],[2]]
--    segmentos odd  [1,2,0,4,9,6,4,5,7,2]  ==  [[1],[9],[5,7]]
-- ---------------------------------------------------------------------

segmentos = undefined
 
-- ---------------------------------------------------------------------
-- Ejercicio 5. La funcion 
--    divideMedia :: [Double] -> ([Double],[Double])
-- dada una lista numerica, xs, calcula el par (ys,zs), donde ys 
-- contiene los elementos de xs estrictamente menores que la media, 
-- mientras que zs contiene los elementos de xs estrictamente mayores 
-- que la media. Por ejemplo, 
--    divideMedia [6,7,2,8,6,3,4] ==  ([2.0,3.0,4.0],[6.0,7.0,8.0,6.0])
--    divideMedia [1,2,3]         ==  ([1.0],[3.0])
-- Definir la funcion divideMedia por filtrado y por recursion. 
-- ---------------------------------------------------------------------
 
-- La definicion por filtrado es
divideMediaF :: [Double] -> ([Double],[Double])
divideMediaF = undefined
 
-- La definicion por recursion es
divideMediaR :: [Double] -> ([Double],[Double])
divideMediaR = undefined
 
-- ---------------------------------------------------------------------
-- Ejercicio 6. Definir la funcion
--    agrupa :: Eq a => [[a]] -> [[a]]
-- tal que (agrupa xss) es la lista de las listas obtenidas agrupando
-- los primeros elementos, los segundos, ... Por
-- ejemplo, 
--    agrupa [[1..6],[7..9],[10..20]]  ==  [[1,7,10],[2,8,11],[3,9,12]]
--    agrupa []                        ==  []
-- ---------------------------------------------------------------------
 
agrupa :: Eq a => [[a]] -> [[a]]
agrupa = undefined
 
-- ---------------------------------------------------------------------
-- Ejercicio 7. Se considera la funcion 
--    filtraAplica :: (a -> b) -> (a -> Bool) -> [a] -> [b]
-- tal que (filtraAplica f p xs) es la lista obtenida aplicandole a los
-- elementos de xs que cumplen el predicado p la funcion f. Por ejemplo,
--    filtraAplica (4+) (<3) [1..7]  =>  [5,6]
-- Se pide, definir la funcion
-- 1. por comprension,
-- 2. usando map y filter,
-- 3. por recursion y
-- 4. por plegado (con foldr).
-- ---------------------------------------------------------------------
 
-- La definicion con lista de comprension es
filtraAplica_1 :: (a -> b) -> (a -> Bool) -> [a] -> [b]
filtraAplica_1 = undefined

-- La definicion con map y filter es
filtraAplica_2 :: (a -> b) -> (a -> Bool) -> [a] -> [b]
filtraAplica_2 = undefined

-- La definicion por recursion es
filtraAplica_3 :: (a -> b) -> (a -> Bool) -> [a] -> [b]
filtraAplica_3 = undefined
 
-- La definicion por plegado es
filtraAplica_4 :: (a -> b) -> (a -> Bool) -> [a] -> [b]
filtraAplica_4 = undefined
 
-- ---------------------------------------------------------------------
-- Ejercicio 8. Definir, usando recursion, plegado, y acumulador, la 
-- funcion
--    inversa :: [a] -> [a]
-- tal que (inversa xs) es la inversa de la lista xs. Por ejemplo,
--    inversa [3,5,2,4,7]  ==  [7,4,2,5,3]
-- ---------------------------------------------------------------------

inversaR :: [a] -> [a]
inversaR = undefined

inversaP :: [a] -> [a]
inversaP = undefined

inversaAC :: [a] -> [a]
inversaAC = undefined

inversaPI :: [a] -> [a]
inversaPI = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 9. La funcion de plegado foldl esta definida por
--    foldl :: (a -> b -> a) -> a -> [b] -> a
--    foldl f ys xs = aux ys xs
--        where aux ys []     = ys
--              aux ys (x:xs) = aux (f ys x) xs
-- Definir, mediante plegado con foldl, la funcion
--    inversaP' :: [a] -> [a]
-- tal que (inversaP' xs) es la inversa de la lista xs. Por ejemplo,
--    inversaP' [3,5,2,4,7]  ==  [7,4,2,5,3]
-- ---------------------------------------------------------------------

inversaP' :: [a] -> [a]
inversaP' = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 10. Redefinir, por recursion y plegado la funcion map. 
-- ---------------------------------------------------------------------

mapR :: (a -> b) -> [a] -> [b]
mapR = undefined

mapP :: (a -> b) -> [a] -> [b]
mapP = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 11. Redefinir, usando foldl y foldr la funcion filter. Por
-- ejemplo, 
--    filter (<4) [1,7,3,2]  =>  [1,3,2]
-- ---------------------------------------------------------------------

filterL :: (a -> Bool) -> [a] -> [a]
filterL = undefined

filterR :: (a -> Bool) -> [a] -> [a]
filterR = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 12. Definir, mediante recursion, plegado, acumulador, y 
-- plegado con foldl la funcion
--    sumll :: Num a => [[a]] -> a
-- tal que (sumll xss) es la suma de las sumas de las listas de xss. 
-- Por ejemplo, 
--    sumll [[1,3],[2,5]]  ==  11
-- ---------------------------------------------------------------------

sumllR :: Num a => [[a]] -> a
sumllR = undefined

sumllP :: Num a => [[a]] -> a
sumllP = undefined

sumllA :: Num a => [[a]] -> a
sumllA = undefined

sumllAP :: Num a => [[a]] -> a
sumllAP = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 13. Definir, mediante recursion y plegado, la funcion
--    borra :: Eq a => a -> a -> [a]
-- tal que (borra y xs) es la lista obtenida borrando las ocurrencias de
-- y en xs. Por ejemplo, 
--    borra 5 [2,3,5,6]    ==  [2,3,6]
--    borra 5 [2,3,5,6,5]  ==  [2,3,6]
--    borra 7 [2,3,5,6,5]  ==  [2,3,5,6,5]
-- ---------------------------------------------------------------------

borraR :: Eq a => a -> [a] -> [a]
borraR = undefined

borraP :: Eq a => a -> [a] -> [a]
borraP = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 14. Definir, mediante recursion y plegado la funcion
--    diferencia :: Eq a => [a] -> [a] -> [a]
-- tal que (diferencia xs ys) es la diferencia del conjunto xs e ys; es
-- decir el conjunto de los elementos de xs que no pertenecen a ys. Por
-- ejemplo,  
--    diferencia [2,3,5,6] [5,2,7]  ==  [3,6]
-- ---------------------------------------------------------------------

diferenciaR :: Eq a => [a] -> [a] -> [a]
diferenciaR = undefined

diferenciaP :: Eq a => [a] -> [a] -> [a]
diferenciaP = undefined

-- -------------------------------------------------------------------
-- Ejercicio 15. Definir mediante plegado la funcion 
--    producto :: Num a => [a] -> a
-- tal que (producto xs) es el producto de los elementos de la lista
-- xs. Por ejemplo, 
--    producto [2,1,-3,4,5,-6] == 720
-- ---------------------------------------------------------------------

producto :: Num a => [a] -> a
producto = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 16. Definir mediante plegado la funcion 
--    productoPred :: Num a => (a -> Bool) -> [a] -> a
-- tal que (productoPred p xs) es el producto de los elementos de la
-- lista xs que verifican el predicado p. Por ejemplo, 
--    productoPred even [2,1,-3,4,-5,6] == 48
-- ---------------------------------------------------------------------

productoPred :: Num a => (a -> Bool) -> [a] -> a
productoPred = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 17.1. Definir, mediante recursion, la funcion
--    maximumR :: Ord a => [a] -> a
-- tal que (maximumR xs) es el maximo de la lista xs. Por ejemplo,
--    maximumR [3,7,2,5]                  ==  7
--    maximumR ["todo","es","falso"]      ==  "todo"
--    maximumR ["menos","alguna","cosa"]  ==  "menos"
-- 
-- Nota: La funcion maximumR es equivalente a la predefinida maximum.
-- ---------------------------------------------------------------------

maximumR :: Ord a => [a] -> a
maximumR = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 17.2. La funcion de plegado foldr1 esta definida por 
--    foldr1 :: (a -> a -> a) -> [a] -> a
--    foldr1 _ [x]    =  x
--    foldr1 f (x:xs) =  f x (foldr1 f xs)
-- 
-- Definir, mediante plegado con foldr1, la funcion
--    maximumP :: Ord a => [a] -> a
-- tal que (maximumR xs) es el maximo de la lista xs. Por ejemplo,
--    maximumP [3,7,2,5]                  ==  7
--    maximumP ["todo","es","falso"]      ==  "todo"
--    maximumP ["menos","alguna","cosa"]  ==  "menos"
-- 
-- Nota: La funcion maximumP es equivalente a la predefinida maximum.
-- ---------------------------------------------------------------------

maximumP :: Ord a => [a] -> a
maximumP = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 18. Definir, por plegado, la funcion
-- sumaDivP :: Int -> [Int] -> Int
-- tal que (SumaDivP x xs) es la suma de los cuadrados de los
-- elementos de xs que son divisibles por x. Por ejemplo,
-- sumaDivP 3 [1..7] == 45
-- sumaDivP 2 [1..7] == 56
-- ---------------------------------------------------------------------

sumaDivP :: Int -> [Int] -> Int
sumaDivP = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 19.1. Definir, con la funcion all, la funcion
--    relacionadosA :: (a -> a -> Bool) -> [a] -> Bool
-- tal que (relacionadosA r xs) se verifica si para todo par (x,y) de
-- elementos consecutivos de xs se cumple la relacion r. Por ejemplo,
--    relacionadosA (<) [2,3,7,9]                ==  True
--    relacionadosA (<) [2,3,1,9]                ==  False
-- ---------------------------------------------------------------------

relacionadosA :: (a -> a -> Bool) -> [a] -> Bool
relacionadosA = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 19.2. Definir, con la funcion foldr, la funcion
--    relacionadosP :: (a -> a -> Bool) -> [a] -> Bool
-- tal que (relacionadosP r xs) se verifica si para todo par (x,y) de
-- elementos consecutivos de xs se cumple la relacion r. Por ejemplo,
--    relacionadosP (<) [2,3,7,9]                ==  True
--    relacionadosP (<) [2,3,1,9]                ==  False
-- ---------------------------------------------------------------------

relacionadosP :: (a -> a -> Bool) -> [a] -> Bool
relacionadosP = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 19.3.
-- Una lista se dira muy creciente si cada elemento es mayor estricto
-- que el triple del siguiente. 
-- Empleando tan solo (relacionadosA p xs), define el predicado 
--          muyCreciente :: [Integer] -> Bool
-- tal que (muyCreciente xs) se verifica si xs es muy creciente. Por
-- ejemplo:
-- muyCreciente [1,5,23,115]  == True
-- muyCreciente [1,2,7,14]    == False
-- muyCreciente [7]           == True
-- muyCreciente []            == True
-- ---------------------------------------------------------------------

muyCreciente :: [Integer] -> Bool
muyCreciente xs = undefined
