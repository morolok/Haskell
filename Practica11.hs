-- PD 2019-20: Práctica tomada de la relación 21 de I1M.
-- Vectores y matrices con las librerías.
-- Departamento de Ciencias de la Computación e I.A.
-- Universidad de Sevilla
-- =====================================================================

-- ---------------------------------------------------------------------
-- Introducción                                                       --
-- ---------------------------------------------------------------------

--
-- El manual, con ejemplos, de la librería de vectores de encuentra en
-- http://bit.ly/17Oq893 y el de matrices en http://bit.ly/17Oq9K5
--
-- Para instalar las librerías basta escribir en la consola
--    cabal update
--    cabal install vector
--    cabal install matrix
 
-- ---------------------------------------------------------------------
-- Importación de librerías                                           --
-- ---------------------------------------------------------------------


import qualified Data.Vector as V

import Data.Matrix

import Data.Ratio

import Data.Maybe


-- ---------------------------------------------------------------------
-- Tipos de los vectores y de las matrices                            --
-- ---------------------------------------------------------------------

-- Los vectores (con elementos de tipo a son del tipo (V.Vector a).
-- Los matrices (con elementos de tipo a son del tipo (Matrix a).

-- ---------------------------------------------------------------------
-- Operaciones básicas con matrices                                   --
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 1. Definir la función
--    listaVector :: Num a => [a] -> V.Vector a
-- tal que (listaVector xs) es el vector correspondiente a la lista
-- xs. Por ejemplo, 
--    ghci> listaVector [3,2,5]
--    fromList [3,2,5]
-- ---------------------------------------------------------------------


listaVector :: Num a => [a] -> V.Vector a

listaVector xs = V.fromList xs


-- ---------------------------------------------------------------------
-- Ejercicio 2. Definir la función
--    listaMatriz :: Num a => [[a]] -> Matrix a
-- tal que (listaMatriz xss) es la matriz cuyas filas son los elementos
-- de xss. Por ejemplo,
--    ghci> listaMatriz [[1,3,5],[2,4,7]]
--    ( 1 3 5 )
--    ( 2 4 7 )
-- ---------------------------------------------------------------------


listaMatriz :: Num a => [[a]] -> Matrix a

listaMatriz xss = fromLists xss


-- ---------------------------------------------------------------------
-- Ejercicio 3. Definir la función
--    numFilas :: Num a => Matrix a -> Int
-- tal que (numFilas m) es el número de filas de la matriz m. Por
-- ejemplo,
--    numFilas (listaMatriz [[1,3,5],[2,4,7]])  ==  2
-- ---------------------------------------------------------------------


numFilas :: Num a => Matrix a -> Int

numFilas m = nrows m


-- ---------------------------------------------------------------------
-- Ejercicio 4. Definir la función
--    numColumnas :: Num a => Matrix a -> Int
-- tal que (numColumnas m) es el número de columnas de la matriz
-- m. Por ejemplo,
--    numColumnas (listaMatriz [[1,3,5],[2,4,7]])  ==  3
-- ---------------------------------------------------------------------


numColumnas :: Num a => Matrix a -> Int

numColumnas m = ncols m


-- ---------------------------------------------------------------------
-- Ejercicio 5. Definir la función
--    dimension :: Num a => Matrix a -> (Int,Int)
-- tal que (dimension m) es la dimensión de la matriz m. Por ejemplo, 
--    dimension (listaMatriz [[1,3,5],[2,4,7]])  ==  (2,3)
-- ---------------------------------------------------------------------

dimension :: Num a => Matrix a -> (Int,Int)
dimension p = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 7. Definir la función
--    matrizLista :: Num a => Matrix a -> [[a]]
-- tal que (matrizLista x) es la lista de las filas de la matriz x. Por
-- ejemplo, 
--    ghci> let m = listaMatriz [[5,1,0],[3,2,6]]
--    ghci> m
--    ( 5 1 0 )
--    ( 3 2 6 )
--    ghci> matrizLista m
--    [[5,1,0],[3,2,6]]
-- ---------------------------------------------------------------------


matrizLista :: Num a => Matrix a -> [[a]]

matrizLista p = toLists p


-- ---------------------------------------------------------------------
-- Ejercicio 8. Definir la función
--    vectorLista :: Num a => V.Vector a -> [a]
-- tal que (vectorLista x) es la lista de los elementos del vector
-- v. Por ejemplo, 
--    ghci> let v = listaVector [3,2,5]
--    ghci> v
--    fromList [3,2,5]
--    ghci> vectorLista v
--    [3,2,5]
-- ---------------------------------------------------------------------


vectorLista :: Num a => V.Vector a -> [a]

vectorLista x = V.toList x


-- ---------------------------------------------------------------------
-- Suma de matrices                                                   --
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 9. Definir la función
--    sumaMatrices:: Num a => Matrix a -> Matrix a -> Matrix a
-- tal que (sumaMatrices x y) es la suma de las matrices x e y. Por
-- ejemplo, 
--    ghci> let m1 = listaMatriz [[5,1,0],[3,2,6]]
--    ghci> let m2 = listaMatriz [[4,6,3],[1,5,2]]
--    ghci> sumaMatrices m1 m2
--    ( 9 7 3 )
--    ( 4 7 8 )
-- ---------------------------------------------------------------------

sumaMatrices:: Num a => Matrix a -> Matrix a -> Matrix a
sumaMatrices p q = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 10. Definir la función
--    filaMat :: Num a => Int -> Matrix a -> V.Vector a
-- tal que (filaMat i p) es el vector correspondiente a la fila i-ésima
-- de la matriz p. Por ejemplo,
--    ghci> let p = listaMatriz [[5,1,0],[3,2,6],[4,5,7]]
--    ghci> filaMat 2 p
--    fromList [3,2,6]
--    ghci> vectorLista (filaMat 2 p)
--    [3,2,6]
-- ---------------------------------------------------------------------

filaMat :: Num a => Int -> Matrix a -> V.Vector a
filaMat = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 11. Definir la función
--    columnaMat :: Num a => Int -> Matrix a -> V.Vector a
-- tal que (columnaMat j p) es el vector correspondiente a la columna
-- j-ésima de la matriz p. Por ejemplo,
--    ghci> let p = listaMatriz [[5,1,0],[3,2,6],[4,5,7]]
--    ghci> columnaMat 2 p
--    fromList [1,2,5]
--    ghci> vectorLista (columnaMat 2 p)
--    [1,2,5]
-- ---------------------------------------------------------------------

columnaMat :: Num a => Int -> Matrix a -> V.Vector a
columnaMat = undefined

-- ---------------------------------------------------------------------
-- Producto de matrices                                               --
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 12. Definir la función
--    prodEscalar :: Num a => V.Vector a -> V.Vector a -> a
-- tal que (prodEscalar v1 v2) es el producto escalar de los vectores v1
-- y v2. Por ejemplo,
--    ghci> let v = listaVector [3,1,10]
--    ghci> prodEscalar v v
--    110
-- ---------------------------------------------------------------------


prodEscalar :: Num a => V.Vector a -> V.Vector a -> a

prodEscalar v1 v2 = sum [x*y | (x,y) <- zip (vectorLista v1) (vectorLista v2)]


-- ---------------------------------------------------------------------
-- Ejercicio 13. Definir la función
--    prodMatrices:: Num a => Matrix a -> Matrix a -> Matrix a
-- tal que (prodMatrices p q) es el producto de las matrices p y q. Por
-- ejemplo, 
--    ghci> let p = listaMatriz [[3,1],[2,4]]
--    ghci> prodMatrices p p
--    ( 11  7 )
--    ( 14 18 )
--    ghci> let q = listaMatriz [[7],[5]]
--    ghci> prodMatrices p q
--    ( 26 )
--    ( 34 )
-- ---------------------------------------------------------------------

prodMatrices:: Num a => Matrix a -> Matrix a -> Matrix a
prodMatrices p q = undefined

-- ---------------------------------------------------------------------
-- Traspuestas y simétricas                                           --
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 14. Definir la función
--    traspuesta :: Num a => Matrix a -> Matrix a
-- tal que (traspuesta p) es la traspuesta de la matriz p. Por ejemplo,
--    ghci> let p = listaMatriz [[5,1,0],[3,2,6]]
--    ghci> traspuesta p
--    ( 5 3 )
--    ( 1 2 )
--    ( 0 6 )
-- ---------------------------------------------------------------------

traspuesta :: Num a => Matrix a -> Matrix a
traspuesta = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 15. Definir la función
--    esCuadrada :: Num a => Matrix a -> Bool
-- tal que (esCuadrada p) se verifica si la matriz p es cuadrada. Por
-- ejemplo, 
--    ghci> let p = listaMatriz [[5,1,0],[3,2,6]]
--    ghci> esCuadrada p
--    False
--    ghci> let q = listaMatriz [[5,1],[3,2]]
--    ghci> esCuadrada q
--    True
-- ---------------------------------------------------------------------

esCuadrada :: Num a => Matrix a -> Bool
esCuadrada p = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 16. Definir la función
--    esSimetrica :: (Num a, Eq a) => Matrix a -> Bool
-- tal que (esSimetrica p) se verifica si la matriz p es simétrica. Por
-- ejemplo, 
--    ghci> let p = listaMatriz [[5,1,3],[1,4,7],[3,7,2]]
--    ghci> esSimetrica p
--    True
--    ghci> let q = listaMatriz [[5,1,3],[1,4,7],[3,4,2]]
--    ghci> esSimetrica q
--    False
-- ---------------------------------------------------------------------    

esSimetrica :: (Num a, Eq a) => Matrix a -> Bool
esSimetrica x = undefined

-- ---------------------------------------------------------------------
-- Diagonales de una matriz                                           --
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 17. Definir la función
--    diagonalPral :: Num a => Matrix a -> V.Vector a
-- tal que (diagonalPral p) es la diagonal principal de la matriz p. Por
-- ejemplo, 
--    ghci> let p = listaMatriz [[5,1,0],[3,2,6]]
--    ghci> diagonalPral p
--    fromList [5,2]
-- ---------------------------------------------------------------------

diagonalPral :: Num a => Matrix a -> V.Vector a
diagonalPral = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 18. Definir la función
--    diagonalSec :: Num a => Matrix a -> V.Vector a
-- tal que (diagonalSec p) es la diagonal secundaria de la matriz p. Por
-- ejemplo, 
--    ghci> let p = listaMatriz [[5,1,0],[3,2,6]]
--    ghci> diagonalSec p
--    fromList [1,3]
--    ghci> let q = traspuesta p
--    ghci> matrizLista q
--    [[5,3],[1,2],[0,6]]
--    ghci> diagonalSec q
--    fromList [1,2]
-- ---------------------------------------------------------------------

diagonalSec :: Num a => Matrix a -> V.Vector a
diagonalSec p = undefined

-- ---------------------------------------------------------------------
-- Submatrices                                                        --
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 19. Definir la función
--    submatriz :: Num a => Int -> Int -> Matrix a -> Matrix a
-- tal que (submatriz i j p) es la matriz obtenida a partir de la p
-- eliminando la fila i y la columna j. Por ejemplo,
--    ghci> let p = listaMatriz [[5,1,0],[3,2,6],[4,6,9]]
--    ghci> submatriz 2 3 p
--    ( 5 1 )
--    ( 4 6 )
-- ---------------------------------------------------------------------

submatriz :: Num a => Int -> Int -> Matrix a -> Matrix a
submatriz = undefined

-- ---------------------------------------------------------------------
-- Transformaciones elementales                                       --
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 20. Definir la función
--    intercambiaFilas :: Num a => Int -> Int -> Matrix a -> Matrix a
-- tal que (intercambiaFilas k l p) es la matriz obtenida intercambiando
-- las filas k y l de la matriz p. Por ejemplo, 
--    ghci> let p = listaMatriz [[5,1,0],[3,2,6],[4,6,9]]
--    ghci> intercambiaFilas 1 3 p
--    ( 4 6 9 )
--    ( 3 2 6 )
--    ( 5 1 0 )
-- ---------------------------------------------------------------------


intercambiaFilas :: Num a => Int -> Int -> Matrix a -> Matrix a

intercambiaFilas k l p = switchRows k l p


-- ---------------------------------------------------------------------
-- Ejercicio 21. Definir la función
--    intercambiaColumnas :: Num a => Int -> Int -> Matrix a -> Matrix a
-- tal que (intercambiaColumnas k l p) es la matriz obtenida
-- intercambiando las columnas k y l de la matriz p. Por ejemplo, 
--    ghci> let p = listaMatriz [[5,1,0],[3,2,6],[4,6,9]]
--    ghci> intercambiaColumnas 1 3 p
--    ( 0 1 5 )
--    ( 6 2 3 )
--    ( 9 6 4 )
-- ---------------------------------------------------------------------


intercambiaColumnas :: Num a => Int -> Int -> Matrix a -> Matrix a

intercambiaColumnas k l p = switchCols k l p


-- ---------------------------------------------------------------------
-- Ejercicio 22. Definir la función
--    multFilaPor :: Num a => Int -> a -> Matrix a -> Matrix a
-- tal que (multFilaPor k x p) es la matriz obtenida multiplicando la
-- fila k de la matriz p por el número x. Por ejemplo,
--    ghci> let p = listaMatriz [[5,1,0],[3,2,6],[4,6,9]]
--    ghci> multFilaPor 2 3 p
--    (  5  1  0 )
--    (  9  6 18 )
--    (  4  6  9 )
-- ---------------------------------------------------------------------


multFilaPor :: Num a => Int -> a -> Matrix a -> Matrix a

multFilaPor k x p = matrix (nrows p) (ncols p) f
    where f (i,j) | i == k = x * (p!(i,j))
                  | otherwise = p!(i,j)


multFilaPor2 k x p = scaleRow x k p


-- ---------------------------------------------------------------------
-- Ejercicio 23. Definir la función
--    sumaFilaFila :: Num a => Int -> Int -> Matrix a -> Matrix a
-- tal que (sumaFilaFila k l p) es la matriz obtenida sumando la fila l
-- a la fila k de la matriz p. Por ejemplo,
--    ghci> let p = listaMatriz [[5,1,0],[3,2,6],[4,6,9]]
--    ghci> sumaFilaFila 2 3 p
--    (  5  1  0 )
--    (  7  8 15 )
--    (  4  6  9 )
-- ---------------------------------------------------------------------


sumaFilaFila :: Num a => Int -> Int -> Matrix a -> Matrix a

sumaFilaFila k l p = combineRows k (1) l p


-- ---------------------------------------------------------------------
-- Ejercicio 24. Definir la función
--    sumaFilaPor :: Num a => Int -> Int -> a -> Matrix a -> Matrix a
-- tal que (sumaFilaPor k l x p) es la matriz obtenida sumando a la fila
-- k de la matriz p la fila l multiplicada por x. Por ejemplo,
--    ghci> let p = listaMatriz [[5,1,0],[3,2,6],[4,6,9]]
--    ghci> sumaFilaPor 2 3 10 p
--    (  5  1  0 )
--    ( 43 62 96 )
--    (  4  6  9 )
-- ---------------------------------------------------------------------

sumaFilaPor :: Num a => Int -> Int -> a -> Matrix a -> Matrix a
sumaFilaPor k l x p = undefined

-- ---------------------------------------------------------------------
-- Triangularización de matrices                                      --
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 25. Definir la función
--    buscaIndiceDesde :: (Num a, Eq a) => 
--                        Matrix a -> Int -> Int -> Maybe Int
-- tal que (buscaIndiceDesde p j i) es el menor índice k, mayor o igual
-- que i, tal que el elemento de la matriz p en la posición (k,j) es no
-- nulo. Por ejemplo,
--    ghci> let p = listaMatriz [[5,1,0],[3,2,6],[4,6,9]]
--    ghci> buscaIndiceDesde p 3 2
--    Just 2
--    ghci> let q = listaMatriz [[5,1,1],[3,2,0],[4,6,0]]
--    ghci> buscaIndiceDesde q 3 2
--    Nothing
-- ---------------------------------------------------------------------


buscaIndiceDesde :: (Num a, Eq a) => Matrix a -> Int -> Int -> Maybe Int

buscaIndiceDesde p j i
    | i > nrows p = Nothing
    | p!(i,j) /= 0 = Just i
    | otherwise = buscaIndiceDesde p j (i+1)


-- ---------------------------------------------------------------------
-- Ejercicio 26. Definir la función
--    buscaPivoteDesde :: (Num a, Eq a) => 
--                        Matrix a -> Int -> Int -> Maybe a
-- tal que (buscaPivoteDesde p j i) es el elemento de la matriz p en la
-- posición (k,j) donde k es (buscaIndiceDesde p j i). Por ejemplo,
--    ghci> let p = listaMatriz [[5,1,0],[3,2,6],[4,6,9]]
--    ghci> buscaPivoteDesde p 3 2
--    Just 6
--    ghci> let q = listaMatriz [[5,1,1],[3,2,0],[4,6,0]]
--    ghci> buscaPivoteDesde q 3 2
--    Nothing
-- ---------------------------------------------------------------------


buscaPivoteDesde :: (Num a, Eq a) => Matrix a -> Int -> Int -> Maybe a

buscaPivoteDesde p j i
    | isNothing k2 = Nothing
    | otherwise = Just (p!(k,j))
    where k2 = buscaIndiceDesde p j i
          k = fromJust k2


-- ---------------------------------------------------------------------
-- Ejercicio 27. Definir la función
--    anuladaColumnaDesde :: (Num a, Eq a) => 
--                           Int -> Int -> Matrix a -> Bool
-- tal que (anuladaColumnaDesde j i p) se verifica si todos los
-- elementos de la columna j de la matriz p desde i+1 en adelante son
-- nulos. Por ejemplo,
--    ghci> let q = listaMatriz [[5,1,1],[3,2,0],[4,6,0]]
--    ghci> anuladaColumnaDesde q 3 2
--    True
--    ghci> let p = listaMatriz [[5,1,0],[3,2,6],[4,6,9]]
--    ghci> anuladaColumnaDesde p 3 2
--    False
-- ---------------------------------------------------------------------


anuladaColumnaDesde :: (Num a, Eq a) => Matrix a -> Int -> Int -> Bool

anuladaColumnaDesde p j i = isNothing $ buscaIndiceDesde p j (i+1)


-- ---------------------------------------------------------------------
-- Ejercicio 28. Definir la función
--    anulaEltoColumnaDesde :: (Fractional a, Eq a) => 
--                             Matrix a -> Int -> Int -> Matrix a
-- tal que (anulaEltoColumnaDesde p j i) es la matriz obtenida a partir
-- de p anulando el primer elemento de la columna j por debajo de la
-- fila i usando el elemento de la posición (i,j). Por ejemplo,
--    ghci> let p = listaMatriz [[2,3,1],[5,0,5],[8,6,9]] :: Matrix Double
--    ghci> matrizLista (anulaEltoColumnaDesde p 2 1)
--    [[2.0,3.0,1.0],[5.0,0.0,5.0],[4.0,0.0,7.0]]
-- ---------------------------------------------------------------------


anulaEltoColumnaDesde :: (Fractional a, Eq a) => Matrix a -> Int -> Int -> Matrix a

anulaEltoColumnaDesde p j i
    | isNothing k2 = p
    | otherwise = sumaFilaPor k i (-p!(k,j)/p!(i,j)) p
    where k2 = buscaIndiceDesde p j (i+1)
          k = fromJust k2


-- ---------------------------------------------------------------------
-- Ejercicio 29. Definir la función
--    anulaColumnaDesde :: (Fractional a, Eq a) => 
--                         Matrix a -> Int -> Int -> Matrix a
-- tal que (anulaColumnaDesde p j i) es la matriz obtenida anulando
-- todos los elementos de la columna j de la matriz p por debajo del la
-- posición (i,j) (se supone que el elemnto p_(i,j) es no nulo). Por
-- ejemplo, 
--    ghci> let p = listaMatriz [[2,2,1],[5,4,5],[10,8,9]] :: Matrix Double
--    ghci> matrizLista (anulaColumnaDesde p 2 1)
--    [[2.0,2.0,1.0],[1.0,0.0,3.0],[2.0,0.0,5.0]]
--    ghci> let p = listaMatriz [[4,5],[2,7%2],[6,10]] 
--    ghci> matrizLista (anulaColumnaDesde p 1 1)
--    [[4 % 1,5 % 1],[0 % 1,1 % 1],[0 % 1,5 % 2]]
-- ---------------------------------------------------------------------


anulaColumnaDesde :: (Fractional a, Eq a) => Matrix a -> Int -> Int -> Matrix a

anulaColumnaDesde p j i
    | anuladaColumnaDesde p j i = p
    | otherwise = anulaColumnaDesde (anulaEltoColumnaDesde p j i) j i


-- ---------------------------------------------------------------------
-- Algoritmo de Gauss para triangularizar matrices                    --
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 30. Definir la función
--    elementosNoNulosColDesde :: (Num a, Eq a) => 
--                                Matrix a -> Int -> Int -> [a]
-- tal que (elementosNoNulosColDesde p j i) es la lista de los elementos
-- no nulos de la columna j a partir de la fila i. Por ejemplo,
--    ghci> let p = listaMatriz [[3,2],[5,1],[0,4]]
--    ghci> elementosNoNulosColDesde p 1 2
--    [5]
-- ---------------------------------------------------------------------

elementosNoNulosColDesde :: (Num a, Eq a) => Matrix a -> Int -> Int -> [a]
elementosNoNulosColDesde p j i = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 31. Definir la función
--    existeColNoNulaDesde :: (Num a, Eq a) => 
--                            Matrix a -> Int -> Int -> Bool
-- tal que (existeColNoNulaDesde p j i) se verifica si la matriz p tiene
-- una columna a partir de la j tal que tiene algún elemento no nulo por
-- debajo de la i; es decir, si la submatriz de p obtenida eliminando
-- las i-1 primeras filas y las j-1 primeras columnas es no nula. Por
-- ejemplo, 
--    ghci> let p = listaMatriz [[3,2,5],[5,0,0],[6,0,0]]
--    ghci> existeColNoNulaDesde p 2 2
--    False
--    ghci> let q = listaMatriz [[3,2,5],[5,7,0],[6,0,0]]
--    ghci> existeColNoNulaDesde q 2 2
-- ---------------------------------------------------------------------
  
existeColNoNulaDesde :: (Num a, Eq a) => Matrix a -> Int -> Int -> Bool
existeColNoNulaDesde p j i = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 32. Definir la función
--    menorIndiceColNoNulaDesde :: (Num a, Eq a) => 
--                                 Matrix a -> Int -> Int -> Maybe Int
-- tal que (menorIndiceColNoNulaDesde p j i) es el índice de la primera
-- columna, a partir de la j, en el que la matriz p tiene un elemento no
-- nulo a partir de la fila i. Por ejemplo,
--    ghci> let p = listaMatriz [[3,2,5],[5,7,0],[6,0,0]]
--    ghci> menorIndiceColNoNulaDesde p 2 2
--    Just 2
--    ghci> let q = listaMatriz [[3,2,5],[5,0,0],[6,0,2]]
--    ghci> menorIndiceColNoNulaDesde q 2 2
--    Just 3
--    ghci> let r = listaMatriz [[3,2,5],[5,0,0],[6,0,0]]
--    ghci> menorIndiceColNoNulaDesde r 2 2
--    Nothing
-- ---------------------------------------------------------------------

menorIndiceColNoNulaDesde :: (Num a, Eq a) => 
                             Matrix a -> Int -> Int -> Maybe Int
menorIndiceColNoNulaDesde p j i = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 33. Definir la función
--    gaussAux :: (Fractional a, Eq a) => 
--                Matrix a -> Int -> Int -> Matrix a
-- tal que (gaussAux p i j) es la matriz que en el que las i-1 primeras
-- filas y las j-1 primeras columnas son las de p y las restantes están
-- triangularizadas por el método de Gauss; es decir,
--    1. Si la dimensión de p es (i,j), entonces p.
--    2. Si la submatriz de p sin las i-1 primeras filas y las j-1
--       primeras columnas son nulas, entonces p.
--    3. En caso contrario, (gaussAux p' (i+1) (j+1)) siendo
--    3.1. j' la primera columna a partir de la j donde p tiene
--         algún elemento no nulo a partir de la fila i,
--    3.2. p1 la matriz obtenida intercambiando las columnas j y j'
--         de p,
--    3.3. i' la primera fila a partir de la i donde la columna j de
--         p1 tiene un elemento no nulo,
--    3.4. p2 la matriz obtenida intercambiando las filas i e i' de
--         la matriz p1 y
--    3.5. p' la matriz obtenida anulando todos los elementos de la
--         columna j de p2 por debajo de la fila i.
-- Por ejemplo,
--    ghci> let p = listaMatriz [[1.0,2,3],[1,2,4],[3,2,5]]
--    ghci> gaussAux p 2 2
--    ( 1.0 2.0 3.0 )
--    ( 1.0 2.0 4.0 )
--    ( 2.0 0.0 1.0 )
-- ---------------------------------------------------------------------

gaussAux :: (Fractional a, Eq a) => Matrix a -> Int -> Int -> Matrix a
gaussAux p i j = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 34. Definir la función
--    gauss :: (Fractional a, Eq a) => Matrix a -> Matrix a
-- tal que (gauss p) es la triangularización de la matriz p por el método
-- de Gauss. Por ejemplo, 
--    ghci> let p = listaMatriz [[1.0,2,3],[1,2,4],[1,2,5]]
--    ghci> gauss p
--    ( 1.0 3.0 2.0 )
--    ( 0.0 1.0 0.0 )
--    ( 0.0 0.0 0.0 )
--    ghci> let p = listaMatriz [[3%1,2,3],[1,2,4],[1,2,5]]
--    ghci> gauss p
--    ( 3 % 1 2 % 1 3 % 1 )
--    ( 0 % 1 4 % 3 3 % 1 )
--    ( 0 % 1 0 % 1 1 % 1 )
--    ghci> let p = listaMatriz [[1.0,0,3],[1,0,4],[3,0,5]]
--    ghci> gauss p
--    ( 1.0 3.0 0.0 )
--    ( 0.0 1.0 0.0 )
--    ( 0.0 0.0 0.0 )
-- ---------------------------------------------------------------------

gauss :: (Fractional a, Eq a) => Matrix a -> Matrix a
gauss p = undefined

-- ---------------------------------------------------------------------
-- Determinante                                                       --
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 35. Definir la función
--    gaussCAux :: (Fractional a, Eq a) => 
--                 Matriz a -> Int -> Int -> Int -> Matriz a
-- tal que (gaussCAux p i j c) es el par (n,q) donde q es la matriz que
-- en el que las i-1 primeras filas y las j-1 primeras columnas son las
-- de p y las restantes están triangularizadas por el método de Gauss;
-- es decir, 
--    1. Si la dimensión de p es (i,j), entonces (c,p).
--    2. Si la submatriz de p sin las i-1 primeras filas y las j-1
--       primeras columnas es nulas, entonces (c,p).
--    3. En caso contrario, (gaussAux p' (i+1) (j+1) c') siendo
--    3.1. j' la primera columna a partir de la j donde p tiene
--         algún elemento no nulo a partir de la fila i,
--    3.2. p1 la matriz obtenida intercambiando las columnas j y j'
--         de p,
--    3.3. i' la primera fila a partir de la i donde la columna j de
--         p1 tiene un elemento no nulo,
--    3.4. p2 la matriz obtenida intercambiando las filas i e i' de
--         la matriz p1 y
--    3.5. p' la matriz obtenida anulando todos los elementos de la
--         columna j de p2 por debajo de la fila i.
--    3.6. c' es c más el número de intercambios de columnas y filas.
-- y n es c más el número de intercambios de columnas y filas que se han 
-- producido durante el cálculo. Por ejemplo,
--    ghci> gaussCAux (fromLists [[1.0,2,3],[1,2,4],[1,2,5]]) 1 1 0
--    (1,( 1.0 3.0 2.0 )
--       ( 0.0 1.0 0.0 )
--       ( 0.0 0.0 0.0 ))
-- ---------------------------------------------------------------------

gaussCAux :: (Fractional a, Eq a) => 
             Matrix a -> Int -> Int -> Int -> (Int,Matrix a)
gaussCAux p i j c = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 36. Definir la función
--    gaussC :: (Fractional a, Eq a) => Matriz a -> Matriz a
-- tal que (gaussC p) es el par (n,q), donde q es la triangularización
-- de la matriz p por el método de Gauss y n es el número de
-- intercambios de columnas y filas que se han producido durante el
-- cálculo. Por ejemplo,  
--    ghci> gaussC (fromLists [[1.0,2,3],[1,2,4],[1,2,5]])
--    (1, ( 1.0 3.0 2.0 )
--        ( 0.0 1.0 0.0 )
--        ( 0.0 0.0 0.0 )
-- ---------------------------------------------------------------------

gaussC :: (Fractional a, Eq a) => Matrix a -> (Int,Matrix a)
gaussC p = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 37. Definir la función
--    determinante :: (Fractional a, Eq a) => Matriz a -> a
-- tal que (determinante p) es el determinante de la matriz p. En una 
-- matriz triangular, el determinante se calcula como -1 elevado al número
-- de intercambios de filas y columnas producidos para calcular la matriz
-- triangular, por el producto de los elementos de la diagonal principal.
-- Por ejemplo, 
--    ghci> determinante (fromLists [[1.0,2,3],[1,3,4],[1,2,5]])
--    2.0
-- ---------------------------------------------------------------------

determinante :: (Fractional a, Eq a) => Matrix a -> a
determinante p = undefined
