-- PD 2019-20: Práctica programación paralela con Haskell
-- Departamento de Ciencias de la Computación e I.A.
-- Universidad de Sevilla
-- =====================================================================

-- ---------------------------------------------------------------------
-- Importación de librerías                                           --
-- ---------------------------------------------------------------------


import Control.Parallel.Strategies

import Control.DeepSeq


-- ---------------------------------------------------------------------
-- Introducción                                                       --
-- ---------------------------------------------------------------------
-- ---------------------------------------------------------------------
-- Práctica paralelismo básico con la mónada Eval
-- 
-- Instrucciones:
--   Para comprobar el rendimiento obtenido en cada solución,
--   hay que compilar el fichero. Para ello abrir una
--   terminal, navegar hasta la carpeta, y ejecutar el
--   siguiente comando:
--     ghc -O2 practica.hs -rtsopts -threaded
--   Si se compila correctamente, ejecutar el binario objtenido:
--     ./practica +RTS -NX -s
--   siendo X el número de procesadores, o nada para detectar
--   los procesadores disponibles automáticamente.
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 1.1. Definir la función fibSer, tal que sea la
-- la función de fibonacci definida por recursión. Es decir:
--   fib(n) = fib(n-1) + fib(n-2), fib(0) = 1 y fib(1) = 1
-- ---------------------------------------------------------------------


fibSer 0 = 1

fibSer 1 = 1

fibSer n = (fibSer (n-1)) + (fibSer (n-2))


-- ---------------------------------------------------------------------
-- Ejercicio 1.2. Definir la función fibPar1, tal que sea la
-- la función de fibonacci definida por recursión empleando la
-- mónada Eval para obtener paralelismo en cada llamada recursiva.
-- Evaluar la función con distintos parámetros de paralelismo
-- y analizar el resultado.
-- ---------------------------------------------------------------------


fibPar1 0 = 1

fibPar1 1 = 1

fibPar1 n = runEval $ do
    fb1 <- rpar (fibPar1 (n-1))
    fb2 <- rpar (fibPar1 (n-2))
    res <- rseq (fb1+fb2)
    return res


-- ---------------------------------------------------------------------
-- Ejercicio 1.3. Definir la función fibPar2, tal que sea la
-- la función de fibonacci definida por recursión empleando la
-- mónada Eval para obtener paralelismo en la evaluación
-- secuencial de la primera llamada recursiva.
-- Evaluar la función con distintos parámetros de paralelismo
-- y analizar el resultado.
-- ---------------------------------------------------------------------


fibPar2 0 = 1

fibPar2 1 = 1

fibPar2 n = runEval $ do
    fb1 <- rpar (fibSer (n-1))
    fb2 <- rpar (fibSer (n-2))
    res <- rseq (fb1+fb2)
    return res

        
-- ---------------------------------------------------------------------
-- Ejercicio 1.4. Definir la función fibPar3 tal que sea la
-- la función de fibonacci definida por recursión empleando la
-- mónada Eval para obtener paralelismo en las llamadas recursivas,
-- hasta llegar a cierto umbral, a partir del cual se evalua de
-- forma secuencial. Evaluar la función con distintos parámetros
-- de paralelismo y analizar el resultado.
-- ---------------------------------------------------------------------


cutoff = 23

fibPar3 0 = 1

fibPar3 1 = 1

fibPar3 n
    | n < cutoff = fibSer n
    | otherwise = runEval $ do
        fb1 <- rpar (fibPar3 (n-1))
        fb2 <- rpar (fibPar3 (n-2))
        res <- rseq (fb1+fb2)
        return res


-- Descomentar el siguiente main para probar las funciones de fib

{-
main :: IO (Int)
main = do
    let fib = fibPar1 40
    print fib
    return fib
-}
  
-- ---------------------------------------------------------------------
-- Ejercicio 2.1. Definir mergesort, que reciba una lista de
-- elementos y ordene sus elementos mediante mezcla ordenada.
-- Es decir: dada una lista, partirla por la mitad, ordenarlas
-- y mezclarlas de forma ordenada.
-- ---------------------------------------------------------------------
 
seqMergeSort [] = []

seqMergeSort [x] = [x]

seqMergeSort xs = mezclaOrdenada (seqMergeSort as) (seqMergeSort bs)
    where (as,bs) = splitAt (length xs `div` 2) xs


mezclaOrdenada [] ys = ys

mezclaOrdenada xs [] = xs

mezclaOrdenada (x:xs) (y:ys)
    | x <= y = x:(mezclaOrdenada xs (y:ys))
    | otherwise = y:(mezclaOrdenada (x:xs) ys)

-- ---------------------------------------------------------------------
-- Ejercicio 2.2. Definir mergesort, que reciba una lista de
-- elementos y ordene sus elementos mediante mezcla ordenada,
-- aplicando paralelismo mediante la mónada eval. Buscar el
-- umbral óptimo a partir del cual no merece la pena aplicar
-- paralelismo.
-- ---------------------------------------------------------------------

parMergeSort xs = runEval $ do
    let (as,bs) = splitAt (length xs `div` 2) xs
    p1 <- rpar (force (seqMergeSort as))
    p2 <- rpar (force (seqMergeSort bs))
    rseq p1
    rseq p2
    return (mezclaOrdenada p1 p2)

-- Descomentar el siguiente main para probar las funciones de 
-- mergesort

main :: IO ([Int])
main = do
    let xs = [1000000,999999..1]
    let oxs = parMergeSort xs 
    print (length oxs)
    return (oxs)


-- ---------------------------------------------------------------------
-- Ejercicio 3.1. Redefinir la función parMap', tal que reciba
-- una función y una lista, y devuelva una evaluación paralela
-- de f sobre los elementos de la lista (ver tema de teoría).
-- ---------------------------------------------------------------------


parMap' :: (a -> b) -> [a] -> Eval [b]

parMap' f [] = return []

parMap' f (a:as) = do
    b <- rpar (f a)
    bs <- parMap' f as
    rseq (b:bs)


-- ---------------------------------------------------------------------
-- Ejercicio 3.2. Definir la función parallelMap, tal que reciba
-- una función y una lista, y devuelva una lista con la evaluación
-- de f sobre los elementos de la lista, empleando parMap'.
-- ---------------------------------------------------------------------


parallelMap :: (a -> b) -> [a] -> [b]

parallelMap f xs = runEval $ parMap' f xs


-- -------------------------------------------------------------
-- Ejercicio 4.1. Definir la función parReduce', tal que reciba
-- una función y una lista, y devuelva una evaluación paralela
-- de f por cada par de elementos consecutivos de la lista
-- (ver tema de teoría).
-- -------------------------------------------------------------

parReduce' :: (a -> a -> a) -> [a] -> Eval [a]
parReduce' = undefined

-- -------------------------------------------------------------
-- Ejercicio 4.2. Definir la función parReduce, tal que reciba
-- una función y una lista, y devuelva un elemento que resulte de
-- reducir la lista con la evaluación de f empleando parReduce'.
-- (ver tema de teoría).
-- -------------------------------------------------------------        

parReduce :: (a -> a -> a) -> [a] -> a
parReduce = undefined

{-main :: IO (Double)
main = do
    let xs = [i/2415632 | i <- [1..10000000]]
    let o = parReduce (+) xs
    print o
    return o-}

-- -------------------------------------------------------------
-- Ejercicio 5.1. Definir la función agrupa, tal que reciba
-- un entero n y una lista xs, y devuelva una lista de listas con
-- agrupaciones de n elementos de xs. Por ejemplo,
--   agrupa 3 [1..10] == [[1,2,3],[4,5,6],[7,8,9],[10]]
-- -------------------------------------------------------------

-- Este ejercicio ya fue resuelto en una práctica anterior. Puede
-- ser de utilidad para implementar un histograma
agrupa :: Int -> [a] -> [[a]]
agrupa n = takeWhile (not . null)
         . map (take n)
         . iterate (drop n)
           
-- ---------------------------------------------------------------------
-- Ejercicio 5.2. Definir la función hist, tal que reciba
-- un entero bins y una lista xs, y devuelva el histograma de
-- bins intervalos sobre los elementos de la lista xs. Es decir,
-- la frecuencia de aparición de los elementos de cada intervalo
-- (un total de bins) en la lista xs. Por ejemplo,
--   hist 2 [1,2,3,2,1,2,3,4] == [5,3]
-- ---------------------------------------------------------------------

hist :: Int -> [ Int ] -> [ Int ]
hist = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 5.3. Definir la función parHist, tal que reciba
-- un entero bins y una lista xs, y devuelva el histograma de
-- bins intervalos sobre los elementos de la lista xs. Definir
-- varias versiones según donde se aplique el paralelismo.
-- Evaluar qué versión es más eficiente.
-- ---------------------------------------------------------------------

parHist :: Int -> [ Int ] -> [ Int ]
parHist = undefined

-- Descomentar el siguiente main para probar las funciones de hist
{-
main :: IO ([Int])
main = do
    let xs = concat (replicate 10000 [1..200])
    let oxs = parHist 25 xs
    print (oxs)
    return (oxs)

-}

-- ---------------------------------------------------------------------
-- Ejercicio 6 (extra). Descargar el siguiente código:
-- https://github.com/simonmar/par-tutorial
-- o bien clonarlo con git:
-- git clone git://github.com/simonmar/par-tutorial.git
-- y realizar los siguientes pasos:
--   A.- Compilar los ficheros:
--     ghc -O2 sudoku1.hs -threaded -rtsopts
--     ghc -O2 sudoku2.hs -threaded -rtsopts
--     ghc -O2 sudoku3.hs -threaded -rtsopts
--   B.- Ejecutarlos y comparar la eficiencia:
--     secuencial: ./sudoku1 sudoku17.1000.txt +RTS -s
--     paralelo estático: ./sudoku2 sudoku17.1000.txt +RTS -s -N2
--     paralelo dinámico: ./sudoku3 sudoku17.1000.txt +RTS -s -N2
--   C.- Ejecutar la versión secuencial y paralela dinámica con
--     un fichero más grande: sudoku17.16000.txt. 
--     * ¿Cuál es el speedup? ¿Puedes explicar la discrepancia?
--     PISTA: ver la información en la línea de SPARKS.
--   D.- Probar el parámetro -eXXX para modificar el tamaño
--       del pool de sparks a la hora de ejecutar el programa.
-- ---------------------------------------------------------------------
