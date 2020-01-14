-- PD 2019-20: Práctica de repaso (ejercicios de exámenes)
-- Departamento de Ciencias de la Computación e I.A.
-- Universidad de Sevilla
-- =====================================================================


-- Descargar una implementación de pilas e importarla:
-- + PilaConTipoDeDatoAlgebraico.hs que está en http://bit.ly/21z3g49
--import PilaConTipoDeDatoAlgebraico
-- + PilaConListas.hs               que está en http://bit.ly/21z3oAD
--import PilaConListas


import Data.Matrix
import Data.Char

-- ---------------------------------------------------------------------
-- Ejercicio 1. (Septiembre 2019, PD). Matrices
-- ---------------------------------------------------------------------
-- Algunos algoritmos de compresión de imágenes hacen uso de los planos
-- de bits, o bitplanes. Sea una matriz m de números en binario, los
-- bitplanes son las matrices de bits correspondientes al bit n-ésimo de
-- cada elemento en m. Es decir, la matriz con el primer bit de todos los
-- elementos es el primer bitplane, la matriz con el segundo bit de todos
-- los elementos es el segundo bitplane, ... Supongamos una representación
-- little-endian (el bit menos significativo (en la posición 1) es el último,
-- el bit 2 es el antepenúltimo, etc.). Por ejemplo, los bitplanes de la matriz
--  ┌             ┐
--  │ 101   1  10 │
--  │   0  11 100 │
--  │  10 110   1 │
--  └             ┘
-- es la lista de matrices siguiente (del tercer bitplane al primero):
--  ┌       ┐   ┌       ┐   ┌       ┐
--  │ 1 0 0 │   │ 0 0 1 │   │ 1 1 0 │
--  │ 0 0 1 │   │ 0 1 0 │   │ 0 1 0 │
--  │ 0 1 0 │   │ 1 1 0 │   │ 0 0 1 │
--  └       ┘ , └       ┘ , └       ┘
--
-- Defina la función (bitplanes m), tal que recibe una matriz
-- de números en binario (por simplicidad de tipo Int, asuma que solo
-- contiene 0s y 1s), y devuelve una lista de matrices con los bitplanes
-- desde el más significativo (el '1' más significativo de todos los
-- elementos de la matriz) hasta el menos significativo. La siguiente
-- matriz de ejemplo corresponde al anterior.


matrizEj :: Matrix Int

matrizEj = fromLists [[101, 1, 10], [0, 11, 100], [10, 110, 1]]


numBitPlanes :: Matrix Int -> Int

numBitPlanes m = maximum [(length.show) (m!(i,j)) | i <- [1..nrows m], j <- [1..ncols m]]


bitI :: Int -> Int -> Int

bitI i n
    | i > length ds || i <= 0 = 0
    | otherwise = ds!!(length ds - i)
    where ds = digitos n


digitos :: Int -> [Int]

digitos n = [digitToInt c | c <- show n]


bitplanes m = [matrix nr nc (f b) | b <- [n,n-1..1]]
    where n = numBitPlanes m
          nc = ncols m
          nr = nrows m
          f b (i,j) = bitI b (m!(i,j))


-- Solución: https://www.cs.us.es/cursos/pd/examenes/Examen_20190912.zip
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 2. (Septiembre 2019, PD). TAD Pila
-- ---------------------------------------------------------------------
-- La sucesión generalizada de Fibonacci de grado N
-- (N >= 1) se construye comenzando con el número 1 y calculando el
-- resto de términos como la suma de los N términos anteriores (si
-- existen). Por ejemplo,
-- + la sucesión generalizada de Fibonacci de grado 2 es:
--   1, 1, 2, 3, 5, 8, 13, 21, 34, 55
-- + la sucesión generalizada de Fibonacci de grado 4 es:
--   1, 1, 2, 4, 8, 15, 29, 56, 108, 208
-- + la sucesión generalizada de Fibonacci de grado 6 es:
--   1, 1, 2, 4, 8, 16, 32, 63, 125, 248
-- Defina la función (fibPila n k), que devuelve una pila con los
-- términos de la sucesión de Fibonacci de grado n menores que k.
-- Se pide usar sólo el TAD de Pila (no se permite el uso de listas).
-- Por ejemplo:
-- λ> fibPila 6 100
--   63|32|16|8|4|2|1|1|-
-- λ> fibPila 6 200
--   125|63|32|16|8|4|2|1|1|-
-- λ> fibPila 4 200
--   108|56|29|15|8|4|2|1|1|-

-- Solución: https://www.cs.us.es/cursos/pd/examenes/Examen_20190912.zip
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 3. (Junio 2019, I1M). Evaluación perezosa
-- ---------------------------------------------------------------------
-- Un número n es k-belga si la sucesión cuyo primer
-- elemento es k y cuyos elementos se obtienen sumando reiteradamente
-- los dígitos de n contiene a n.  
-- 
-- El 18 es 0-belga, porque a partir del 0 vamos a ir sumando
-- sucesivamente 1, 8, 1, 8, ... hasta llegar o sobrepasar el 18: 0, 1,
-- 9, 10, 18, ... Como se alcanza el 18, resulta que el 18 es 0-belga. 
-- 
-- El 19 no es 1-belga, porque a partir del 1 vamos a ir sumando
-- sucesivamente 1, 9, 1, 9, ... hasta llegar o sobrepasar el 18: 1, 2,
-- 11, 12, 21, ... Como no se alcanza el 19, resulta que el 19 no es
-- 1-belga. 
--
-- Definir la función 
--    esBelga :: Integer -> Integer -> Bool
-- tal que (esBelga k n)  se verifica si n es k-belga. Por ejemplo,
--    esBelga 0 18                              ==  True
--    esBelga 1 19                              ==  False
--    esBelga 0 2016                            ==  True
--    [x | x <- [0..30], esBelga 7 x]           ==  [7,10,11,21,27,29]
--    [x | x <- [0..30], esBelga 10 x]          ==  [10,11,20,21,22,24,26]
--    length [n | n <- [1..9000], esBelga 0 n]  ==  2857
--
-- Comprobar con QuickCheck que para todo número entero positivo n, si
-- k es el resto de n entre la suma de los dígitos de n, entonces n es
-- k-belga.

-- Solución: https://github.com/jaalonso/Examenes_de_PF_con_Haskell_Vol10/blob/master/Examenes/Grupo_3/examen_6_10_jun.hs
-- ---------------------------------------------------------------------


obtenDigitos :: Int -> [Int]

obtenDigitos n = [digitToInt c | c <- show n]


sucBelga k n = map fst (iterate f (k,digitos n))
    where f (x,ds) = (x+head ds,(tail ds)++[head ds])


obtenSucesion dig cont res
    | cont == (length dig) = obtenSucesion dig 0 res
    | cont == 0 && length res == 1 = res++[num]++(obtenSucesion dig (cont+1) (res++[num]))
    | otherwise = [num]++(obtenSucesion dig (cont+1) (res++[num]))
    where num = (last res)+(dig!!cont)



esBelga k n
    | elem n ls = True
    | otherwise = False
    where dig = obtenDigitos n
          ls = takeWhile (<=n) (obtenSucesion dig 0 [k])
          -- ls = takeWhile (<=n) (sucBelga k n)

--esBelga k n = elem n $ takeWhile (<=n) $ map (fst) $ iterate (\(ac,n) -> (ac + read [(n!!0)], shiftL n)) (k, show n)

--shiftL xs = (drop 1 xs)++(take 1 xs)



-- ---------------------------------------------------------------------
-- Ejercicio 4. (Junio 2019, I1M). Programación Dinámica
-- ---------------------------------------------------------------------
-- El problema del número de emparejamiento de amigos
-- consiste en calcular el número de formas de emparejar n amigos
-- teniendo en cuenta que cada uno puede permanecer soltero o puede ser
-- emparejado con algún otro amigo y que cada amigo puede ser emparejado
-- sólo una vez. Por ejemplo, los 4 posibles emparejamientos de 3 amigos
-- son   
--    {1}, {2}, {3} 
--    {1}, {2, 3} 
--    {1, 2}, {3} 
--    {1, 3}, {2}
--
-- Definir, usando programación dinámica, la función
--    nEmparejamientos :: Integer -> Integer
-- tal que (nEmparejamientos n) es el número de formas de emparejar a
-- los n amigos. Por ejemplo,  
--    nEmparejamientos 2   ==  2
--    nEmparejamientos 3   ==  4
--    nEmparejamientos 4   ==  10
--    nEmparejamientos 10  ==  9496
--    nEmparejamientos 30  ==  606917269909048576
--    length (show (nEmparejamientos3 (10^4)))  ==  17872

-- Solución: https://github.com/jaalonso/Examenes_de_PF_con_Haskell_Vol10/blob/master/Examenes/Grupo_3/examen_6_10_jun.hs
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 5. (Parcial 2 2018/19, PD). Listas. Paralelismo
-- ---------------------------------------------------------------------
-- Ejercicio 5.1 
-- Un número entero n es libre de cuadrados si no existe un número primo
-- p tal que p^2 divide a n. Por ejemplo, 10 es libre de cuadrado porque
-- 10 = 2*5, pero 12 no lo es porque es divisible entre 2^2. Definir
-- la función (libresDeCuadrado m) que devuelva la lista de booleanos
-- que indique para cada número entre el 1 y m si es libre de cuadrado.
-- Por ejemplo,
--   libresDeCuadrado 10 ==
--          [True,True,True,False,True,True,True,False,False,True]


-- Ejercicio 5.2 
-- Paralelizar la definición de libresDeCuadrado
-- empleando la función parallel map vista en teoría. Indicar en un
-- comentario cuál es el speedup alcanzado cuando se compara la versión
-- secuencial (anterior) y paralela (aquí solicitada) con m=5000.

-- Nota: emplear esta función main para comprobar cada versión
{-main = do
    let oxs = parLibresDeCuadrado 5000 
    print $ length $ filter (\a -> a) oxs
    return (oxs) -}

-- speedup = 11,616s / 1,451s = 8x

-- Solución: https://www.cs.us.es/cursos/pd/ejercicios/examenes/Parcial2_20190118_Sol.hs
-- ---------------------------------------------------------------------

