-- PD 
-- Definiciones por recursion.
-- Departamento de Ciencias de la Computacion e I.A.
-- Universidad de Sevilla
-- =====================================================================

-- ---------------------------------------------------------------------
-- Ejercicio 1. Definir, por recursion, la funcion
--    sumaCuadradosImpares :: [Integer] -> Integer
-- tal que (sumaCuadradosImparesR xs) es la suma de los cuadrados de los
-- numeros impares de la lista xs. Por ejemplo,
--    sumaCuadradosImparesR [1,2,3]  ==  10
-- ---------------------------------------------------------------------


sumaCuadradosImparesR xs = sumaCuadradosImparesRAux xs 0 0

sumaCuadradosImparesR2 xs = sumaCuadradosImparesRAux2 xs 0


sumaCuadradosImparesRAux xs cont res
                         | cont == length xs = res
                         | odd num = sumaCuadradosImparesRAux xs (cont+1) (res+num^2)
                         | otherwise = sumaCuadradosImparesRAux xs (cont+1) res
                         where num = xs!!cont

sumaCuadradosImparesRAux2 xs cont
                          | cont == ((length xs) - 1) && odd num = num^2
                          | odd num = (num^2) + sumaCuadradosImparesRAux2 xs (cont+1)
                          | otherwise = 0 + sumaCuadradosImparesRAux2 xs (cont+1)
                          where num = xs!!cont


-- ---------------------------------------------------------------------
-- Ejercicio 2. Definir, usando recursion, la funcion
--    entre :: Integer -> Integer -> [Integer]
-- tal que (entreL m n) es la lista de los numeros entre m y n. Por
-- ejemplo, 
--    entreL 2 5  ==  [2,3,4,5]
-- ---------------------------------------------------------------------


entreL x y
       | x == y = [x]
       | x > y = entreLDesc x y []
       | x < y = entreLAsc x y []

entreLDesc x y res
           | x == y = res++[x]
           | otherwise = entreLDesc (x-1) y (res++[x])

entreLAsc x y res
          | x == y = res++[x]
          | otherwise = entreLAsc (x+1) y (res++[x])


-- ---------------------------------------------------------------------
-- Ejercicio 3. Definir, por recursion, la funcion
--    sumaPositivosRec :: [Int] -> Int
-- tal que (sumaPositivosRec xs) es la suma de los numeros positivos de
-- xs. Por ejemplo, 
--    sumaPositivosRec [0,1,-3,-2,8,-1,6]  ==  15
-- ---------------------------------------------------------------------


sumaPositivosRec (x:xs)
                 | x > 0 = x + sumaPositivosRec xs
                 | otherwise = sumaPositivosRec xs


-- ---------------------------------------------------------------------
-- Ejercicio 4. El doble factorial de un numero n se define por 
--    n!! = n*(n-2)* ... * 3 * 1, si n es impar
--    n!! = n*(n-2)* ... * 4 * 2, si n es par
--    1!! = 1
--    0!! = 1    
-- Por ejemplo,
--    8!! = 8*6*4*2   = 384
--    9!! = 9*7*5*3*1 = 945
-- Definir, por recursion, la funcion
--    dobleFactorial :: Integer -> Integer
-- tal que (dobleFactorial n) es el doble factorial de n. Por ejemplo,
--    dobleFactorial 8  ==  384
--    dobleFactorial 9  ==  945
-- ---------------------------------------------------------------------


dobleFactorial 0 = 1
dobleFactorial 1 = 1
dobleFactorial x = x * (dobleFactorial (x-2))


-- ---------------------------------------------------------------------
-- Ejercicio 5. La distancia de Hamming entre dos listas es el
-- numero de posiciones en que los correspondientes elementos son
-- distintos. Por ejemplo, la distancia de Hamming entre "roma" y "loba"
-- es 2 (porque hay 2 posiciones en las que los elementos
-- correspondientes son distintos: la 1ª y la 3ª).
--    
-- Definir la funcion
--    distancia :: Eq a => [a] -> [a] -> Int
-- tal que (distancia xs ys) es la distancia de Hamming entre xs e
-- ys. Por ejemplo,
--    distancia "romano" "comino"  ==  2
--    distancia "romano" "camino"  ==  3
--    distancia "roma"   "comino"  ==  2
--    distancia "roma"   "camino"  ==  3
--    distancia "romano" "ron"     ==  1
--    distancia "romano" "cama"    ==  2
--    distancia "romano" "rama"    ==  1
-- ---------------------------------------------------------------------


distancia xs ys = distanciaAux xs ys 0 0 minimoLis
          where minimoLis = min (length xs) (length ys)

distanciaAux xs ys res cont minimoLis
             | (cont < minimoLis) && ((xs!!cont)==(ys!!cont)) = distanciaAux xs ys res (cont+1) minimoLis
             | (cont < minimoLis) && ((xs!!cont)/=(ys!!cont)) = distanciaAux xs ys (res+1) (cont+1) minimoLis
             | cont == minimoLis = res


-- ---------------------------------------------------------------------
-- Ejercicio 6. Definir por recursion la funcion 
--    sustituyeImpar :: [Int] -> [Int]
-- tal que (sustituyeImpar xs) es la lista obtenida sustituyendo cada
-- numero impar de xs por el siguiente numero par. Por ejemplo,
--    sustituyeImpar [2,5,7,4]  ==  [2,6,8,4]
-- ---------------------------------------------------------------------


sustituyeImpar xs = sustituyeImparAux xs 1 []

sustituyeImparAux [] _ res = res

sustituyeImparAux (x:xs) cont res
                  | odd x = sustituyeImparAux xs (cont+1) (res++[nuevaX])
                  | even x = sustituyeImparAux xs (cont+1) (res++[x])
                  where nuevaX = x+1


-- ---------------------------------------------------------------------
-- Ejercicio 7. Definir, por recursion, la funcion
--    digitosR :: Integer -> [Integer]
-- tal que (digitosR n) es la lista de los digitos del numero n. Por
-- ejemplo, 
--    digitosR 320274  ==  [3,2,0,2,7,4]
-- ---------------------------------------------------------------------


digitosR n = digitosRAux n []

digitosRAux n res
            | n < 10 = reverse (res++[n])
            | otherwise = digitosRAux (div n 10) (res++[mod n 10])


-- ---------------------------------------------------------------------
-- Ejercicio 8. Definir por recursion la funcion
--    potencia :: Integer -> Integer -> Integer
-- tal que (potencia x n) es x elevado al numero natural n. Por ejemplo,  
--    potencia 2 3  ==  8
-- ---------------------------------------------------------------------


potencia x n
         | n == 1 = x
         | otherwise = x * (potencia x (n-1))


-- ---------------------------------------------------------------------
-- Ejercicio 9. Definir por recursion la funcion
--    replicate' :: Int -> a -> [a]
-- tal que (replicate' n x) es la lista formado por n copias del
-- elemento x. Por ejemplo,
--    replicate' 3 2  ==  [2,2,2]
-- ---------------------------------------------------------------------
 

-- ---------------------------------------------------------------------
-- Ejercicio 10. Dados dos numeros naturales, a y b, es posible
-- calcular su maximo comun divisor mediante el Algoritmo de
-- Euclides. Este algoritmo se puede resumir en la siguiente formula:
--    mcd(a,b) = a,                   si b = 0
--             = mcd (b, a modulo b), si b > 0
-- 
-- Definir la funcion 
--    mcd :: Integer -> Integer -> Integer
-- tal que (mcd a b) es el maximo comun divisor de a y b calculado
-- mediante el algoritmo de Euclides. Por ejemplo,
--    mcd 30 45  ==  15
-- ---------------------------------------------------------------------


-- ---------------------------------------------------------------------
-- Ejercicio 11. En un templo hindu se encuentran tres varillas de
-- platino. En una de ellas, hay 64 anillos de oro de distintos radios,
-- colocados de mayor a menor.
-- 
-- El trabajo de los monjes de ese templo consiste en pasarlos todos a
-- la tercera varilla, usando la segunda como varilla auxiliar, con las
-- siguientes condiciones: 
--   * En cada paso solo se puede mover un anillo.
--   * Nunca puede haber un anillo de mayor diametro encima de uno de
--     menor diametro.
-- La leyenda dice que cuando todos los anillos se encuentren en la
-- tercera varilla, sera el fin del mundo.  
-- 
-- Definir la funcion 
--    numPasosHanoi :: Integer -> Integer
-- tal que (numPasosHanoi n) es el numero de pasos necesarios para
-- trasladar n anillos. Por ejemplo, 
--    numPasosHanoi 2   ==  3
--    numPasosHanoi 7   ==  127
--    numPasosHanoi 64  ==  18446744073709551615
-- ---------------------------------------------------------------------

-- Sean A, B y C las tres varillas. La estrategia recursiva es la
-- siguiente: 
-- * Caso base (N=1): Se mueve el disco de A a C.
-- * Caso inductivo (N=M+1): Se mueven M discos de A a C. Se mueve el disco
--   de A a B. Se mueven M discos de C a B.
-- Por tanto,


numPasosHanoi 1 = 1
numPasosHanoi n = 1 + 2*(numPasosHanoi (n-1))


-- ---------------------------------------------------------------------
-- Ejercicio 12. Definir por recursion la funcion
--    and' :: [Bool] -> Bool
-- tal que (and' xs) se verifica si todos los elementos de xs son
-- verdadero. Por ejemplo,
--    and' [1+2 < 4, 2:[3] == [2,3]]  ==  True
--    and' [1+2 < 3, 2:[3] == [2,3]]  ==  False
-- ---------------------------------------------------------------------


and' xs = andAux xs 0 True

andAux xs cont res
       | cont == (length xs) = res
       | otherwise = andAux xs (cont+1) (res&&(xs!!cont))


-- ---------------------------------------------------------------------
-- Ejercicio 13. Definir por recursion la funcion
--    elem' :: Eq a => a -> [a] -> Bool
-- tal que (elem' x xs) se verifica si x pertenece a la lista xs. Por
-- ejemplo, 
--    elem' 3 [2,3,5]  ==  True
--    elem' 4 [2,3,5]  ==  False
-- ---------------------------------------------------------------------


elem' x xs = elemAux x xs 0

elemAux x xs cont
        | cont == length xs = False
        | x == (xs!!cont) = True
        | otherwise = elemAux x xs (cont+1)


-- ---------------------------------------------------------------------
-- Ejercicio 14. Definir por recursion la funcion
--    last' :: [a] -> a
-- tal que (last xs) es el ultimo elemento de xs. Por ejemplo,
--    last' [2,3,5]  =>  5
-- ---------------------------------------------------------------------


last' xs = lastAux xs 0

lastAux xs cont
        | cont == (length xs) - 1 = xs!!cont
        | otherwise = lastAux xs (cont+1)


-- ---------------------------------------------------------------------
-- Ejercicio 15. Definir por recursion la funcion
--    concat' :: [[a]] -> [a]
-- tal que (concat' xss) es la lista obtenida concatenando las listas de
-- xss. Por ejemplo,
--    concat' [[1..3],[5..7],[8..10]]  ==  [1,2,3,5,6,7,8,9,10]
-- ---------------------------------------------------------------------


concat' xss = concatAux xss 0 []

concatAux xss cont1 res
          | cont1 == length xss = res
          | otherwise = concatAux2 xss (cont1+1) (xss!!cont1) res 0


concatAux2 xss cont1 xs res cont2
           | cont2 == length xs = concatAux xss cont1 res
           | otherwise = concatAux2 xss cont1 xs (res++[xs!!cont2]) (cont2+1)
 

-- ---------------------------------------------------------------------
-- Ejercicio 16. Definir por recursion la funcion
--    selecciona :: [a] -> Int -> a
-- tal que (selecciona xs n) es el n-esimo elemento de xs. Por ejemplo,
--    selecciona [2,3,5,7] 2  ==  5 
-- ---------------------------------------------------------------------


selecciona (x:xs) n
           | n == 0 = x
           | otherwise = selecciona xs (n-1)


-- ---------------------------------------------------------------------
-- Ejercicio 17. Definir por recursion la funcion
--    take' :: Int -> [a] -> [a]
-- tal que (take' n xs) es la lista de los n primeros elementos de
-- xs. Por ejemplo, 
--    take' 3 [4..12]  =>  [4,5,6]
-- ---------------------------------------------------------------------


take' n xs = takeAux n xs []

takeAux n (x:xs) res
        | n == 0 = res
        | otherwise = takeAux (n-1) xs (res++[x])


-- ---------------------------------------------------------------------
-- Ejercicio 18. Definir por recursion la funcion
--    mezcla :: Ord a => [a] -> [a] -> [a] 
-- tal que (mezcla xs ys) es la lista obtenida mezclando las listas
-- ordenadas xs e ys. Por ejemplo,  
--    mezcla [2,5,6] [1,3,4]  ==  [1,2,3,4,5,6]
-- ---------------------------------------------------------------------


mezcla xs [] = xs
mezcla [] ys = ys

mezcla (x:xs) (y:ys)
       | x < y = x:(mezcla xs (y:ys))
       | otherwise = y:(mezcla (x:xs) ys)


-- ---------------------------------------------------------------------
-- Ejercicio 19. Definir la funcion 
--    mitades :: [a] -> ([a],[a]) 
-- tal que (mitades xs) es el par formado por las dos mitades en que se
-- divide xs tales que sus longitudes difieren como maximo en uno. Por
-- ejemplo, 
--    mitades [2,3,5,7,9]  ==  ([2,3],[5,7,9])
-- ---------------------------------------------------------------------


mitades xs = (take mitad xs, drop mitad xs)
        where mitad = div (length xs) 2

-- ---------------------------------------------------------------------
-- Ejercicio 20. Definir por recursion la funcion 
--    ordMezcla :: Ord a => [a] -> [a]
-- tal que (ordMezcla xs) es la lista obtenida ordenado xs por mezcla
-- (es decir, considerando que la lista vacia y las listas unitarias
-- estan ordenadas y cualquier otra lista se ordena mezclando las dos
-- listas que resultan de ordenar sus dos mitades por separado). Por
-- ejemplo, 
--    ordMezcla [5,2,3,1,7,2,5]  ==  [1,2,2,3,5,5,7]
-- ---------------------------------------------------------------------


ordMezcla [] = []
ordMezcla [x] = [x]
ordMezcla xs = mezcla fs2 ss2
          where (fs,ss) = mitades xs
                fs2 = ordMezcla fs
                ss2 = ordMezcla ss

    
-- ---------------------------------------------------------------------
-- Ejercicio 21. Definir por recursion la funcion
--    borra :: Eq a => a -> [a] -> [a]
-- tal que (borra x xs) es la lista obtenida borrando una ocurrencia de
-- x en la lista xs. Por ejemplo, 
--    borra 1 [1,2,1]  ==  [2,1]
--    borra 3 [1,2,1]  ==  [1,2,1]
-- ---------------------------------------------------------------------


borra :: Eq a => a -> [a] -> [a]
borra x xs = borraAux x xs 0 []
borraAux x [] cont res = res
borraAux x (y:xs) cont res
         | x == y && cont == 0 = borraAux x xs (cont+1) res
         | otherwise = borraAux x xs cont res++[y]
