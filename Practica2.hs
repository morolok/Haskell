-- PD-Practica 2
-- Definiciones con condicionales, guardas o patrones.
-- Departamento de Ciencias de la Computacion e I.A.
-- Universidad de Sevilla
-- =====================================================================

-- ---------------------------------------------------------------------
-- Introduccion                                                       --
-- ---------------------------------------------------------------------

-- En esta relacion se presentan ejercicios con definiciones elementales
-- (no recursivas) de funciones que usan condicionales, guardas o
-- patrones. 
-- 
-- Estos ejercicios se corresponden con el tema 3

-- ---------------------------------------------------------------------
-- § Librerias auxiliares                                             --
-- ---------------------------------------------------------------------

import Test.QuickCheck

-- ---------------------------------------------------------------------
-- Ejercicio 1. Definir la funcion 
--    divisionSegura :: Double -> Double -> Double
-- tal que (divisionSegura x y) es x/y si y no es cero y 9999 en caso
-- contrario. Por ejemplo,
--    divisionSegura 7 2  ==  3.5
--    divisionSegura 7 0  ==  9999.0
-- ---------------------------------------------------------------------

divisionSegura :: Double -> Double -> Double
divisionSegura x y
               | y == 0 = 9999.0
               | otherwise = x/y

-- ---------------------------------------------------------------------
-- Ejercicio 2.1. La disyuncion excluyente xor de dos formulas se
-- verifica si una es verdadera y la otra es falsa. Su tabla de verdad
-- es
--    x     | y     | xor x y
--    ------+-------+---------
--    True  | True  | False 
--    True  | False | True
--    False | True  | True
--    False | False | False
--    
-- Definir la funcion 
--    xor1 :: Bool -> Bool -> Bool
-- tal que (xor1 x y) es la disyuncion excluyente de x e y, calculada a
-- partir de la tabla de verdad. Usar 4 ecuaciones, una por cada linea
-- de la tabla. 
-- ---------------------------------------------------------------------

xor1 :: Bool -> Bool -> Bool
xor1 x y
     | x == True && y == True = False
     | x == True && y == False = True
     | x == False && y == True = True
     | x == False && y == False = False

-- ---------------------------------------------------------------------
-- Ejercicio 2.2. Definir la funcion 
--    xor2 :: Bool -> Bool -> Bool
-- tal que (xor2 x y) es la disyuncion excluyente de x e y, calculada a
-- partir de la tabla de verdad y patrones. Usar 2 ecuaciones, una por
-- cada valor del primer argumento. 
-- ---------------------------------------------------------------------

xor2 :: Bool -> Bool -> Bool
xor2 x y
     | x == y = False
     | otherwise = True

-- ---------------------------------------------------------------------
-- Ejercicio 2.3. Definir la funcion 
--    xor3 :: Bool -> Bool -> Bool
-- tal que (xor3 x y) es la disyuncion excluyente de x e y, calculada 
-- a partir de la disyuncion (||), conjuncion (&&) y negacion (not). 
-- Usar 1 ecuacion. 
-- ---------------------------------------------------------------------

xor3 :: Bool -> Bool -> Bool
xor3 x y = (x && not y) || (not x && y)

-- ---------------------------------------------------------------------
-- Ejercicio 2.4. Definir la funcion 
--    xor4 :: Bool -> Bool -> Bool
-- tal que (xor3 x y) es la disyuncion excluyente de x e y, calculada
-- a partir de desigualdad (/=). Usar 1 ecuacion.
-- ---------------------------------------------------------------------

xor4 :: Bool -> Bool -> Bool
xor4 x y = x /= y

-- ---------------------------------------------------------------------
-- Ejercicio 2.5. Comprobar con QuickCheck que las cuatros definiciones
-- de xor son equivalentes.
-- ---------------------------------------------------------------------

-- La propiedad es
prop_xor_equivalentes :: Bool -> Bool -> Bool
prop_xor_equivalentes x y = (xor1 x y) == (xor2 x y)
prop_xor_equivalentes2 x y = (xor1 x y) == (xor3 x y)
prop_xor_equivalentes3 x y = (xor1 x y) == (xor4 x y)

-- La comprobacion es

-- ---------------------------------------------------------------------
-- Ejercicio 3. Las dimensiones de los rectangulos puede representarse 
-- por pares; por ejemplo, (5,3) representa a un rectangulo de base 5 y 
-- altura 3. 
-- 
-- Definir la funcion 
--    mayorRectangulo :: (Num a, Ord a) => (a,a) -> (a,a) -> (a,a)
-- tal que (mayorRectangulo r1 r2) es el rectangulo de mayor area ente
-- r1 y r2. Por ejemplo,  
--    mayorRectangulo (4,6) (3,7)  ==  (4,6)
--    mayorRectangulo (4,6) (3,8)  ==  (4,6)
--    mayorRectangulo (4,6) (3,9)  ==  (3,9)
-- ---------------------------------------------------------------------

mayorRectangulo :: (Num a, Ord a) => (a,a) -> (a,a) -> (a,a)
mayorRectangulo (br1, ar1) (br2, ar2)
                | (br1*ar1) > (br2*ar2) = (br1, ar1)
                | otherwise = (br2, ar2)

-- ---------------------------------------------------------------------
-- Ejercicio 4.1. Definir la funcion 
--    intercambia :: (a,b) -> (b,a)
-- tal que (intercambia p)  es el punto obtenido intercambiando las
-- coordenadas del punto p. Por ejemplo, 
--    intercambia (2,5)  ==  (5,2)
--    intercambia (5,2)  ==  (2,5)
-- ---------------------------------------------------------------------

intercambia :: (a,b) -> (b,a)
intercambia p = (snd p, fst p)

-- ---------------------------------------------------------------------
-- Ejercicio 4.2. Comprobar con QuickCheck que la funcion intercambia es
-- idempotente; es decir, si se aplica dos veces es lo mismo que no
-- aplicarla ninguna.
-- ---------------------------------------------------------------------

-- La propiedad es
prop_intercambia :: (Int,Int) -> Bool
prop_intercambia p = intercambia p == (snd p, fst p)

-- La comprobacion es

-- ---------------------------------------------------------------------
-- Ejercicio 5.1. Definir la funcion 
--    distancia :: (Double,Double) -> (Double,Double) -> Double
-- tal que (distancia p1 p2) es la distancia entre los puntos p1 y
-- p2. Por ejemplo, 
--    distancia (1,2) (4,6)  ==  5.0
-- ---------------------------------------------------------------------
 
distancia :: (Double,Double) -> (Double,Double) -> Double
distancia p1 p2 = sqrt(((fst p1) - (fst p2))**2 + ((snd p1) - (snd p2))**2)

-- ---------------------------------------------------------------------
-- Ejercicio 5.2. Comprobar con QuickCheck que se verifica la propiedad
-- triangular de la distancia; es decir, dados tres puntos p1, p2 y p3,
-- la distancia de p1 a p3 es menor o igual que la suma de la distancia
-- de p1 a p2 y la de p2 a p3.
-- ---------------------------------------------------------------------

-- La propiedad es
prop_triangular :: (Double,Double) -> (Double,Double) -> (Double,Double)
                -> Bool
prop_triangular p1 p2 p3 = (distancia p1 p3) <= ((distancia p1 p2) + (distancia p2 p3))

-- La comprobacion es

-- ---------------------------------------------------------------------
-- Ejercicio 6.1. Definir una funcion 
--    ciclo :: [a] -> [a]
-- tal que (ciclo xs) es la lista obtenida permutando ciclicamente los
-- elementos de la lista xs, pasando el ultimo elemento al principio de
-- la lista. Por ejemplo, 
--    ciclo [2,5,7,9]  == [9,2,5,7]
--    ciclo []         == []
--    ciclo [2]        == [2]
-- ---------------------------------------------------------------------

ciclo :: [a] -> [a]
ciclo [] = []
ciclo ls
      | (length ls) == 1 = ls
      | otherwise = (head (reverse ls)):(reverse (tail (reverse ls)))

cicloProf [] = []
cicloProf xs = last xs : init xs

-- ---------------------------------------------------------------------
-- Ejercicio 6.2. Comprobar que la longitud es un invariante de la
-- funcion ciclo; es decir, la longitud de (ciclo xs) es la misma que la
-- de xs.
-- ---------------------------------------------------------------------

-- La propiedad es
prop_ciclo :: [Int] -> Bool 
prop_ciclo xs = undefined

-- La comprobacion es

-- ---------------------------------------------------------------------
-- Ejercicio 7. Definir la funcion 
--    numeroMayor :: (Num a, Ord a) => a -> a -> a
-- tal que (numeroMayor x y) es el mayor numero de dos cifras que puede
-- construirse con los digitos x e y. Por ejemplo,  
--    numeroMayor 2 5 ==  52
--    numeroMayor 5 2 ==  52
-- ---------------------------------------------------------------------

numeroMayor :: (Num a, Ord a) => a -> a -> a
numeroMayor x y = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 8. Definir la funcion 
--    numeroDeRaices :: (Num t, Ord t) => t -> t -> t -> Int
-- tal que (numeroDeRaices a b c) es el numero de raices reales de la
-- ecuacion a*x^2 + b*x + c = 0. Por ejemplo,
--    numeroDeRaices 2 0 3    ==  0
--    numeroDeRaices 4 4 1    ==  1
--    numeroDeRaices 5 23 12  ==  2
-- ---------------------------------------------------------------------


numeroDeRaices :: (Num t, Ord t) => t -> t -> t -> Int
numeroDeRaices a b c
               | (b*b) - 4*a*c > 0 = 2
               | (b*b) - 4*a*c == 0 = 1
               | otherwise = 0


-- ---------------------------------------------------------------------
-- Ejercicio 9.1. Definir la funcion 
--    raices :: Double -> Double -> Double -> [Double]
-- tal que (raices a b c) es la lista de las raices reales de la
-- ecuacion ax^2 + bx + c = 0. Por ejemplo, 
--    raices 1 3 2    ==  [-1.0,-2.0]
--    raices 1 (-2) 1 ==  [1.0,1.0]
--    raices 1 0 1    ==  []
-- ---------------------------------------------------------------------

raices :: Double -> Double -> Double -> [Double]
raices a b c
       | numeroDeRaices a b c == 0 = []
       | numeroDeRaices a b c == 1 = [-b/(2*a)]
       | otherwise = [((-b+r)/(2*a)),((-b-r)/(2*a))]
       where r = sqrt(b^2-4*a*c)

-- ---------------------------------------------------------------------
-- Ejercicio 9.2. Definir el operador
--    (~=) :: (Fractional a, Ord a) => a -> a -> Bool
-- tal que (x ~= y) se verifica si x e y son casi iguales; es decir si
-- el valor absoluto de su diferencia es menor que una milesima. Por
-- ejemplo, 
--    12.3457 ~= 12.3459  ==  True
--    12.3457 ~= 12.3479  ==  False
-- ---------------------------------------------------------------------

(~=) :: (Fractional a, Ord a) => a -> a -> Bool
x ~= y = abs(x-y)<0.001


-- ---------------------------------------------------------------------
-- Ejercicio 10. En geometria, la formula de Heron, descubierta por
-- Heron de Alejandria, dice que el area de un triangulo cuyo lados
-- miden a, b y c es la raiz cuadrada de s(s-a)(s-b)(s-c) donde s es el
-- semiperimetro 
--    s = (a+b+c)/2
-- 
-- Definir la funcion 
--    area :: Double -> Double -> Double -> Double 
-- tal que (area a b c) es el area del triangulo de lados a, b y c. Por
-- ejemplo, 
--    area 3 4 5  ==  6.0
-- ---------------------------------------------------------------------

area :: Double -> Double -> Double -> Double 
area a b c = sqrt(s*(s-a)*(s-b)*(s-c))
     where s = (a+b+c)/2

-- ---------------------------------------------------------------------
-- Ejercicio 11.1. Los intervalos cerrados se pueden representar mediante
-- una lista de dos numeros (el primero es el extremo inferior del
-- intervalo y el segundo el superior). 
-- 
-- Definir la funcion 
--    interseccion :: Ord a => [a] -> [a] -> [a]
-- tal que (interseccion i1 i2) es la interseccion de los intervalos i1 e
-- i2. Por ejemplo,
--    interseccion [] [3,5]     ==  []
--    interseccion [3,5] []     ==  []
--    interseccion [2,4] [6,9]  ==  []
--    interseccion [2,6] [6,9]  ==  [6,6]
--    interseccion [2,6] [0,9]  ==  [2,6]
--    interseccion [2,6] [0,4]  ==  [2,4]
--    interseccion [4,6] [0,4]  ==  [4,4]
--    interseccion [5,6] [0,4]  ==  []
-- ---------------------------------------------------------------------

interseccion :: Ord a => [a] -> [a] -> [a]
interseccion = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 11.2. Comprobar con QuickCheck que la interseccion de
-- intervalos es conmutativa.
-- ---------------------------------------------------------------------

-- La propiedad es
prop_interseccion :: Int -> Int -> Int -> Int -> Bool
prop_interseccion a1 b1 a2 b2 = undefined

-- La comprobacion es

-- ---------------------------------------------------------------------
-- Ejercicio 12.1. Los numeros racionales pueden representarse mediante
-- pares de numeros enteros. Por ejemplo, el numero 2/5 puede
-- representarse mediante el par (2,5). 
-- 
-- Definir la funcion 
--    formaReducida :: (Int,Int) -> (Int,Int) 
-- tal que (formaReducida x) es la forma reducida del numero racional
-- x. Por ejemplo, 
--    formaReducida (4,10)  ==  (2,5)
-- ---------------------------------------------------------------------

formaReducida :: (Int,Int) -> (Int,Int) 
formaReducida (a,b) = (a `div`(gcd a b), b `div`(gcd a b))

-- ---------------------------------------------------------------------
-- Ejercicio 12.2. Definir la funcion 
--    sumaRacional :: (Int,Int) -> (Int,Int) -> (Int,Int)
-- tal que (sumaRacional x y) es la suma de los numeros racionales x e
-- y, expresada en forma reducida. Por ejemplo, 
--    sumaRacional (2,3) (5,6)  ==  (3,2)
-- ---------------------------------------------------------------------

sumaRacional :: (Int,Int) -> (Int,Int) -> (Int,Int)
sumaRacional (a,b) (c,d) = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 12.3. Definir la funcion 
--    productoRacional :: (Int,Int) -> (Int,Int) -> (Int,Int)
-- tal que (productoRacional x y) es el producto de los numeros
-- racionales x e y. Por ejemplo, 
--    productoRacional (2,3) (5,6)  ==  (5,9)
-- ---------------------------------------------------------------------

productoRacional :: (Int,Int) -> (Int,Int) -> (Int,Int)
productoRacional (a,b) (c,d) = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 12.4. Definir la funcion 
--    igualdadRacional :: (Int,Int) -> (Int,Int) -> Bool
-- tal que (igualdadRacional x y) se verifica si los numeros racionales
-- x e y son iguales. Por ejemplo, 
--    igualdadRacional (6,9) (10,15)  ==  True
--    igualdadRacional (6,9) (11,15)  ==  False
--    igualdadRacional (0,2) (0,-5)   ==  True
-- ---------------------------------------------------------------------

igualdadRacional :: (Int,Int) -> (Int,Int) -> Bool
igualdadRacional (a,b) (c,d) = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 12.5. Comprobar con QuickCheck la propiedad distributiva
-- del producto racional respecto de la suma.
-- ---------------------------------------------------------------------

-- La propiedad es
prop_distributiva :: (Int,Int) -> (Int,Int) -> (Int,Int) -> Property
prop_distributiva x y z = undefined

-- La comprobacion es
