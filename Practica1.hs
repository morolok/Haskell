import Test.QuickCheck

-- ---------------------------------------------------------------------
-- Ejercicio 1. Evalua las siguientes lineas para entender como funciona
-- el sistema de tipos que proporciona Haskell.
-- ---------------------------------------------------------------------
-- :type True
-- :t True
-- :t 1
-- :t 1.1
-- :t 'a'
-- :t "a"
-- :t [1,2]
-- :t [1,2.1]
-- :t [1,'a']
-- :t (1,'s')
-- :t [[1],[1,2]] 
-- :t not
-- :t sum
-- :t (+)
-- :t []
-- :t ()
-- :t (3+)
-- :t length
-- :t zip
-- :t take

-- ---------------------------------------------------------------------
-- Ejercicio 2. Sin evaluar las expresiones en GHC, decide que tipo es  
-- el adecuado para cada una de ellas. Intenta dar el tipo mas general.
-- ---------------------------------------------------------------------

-- i1:: Integer  -- El primero va de regalo
-- i1 = 45

-- i2 = "123"
-- i3 = 45 <= i1
-- i4 = 'c'
-- i5 = ["abc","ok"]
-- i6 = head i5
-- i7 = tail "abc"
-- i8 = (True,4.5)
-- i9 = [i1,34]
-- i10 = sum
-- i11 x = length [1..x]

-- ---------------------------------------------------------------------
-- Ejercicio 3. Para cada una de las siguientes expresiones, reemplaza
-- undefined por una expresion valida para el tipo que la declara.
-- ---------------------------------------------------------------------

j1:: (String,Integer)
j1 = ("Torres",9)

j2:: [Integer]
j2 = [1,2,3]

j3:: Char
j3 = 'C'

j4:: Double
j4 = 9.9

j5:: (Integer,String,Integer,Char)
j5 = (9,"Torres",9,'F')

j6:: ([Char],(Bool,String))
j6 = (['F','T'],(True, "Torres"))

j7:: [[Bool]]
j7 = [[True,False],[False,False],[True,True]]

j8:: [(String,Bool)]
j8 = [("True",True),("False",False)]

j9:: Integer -> Integer
j9 = undefined

j10:: Float -> [Bool] -> Bool
j10 = undefined

j11:: [Char] -> [[Int]]
j11 = undefined


{--
Problema 1:

  Conocemos el cambio actual del euro a dolares estadounidenses: 1
  Euro son 1.17507 dolares

  * Definir la constante tipoCambio con dicho valor.

  * Calcular el cambio a dolares de distintas cantidades de euros y viceversa

  * Definir dos funciones, aEuros y aDolares, que dada una cantidad de
    dolares (resp. euros) permita obtener la cantidad de euros (resp.
    dolares) equivalente. 

    Nota: No es necesario redondear el resultado.

  * Volver a calcular los cambios anteriores utilizando las funciones
    definidas.

  * Escribir la siguiente propiedad: dada cualquier cantidad de euros,
    si la cambiamos a dolares y los dolares obtenidos los volvemos a
    cambiar a euros, obtenemos la cantidad de euros original.

  * Si la propiedad anterior ha fallado analiza el posible problema y
    busca Escribir la siguiente propiedad: dada cualquier cantidad de
    euros, una solucion al mismo.

  * Utilizar :browse para conocer los tipos de las definiciones
    anteriores y añadirselos a cada una.
--}


tipoCambio = 1.17507

aEuros x = x*tipoCambio

aDolares x = x/tipoCambio


{--
Indicacion:
Para cada una de las constantes y funciones que se definan a
continuacion usar :t para averiguar el tipo que infiere haskell y
añadirlo a la definicion (rectificandolo cuando sea conveniente).
--}

{--
Problema 2:

  Conocemos que 0�C se corresponden con 32�F y que un incremento de
  5�C suponen un incremento de 9�F.

  * Definir una funcion que permita pasar de ºC a ºF (y otra para el
    cambio contrario).

  * Si para mañana esta prevista un minimo de 19ºC y un maximo de
    34ºC, ¿cual seria el rango expresado en ºF?
--}


deCaF g = (g * (9/5)) + 32

deFaC f = (f - 32) * (5/9)


{--
Problema 3:

  Una tienda vende las mallas de 2kg de patatas a 2.70 euros. Para 
  favorecer la venta de cantidades mayores ofrece un precio reducido
  de 2.20 euros a partir de la quinta malla. Es decir, si un cliente
  compra 18 mallas, las cinco primeras las cobra a 2.70 y las 13
  restantes a 2.20.

  * Definir una funcion que, dada la cantidad de mallas calcule el
    precio sin tener en cuenta la promocion. Calcular el precio del
    ejemplo proporcionado.

  * Definir una funcion que, dada la cantidad de mallas, calcular el
    precio correspondiente segun la promocion. Usar dicha funcion
    para calcular, de nuevo, el precio del ejemplo.

  La oferta ha tenido tanto exito que el vendedor decide ampliarla
  reduciendo el precio a 2 euros a partir de la decima malla.

  * Definir una funcion para la nueva promocion y volver al calcular
    el precio del ejemplo.
--}


precioSinPromocion mayas = mayas * 2.70

precioConPromocion mayas = if mayas <= 5 then (precioSinPromocion mayas) else (5 * 2.70) + ((mayas - 5) * 2.20)

precioNuevaPromocion mayas = if mayas <= 10 then (precioSinPromocion mayas) else (10 * 2.70) + ((mayas - 10) * 2)

precioTodasPromociones mayas = if mayas <= 5 then (precioSinPromocion mayas) else if mayas <= 10 then (precioConPromocion mayas) else (precioNuevaPromocion mayas)


{--
Problema 4:

  Consideremos el siguiente juego: Dado un numero mayor que 1, si es
  par dividelo entre 2 y si es impar multiplicalo por 3 y sumale 1.
  Si el resultado es 1 ya has terminado, en caso contrario repite el
  procedimiento sobre el resultado.

  Pregunta: Dado un numero inicial cualquiera, cuantas veces tendras
  que aplicar el procedimiento.

  Ejemplos:

  Si empezamos por 10 => dividimos por 2 y obtenemos 5 =>
  multiplicamos por 3 y sumamos 1, obteniendo 16 => toca volver a
  dividir y obtenemos 8 => repetimos y obtenemos 4 => seguimos y
  obtenemos 2 => alcanzamos el 1.

  los valores han sido 5, 16, 8, 4, 2, 1: lo hemos aplicado 6 veces

  Si empezamos por 7 los valores seran 22, 11, 34, 17, 52, 26, 13, 40, 20, 10, 5,
  16, 8, 4, 2, 1: lo hemos aplicado 16 veces.


  * Definir una funcion que aplique una vez el procedimiento
    anterior. Utilizarla sucesivamente para verificar que los
    resultados proporcionados a partir de 10 y de 7 son correctos.

    Nota: Pueden ser de utilidad las funciones even y div

  * Definir una funcion que dado un numero natural mayor que uno
    calcule el numero de veces que se repite el resultado.

  * Definir una funcion que devuelva la lista de resultados hasta
    llegar  a 1.
--}


--problema4:: Int -> [Int]

problema4 n
          | n < 1 = error "Numero no valido"
          | otherwise = problema4Aux n []

problema4Aux n ls
         | n == 1 = reverse ls
         | even n = problema4Aux np (np:ls)
         | otherwise = problema4Aux ni (ni:ls)
           where np = n `div` 2
                 ni = n * 3 + 1
