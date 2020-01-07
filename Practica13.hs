-- PD 2019-20: Codeworld (de la práctica 3 del año 2017/18)
-- Departamento de Ciencias de la Computación e I.A.
-- Universidad de Sevilla
-- =====================================================================

-- ---------------------------------------------------------------------
-- Importación de librerías                                           --
-- ---------------------------------------------------------------------


{-# LANGUAGE OverloadedStrings #-}

import CodeWorld


-- ---------------------------------------------------------------------
-- Introducción                                                       --
-- ---------------------------------------------------------------------

{--
Supongamos que tenemos un tablero rectangular negro dividido en
cuadrados. El objetivo es pintar todos los cuadrados de rojo. Para
ello disponemos de un robot que es capaz de pintar cuadrados
individuales de rojo con las siguientes restricciones:

* El robot nunca puede ir a un cuadrado pintado de rojo. Inicialmente,
  todos los cuadrados están pintados de negro, excepto aquél sobre el
  que se sitúa inicialmente el robot. 
* Inicialmente, el robot se encuentra sobre el cuadrado de la esquina
  superior izquierda (que ya está pintado de rojo).
* El robot se mueve por el tablero de cuadrado en cuadrado con
  movimientos simples iguales a los del caballo en el ajedrez.
* Una vez que ha realizado el movimiento, el robot se coloca sobre el
  cuadrado correspondiente y lo pinta de color rojo.

La situación inicial para un tablero 3x4 sería:

       +---+---+---+---+
       | X |   |   |   |   
       +---+---+---+---+
       |   |   |   |   |   
       +---+---+---+---+
       |   |   |   |   |   
       +---+---+---+---+

donde todos los cuadrado son negros salvo el marcado con la X, que es
donde se sitúa el robot, que está pintado de rojo.

Llamamos Estado el tipo definido para representar cualquier
situación intermedia que pueda darse a lo largo del proceso.
Utilizaremos una tupla con cinco elementos:
--}


type Estado = (Int, Int, Robot, [Rojo], [Opcion])


{--
* Número de filas del tablero
* Número de columnas del tablero
* Posición del robot dentro del tablero. Para representar la posición
  del robot utilizaremos una tupla con las coordenadas del cuadrado
  correspondiente. Llamamos Robot al tipo de tuplas descrito.
--}


type Robot = (Int, Int)


{--
* Cuadrados del tablero que ya se han pintado de rojo (excluyendo la
  posición del robot). Para representar cada uno de los cuadrados
  pintados de rojo utilizaremos una tupla con sus coordenadas.
  Llamamos Rojo al tipo de tuplas descrito.
--}


type Rojo = (Int, Int)


{--
* Opciones de movimiento que el robot puede hacer desde su
  posición. Para representar cada una de dichas opciones utilizaremos
  una tupla con la tecla que será necesaria para llevarla a cabo y
  las coordenadas del cuadrado que ocuparía el robot. Llamamos Opcion
  al tipo de tupla descrito.
--}


type Opcion = (Event, Int, Int)


-- ---------------------------------------------------------------------
-- Ejercicios                                                         --
-- ---------------------------------------------------------------------

{--
Ejercicio 1:
En primer lugar definiremos un conjunto de funciones que proporcionen
una imagen de cualquier estado.

* Definir una función pintaNegro que dados dos enteros positivos, n y
  m, dibujen un tablero negro de tamaño nxm.
--}


pintaNegro :: Int -> Int -> Picture

pintaNegro n m = translated ((x+1)/2) ((y+1)/2) (solidRectangle x y)
    where x = fromIntegral n
          y = fromIntegral m


--main :: IO()
--main = drawingOf ((pintaNegro 3 4) & coordinatePlane) -- (1)
--main = drawingOf ((pintaNegro 8 8) & coordinatePlane) -- (2)
              
{--
* Definir una función pintaRobot que dado un robot dibuje un circulo
  amarillo en el cuadrado del tablero indicado. El cuadrado, además,
  deberá estar pintado de rojo.
--}


pintaRobot :: Robot -> Picture

pintaRobot (r1, r2) = (translated x y (colored yellow (solidCircle 0.4))) & (translated x y (colored red (solidRectangle 1 1)))
  where x = fromIntegral r1
        y = fromIntegral r2


--main :: IO()
--main = drawingOf ((pintaRobot (1, 3)) & (pintaNegro 3 4) & coordinatePlane) -- (3)
--main = drawingOf ((pintaRobot (1, 8)) & (pintaNegro 8 8) & coordinatePlane) -- (4)

{--
* Definir una función pintaRojo que dada una lista de rojos dibuje
  de rojo cada uno de los cuadrados indicados.
--}


pintaRojo :: [Rojo] -> Picture

pintaRojo [] = blank

pintaRojo ((i, j):lr) = (translated x y (colored red (solidRectangle 1 1))) & (pintaRojo lr)
  where x = fromIntegral i
        y = fromIntegral j


--main :: IO()
--main = drawingOf ((pintaRojo [(2, 1), (1, 3)]) & (pintaRobot (3, 3)) & (pintaNegro 3 4) & coordinatePlane) -- (5)
--main = drawingOf ((pintaRojo [(2, 6), (1, 8)]) & (pintaRobot (4, 5)) & (pintaNegro 8 8) & coordinatePlane) -- (6)

{--
* Definir una función pintaOpcion que dada una lista de opciones
  dibuje de verde y en el cuadrado que ocuparía el robot cada una de
  las teclas que hay que pulsar para mover el robot.
--}


pintaOpcion :: [Opcion] -> Picture

pintaOpcion [] = blank

pintaOpcion ((KeyPress t, f, c):os) = translated x y (colored green (scaled 0.5 0.5 (lettering t))) & pintaOpcion os
  where x = fromIntegral f
        y = fromIntegral c


--main :: IO()
--main = drawingOf ((pintaOpcion [(KeyPress "1",2,1), (KeyPress "2",3,2)]) & (pintaRojo []) & (pintaRobot (1, 3)) & (pintaNegro 3 4) & coordinatePlane) -- (7)
--main = drawingOf ((pintaOpcion [(KeyPress "1",1,4), (KeyPress "2",3,4), (KeyPress "3",4,5), (KeyPress "4",4,7), (KeyPress "5",3,8)]) & (pintaRojo [(1, 8)]) & (pintaRobot (2, 6)) & (pintaNegro 8 8) & coordinatePlane) -- (8)

{--
* Por último, definir una función pintaEstado que dado un estado
  dibuje la situación descrita.
--}


pintaEstado :: Estado -> Picture

pintaEstado (n, m, r, rs, os) = translated (-x/2) (-y/2) (pintaRobot r & pintaOpcion os & pintaRojo rs & pintaNegro n m)
  where x = fromIntegral n
        y = fromIntegral m


main :: IO()
--main = drawingOf ((pintaEstado (3, 4, (1, 3), [], [(KeyPress "1",2,1), (KeyPress "2",3,2)])) & coordinatePlane) -- (7)       
main = drawingOf ((pintaEstado (8, 8, (2, 6), [(1, 8)], [(KeyPress "1",1,4), (KeyPress "2",3,4), (KeyPress "3",4,5), (KeyPress "4",4,7), (KeyPress "5",3,8)])) & coordinatePlane) -- (8)


{--
Ejercicio 2:
En los ejemplos anteriores se ha proporcionado la lista de opciones
correspondiente a la situación descrita en un determinado momento. En
general, estas opciones se pueden determinar a partir de la posición
del robot y los cuadrados ya pintados de rojo. Hay que tener en
cuenta que el caballo de ajedrez tiene 8 opciones de movimiento
posibles, que el robot no puede salir del tablero y que no puede
ocupar un cuadrado pintado de rojo.

Definir una función calculaOpciones que, dados el número de filas del
tablero, el número de columnas, la posición del robot y los cuadrados
ya pintados de rojo devuelva la lista de opciones correspondiente.
--}

type Movimiento = (Event, Int, Int)

movimientos :: [Movimiento]

movimientos = [(KeyPress "1", 1, -2), (KeyPress "2", 2, -1), (KeyPress "3", 2, 1), (KeyPress "4", 1, 2), (KeyPress "5", -1, 2), (KeyPress "6", -2, 1),
               (KeyPress "7", -2, -1), (KeyPress "8", -1, -2)]


mov2op :: Movimiento -> Robot -> Opcion

mov2op (e, x, y) (i, j) = (e, i+x, y+j)


aplicaMovimientos :: Robot -> [Movimiento] -> [Opcion]

aplicaMovimientos r ms = map (\m -> mov2op m r) ms


calculaOpciones :: Int -> Int -> [Rojo] -> Robot -> [Opcion]

calculaOpciones n m rs r = [(e, x, y) | (e, x, y) <- os, (x >= 1) && (x <= n) && (y >= 1) && (y <= m) && (notElem (x, y) rs)]
    where os = aplicaMovimientos r movimientos


-- *Main> calculaOpciones 3 4 [] (1, 3)
-- [(KeyPress "5",2,1),(KeyPress "7",3,2)]
-- *Main> calculaOpciones 8 8 [(1, 8)] (2, 6)
-- [(KeyPress "1",1,4),(KeyPress "5",3,4),(KeyPress "6",3,8),
--  (KeyPress "7",4,5),(KeyPress "8",4,7)]

{--
Ejercicio 3:
Por último tenemos que describir como afecta la opción que elija el
usuario elegida a la situación descrita en la imagen.

* Definir una función manejaEvento que dado un evento y un estado
  determine el estado resultante.
--}


manejaEvento :: Event -> Estado -> Estado

manejaEvento e es = undefined

-- manejaEvento _ estado = estado

-- main :: IO()
-- main = interactionOf (3, 4, (1, 3), [], (calculaOpciones 3 4 [] (1, 3)))
--        (\_ estado -> estado) manejaEvento pintaEstado
-- main = interactionOf (8, 8, (1, 8), [], (calculaOpciones 8 8 [] (1, 8)))
--        (\_ estado -> estado) manejaEvento pintaEstado

{--
* Definir una función juego que dados el número de filas y de
  columnas del tablero inicie la interacción. Recordar que el robot
  debe situarse, inicialmente, en la casilla superior izquierda del tablero.
--}

juego :: Int -> Int -> IO()
juego = undefined

-- main :: IO()
-- main = juego 3 4
-- main = juego 8 8
