-- PD 2019-20: de la relación de entrada/salida de I1M
-- El juego del nim y las funciones de entrada/salida. 
-- Departamento de Ciencias de la Computación e I.A.
-- Universidad de Sevilla
-- =====================================================================

------------------------------------------------------------------------
-- § Introducción                                                     --
------------------------------------------------------------------------

-- En el juego del nim el tablero tiene 5 filas numeradas de estrellas,
-- cuyo contenido inicial es el siguiente 
--    1: * * * * * 
--    2: * * * * 
--    3: * * * 
--    4: * * 
--    5: * 
-- Dos jugadores retiran por turno una o más estrellas de una fila. El
-- ganador es el jugador que retire la última estrella. En este
-- ejercicio se va implementar el juego del Nim para practicar con las
-- funciones de entrada y salida.
-- Nota: El juego debe de ejecutarse en una consola, no en la shell de
-- emacs. 

-- ---------------------------------------------------------------------
-- § Librerías auxiliares                                             --
-- ---------------------------------------------------------------------

module Nim 
    (Tablero,
    finalizado,
    valida,
    jugada,
    estrellas,
    siguiente,
    escribeTablero,
    nuevaLinea,
    leeDigito
    ) where

import Data.Char

-- ---------------------------------------------------------------------
-- § Representación                                                   --
-- ---------------------------------------------------------------------
 
-- El tablero se representará como una lista de números indicando el
-- número de estrellas de cada fila. Con esta representación, el tablero
-- inicial es [5,4,3,2,1]. 

-- Representación del tablero.

type Tablero = [Int]

-- inicial es el tablero al principio del juego.

inicial ::  Tablero

inicial =  [5,4,3,2,1]

-- ---------------------------------------------------------------------
-- Ejercicio 1. Definir la función
--    finalizado :: Tablero -> Bool
-- tal que (finalizado t) se verifica si t es el tablero de un juego
-- finalizado; es decir, sin estrellas. Por ejemplo,
--    finalizado [0,0,0,0,0]  ==  True
--    finalizado [1,3,0,0,1]  ==  False
-- ---------------------------------------------------------------------


finalizado :: Tablero -> Bool

finalizado t = (sum t) == 0


-- ---------------------------------------------------------------------
-- Ejecicio 2.2. Definir la función
--    valida :: Tablero -> Int -> Int -> Bool
-- tal que (valida t f n) se verifica si se puede coger n estrellas en
-- la fila f del tablero t y n es mayor o igual que 1. Por ejemplo,
--    valida [4,3,2,1,0] 2 3  ==  True
--    valida [4,3,2,1,0] 2 4  ==  False
--    valida [4,3,2,1,0] 2 2  ==  True
--    valida [4,3,2,1,0] 2 0  ==  False
-- ---------------------------------------------------------------------


valida :: Tablero -> Int -> Int -> Bool

valida t f n = (n > 0) && ((t!!(f-1)) >= n)


-- ---------------------------------------------------------------------
-- Ejercicio 3. Definir la función
--    jugada :: Tablero -> Int -> Int -> Tablero
-- tal que (jugada t f n) es el tablero obtenido a partir de t
-- eliminando n estrellas de la fila f. Por ejemplo,
--    jugada [4,3,2,1,0] 2 1  ==  [4,2,2,1,0]
-- ---------------------------------------------------------------------


jugada :: Tablero -> Int -> Int -> Tablero

jugada t f n = if (valida t f n) then (jugadaAux t (f-1) n []) else (error "La jugada no es valida")

jugadaAux [] _ _ res = res

jugadaAux (e:t) f n res
    | f == 0 = jugadaAux t (f-1) n (res++[estrellas])
    | otherwise = jugadaAux t (f-1) n (res++[e])
    where estrellas = e-n


-- ---------------------------------------------------------------------
-- Ejercicio 4. Definir la acción
--    nuevaLinea :: IO ()
-- que consiste en escribir una nueva línea. Por ejemplo,
--    ghci> nuevaLinea
--    
--    ghci> 
-- ---------------------------------------------------------------------


nuevaLinea :: IO ()

nuevaLinea = do
    putStrLn ""


-- ---------------------------------------------------------------------
-- Ejercicio 5. Definir la función
--    estrellas :: Int -> String
-- tal que (estrellas n) es la cadena formada con n estrellas. Por
-- ejemplo, 
--    ghci> estrellas 3
--    "* * * "
-- ---------------------------------------------------------------------


estrellas :: Int -> String

estrellas n
    | n < 0 = error "No se puede pintar ese numero de estrellas"
    | n == 0 = ""
    | n == 1 = "*"
    | otherwise = concat ["* ", estrellas (n-1)]

                              
-- ---------------------------------------------------------------------
-- Ejercicio 6. Definir la acción
--    escribeFila :: Int -> Int -> IO ()
-- tal que (escribeFila f n) escribe en la fila f n estrellas. Por
-- ejemplo, 
--    ghci> escribeFila 2 3
--    2: * * *
-- ---------------------------------------------------------------------


escribeFila :: Int -> Int -> IO ()

escribeFila f n = do
    putStr ((show f) ++ ": " ++ (estrellas n))
    nuevaLinea


-- ---------------------------------------------------------------------
-- Ejercicio 7. Definir la acción
--    escribeTablero :: Tablero -> IO ()
-- tal que (escribeTablero t) escribe el tablero t. Por
-- ejemplo,
--    ghci> escribeTablero [3,4,1,0,1]
--    1: * * * 
--    2: * * * * 
--    3: * 
--    4: 
--    5: * 
-- ---------------------------------------------------------------------


escribeTablero :: Tablero -> IO ()

escribeTablero t = sequence_ [escribeFila (f+1) (t!!f) | f <- [0..(length t)-1]]


-- ---------------------------------------------------------------------
-- Ejercicio 8. Definir la acción
--    leeDigito :: String -> IO Int
-- tal que (leeDigito c) escribe una nueva línea con l cadena "prueba",
-- lee un carácter y comprueba que es un dígito. Además, si el carácter
-- leido es un dígito entonces devuelve el entero correspondiente y si
-- no lo es entonces escribe el mensaje "Entrada incorrecta" y vuelve a
-- leer otro carácter. Por ejemplo,  
--    ghci> leeDigito "prueba "
--    prueba 3
--    3
--    ghci> leeDigito "prueba "
--    prueba c
--    ERROR: Entrada incorrecta
--    prueba 3
--    3
-- ---------------------------------------------------------------------


leeDigito :: String -> IO Int

leeDigito c = do
    putStr c
    xs <- getLine
    if (length xs == 1) && (isDigit (head xs))
        then return (read xs)
    else do
        putStrLn "ERROR: Entrada incorrecta"
        leeDigito c


-- ---------------------------------------------------------------------
-- Ejercicio 9. Los jugadores se representan por los números 1 y 2.
-- Definir la función 
--    siguiente :: Int -> Int
-- tal que (siguiente j) es el jugador siguiente de j. 
-- ---------------------------------------------------------------------


siguiente :: Int -> Int

siguiente j = if (j == 1) then 2 else 1


-- ---------------------------------------------------------------------
-- Ejercicio 10. Definir la acción
--    juego :: Tablero -> Int -> IO ()
-- tal que (juego t j) es el juego a partir del tablero t y el turno del
-- jugador j. Por ejemplo,
--    ghci> juego [0,1,0,1,0] 2
--    
--    1: 
--    2: * 
--    3: 
--    4: * 
--    5: 
--    
--    J 2
--    Elige una fila: 2
--    Elige cuantas estrellas retiras: 1
--    
--    1: 
--    2: 
--    3: 
--    4: * 
--    5: 
--    
--    J 1
--    Elige una fila: 4
--    Elige cuantas estrellas retiras: 1
--    
--    1: 
--    2: 
--    3: 
--    4: 
--    5: 
--    
--    J 1 He ganado
-- ---------------------------------------------------------------------


juego :: Tablero -> Int -> IO ()

juego t j = do
    escribeTablero t
    nuevaLinea
    putStrLn ("J " ++ (show j))
    fila <- leeDigito "Elige una fila: "
    estrellas <- leeDigito "Elige cuantas estrellas retiras: "
    nuevaLinea
    let tablero = jugada t fila estrellas
    if (finalizado tablero) then do
        putStrLn ("J " ++ (show j) ++ " ha ganado")
    else do
        let siguienteJ = siguiente j
        juego tablero siguienteJ


-- ---------------------------------------------------------------------
-- Ejercicio 11. Definir la acción
--    nim :: IO ()
-- consistente en una partida del nim. Por ejemplo, se puede desarrollar
-- en una consola (no en la shell de emacs) como sigue
--    ghci> nim
--    
--    1: * * * * * 
--    2: * * * * 
--    3: * * * 
--    4: * * 
--    5: * 
--    
--    J 1
--    Elige una fila: 1
--    Elige cuantas estrellas retiras: 4
--    
--    1: * 
--    2: * * * * 
--    3: * * * 
--    4: * * 
--    5: * 
--    
--    J 2
--    Elige una fila: 3
--    Elige cuantas estrellas retiras: 3
--    
--    1: * 
--    2: * * * * 
--    3: 
--    4: * * 
--    5: * 
--    
--    J 1
--    Elige una fila: 2
--    Elige cuantas estrellas retiras: 4
--    
--    1: * 
--    2: 
--    3: 
--    4: * * 
--    5: * 
--    
--    J 2
--    Elige una fila: 4
--    Elige cuantas estrellas retiras: 1
--    
--    1: * 
--    2: 
--    3: 
--    4: * 
--    5: * 
--    
--    J 1
--    Elige una fila: 1
--    Elige cuantas estrellas retiras: 1
--    
--    1: 
--    2: 
--    3: 
--    4: * 
--    5: * 
--    
--    J 2
--    Elige una fila: 4
--    Elige cuantas estrellas retiras: 1
--    
--    1: 
--    2: 
--    3: 
--    4: 
--    5: * 
--    
--    J 1
--    Elige una fila: 5
--    Elige cuantas estrellas retiras: 1
--    
--    1: 
--    2: 
--    3: 
--    4: 
--    5: 
--    
--    J 1 He ganado
-- ---------------------------------------------------------------------

nim :: IO ()
nim = juego inicial 1

-- ---------------------------------------------------------------------
-- Ejercicio 12. Definir la función principal para poder compilar el
-- el fichero. Compila el fichero y genera un ejecutable.
-- ---------------------------------------------------------------------
