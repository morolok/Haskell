-- PD 2019-20: 
-- El juego del nim y las funciones de ficheros. 
-- Departamento de Ciencias de la Computación e I.A.
-- Universidad de Sevilla
-- =====================================================================

------------------------------------------------------------------------
-- § Ampliación de Entrada y Salida con Ficheros                      --
------------------------------------------------------------------------

------------------------------------------------------------------------
-- IMPORTANTE                                                         --
-- Para poder probar los ejemplos de esta práctica, crea un fichero   --
-- entrada.txt junto al fichero haskell, con el siguiente contenido   --
-- (sin "{-" ni "-}")                                                 --
{-

tamano=6                                                           
tablero=6,5,4,3,2,1                                                
jugador1=miguel                                                    
jugador2=leugim

-}
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- § Librerías auxiliares                                             --
-- ---------------------------------------------------------------------

import Data.Char
import Data.List.Split
import Control.Exception (catch, SomeException)
import System.Environment (getArgs)


-- ---------------------------------------------------------------------
-- § Importación del módulo Nim						      --
-- ---------------------------------------------------------------------

-- --------------------------------------------------------------------- 
-- Ejercicio 1.
-- Prepara la solución a la práctica 12.1 (nim con E/S) para que sea un
-- módulo que podamos importar con nombre Nim, de tal forma que podamos
-- usar todas las siguientes funciones:
-- finalizado, valida, jugada, estrellas, siguiente, escribeTablero,
-- nuevaLinea, leeDigito

import Nim

-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 2.
-- Define la función (leeFichero fs) tal que lea el contenido del fichero
-- cuyo nombre está en la cadena fs, obteniendo una lista con las líneas
-- del mismo. Por ejemplo,
-- > leeFichero "entrada.txt"
-- ["tamano=6","tablero=6,5,4,3,2,1","jugador1=miguel","jugador2=leugim"]

leeFichero :: String -> IO [String]

leeFichero fs = do
    fichero <- readFile fs
    let res = lines fichero
    return res

-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 3.
-- Dada una lista con opciones definidas por cadenas de texto, como del
-- ejercicio anterior, devolver una lista de pares donde el primero del
-- par sea el nombre de la opción, y el segundo el valor. Por ejemplo,
-- λ> procesaEntrada $ leeFichero "entrada.txt"
-- [("tamano","6"),("tablero","6,5,4,3,2,1"),("jugador1","miguel"),
--  ("jugador2","leugim")]

agrupaContenido :: [String] -> [(String,String)]

agrupaContenido [] = []

agrupaContenido (c:cont) = cProcesado:(agrupaContenido cont)
    where separado = splitOn "=" c
          cProcesado = (separado!!0, separado!!1)


procesaEntrada :: IO [String] -> IO [(String,String)]

procesaEntrada entrada = do
    contenido <- entrada
    let res = agrupaContenido contenido
    return res

-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 4.1
-- Dada una lista con pares de opciones como la creada en el ejercicio
-- anterior, buscar y devolver el tablero inicial como una lista de
-- enteros, el cual viene definido como "tablero".
-- λ> tablero $ procesaEntrada $ leeFichero "entrada.txt"
-- [6,5,4,3,2,1]

sacaTablero :: String -> [Int]

sacaTablero [] = []

sacaTablero (t:tab)
    | t == ',' = sacaTablero tab
    | otherwise = estrellas:(sacaTablero tab)
    where estrellas = digitToInt t


tablero :: IO [(String,String)] -> IO [Int]

tablero entrada = do
    contenido <- entrada
    let tab = snd (contenido!!1)
    let res = sacaTablero tab
    return res

-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 4.3
-- Dada una lista con pares de opciones como la creada en el ejercicio
-- anterior, buscar y devolver en un par el nombre del primer y del
-- segundo jugador.
-- λ> jugadores $ procesaEntrada $ leeFichero "entrada.txt"
-- ("miguel","leugim")

jugadores :: IO [(String,String)] -> IO (String,String)

jugadores entrada = do
    contenido <- entrada
    let j1 = snd (contenido!!2)
    let j2 = snd (contenido!!3)
    let res = (j1,j2)
    return res
  
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 5.1
-- Definir la acción
--    escribeTablerosF :: String -> Tablero -> IO ()
-- tal que (escribeTablerosF fs t) escribe el tablero t en el fichero
-- con nombre fs. Por
-- ejemplo,
--   λ> escribeTablerosF "salida.txt"  [[3,4,1,0,1],[5,1,2,1,0]]
--   λ> readFile "salida.txt"
--   "1: * * * \n2: * * * * \n3: * \n4: \n5: * \n1: * * * * * \n2: * \n3: * * \n4: * \n5: \n"

escribeTablerosF :: FilePath -> [Tablero] -> IO ()

escribeTablerosF fs ts = writeFile fs $ unlines [fila n (t!!(n-1)) | t <- ts, n <- [1..length t]]
  where fila f n = (show f ++ ": " ++ estrellas n ) 

  
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 6. Definir la función principal para poder compilar el
-- el fichero. Compila el fichero y genera un ejecutable. El procedimiento
-- es el siguiente:
--  1) el programa debe leer como primer argumento el nombre del fichero
--     de entrada, con captura de excepciones
--  2) el programa debe leer como segundo argumento el nombre del fichero
--     de salida, con captura de excepciones
--  3) el programa debe procesar el fichero de entrada para sacar el
--     tablero inicial y los nombres de los jugadores
--  4) se debe dibujar la tabla inicial por pantalla, mostrar el nombre
--     del jugador, solicitar las acciones y actualizar en consecuencia.
--     Puedes basarte en la función (juego :: Tablero -> Int -> IO ())
--     de la práctica anterior. Esta vez, la función juego debe devolver
--     la secuencia de tableros, así que su tipado es:
--     juego :: Tablero -> Int -> (String,String) -> IO [Tablero]
--  5) Cuando finalice la partida, se debe escribir en el fichero de
--     la salida la secuencia de tableros obtenida.
-- ---------------------------------------------------------------------

-- Diferencias de este juego con el anterior:
--   Ahora imprime el nombre de los jugadores
--   Ahora devuelve la lista de tableros

juego :: Tablero -> Int -> (String,String) -> IO [Tablero]
juego t j jps = do nuevaLinea
                   escribeTablero t
                   if finalizado t 
                     then do nuevaLinea
                             putStr "J "
                             putStr (jug (siguiente j))
                             putStrLn " He ganado"
                             return []
                     else do nuevaLinea
                             putStr "J "
                             putStr (jug (siguiente j))
                             f <- leeDigito "Elige una fila: "
                             n <- leeDigito "Elige cuantas estrellas retiras: "
                             if valida t f n 
                               then do ts <- juego (jugada t f n) (siguiente j) jps
                                       return (t:ts)                                 
                               else do nuevaLinea
                                       putStrLn "ERROR: jugada incorrecta"
                                       ts <- juego t j jps
                                       return (t:ts)
        where jug x | x==1 = fst jps
                    | x==2 = snd jps


-- opciones por defecto
defecto = [("tablero","5,4,3,2,1"),("jugador1","1"),("jugador2","2")]

main :: IO ()
main = do
  args <- getArgs
  let inputfile = case args of
                    (a:_) -> a
                    _ -> error "Faltan parámetros: nim input.txt output.txt"
  let outputfile = case tail args of
                     (a:_) -> a
                     _ -> error "Faltan parámetros: nim input.txt output.txt"
  let input = catch (procesaEntrada (leeFichero inputfile))
                (\err -> print (err::SomeException) >> return defecto)
  inicial <- tablero input
  nombresJ <- jugadores input
  ts <- juego inicial 1 nombresJ
  catch (escribeTablerosF outputfile ts)
               (\err -> print (err::SomeException) >> return ())