-- PD 2019-20: Práctica sobre acceso a fuentes de datos en ficheros.
-- Departamento de Ciencias de la Computación e I.A.
-- Universidad de Sevilla
-- =====================================================================

-- ---------------------------------------------------------------------
-- Introducción                                                       --
-- ---------------------------------------------------------------------

--
-- Esta práctica trata de asentar algunos de los primeros conceptos
-- introducidos en el tema de entrada y salida con ficheros.

 
-- ---------------------------------------------------------------------
-- Importación de librerías                                           --
-- ---------------------------------------------------------------------


import System.Environment (getArgs)

import Control.Exception (catch, SomeException)


main0 :: IO ()

main0 = do
  putStrLn "Vamos con los ejercicios propuestos..."
  putStrLn "--------------------------------------"
  putStrLn "Comente la linea main=main0 y descomente las de ejercicios"
  putStrLn "posteriores para que su main vaya llamando a cada mainN"
  putStrLn "conforme vaya avanzando."


--main = main0


-- ---------------------------------------------------------------------
-- Ejercicio 1. Crear un programa que lea el fichero "lorem_ipsum.txt"
-- (descárguelo de http://www.cs.us.es/cursos/pd/ejercicios/lorem_ipsum.txt) 
-- y devuelva una tupla conteniendo:
--    * El número de párrafos
--    * El número de palabras por párrafo
--    * El número de apariciones de la letra 'e' por párrafo
-- 
--    $ runhaskell Practica12.2.hs
--    (5,[105,158,46,64,52],[41,60,16,24,28])
-- ---------------------------------------------------------------------


-- Metodo para contar los parrafos de un texto

contarParrafos :: [String] -> Int

contarParrafos [] = 0

contarParrafos (l:lineas)
    | l == "\r" = 0 + (contarParrafos lineas)
    | otherwise = 1 + (contarParrafos lineas)


-- Metodo para contar las palabras de cada parrafo de un texto

contarPalabrasxParrafo :: [String] -> [Int]

contarPalabrasxParrafo [] = []

contarPalabrasxParrafo (l:lineas)
    | l == "\r" = contarPalabrasxParrafo lineas
    | otherwise = (palabras):(contarPalabrasxParrafo lineas)
    where palabras = length (words l)


-- Metodo para contar cada letra e de un parrafo de un texto

contarExParrafo :: [String] -> [Int]

contarExParrafo [] = []

contarExParrafo (l:lineas) = if (l /= "\r") then (contarEs palabras 0):(contarExParrafo lineas) else contarExParrafo lineas
    where palabras = words l

contarEs [] res = res

contarEs (p:palabras) res = contarEs palabras (res + numEs)
    where numEs = length (filter (=='e') p)


-- Metodo para procesar un fichero con los metodos anteriores

procesaFichero :: FilePath -> IO()

procesaFichero nf = do
    f <- readFile nf
    let lineas = lines f
    let parrafos = contarParrafos lineas
    let palabrasxParrafo = contarPalabrasxParrafo lineas
    let exParrafo = contarExParrafo lineas
    putStrLn ("(" ++ (show parrafos) ++ ", " ++ (show palabrasxParrafo) ++ ", " ++ (show exParrafo) ++ ")")



main1 :: IO ()

main1 = procesaFichero "lorem_ipsum.txt"


--main = main1


-- ---------------------------------------------------------------------
-- Ejercicio 2. Adaptar el ejercicio anterior para que podamos
-- pasarle al main como argumento el nombre del archivo a leer
-- (Descargue los ficheros: 
-- http://www.cs.us.es/cursos/pd/ejercicios/lorem_ipsum.txt
-- http://www.cs.us.es/cursos/pd/ejercicios/otro.txt
-- )
-- Si no recibimos archivo, debemos tratar el del ejercicio 1
-- 
--    $ runhaskell Practica12.2.hs
--    (5,[105,158,46,64,52],[79,108,38,43,30])
--
--    $ runhaskell Practica12.2.hs otro.txt
--    (8,[86,89,65,103,97,100,95,112],[62,57,50,69,63,60,69,65])
-- ---------------------------------------------------------------------


main2 :: IO ()

main2 = do
    args <- getArgs
    let ns = nombreFichero args
    procesaFichero ns



nombreFichero :: [String] -> String

nombreFichero [] = "default.txt"

nombreFichero (xs:xss) = xs
  
--main = main2


-- ---------------------------------------------------------------------
-- Ejercicio 3. Adaptar el ejercicio anterior para que trate el posible
-- error de lectura del fichero, como hemos visto en el tema.
--
--    $ runhaskell Practica12.2.hs
--    (5,[105,158,46,64,52],[79,108,38,43,30])
--
--    $ runhaskell Practica12.2.hs otro.txt
--    (8,[86,89,65,103,97,100,95,112],[62,57,50,69,63,60,69,65])
--
--    $ runhaskell Practica12.2.hs inexistente.txt
--    inexistente.txt: openFile: does not exist (No such file or directory)
--    "El fichero no existe"
-- ---------------------------------------------------------------------


procesaFicheroExcp :: FilePath -> IO()

procesaFicheroExcp nf = do
    f <- catch (readFile nf)
        (\err -> do print(err::SomeException)
                    putStrLn ("El fichero no existe")
                    return "")
    let lineas = lines f
    let parrafos = contarParrafos lineas
    let palabrasxParrafo = contarPalabrasxParrafo lineas
    let exParrafo = contarExParrafo lineas
    putStrLn ("(" ++ (show parrafos) ++ ", " ++ (show palabrasxParrafo) ++ ", " ++ (show exParrafo) ++ ")")


main3 :: IO ()

main3 = do
    args <- getArgs
    let ns = nombreFichero args
    procesaFicheroExcp ns


main = main3

