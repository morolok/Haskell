-- PD 2019-20: Pr�ctica sobre acceso a fuentes de datos en ficheros.
-- Departamento de Ciencias de la Computaci�n e I.A.
-- Universidad de Sevilla
-- =====================================================================

-- ---------------------------------------------------------------------
-- Introducci�n                                                       --
-- ---------------------------------------------------------------------

--
-- Esta pr�ctica trata de asentar algunos de los primeros conceptos
-- introducidos en el tema de entrada y salida con ficheros.

 
-- ---------------------------------------------------------------------
-- Importaci�n de librer�as                                           --
-- ---------------------------------------------------------------------


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
-- (desc�rguelo de http://www.cs.us.es/cursos/pd/ejercicios/lorem_ipsum.txt) 
-- y devuelva una tupla conteniendo:
--    * El n�mero de p�rrafos
--    * El n�mero de palabras por p�rrafo
--    * El n�mero de apariciones de la letra 'e' por p�rrafo
-- 
--    $ runhaskell Practica12.2.hs
--    (5,[105,158,46,64,52],[41,60,16,24,28])
-- ---------------------------------------------------------------------


main1 :: IO ()

main1 = undefined


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
main2 = undefined
  
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

main3 :: IO ()
main3 = undefined

-- main = main3

