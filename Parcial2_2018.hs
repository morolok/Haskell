-- Programación Declarativa
-- Grado de Ingeniería Informática - Tecnologías Informáticas
-- Parcial 2                                       17 de Enero de 2019
-- -------------------------------------------------------------------
-- Apellidos:
-- Nombre:
-- -------------------------------------------------------------------
-- AVISOS IMPORTANTES
-- · Antes de continuar, cambie el nombre de este archivo por:
--                   Parcial2_<uvus>.hs
--   donde <uvus> debe ser su usuario virtual.
-- · Escriba la solución de cada ejercicio en el hueco reservado para
--   ello.
-- · Asegúrese de utilizar correctamente el nombre y el tipo indicado
--   para cada función solicitada. Puede añadir tantas funciones
--   auxiliares (incluyendo el tipo adecuadamente) como necesite,
--   describiendo su objetivo.
-- -------------------------------------------------------------------


import Data.Default


-- -------------------------------------------------------------------
-- Ejercicio 3. [2 ptos]

-- 1. Defina, con sintaxis de registro, un nuevo tipo que contenga la
--    información sobre planetas que aparecen en las películas
--    de star wars:
--    * name, diameter, population, de tipo String
--    * residents, de tipo lista de String

-- 2. Haga que el tipo anterior disponga de un valor por defecto,
-- de modo que podamos posteriormente crear elementos del tipo
-- sin necesidad de proporcionar todos los datos solicitados

-- 3. Defina un tipo sinónimo de una lista de planetas

-- 4. Realice un programa principal que:
--    a) Importe el archivo "planets.json",
--    b) Para cada planeta, imprima por pantalla su nombre,
--       seguido de su radio (la mitad de su diámetro)
--    ** Nota: si no puede resolver este apartado,
--             puede optar por un ejercicio simplificado,
--             por 1.5 puntos, que en lugar de "planets.json"
--             procese "planet.json", conteniendo un único
--             planeta, y devolviendo el nombre del mismo
--             junto con el número de residentes ilustres
--             (ver residents)
-- -------------------------------------------------------------------


data Planeta = Planeta {name :: String, diameter :: String, population :: String, residents :: [String]}
    deriving (Show, Eq)

instance Default Planeta where
    def = Planeta {name = "Nombre", diameter = "Diametro", population = "Poblacion", residents = def}

dagobah = def {name = "Dagobah", population = "1"}