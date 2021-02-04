-------------------------------------------------Imports-----------------------------------------------------


import Data.Char
import Text.Printf
import System.IO
import System.Random
import Data.Default


--------------------------------------------------------------------------------------------------------------

-------------------------------------------------ANOTACIONES--------------------------------------------------
{-

Orden de habilidades
1 lucha
2 sanar
3 hablar
4 felicidad

-}

-------------------------------------------------Recursos-----------------------------------------------------


-- Estructura básica de las habilidades de un personaje


data Personaje = Pers { nombre :: String, felicidad :: Double, talkNoJutsu :: Double, 
                        sanar :: Double, luchar :: Double }
                deriving (Show)


-- Definición básica de un personaje por si se nos olvida definir alguno del todo bien


instance Default Personaje where
        def = Pers { nombre = "nombreDefecto" , felicidad = def , talkNoJutsu = def , 
            sanar = def , luchar = def}


-- Para acceder al atributo concreto de un campo del registro, primero se nombra el campo y después el regis-
-- tro definido. Por ejemplo, para acceder a la felicidad de este personaje -> felicidad niñoPueblo

niñoPueblo = Pers {nombre = "Pepe", felicidad = -1, talkNoJutsu = 0, sanar = 0, luchar = 1}


--------------------------------------------------------------------------------------------------------------


---------------------------------------------Funciones Auxiliares---------------------------------------------


--Devuelve un número aleatorio entre 0 y 1 de tipo double, usarlo en bloques "do" -> IO


aleatorio :: IO Double
aleatorio = getStdRandom (randomR (0,1))


-- Sólo una pequeña prueba para una idea de la división de los puntos conseguidos entre los totales, 
-- para las diferentes probabilidades 

{-
main = do
        let x = max 0 (5.0/6.0) -- Idea de ReLU por si un cálculo sale negativo 
        y <- aleatorio -- Así pasamos de IO Double a Double para tener soporte de comparadores
        putStrLn $ show y
        return (x > y)
-}


-- Obtener dato de un registro


obtenerFelicidad,obtenerSanar,obtenerLuchar,obtenerTnJ :: Personaje -> Double

obtenerFelicidad personaje = felicidad personaje

obtenerSanar personaje = sanar personaje

obtenerLuchar personaje = luchar personaje

obtenerTnJ personaje = talkNoJutsu personaje

-- Cambiar a case of
obtenerStat :: String -> Personaje -> Double
obtenerStat stat personaje
                        | stat == "Felicidad" = obtenerFelicidad personaje
                        | stat == "Sanar" = obtenerSanar personaje
                        | stat == "Luchar" = obtenerLuchar personaje
                        | otherwise = obtenerTnJ personaje

{-
main  = do
        let kal = def {nombre="Kal", luchar = 5}
        putStrLn $ show kal -- Para mostrar el registro personaje meterlo dentro de un Show
        return (obtenerStat "Luchar" kal)
-}


-- Modificar un dato de un registro

modificarFelicidad,modificarSanar,modificarLuchar,modificarTnJ :: Double -> Personaje -> Personaje

modificarFelicidad act personaje = personaje { felicidad = act + (felicidad personaje) }

modificarSanar act personaje = personaje { sanar = act + (sanar personaje) }

modificarLuchar act personaje = personaje { luchar = act + (luchar personaje) }

modificarTnJ act personaje = personaje { talkNoJutsu = act + (talkNoJutsu personaje) }


modificaStat :: String -> Double -> Personaje -> Personaje
modificaStat stat val personaje
                        | stat == "Felicidad" = modificarFelicidad val personaje
                        | stat == "Sanar" = modificarSanar val personaje
                        | stat == "Luchar" = modificarLuchar val personaje
                        | otherwise = modificarTnJ val personaje

-- De nuevo lo siguiente es una prueba para probar un poco el testeo y ver cómo funcionan las cosas

{-
main  = do
        let kal = def {nombre="Kal"}
        putStrLn $ show kal -- Para mostrar el registro personaje meterlo dentro de un Show
        let kal' = modificaStat "Felicidad" 2 kal -- Buscar la forma de modificar la misma variable sin que se forme un bucle infinito
        let kal = kal'
        putStrLn $ show kal'
        let kal' = modificaStat "Felicidad" 2 kal
        let kal = kal'
        return kal
-}


--------------------------------------------------------------------------------------------------------------
