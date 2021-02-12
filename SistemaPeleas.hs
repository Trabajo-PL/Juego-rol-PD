-------------------------------------------------Modulo------------------------------------------------------

module SistemaPeleas
(
    ejecutaAccion,
    finalCombate,
    aleatorio,
    accionAleatoria,
    modificaStat,
    statsCaracter,
    Personaje(Pers)
) where


-------------------------------------------------Imports-----------------------------------------------------


import Data.Char
import Text.Printf
import System.IO
import System.Random
import Data.Default
import I1M.Pila -- Import añadido para la pila de enemigos

--------------------------------------------------------------------------------------------------------------


-------------------------------------------------Recursos-----------------------------------------------------


-- Estructura básica de las habilidades de un personaje


data Personaje = Pers { nombre :: String, felicidad :: Double, talkNoJutsu :: Double, 
                        sanar :: Double, luchar :: Double, vida :: Double }
                deriving (Show)


-- Definición básica de un personaje por si se nos olvida definir alguno del todo bien


instance Default Personaje where
        def = Pers { nombre = "nombreDefecto" , felicidad = def , talkNoJutsu = 1.0 , 
            sanar = 1.0 , luchar = 1.0, vida = 3.0}


-- Para acceder al atributo concreto de un campo del registro, primero se nombra el campo y después el regis-
-- tro definido. Por ejemplo, para acceder a la felicidad de este personaje -> felicidad niñoPueblo

ninoPueblo :: Personaje
ninoPueblo = Pers "Pepe" 0 0 1 1 4.0

guerrero :: Personaje
guerrero = "Guerrero Juan" 1 1 2 2 4.0 

ultimoEn :: Personaje
ultimoEn = Pers "Random Bro" 2 1 2 3 6.0

pilaEnemys :: Pila SP.Personaje
pilaEnemys = foldr apila vacia [ninoPueblo,guerrero,ultimoEn]


-- Declaración de los puntos totales de cada atributo

ptsFel :: Double
ptsFel = 8


--------------------------------------------------------------------------------------------------------------


---------------------------------------------Funciones Auxiliares---------------------------------------------


--Devuelve un número aleatorio entre 0 y 1 de tipo double, usarlo en bloques "do" -> IO


--aleatorio :: IO Double
--aleatorio = getStdRandom (randomR (0,1))

aleatorio :: IO [Double]
aleatorio = do 
        g <- getStdGen
        let fgen = randomRs (0, 1) g :: [Double]
        return fgen


-- Sólo una pequeña prueba para una idea de la división de los puntos conseguidos entre los totales, 
-- para las diferentes probabilidades 

{-
main = do
        a <- aleatorio
        let as = take 500 a
        putStrLn $ show as
-}


-- Obtener dato de un registro


{-obtenerFelicidad,obtenerSanar,obtenerLuchar,obtenerTnJ :: Personaje -> Double

obtenerFelicidad personaje = felicidad personaje

obtenerSanar personaje = sanar personaje

obtenerLuchar personaje = luchar personaje

obtenerTnJ personaje = talkNoJutsu personaje-}


obtenerStat :: Integer -> Personaje -> Double
obtenerStat stat personaje
                        | stat == 1 = obtenerLuchar personaje 
                        | stat == 2 = obtenerSanar personaje
                        | stat == 3 = obtenerTnJ personaje
                        | stat == 4 = obtenerFelicidad personaje
                        | otherwise = obtenerVida personaje
                        where obtenerFelicidad personaje = felicidad personaje
                              obtenerSanar personaje = sanar personaje
                              obtenerLuchar personaje = luchar personaje
                              obtenerTnJ personaje = talkNoJutsu personaje
                              obtenerVida personaje = vida personaje



{-
main  = do
        let kal = def {nombre="Kal", luchar = 5}
        putStrLn $ show kal -- Para mostrar el registro personaje meterlo dentro de un Show
        return (obtenerStat "Luchar" kal)
-}


-- Modificar un dato de un registro

modificarFelicidad,modificarSanar,modificarLuchar,modificarTnJ,modificarVida :: Double -> Personaje -> Personaje

modificarFelicidad act personaje = personaje { felicidad = max 0 (act + (felicidad personaje)) }

modificarSanar act personaje = personaje { sanar = act + (sanar personaje) }

modificarLuchar act personaje = personaje { luchar = act + (luchar personaje) }

modificarTnJ act personaje = personaje { talkNoJutsu = act + (talkNoJutsu personaje) }

modificarVida act personaje = personaje { vida = min 10 (act + (vida personaje)) }


modificaStat :: Integer -> Double -> Personaje -> Personaje
modificaStat stat val personaje = case stat of
                        1 -> modificarLuchar val personaje
                        2 -> modificarSanar val personaje
                        3 -> modificarTnJ val personaje
                        4 -> modificarFelicidad val personaje
                        5 -> modificarVida val personaje
                        _ -> personaje


-- De nuevo lo siguiente es una prueba para probar un poco el testeo y ver cómo funcionan las cosas

{-
main  = do
        let kal = def {nombre="Kal"}
        putStrLn $ show kal -- Para mostrar el registro personaje meterlo dentro de un Show
        let kal' = modificaStat "Felicidad" 2 kal
        let kal = kal'
        return kal
-}


--Ver si un combate ha finalizado y quién ha ganado si ha finalizado


finalCombate :: Personaje -> Personaje -> (Integer,String)
finalCombate player enemy
                        | ve <= 0 = (1,"Has ganado el combate")
                        | vp <= 0 = (3, "Has perdido el combate")
                        | otherwise = (2, "")
                                where vp = obtenerStat 5 player
                                      ve = obtenerStat 5 enemy


--Testeo habitual de la función, vemos que al recibir un ataque, 
--si este nos deja a 0 de vida hemos perdido el combate

{-
main  = do
        let kal = def {nombre="Kal"}
        let (x,y) = finalCombate kal ninoPueblo
        putStrLn $ show $ (x,y)
        let kal' = modificaStat 5 (-3) kal
        return (finalCombate kal' ninoPueblo)
-}


-- Esquiva, esta función calculará la probabilidad de esquivar del personaje en función a sus puntos 
-- de Felicidad sobre el total

esquiva :: Personaje -> Double -> Bool
esquiva personaje rand = ((max 0 (obtenerStat 4 personaje))/ptsFel) > rand


-- Un pequeño test en el que vemos que si la felicidad es cercana al máximo, casi siempre se esquiva
{-
main = do
        let kal = def {nombre="Kal", felicidad=7}
        y <- aleatorio
        let x = (esquiva kal y)
        putStrLn $ show x
-}



-- Recibe ataque, esta función se encargará de devolver el personaje actualizado si ha recibido el ataque de otro

recibeAtaque :: Personaje -> Personaje -> Double -> Personaje
recibeAtaque p1 p2 rand
                | esquiva p1 rand = p1
                | otherwise = modificaStat 5 (-(obtenerStat 1 p2)) p1


-- Efectivamente la felicidad afecta a la probabilidad de esquivar el personaje que vaya a recibir el ataque
{-
main = do
        let kal = def {nombre="Kal", felicidad=0}
        y <- aleatorio
        let x = recibeAtaque kal ninoPueblo y
        putStrLn $ show x
---}


-- Acción aleatoria, esta función tendrá un resultado entre 1 y 4 que dado un número aleatorio determinará la 
-- acción que llevará a cabo el enemigo

accionAleatoria :: Double -> Integer
accionAleatoria r = ceiling (r*4)


-- Bueno mira, no quería ser pesado, pero hay que realizar testeos para comprobar que todo funciona de manera individual

{-
main = do
        a <- aleatorio
        let as = take 20 a
        let kal = def {nombre="Kal", felicidad=4, luchar=1}
        putStrLn $ show $ accionAleatoria $ as !! 2
        putStrLn $ show $ accionAleatoria $ as !! 3
        putStrLn $ show $ accionAleatoria $ as !! 4
        putStrLn $ show $ ejecutaAccion kal 1 ninoPueblo (accionAleatoria (head as)) (head (tail as))
-}


{-
En la siguiente función se especificará cómo afectan las acciones que realice el jugador en un combate
Cada acción tendrá un efecto relevante que veremos a continuación
Sí tenemos en cuenta el siguiente orden:
        1.- Ataque
        2.- Defensa
        3.- Hablar
        4.- Sanarse
-}


ejecutaAccion :: Personaje -> Integer -> Personaje -> Integer -> Double -> (Personaje,Personaje)
ejecutaAccion player ap enemy ae rand -- Acción player, Acción enemigo
                        | (ap == 1 && ae == 2) || (ap == 2 && ae == 1) = (player,enemy) --Si uno defiende y otro ataca se quedan igual
                        | ap == 2 && ae == 2 = (player,enemy) -- Si los dos defienden se quedan igual
                        | ap == 1 && ae == 1 = (recibeAtaque player enemy rand,recibeAtaque enemy player rand) -- Los dos atacan
                        | ap == 1 && ae == 3 = (tnj enemy player,recibeAtaque enemy player rand) -- El enemigo te baja la moral y tú le atacas
                        | ap == 1 && ae == 4 = (player,cura (recibeAtaque enemy player rand)) -- Tú le atacas y él se cura
                        | ap == 2 && ae == 3 = (tnj enemy player, enemy) -- Él te habla y tú te defiendes
                        | ap == 2 && ae == 4 = (player, cura enemy) -- Tú te defiendes y él se cura
                        | ap == 3 && ae == 1 = (recibeAtaque player enemy rand, tnj player enemy) -- Él te ataca y tú le bajas la moral
                        | ap == 3 && ae == 2 = (player, tnj player enemy) -- Él se defiende y tú le bajas la moral
                        | ap == 3 && ae == 3 = (tnj enemy player, tnj player enemy) -- Los dos os bajáis la moral
                        | ap == 3 && ae == 4 = (player, tnj player (cura enemy)) -- Él se cura y tú le bajas la moral
                        | ap == 4 && ae == 1 = (cura (recibeAtaque player enemy rand), enemy) -- Tú te curas y él te ataca
                        | ap == 4 && ae == 2 = (cura player, enemy) -- Tú te curas y él se defiende
                        | ap == 4 && ae == 3 = (tnj enemy (cura player),enemy) -- Tú te curas y él te baja la moral
                        | ap == 4 && ae == 4 = (cura player,cura enemy) -- Los dos os curáis
                                where cura p = modificaStat 5 (obtenerStat 2 p) p
                                      tnj h o = modificaStat 4 (- obtenerStat 3 h) o


-- Efectivamente, justo debajo, como siempre, otro test, sí, pruebo diferentes opciones...

{-
main = do
        let kal = def {nombre="Kal", felicidad=4, luchar=1}
        let nP = def {nombre="El Lolo", felicidad=0, luchar=1}
        rand <- aleatorio
        putStrLn $ show rand
        let (kal',nP') = ejecutaAccion kal 1 nP 1 rand
        putStrLn $ show (kal',nP')
        let (kal,nP) = ejecutaAccion kal' 1 nP' 3 rand
        putStrLn $ show (kal,nP)
        let (kal',nP') = ejecutaAccion kal 3 nP 1 rand
        putStrLn $ show (kal',nP')
        let (kal,nP) = ejecutaAccion kal' 4 nP' 1 rand
        putStrLn $ show (kal,nP)
        let (kal',nP') = ejecutaAccion kal 1 nP 1 rand
        putStrLn $ show (kal',nP')
        let (kal,nP) = ejecutaAccion kal' 1 nP' 1 rand
        putStrLn $ show (kal,nP)
        let (kal',nP') = ejecutaAccion kal 1 nP 1 rand
        putStrLn $ show (kal',nP')
---}
statsCaracter:: Personaje -> (String, Double, Double, Double, Double, Double)
statsCaracter principalC = (name, figth, heal, talk, happy, health)
        where   name = (nombre principalC)
                figth = (luchar principalC)
                heal = (sanar principalC)
                talk = (talkNoJutsu principalC)
                happy = (felicidad principalC)
                health = (vida principalC)
                
                
               
selEnem :: Integer -> Pila SP.Personaje-> SP.Personaje
selEnem n = enem n pilaEnemys
        where   enem 1 pila = cima pila 
                enem n pila = enem (n-1) (desapila pila)
               
               
--------------------------------------------------------------------------------------------------------------
