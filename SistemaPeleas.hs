-------------------------------------------------Modulo------------------------------------------------------

module SistemaPeleas
(
    ejecutaAccion,
    finalCombate,
    aleatorio,
    accionAleatoria,
    setStat,
    statsCaracter,
    selEnem,
    Personaje(Pers)
) where

-------------------------------------------------Imports-----------------------------------------------------
import I1M.Pila
import System.IO
import System.Random
import Data.Default

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
guerrero = Pers "Guerrero Juan" 1 1 2 2 4.0 

ultimoEn :: Personaje
ultimoEn = Pers "Final Boss" 3 2 1 4 8.0

pilaEnemys :: Pila Personaje
pilaEnemys = foldr apila vacia [ninoPueblo,guerrero,ultimoEn]

-- Declaración de los puntos totales de cada atributo

ptsFel :: Double
ptsFel = 8

--------------------------------------------------------------------------------------------------------------

---------------------------------------------Funciones Auxiliares---------------------------------------------

aleatorio :: IO [Double]
aleatorio = do 
        g <- getStdGen
        let fgen = randomRs (0, 1) g :: [Double]
        return fgen

getStat :: Integer -> Personaje -> Double
getStat stat personaje
                        | stat == 1 = luchar personaje
                        | stat == 2 = sanar personaje
                        | stat == 3 = talkNoJutsu personaje
                        | stat == 4 = felicidad personaje
                        | otherwise = vida personaje

-- Modificar un dato de un registro

setFelicidad,setSanar,setLuchar,setTnJ,setVida :: Double -> Personaje -> Personaje

setFelicidad act personaje = personaje { felicidad = max 0 (act + (felicidad personaje)) }

setSanar act personaje = personaje { sanar = act + (sanar personaje) }

setLuchar act personaje = personaje { luchar = act + (luchar personaje) }

setTnJ act personaje = personaje { talkNoJutsu = act + (talkNoJutsu personaje) }

setVida act personaje = personaje { vida = min 10 (act + (vida personaje)) }

setStat :: Integer -> Double -> Personaje -> Personaje
setStat stat val personaje = case stat of
                        1 -> setLuchar val personaje
                        2 -> setSanar val personaje
                        3 -> setTnJ val personaje
                        4 -> setFelicidad val personaje
                        5 -> setVida val personaje
                        _ -> personaje

--Ver si un combate ha finalizado y quién ha ganado si ha finalizado

finalCombate :: Personaje -> Personaje -> (Integer,String)
finalCombate player enemy
                        | ve <= 0 = (1,"Has ganado el combate")
                        | vp <= 0 = (3, "Has perdido el combate")
                        | otherwise = (2, "")
                                where vp = getStat 5 player
                                      ve = getStat 5 enemy

-- Esquiva, esta función calculará la probabilidad de esquivar del personaje en función a sus puntos 
-- de Felicidad sobre el total

esquiva :: Personaje -> Double -> Bool
esquiva personaje rand = ((getStat 4 personaje)/ptsFel) > rand

-- Recibe ataque, esta función se encargará de devolver el personaje actualizado si ha recibido el ataque de otro

recibeAtaque :: Personaje -> Personaje -> Double -> Personaje
recibeAtaque p1 p2 rand
                | esquiva p1 rand = p1
                | otherwise = setStat 5 (-(getStat 1 p2)) p1

-- Acción aleatoria, esta función tendrá un resultado entre 1 y 4 que dado un número aleatorio determinará la 
-- acción que llevará a cabo el enemigo

accionAleatoria :: Double -> Integer
accionAleatoria r = ceiling (r*4)

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
                                where cura p = setStat 5 (getStat 2 p) p
                                      tnj h o = setStat 4 (- getStat 3 h) o


statsCaracter:: Personaje -> (String, Double, Double, Double, Double, Double)
statsCaracter principalC = (name, figth, heal, talk, happy, health)
        where   name = (nombre principalC)
                figth = (luchar principalC)
                heal = (sanar principalC)
                talk = (talkNoJutsu principalC)
                happy = (felicidad principalC)
                health = (vida principalC)
                
                
               
selEnem :: Integer ->  Personaje
selEnem n = enem n pilaEnemys
            where enem 1 pila = cima pila
                  enem n pila = enem (n-1) (desapila pila)
               
               
--------------------------------------------------------------------------------------------------------------
