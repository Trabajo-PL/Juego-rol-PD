{-# LANGUAGE OverloadedStrings #-}
import CodeWorld
import Data.Text (pack)
import System.IO
import Data.Array
import Data.Default
import Data.List
-- Modulos nuestros:
import Datos
import SistemaPeleas as SP
import Draws as Dr


-- ---------******** TIPOS ********---------- --

data World = World {rowH :: Integer, rowPA:: Integer, optiosH:: Datos.OpcionesH, principalC:: SP.Personaje, actualT:: Integer, powerUps:: Matriz Datos.Opciones,
                      dataH:: [Datos.HistoriaCSV], battle:: (SP.Personaje, SP.Personaje), randoms:: [Double], inCombat:: Integer, textHistory:: [String]}


type Matriz a = Array (Int,Int) Datos.Opciones

-- EL MAIN

main = do
  initialWorld' <- initialWorld
  activityOf initialWorld' evento drawWorld

  -- Mundo inicial -> Inicio del juego
initialWorld:: IO World
initialWorld = do
      hCSV' <- historiaCSV
      datosAumento' <- datosAumento
      as <-  SP.aleatorio
      text <- Datos.readerText
      let rands = as
      let world = World 1 0 ("","","") kal 0 datosAumento' hCSV' (py,en) rands 0 text
      return world
      where py = kal
            en = kal

kal:: SP.Personaje
kal = SP.Pers "Kal" (-1) 1 2 1 10

  -- Evento
evento:: Event -> World -> World
evento (KeyPress k) world = case (actualT world) of
          0 -> case k of
                "Enter" -> firstWorld
                _ -> world
                  
          1 -> case k of
                "1" -> (updateWorld (rowPA world) 1 world)
                "2" -> (updateWorld (rowPA world) 2 world)
                "3" -> (updateWorld (rowPA world) 3 world)
                "Esc" -> world {actualT = 5} 
                _   -> world
          2 ->case (inCombat world) of 
                  0 -> case k of
                        "Enter" -> world {battle = (kal, nextEnemy), inCombat = 1}
                        _ -> world
                        where nextEnemy = SP.selEnem (ceiling (fromIntegral ((rowH world) `div` 3)))
                  1 -> case k of
                        "1" -> (getCombate world 1)
                        "2" -> (getCombate world 2)
                        "3" -> (getCombate world 3)
                        "4" -> (getCombate world 4)
                        "Esc" -> world {actualT = 6}
                        _ -> world
                  2 -> case k of
                        "Enter" -> world {actualT = 1, inCombat = 0}
                        _ -> world
                  _ -> world
          3 -> case k of
                "Enter" -> iniWorld
                "R" ->  firstWorld
                _ -> world
          4 -> case k of
                "Enter" -> iniWorld
                "R" ->  firstWorld
                _ -> world
          5 -> case k of
                "Esc" -> world {actualT = 1}
                "Enter" -> iniWorld
                "R" ->  firstWorld
                _ -> world
          6 -> case k of
                "Esc" -> world {actualT = 2}
                "Enter" -> iniWorld
                "R" ->  firstWorld
                _ -> world
          _ -> world
          where iniWorld = world {rowH = 1, rowPA = 0, optiosH = ("","",""), principalC = kal, actualT = 0, battle = (kal, kal), inCombat = 0}
                  where py = kal
                        en = kal
                firstWorld = world {rowH = nextR, rowPA = 1, optiosH = optiosH', actualT = 1}
                  where (nextR, optiosH', _) = (dataH world)!!0
evento _ world = world 

historiaCSV:: IO [Datos.HistoriaCSV]
historiaCSV  = readerHistory

datosAumento:: IO (Matriz Datos.Opciones)
datosAumento =lectorFicheroAumento

-- FICHEROS

updateWorld:: Integer -> Int  -> World -> World
updateWorld actualR col world = world {rowH = nextR, rowPA = nextH, optiosH = optionsH', principalC = principalC', actualT = actualT'}
  where   (actualT', nextH, value, hability) = seleccionaElemento (fromIntegral actualR) col (powerUps world)
          (nextR,optionsH',_) = getTexto (nextH - 1) actualT' (dataH world)
          principalC' = SP.modificaStat (fromIntegral hability) (fromIntegral value) (principalC world)

        
getTexto:: Integer -> Integer -> [Datos.HistoriaCSV] -> Datos.HistoriaCSV
getTexto fila tipoA hCSV' = case tipoA of
              0 -> (0, ("ataque", "defensa", "insulto"),1)
              1 -> hCSV'!!(fromIntegral fila)--historiaCSV!!(fromIntegral fila)
              2 -> hCSV'!!(fromIntegral fila)

-- DIBUJO

drawWorld:: World -> Picture
drawWorld world = 
  case (actualT world) of
    0 -> Dr.initDraw --{- startDraw -} lettering (pack (show (rowH world))) <> colored (red) (solidCircle 1) 
    1 -> Dr.textDraw optionsH' statsC textH'  
        where   statsC = SP.statsCaracter (principalC world)
                optionsH' = (optiosH world)
                textH' = (textHistory world)!!(fromIntegral (rowH world)-1)
    2 -> case (inCombat world) of
          0 -> intoBDraw "Vas a entrar en batalla" "Pulsa ENTER para comenzar" (light blue)
          1 -> combatDraw (SP.statsCaracter py) (SP.statsCaracter en)
            where   (py, en) = (battle world)
          2 -> intoBDraw "Has ganado la batalla" "Pulsa ENTER para continuar" (light green) 
    3 -> Dr.endDraw "Has Perdido" (dark (dark gray)) 
    4 -> Dr.endDraw "Has Ganado" (light (light blue))
    5 -> Dr.resumeDraw 
    6 -> Dr.resumeDraw

-- COMBATE

getCombate :: World -> Integer -> World
getCombate world action = getCombateAux world py' en' rands'
          where ae = SP.accionAleatoria (head (tail (randoms world)))
                nextEnemy = SP.selEnem (ceiling (fromIntegral ((rowH world) `div` 3)))
                rands' = tail $ tail (randoms world)
                (py, en) = (battle world)
                (py',en') = SP.ejecutaAccion py action en ae (head (randoms world))

getCombateAux :: World -> SP.Personaje -> SP.Personaje -> [Double] -> World
getCombateAux world player enemy rAct
          | finComb == 1 = world {actualT = 2, battle  = ((principalC world), enemy), randoms = rAct, inCombat = 2}
          | finComb == 2 = world {actualT = 2, battle  = (player, enemy), randoms = rAct, inCombat = 1}
          | finComb == 3 = world {actualT = 3, battle  = ((principalC world), enemy), randoms = rAct, inCombat = 0}
            where (finComb,_) = SP.finalCombate player enemy
