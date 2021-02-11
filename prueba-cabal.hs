{-# LANGUAGE OverloadedStrings #-}
import CodeWorld
import Data.Text (pack)
import System.IO
import Data.Array
import Data.Default
import Data.List
import I1M.Pila -- Import añadido para la pila de enemigos
-- Modulos nuestros:
import Datos
import SistemaPeleas as SP


-- ---------******** TIPOS ********---------- --
-- El primer String será Integer y representará la línea del texto en la que nos encontramos "texto principal de la historia"
type World = (Integer, Integer, Datos.OpcionesH, SP.Personaje, Integer, Matriz Datos.Opciones, 
              [Datos.HistoriaCSV], (SP.Personaje,SP.Personaje), [Double], Integer, [String])


  -- Texto de la historia ; Fila para el aumento ; Las opciones de la historia ; El prota --
-- añadir 2 tipos relacionados con tooooodo el tema de los almacenes de variables y datos en el mundo inicial, y vamos sacando de ahí, así quitamos problemas futuros.

ninoPueblo :: SP.Personaje
ninoPueblo = SP.Pers "Pepe" (-1) 0 1 1 4.0

guerrero :: SP.Personaje
guerrero = def 


ultimoEn :: SP.Personaje
ultimoEn = Pers "Random Bro" 3 3 3 4 8.0


pilaEnemys :: Pila SP.Personaje
pilaEnemys = foldr apila vacia [ninoPueblo,guerrero,ultimoEn]


{-
data Personaje = Pers { nombre :: String, felicidad :: Double, talkNoJutsu :: Double, 
                        sanar :: Double, luchar :: Double, vida :: Double }
                deriving (Show)

instance Default Personaje where
        def = Pers { nombre = "nombreDefecto" , felicidad = def , talkNoJutsu = 1.0 , 
            sanar = 1.0 , luchar = 1.0, vida = 3.0}
-}

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
      return (1, 0, ("","",""), kal, 0, datosAumento', hCSV', (py,en), rands ,0, text)
      where py = kal
            en = kal

kal:: SP.Personaje
kal = SP.Pers "Kal" (-1) 1 2 1 10

  -- Evento
evento:: Event -> World -> World
evento (KeyPress k) mundo@(texto, filaAumento, opciones, personaje, tipoActual, datosAumento', hCSV',(py,en), rands,estaEnC, text)  = case tipoActual of
          0 -> case k of
                "1" -> (texto0, 1, opcionesHistoria0, kal, 1, datosAumento', hCSV',(py,en), rands,estaEnC, text)
                  where (texto0, opcionesHistoria0, _) = hCSV'!!0
                _ -> mundo
                  
          1 -> case k of
                "1" -> (updateWorld filaAumento 1 mundo)
                "2" -> (updateWorld filaAumento 2 mundo)
                "3" -> (updateWorld filaAumento 3 mundo) 
                _   -> mundo
          2 -> case k of
                "1" -> (getCombate mundo 1)
                "2" -> (getCombate mundo 2)
                "3" -> (getCombate mundo 3)
                "4" -> (getCombate mundo 4)
                _ -> mundo
          3 -> case k of
                "Enter" -> (1, 0, ("","",""), kal, 0, datosAumento', hCSV', (py,en), rands ,0, text) -- Volver a la pantalla de inicio
                    where py = kal
                          en = kal
                "R" -> (texto0, 1, opcionesHistoria0, kal, 1, datosAumento', hCSV',(py,en), rands,estaEnC, text) -- Volver a empezar otra partida
                    where (texto0, opcionesHistoria0, _) = hCSV'!!0
                _ -> mundo
          4 -> case k of
                "Enter" -> (1, 0, ("","",""), kal, 0, datosAumento', hCSV', (py,en), rands ,0, text) -- Volver a la pantalla de inicio
                    where py = kal
                          en = kal
                "R" -> (texto0, 1, opcionesHistoria0, kal, 1, datosAumento', hCSV',(py,en), rands,estaEnC, text) -- Volver a empezar otra partida
                    where (texto0, opcionesHistoria0, _) = hCSV'!!0
                _ -> mundo
          _ -> mundo
evento _ mundo = mundo 

historiaCSV:: IO [Datos.HistoriaCSV]
historiaCSV  = readerHistory

datosAumento:: IO (Matriz Datos.Opciones)
datosAumento =lectorFicheroAumento

-- FICHEROS

updateWorld:: Integer -> Int  -> World -> World
updateWorld filaA columna mundo@(texto, sFila, opciones, personaje, tipoA, datosAumento', hCSV',(py,en), rands,estaEnC, text) = (texto, siguienteFilaHistoria, opcionesH, personaje', tipoA, datosAumento', hCSV',(py,en), rands,estaEnC, text)
  where   (tipoA, siguienteFilaHistoria, valor, habilidad) = seleccionaElemento (fromIntegral filaA) columna datosAumento'
          (texto,opcionesH,_) = sacoTexto (siguienteFilaHistoria - 1) tipoA hCSV'
          personaje' = SP.modificaStat (fromIntegral habilidad) (fromIntegral valor) personaje

        
sacoTexto:: Integer -> Integer -> [Datos.HistoriaCSV] -> Datos.HistoriaCSV
sacoTexto fila tipoA hCSV' = case tipoA of
              0 -> (0, ("ataque", "defensa", "insulto"),1)
              1 -> hCSV'!!(fromIntegral fila)--historiaCSV!!(fromIntegral fila)
              2 -> hCSV'!!(fromIntegral fila)

-- DIBUJO

drawWorld:: World -> Picture
drawWorld mundo@(texto, sFila, opciones, personaje, tipoA, datosAumento', hCSV',(py,en), rands,estaEnC, text) = 
  case tipoA of
    0 -> {- startDraw -} lettering (pack (show texto)) <> personaje' <> colored (red) (solidCircle 1) -- <> texto3
    1 -> {-textDraw  -} lettering (pack (show (personaje, texto))) <> personaje' <> colored (blue) (solidCircle 1)
    2 -> {-combatDraw -} lettering (pack "Estamos en una PELEA") <> personaje' <> colored (green) (solidCircle 1) <> texto3 <> texto4
    3 -> {-goDraw -} lettering (pack (show (personaje, "HAS PERDIDO"))) <> colored (yellow) (solidCircle 1)
    4 -> {-winDraw -} lettering (pack (show (personaje, "HAS GANADO"))) <> colored (pink) (solidCircle 1)
    where personaje' = translated (0) (3) (scaled 0.5 0.5 texto2)
          texto2 = (lettering (pack( show (sFila, tipoA, estaEnC, texto))))
          texto3 = translated (0) (4) (lettering (pack( "1 - Pierde Vida"++"2- Algo"++"3 insulto a alguien"++"me la pela")))
          texto4 = scaled 0.5 0.5 $ translated (0) (5) (lettering (pack( show (py, en))))




-- COMBATE
{-

Modificaciones que le he hecho al mundo:
  - Una tupla que representa el combate, contiene al player y al enemigo correspondiente al combate
  - Una lista llena de números aleatorios 
  - Una variable entera que indica si está en combate o no

-}

getCombate :: World -> Integer -> World
getCombate mundo@(texto, sFila, opciones, personaje, tipoA, datosAumento', hCSV',(py,en), rands,estaEnC, text) accion =
        case estaEnC of
          0 -> (texto, sFila, opciones, personaje, tipoA , datosAumento', hCSV',combt', rands',estaEnC', text)
          1 -> getCombateAux mundo py' en' rands'
          where combt' = SP.ejecutaAccion personaje accion (selEnem (ceiling (fromIntegral (texto `div` 3))) pilaEnemys) ae (head rands)
                ae = SP.accionAleatoria (head (tail rands))
                estaEnC' = 1
                rands' = tail $ tail rands
                (py',en') = SP.ejecutaAccion py accion en ae (head rands)


getCombateAux :: World -> SP.Personaje -> SP.Personaje -> [Double] -> World
getCombateAux mundo@(texto, sFila, opciones, personaje, tipoA, datosAumento', hCSV',(py,en), rands,estaEnC, text) player enemy rAct
          | finComb == 1 = (texto,sFila, opciones, personaje, 1, datosAumento', hCSV', (personaje,enemy), rAct, 0, text)
          | finComb == 2 = (texto,sFila, opciones, personaje, 2, datosAumento', hCSV', (player,enemy), rAct, 1, text)
          | finComb == 3 = (texto,sFila, opciones, personaje, 3, datosAumento', hCSV', (player,enemy), rAct, 0, text)
            where (finComb,_) = SP.finalCombate player enemy


selEnem :: Integer -> Pila SP.Personaje -> SP.Personaje
selEnem 1 pila = cima pila
selEnem n pila = selEnem (n-1) (desapila pila)
