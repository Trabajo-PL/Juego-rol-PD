{-# LANGUAGE OverloadedStrings #-}
import CodeWorld
import Data.Char
import Text.Printf
import Data.Text (pack)
import System.IO
import Data.Array
import System.Random
import Data.Default
import Data.List
import Datos
import I1M.Pila -- Import añadido para la pila de enemigos
import SistemaPeleas as SP


-- ---------******** TIPOS ********---------- --
-- El primer String será Integer y representará la línea del texto en la que nos encontramos "texto principal de la historia"
type World = (Integer, Integer, Datos.OpcionesH, Personaje, Integer, Matriz Datos.Opciones, 
              [Datos.HistoriaCSV], (Personaje,Personaje), [Double], Integer)


  -- Texto de la historia ; Fila para el aumento ; Las opciones de la historia ; El prota --
-- añadir 2 tipos relacionados con tooooodo el tema de los almacenes de variables y datos en el mundo inicial, y vamos sacando de ahí, así quitamos problemas futuros.

ninoPueblo :: Personaje
ninoPueblo = Pers {nombre = "Pepe", felicidad = -1, talkNoJutsu = 0, sanar = 1, luchar = 1, vida = 4.0}


guerrero :: Personaje
guerrero = Pers {nombre = "Guerrero John", felicidad = 2, talkNoJutsu = 2, sanar = 2, luchar = 3, vida = 6.0}


ultimoEn :: Personaje
ultimoEn = Pers {nombre = "Random Bro", felicidad = 3, talkNoJutsu = 3, sanar = 3, luchar = 4, vida = 8.0}


pilaEnemys :: Pila Personaje
pilaEnemys = ninoPueblo|guerrero|ultimoEn|-


data Personaje = Pers { nombre :: String, felicidad :: Double, talkNoJutsu :: Double, 
                        sanar :: Double, luchar :: Double }
                deriving (Show)

instance Default Personaje where
        def = Pers { nombre = "nombreDefecto" , felicidad = def , talkNoJutsu = 1.0 , 
            sanar = 1.0 , luchar = 1.0, vida = 3.0}

type Matriz a = Array (Int,Int) Datos.Opciones


-- EL MAIN

main = do
  mundoInicial' <- mundoInicial
  activityOf mundoInicial' evento dibujo

  -- Mundo inicial -> Inicio del juego
mundoInicial:: IO World
mundoInicial = do
      hCSV' <- historiaCSV
      datosAumento' <- datosAumento
      as <-  SP.aleatorio
      let rands = take 500 as
      return (texto, filaAumento, opcionesHistoria, prota, tipoActual, datosAumento', hCSV', (py,en), rands ,estaEnC)
      where texto = 1
            filaAumento = 0
            opcionesHistoria = ("","","")
            prota = kal
            tipoActual = 0
            estaEnC = 0

kal = Pers {nombre = "Kal", felicidad = -1, talkNoJutsu = 0, sanar = 0, luchar = 1}

  -- Evento
evento:: Event -> World -> World
evento (KeyPress k) mundo@(texto, filaAumento, opciones, personaje, tipoActual, datosAumento', hCSV',(py,en), rands,estaEnC)  = case tipoActual of
          0 -> case k of
                "1" -> (texto0, 1, opcionesHistoria0, kal, 1, datosAumento', hCSV',(py,en), rands,estaEnC)
                  where (texto0, opcionesHistoria0, _) = (1,("Jugar con Juan","Ayudar a tu padre","Practicar con la lanza"),0)
                _ -> mundo
                  
          1 -> case k of
                "1" -> (funcionW filaAumento 1 hCSV' datosAumento')
                "2" -> (funcionW filaAumento 2 hCSV' datosAumento')
                "3" -> (funcionW filaAumento 3 hCSV' datosAumento') 
                _   -> mundo
          2 -> case k of
                "1" -> getCombate mundo 1
                "2" -> getCombate mundo 2
                "3" -> getCombate mundo 3
                "4" -> getCombate mundo 4
                _ -> mundo
          _ -> mundo
evento _ mundo = mundo 

historiaCSV:: IO [Datos.HistoriaCSV]
historiaCSV  = readerHistory

datosAumento:: IO (Matriz Datos.Opciones)
datosAumento =lectorFicheroAumento

-- FICHEROS

funcionW:: Integer -> Int  -> [Datos.HistoriaCSV] -> Matriz Datos.Opciones -> World
funcionW filaA columna hCSV' datosAumento' = (texto, siguienteFilaHistoria, opcionesH, kal, tipoA, datosAumento', hCSV',(py,en), rands,estaEnC)
  where   (tipoA, siguienteFilaHistoria, valor, habilidad) = seleccionaElemento (fromIntegral filaA) columna datosAumento'
          (texto,opcionesH,_) = sacoTexto (siguienteFilaHistoria - 1) tipoA hCSV'

        
sacoTexto:: Integer -> Integer -> [Datos.HistoriaCSV] -> Datos.HistoriaCSV
sacoTexto fila tipoA hCSV' = case tipoA of
              0 -> (0, ("ataque", "defensa", "insulto"),1)
              1 -> hCSV'!!(fromIntegral fila)--historiaCSV!!(fromIntegral fila)
              2 -> (0, ("ataque", "defensa", "insulto"),1)
              

-- DIBUJO

dibujo:: World -> Picture
dibujo mundo@(texto, sFila, opciones, personaje, tipoA, datosAumento', hCSV',(py,en), rands,estaEnC) = 
  case tipoA of
    0 -> lettering (pack (show texto)) <> personaje' <> colored (red) (solidCircle 1) -- <> texto3
    1 -> lettering (pack (show texto)) <> personaje'
    2 -> lettering (pack (show texto)) <> personaje' <> colored (green) (solidCircle 1)  -- <> texto3
    where personaje' = translated (0) (3) (scaled 0.5 0.5 texto2)
          texto2 = (lettering (pack( show (sFila, tipoA))))
          texto3 = translated (0) (4) (lettering (pack( show (hCSV'!!0))))




-- COMBATE
{-

Modificaciones que le he hecho al mundo:
  - Una tupla que representa el combate, contiene al player y al enemigo correspondiente al combate
  - Una lista llena de números aleatorios 
  - Una variable entera que indica si está en combate o no

-}

getCombate :: World -> Integer -> World
getCombate mundo@(texto, sFila, opciones, personaje, tipoA, datosAumento', hCSV',(py,en), rands,estaEnC) accion =
        case estaEnC of
          0 -> (texto, sFila, opciones, personaje, tipoA, datosAumento', hCSV',combt', rands',estaEnC')
          1 -> getCombateAux mundo py' en' rands'
            where combt' = SP.ejecutaAccion personaje accion (selEnem (ceiling (texto/3)) pilaEnemys) ae (head rands)
                  ae = SP.accionAleatoria (head (tail rands))
                  estaEnC' = 1
                  rands' = tail $ tail rands
                  (py',en') = SP.ejecutaAccion py accion en ae (head rands)


getCombateAux :: World -> Personaje -> Personaje -> Double -> World
getCombate mundo@(texto, sFila, opciones, personaje, tipoA, datosAumento', hCSV',(py,en), rands,estaEnC) player enemy rAct
          | finComb == 1 = (texto,sFila, opciones, personaje, tipoA, datosAumento', hCSV', (personaje,enemy), rAct, estaEnC')
          | finComb == 2 = (texto,sFila, opciones, personaje, tipoA, datosAumento', hCSV', (player,enemy), rAct, estaEnC)
          | finComb == 3 = (texto,sFila, opciones, personaje, tipoA', datosAumento', hCSV', (player,enemy), rAct, estaEnC')
            where estaEnC' = 0
                  tipoA' = 3


selEnem :: Integer -> Pila -> Personaje
selEnem 1 pila = cima pila
selEnem n pila = selEnem (n-1) (desapila pila)
