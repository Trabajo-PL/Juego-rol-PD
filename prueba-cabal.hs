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
type World = (Integer, Integer, Datos.OpcionesH, SP.Personaje, Integer, Matriz Datos.Opciones, 
              [Datos.HistoriaCSV], (SP.Personaje,SP.Personaje), [Double], Integer)


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
            py = kal
            en = kal

kal:: SP.Personaje
kal = SP.Pers "Kal" (-1) 1 2 1 10

  -- Evento
evento:: Event -> World -> World
evento (KeyPress k) mundo@(texto, filaAumento, opciones, personaje, tipoActual, datosAumento', hCSV',(py,en), rands,estaEnC)  = case tipoActual of
          0 -> case k of
                "1" -> (texto0, 1, opcionesHistoria0, kal, 1, datosAumento', hCSV',(py,en), rands,estaEnC)
                  where (texto0, opcionesHistoria0, _) = (1,("Jugar con Juan","Ayudar a tu padre","Practicar con la lanza"),0)
                _ -> mundo
                  
          1 -> case k of
                "1" -> (funcionW filaAumento 1 mundo)
                "2" -> (funcionW filaAumento 2 mundo)
                "3" -> (funcionW filaAumento 3 mundo) 
                _   -> mundo
          2 -> case k of
                "1" -> (getCombate mundo 1)
                "2" -> (getCombate mundo 2)
                "3" -> (getCombate mundo 3)
                "4" -> (getCombate mundo 4)
                _ -> mundo
          3 -> case k of
                "1" -> mundo
                _ -> mundo
          _ -> mundo
evento _ mundo = mundo 

historiaCSV:: IO [Datos.HistoriaCSV]
historiaCSV  = readerHistory

datosAumento:: IO (Matriz Datos.Opciones)
datosAumento =lectorFicheroAumento

-- FICHEROS

funcionW:: Integer -> Int  -> World -> World
funcionW filaA columna mundo@(texto, sFila, opciones, personaje, tipoA, datosAumento', hCSV',(py,en), rands,estaEnC) = (texto, siguienteFilaHistoria, opcionesH, kal, tipoA, datosAumento', hCSV',(py,en), rands,estaEnC)
  where   (tipoA, siguienteFilaHistoria, valor, habilidad) = seleccionaElemento (fromIntegral filaA) columna datosAumento'
          (texto,opcionesH,_) = sacoTexto (siguienteFilaHistoria - 1) tipoA hCSV'

        
sacoTexto:: Integer -> Integer -> [Datos.HistoriaCSV] -> Datos.HistoriaCSV
sacoTexto fila tipoA hCSV' = case tipoA of
              0 -> (0, ("ataque", "defensa", "insulto"),1)
              1 -> hCSV'!!(fromIntegral fila)--historiaCSV!!(fromIntegral fila)
              2 -> hCSV'!!(fromIntegral fila)
              

-- DIBUJO

dibujo:: World -> Picture
dibujo mundo@(texto, sFila, opciones, personaje, tipoA, datosAumento', hCSV',(py,en), rands,estaEnC) = 
  case tipoA of
    0 -> lettering (pack (show texto)) <> personaje' <> colored (red) (solidCircle 1) -- <> texto3
    1 -> lettering (pack (show texto)) <> personaje' <> colored (blue) (solidCircle 1)
    2 -> lettering (pack "Estamos en una PELEA") <> personaje' <> colored (green) (solidCircle 1) <> texto3 <> texto4
    3 -> lettering (pack "HAS PERDIDO") <> colored (yellow) (solidCircle 1)
    where personaje' = translated (0) (3) (scaled 0.5 0.5 texto2)
          texto2 = (lettering (pack( show (sFila, tipoA, estaEnC))))
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
getCombate mundo@(texto, sFila, opciones, personaje, tipoA, datosAumento', hCSV',(py,en), rands,estaEnC) accion =
        case estaEnC of
          0 -> (texto, sFila, opciones, personaje, tipoA , datosAumento', hCSV',combt', rands',estaEnC')
          1 -> getCombateAux mundo py' en' rands'
          where combt' = SP.ejecutaAccion personaje accion (selEnem (ceiling (fromIntegral (texto `div` 3))) pilaEnemys) ae (head rands)
                ae = SP.accionAleatoria (head (tail rands))
                estaEnC' = 1
                rands' = tail $ tail rands
                (py',en') = SP.ejecutaAccion py accion en ae (head rands)


getCombateAux :: World -> SP.Personaje -> SP.Personaje -> [Double] -> World
getCombateAux mundo@(texto, sFila, opciones, personaje, tipoA, datosAumento', hCSV',(py,en), rands,estaEnC) player enemy rAct
          | finComb == 1 = (texto,sFila, opciones, personaje, 1, datosAumento', hCSV', (personaje,enemy), rAct, 0)
          | finComb == 2 = (texto,sFila, opciones, personaje, 2, datosAumento', hCSV', (player,enemy), rAct, 1)
          | finComb == 3 = (texto,sFila, opciones, personaje, 3, datosAumento', hCSV', (player,enemy), rAct, 0)
            where (finComb,_) = SP.finalCombate player enemy


selEnem :: Integer -> Pila SP.Personaje -> SP.Personaje
selEnem 1 pila = cima pila
selEnem n pila = selEnem (n-1) (desapila pila)
