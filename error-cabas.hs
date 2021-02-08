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
import System.IO.Unsafe


-- ---------******** TIPOS ********---------- --
type World = (String, Integer, Datos.OpcionesH, Personaje, Integer)
  -- Texto de la historia ; Fila para el aumento ; Las opciones de la historia ; El prota --

data Personaje = Pers { nombre :: String, felicidad :: Double, talkNoJutsu :: Double, 
                        sanar :: Double, luchar :: Double }
                deriving (Show)
instance Default Personaje where
        def = Pers { nombre = "nombreDefecto" , felicidad = def , talkNoJutsu = def , 
            sanar = def , luchar = def}
            
--type OpcionesH = (String, String, String)

--type Opciones = (Integer,Integer, Integer, Integer)

type ValorYHabilidad = (Integer, Integer)

-- type HistoriaCSV = (String, OpcionesH, Integer)

type Matriz a = Array (Int,Int) Datos.Opciones
-- EL MAIN
main = do
  print "he entrado al menos en el MAIN"
  historiaCSV' <- historiaCSV
  datosAumento' <- datosAumento
  evento <- eventoIO
  activityOf mundoInicial evento dibujo
  return()

  -- Mundo inicial -> Inicio del juego
mundoInicial:: World
mundoInicial = (texto, filaAumento, opcionesHistoria, prota, tipoActual)
  where texto = "Inicio del juego"
        filaAumento = 0
        opcionesHistoria = ("","","")
        prota = kal
        tipoActual = 0

kal = Pers {nombre = "Pepe", felicidad = -1, talkNoJutsu = 0, sanar = 0, luchar = 1}

  -- Evento
eventoIO::IO(Event -> World -> World)
eventoIO  = do
  print "he entrado al menos en el eventoIO"
  historiaCSV' <- historiaCSV
  datosAumento' <- datosAumento
  --mundo' <-
  evento' historiaCSV' datosAumento' 
  -- return(mundo')
-- @(texto, filaAumento, opciones, personaje, tipoActual) 
evento':: [Datos.HistoriaCSV] -> Matriz Datos.Opciones  -> IO (Event -> World -> World)
evento' datosAumento' historiaCSV' = do
    print "he entrado al menos"
    let mundo' (KeyPress k) mundo@(texto, filaAumento, opciones, personaje, tipoActual)  = case tipoActual of
          0 -> case k of
                "1" -> (texto0, 1, opcionesHistoria0, kal, 1)
                  where (texto0, opcionesHistoria0, _) = ("1",("Jugar con Juan","Ayudar a tu padre","Practicar con la lanza"),0)
                _ -> mundo
                  
          1 -> case k of
                "1" -> (funcionW filaAumento 1 datosAumento' historiaCSV')
                "2" -> (funcionW filaAumento 2 datosAumento' historiaCSV')
                "3" -> (funcionW filaAumento 3 datosAumento' historiaCSV') 
                _ -> mundo
          2 -> case k of
                "1" -> mundo
                _ -> mundo
          _ -> mundo
    let mundo' _ mundo = mundo
    return (mundo')

-- evento'':: (Event -> World -> World)
-- evento'' mundo = undefined

historiaCSV:: IO [Datos.HistoriaCSV]
historiaCSV  = readerHistory

datosAumento:: IO (Matriz Datos.Opciones)
datosAumento =lectorFicheroAumento

-- FICHEROS
{-
historiaCSV:: [HistoriaCSV]
historiaCSV = [("1",("Jugar con Juan","Ayudar a tu padre","Practicar con la lanza"),0),("2",("Levantarse y pegarle con la lanza","Intentar hablar con \9500\174l e intimidarlo","Huir r\9500\237pido para no que no le haga da\9500\9618o"),1),("3",("No hago nada y dejo morir a mi hermano","Me levanto y me presento voluntario para salvarlo","Me levanto y salto a pegar a Cayetano y me reclutan tambi\9500\174n por agresi\9500\9474n"),2)]

              
lectorFicheroAumento:: Matriz Integer
lectorFicheroAumento = array ((1,1),(3,3)) [((1,1),(1,3,1,4)),((1,2),(1,3,1,2)),((1,3),(1,2,0,0)),((2,1),(2,3,2,3)),((2,2),(1,3,1,1)),((2,3),(1,-4,-1,4)),((3,1),(3,0,0,0)),((3,2),(1,4,0,0)),((3,3),(1,4,1,4))]

type Vector1 a = Array Int Opciones

listaVector :: [Opciones] -> Vector1 Opciones
listaVector xs = array (1,n) (zip [1..n] xs)
    where   n = length xs

type Matriz a = Array (Int,Int) Opciones

listaMatriz :: [[Opciones]] -> Matriz Opciones
listaMatriz xss = listArray ((1,1),(n,m)) (concat xss)
    where   n = length xss
            m = length (xss!!0)

    -- Función para seleccionar directamente el elemento que necesitemos.
seleccionaElemento:: Int -> Int -> Matriz Opciones -> Opciones
seleccionaElemento f c m = m ! (f,c)

-- -}
funcionW:: Integer -> Int  -> [Datos.HistoriaCSV] -> Matriz Datos.Opciones -> World
funcionW filaA columna historiaCSV' datosAumento' = (texto, siguienteFilaHistoria, opcionesH, kal, tipoA)
  where   (tipoA, siguienteFilaHistoria, valor, habilidad) = seleccionaElemento (fromIntegral filaA) columna datosAumento'
          (texto,opcionesH,_) = sacoTexto (siguienteFilaHistoria - 1) tipoA historiaCSV'

        
sacoTexto:: Integer -> Integer -> [Datos.HistoriaCSV] -> Datos.HistoriaCSV
sacoTexto fila tipoA historiaCSV' = case tipoA of
              0 -> ("texto NADA", ("ataque", "defensa", "insulto"),1)
              1 -> historiaCSV'!!(fromIntegral fila)--historiaCSV!!(fromIntegral fila)
              2 -> ("texto pelea", ("ataque", "defensa", "insulto"),1)
              

-- DIBUJO

dibujo:: World -> Picture
dibujo mundo@(texto, sFila, opciones, personaje, tipoA) = 
  case tipoA of
    0 -> lettering (pack texto) <> personaje' <> colored (red) (solidCircle 1) -- <> texto3
    1 -> lettering (pack texto) <> personaje'
    2 -> lettering (pack texto) <> personaje' <> colored (green) (solidCircle 1)  -- <> texto3
    where personaje' = translated (0) (3) (scaled 0.5 0.5 texto2)
          texto2 = (lettering (pack( show (sFila, tipoA))))
          -- texto3 = translated (0) (4) (lettering (pack( show (historiaCSV!!0))))
