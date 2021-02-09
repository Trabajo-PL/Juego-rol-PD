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


-- ---------******** TIPOS ********---------- --
-- El primer String será Integer y representará la línea del texto en la que nos encontramos "texto principal de la historia"
type World = (Integer, Integer, Datos.OpcionesH, Personaje, Integer, Matriz Datos.Opciones, [Datos.HistoriaCSV])
  -- Texto de la historia ; Fila para el aumento ; Las opciones de la historia ; El prota --
-- añadir 2 tipos relacionados con tooooodo el tema de los almacenes de variables y datos en el mundo inicial, y vamos sacando de ahí, así quitamos problemas futuros.
data Personaje = Pers { nombre :: String, felicidad :: Double, talkNoJutsu :: Double, 
                        sanar :: Double, luchar :: Double }
                deriving (Show)
instance Default Personaje where
        def = Pers { nombre = "nombreDefecto" , felicidad = def , talkNoJutsu = def , 
            sanar = def , luchar = def}

type Matriz a = Array (Int,Int) Datos.Opciones
-- EL MAIN

main = do
  mundoInicial' <- mundoInicial
  activityOf mundoInicial' evento dibujo

  -- Mundo inicial -> Inicio del juego
mundoInicial:: IO World
mundoInicial = do
      historiaCSV' <- historiaCSV
      datosAumento' <- datosAumento 
      return (texto, filaAumento, opcionesHistoria, prota, tipoActual, datosAumento', historiaCSV')
      where texto = 1
            filaAumento = 0
            opcionesHistoria = ("","","")
            prota = kal
            tipoActual = 0

kal = Pers {nombre = "Pepe", felicidad = -1, talkNoJutsu = 0, sanar = 0, luchar = 1}

  -- Evento
evento:: Event -> World -> World
evento (KeyPress k) mundo@(texto, filaAumento, opciones, personaje, tipoActual, datosAumento', historiaCSV')  = case tipoActual of
          0 -> case k of
                "1" -> (texto0, 1, opcionesHistoria0, kal, 1, datosAumento', historiaCSV')
                  where (texto0, opcionesHistoria0, _) = (1,("Jugar con Juan","Ayudar a tu padre","Practicar con la lanza"),0)
                _ -> mundo
                  
          1 -> case k of
                "1" -> (funcionW filaAumento 1 historiaCSV' datosAumento')
                "2" -> (funcionW filaAumento 2 historiaCSV' datosAumento')
                "3" -> (funcionW filaAumento 3 historiaCSV' datosAumento') 
                _   -> mundo
          2 -> case k of
                "1" -> mundo
                "2" -> mundo
                "3" -> mundo
                "4" -> mundo
                _ -> mundo
          _ -> mundo
evento _ mundo = mundo 

historiaCSV:: IO [Datos.HistoriaCSV]
historiaCSV  = readerHistory

datosAumento:: IO (Matriz Datos.Opciones)
datosAumento =lectorFicheroAumento

-- FICHEROS

funcionW:: Integer -> Int  -> [Datos.HistoriaCSV] -> Matriz Datos.Opciones -> World
funcionW filaA columna historiaCSV' datosAumento' = (texto, siguienteFilaHistoria, opcionesH, kal, tipoA, datosAumento', historiaCSV')
  where   (tipoA, siguienteFilaHistoria, valor, habilidad) = seleccionaElemento (fromIntegral filaA) columna datosAumento'
          (texto,opcionesH,_) = sacoTexto (siguienteFilaHistoria - 1) tipoA historiaCSV'

        
sacoTexto:: Integer -> Integer -> [Datos.HistoriaCSV] -> Datos.HistoriaCSV
sacoTexto fila tipoA historiaCSV' = case tipoA of
              0 -> (0, ("ataque", "defensa", "insulto"),1)
              1 -> historiaCSV'!!(fromIntegral fila)--historiaCSV!!(fromIntegral fila)
              2 -> (0, ("ataque", "defensa", "insulto"),1)
              

-- DIBUJO

dibujo:: World -> Picture
dibujo mundo@(texto, sFila, opciones, personaje, tipoA, datosAumento', historiaCSV') = 
  case tipoA of
    0 -> lettering (pack (show texto)) <> personaje' <> colored (red) (solidCircle 1) -- <> texto3
    1 -> lettering (pack (show texto)) <> personaje'
    2 -> lettering (pack (show texto)) <> personaje' <> colored (green) (solidCircle 1)  -- <> texto3
    where personaje' = translated (0) (3) (scaled 0.5 0.5 texto2)
          texto2 = (lettering (pack( show (sFila, tipoA))))
          texto3 = translated (0) (4) (lettering (pack( show (historiaCSV'!!0))))

{-
pila -> enemig1, enemigo2, enemigo3
head pila -> enemigo3 
elimina pila -> enemig1, enemigo2
actualizas enemigo3 -> enemigo3'
añade pila enemigo' -> enemig1, enemigo2, enemigo3'-}
