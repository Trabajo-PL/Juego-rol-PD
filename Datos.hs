module Datos
(
    lectorFicheroAumento,
    seleccionaElemento,
    readerHistory,
    readerText,
    HistoriaCSV,
    OpcionesH,
    Opciones,
    Matriz
) where


import Text.CSV
import Text.Printf
import Data.Array
import Data.List
import CodeWorld

type Opciones = (Integer,Integer, Integer, Integer)
type ValorYHabilidad = (Integer, Integer)
type OpcionesH = (String, String, String)
type HistoriaCSV = (Integer, OpcionesH, Integer)
---------------------------- Lectura del fichero CSV y pasar a Matriz ---------------------------

lectorFicheroAumento:: IO (Matriz Opciones)
lectorFicheroAumento = do
    let entrada = "Files\\aumentos.csv"
    contenido <- parseCSVFromFile entrada  -- cargar el contenido
    let filas = case contenido of
                Right filas -> filas
                _ -> []
    let filas' = map (\y -> map (\x -> read x::Integer) y) filas
    let matriz' = listaMatriz (csvLista filas')
    return (matriz')

    -- Función para transformar el csv de [[String]] -> [[(Integer, Integer, Integer, Integer)]], y esto pasarlo a matriz.
        -- Tenemos codificado tanto la línea siguiente de la historia, como el valor a aumentar tras la decisión y la habilidad a aumentar
        -- en un único número, tendremos codificada toda la información y con estas funciones lo decodificaremos. 
csvLista:: [[Integer]] -> [[Opciones]]
csvLista xss = [[(s,a,b,c) | el <- xs, let s = siguienteTipo el, let a = filaHistoria el, let (b,c) = valorYHabilidad el] | xs <- xss]

        -- Funciones para sacar los elementos de la funcion anterior y modularizar.
siguienteTipo:: Integer -> Integer
siguienteTipo el 
    | el < 0 = div (-1*el) 1000
    | el >= 0 = div el 1000

filaHistoria:: Integer -> Integer
filaHistoria el = div aux 100
    where   aux     = if el < 0 then mod (-1*el) 1000 else mod el 1000

valorYHabilidad:: Integer -> ValorYHabilidad
valorYHabilidad el 
    | el < 0    = (-1*b,c)
    |otherwise  = (b,c)
    where   aux     = if el < 0 then mod (-1*el) 100 else mod el 100
            (b,c)   = if aux < 0 then divMod (-1*aux) 10 else divMod aux 10

    -- Matriz; funciones reutilizadas de las prácticas. 

type Matriz a = Array (Int,Int) Opciones

listaMatriz :: [[Opciones]] -> Matriz Opciones
listaMatriz xss = listArray ((1,1),(n,m)) (concat xss)
    where   n = length xss
            m = length (xss!!0)

    -- Función para seleccionar directamente el elemento que necesitemos.
seleccionaElemento:: Int -> Int -> Matriz Opciones -> Opciones
seleccionaElemento f c m = m ! (f,c)
----------------------------------------------------------------------------
----------------------------------------------------------------------------
-- **** Fichero csv Historia **** --
------ ---------- ----------- ------------ ---------

readerHistory:: IO [HistoriaCSV]
readerHistory = do
    let entrada = "Files\\historia.csv"
    contenido <- parseCSVFromFile entrada
    let filas = case contenido of
                Right filas -> filas
                _ -> []
    let cabecera = head filas
    let datos = tail filas
    let historyCSV = [(read texto:: Integer,(a,b,c), read siguienteF::Integer) | fil <- datos,let texto = fil!!0, let siguienteF = fil!!4, let a = fil!!1, let b = fil!!2, let c = fil!!3]
    return (historyCSV)

----------------------------------------------------------------------------
-- **** Fichero txt Historia **** --
------ ---------- ----------- ------------ ---------
readerText:: IO [String]
readerText = do
    let entrada = "Files\\historiaPrincipal.txt"
    contenido <- readFile entrada
    let lineas = filter (\x -> length x > 1) (lines contenido)
    return (lineas)