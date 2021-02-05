import System.Environment (getArgs)
import Control.Exception (catch, SomeException)
import System.Directory
import Text.CSV
import Text.Printf
import Data.Array
import Data.List
import Test.QuickCheck

---------------------------- Lectura del fichero CSV y pasar a Matriz ---------------------------

lectorFicheroAumento:: IO([[Integer]])
lectorFicheroAumento = do
    let entrada = "E:\\Universidad\\tercero\\PD\\pruebas-trabajo\\aumentos.csv"
    contenido <- parseCSVFromFile entrada  -- cargar el contenido
    let filas = case contenido of
                Right filas -> filas
                _ -> []
    let filas' = [[read elemento::Integer | elemento <- f] | f <- filas]
    putStrLn $ show (csvLista filas')
    print "-------------"
    putStrLn $ show (listaMatriz (csvLista filas'))
    let matriz' = listaMatriz (csvLista filas')
    print "-------------"
    putStrLn $ show (seleccionaElemento 1 2 matriz')
    return(filas')


    -- Función para transformar el csv de [[String]] -> [[(Integer, Integer, Integer)]], y esto pasarlo a matriz.
        -- Tenemos codificado tanto la línea siguiente de la historia, como el valor a aumentar tras la decisión y la habilidad a aumentar
        -- en un único número, tendremos codificada toda la información y con estas funciones lo decodificaremos. 
csvLista:: [[Integer]] -> [[(Integer, Integer, Integer)]]
csvLista xss = [[(a,b,c) | el <- xs, let a = filaHistoria el, let (b,c) = valorYHabilidad el] | xs <- xss]

        -- Funciones para sacar los elementos de la funcion anterior y modularizar.
filaHistoria:: Integer -> Integer
filaHistoria el 
    | el < 0 = div (-1*el) 100
    | el >= 0 = div el 100
    -- | otherwise = error "Por ahora error, esto significa que vamos pegaaaaarnos"

valorYHabilidad:: Integer -> (Integer, Integer)
valorYHabilidad el 
    | el < 0    = (-1*b,c)
    |otherwise  = (b,c)
    where   aux     = if el < 0 then mod (-1*el) 100 else mod el 100
            (b,c)   = if aux < 0 then divMod (-1*aux) 10 else divMod aux 10

    -- Array y Matriz; funciones reutilizadas de las prácticas. 
type Vector a = Array Int (Integer,Integer,Integer)

listaVector :: [(Integer,Integer,Integer)] -> Vector (Integer,Integer,Integer)
listaVector xs = array (1,n) (zip [1..n] xs)
    where   n = length xs

type Matriz a = Array (Int,Int) (Integer,Integer,Integer)

listaMatriz :: [[(Integer,Integer,Integer)]] -> Matriz (Integer,Integer,Integer)
listaMatriz xss = listArray ((1,1),(n,m)) (concat xss)
    where   n = length xss
            m = length (xss!!0)

    -- Función para seleccionar directamente el elemento que necesitemos.
seleccionaElemento:: Int -> Int -> Matriz (Integer,Integer,Integer) -> (Integer,Integer,Integer)
seleccionaElemento f c m = m ! (f,c)