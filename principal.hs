
--------------------------------Imports-------------------------------------------------------
import Data.Char
import Text.Printf
import System.IO
import System.Environment (getArgs)
import System.Directory
----------------------------------------------------------------------------------------------


------------------------------------------Atributos prota-------------------------------------

luchar :: Ord

sanar :: Ord

hablar :: Ord

felicidad :: Ord

--Discutir si crear una clase "Protagonista" para trabajar con los atributos y demáses

----------------------------------------------------------------------------------------------


---------------------------------------Funciones y Métodos------------------------------------

--Función para comprobar que los ficheros se importan bien y no hay fallos en los directorios

compruebaFichero :: String -> IO ()
compruebaFichero f =
  do
   if length f > 0 then do
    existe <- doesFileExist f
    if existe then
      return ""      
    else
      putStrLn("El fichero " ++ f ++ " no existe")
   else do
    putStrLn "Debe indidcar el nombre de algún fichero"
    
    
    
    
