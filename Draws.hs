module Draws(
    
    textDraw,
    combatDraw

 ) where 
{-# LANGUAGE OverloadedStrings #-}
import CodeWorld
import Text.CSV
import Text.Printf
import Data.Array
import Data.List
import Data.Text (pack)
import Datos as D

textDraw:: D.OpcionesH -> (String, Double, Double, Double, Double, Double) -> String -> Picture
textDraw optionsH principalC textH = ((texto2 textH) & principalTextRectangle & (colored (light yellow) (blank)) & optionsRectangles optionsH & statsRectangle principalC & (drawBackground))
-- Dibujo de la parte del texto:
    -- Primero las funciones necesarias para imprimir diferentes lineas, ya que el .txt aparece todo como 1.
--texto:: String
--texto = "Manolito es un chaval que vive en Despena Perros, su padre medico le ensena desde pequeno su profesion, vive ademas con su madre y su hermano, Helena y Juan. Tiene una muy buena relacion con su hermano, quien siempre le alegra puesto que es un sad boy. Su padre esta en contra de la guerra, pues quita vidas y el piensa que las vidas no deben desperdiciarse, pero Manolito siente atraccion por el arte de la lanza. Hoy no hay pacientes en la consulta por lo que decide irse, ?que prefieres hacer?"

texto2:: String -> Picture
texto2 linea = (translated (0) (7.5) (scaled (0.65) (0.75) (drawText añadirLinea (0))))
    where   lineasSeparadas = separaLinea (words linea)
            añadirLinea = añadeSeparación lineasSeparadas
        -- Separo la linea principal en sublineas de 25 palabras
separaLinea:: [String] -> [[String]]
separaLinea [] = [[]]
separaLinea linea = [(take 25 linea)] ++ (separaLinea (drop 25 linea))
        -- le añado la separación ya que al usar las palabras quitamos los espacios y son necesarios, y además lo concatenamos para sacar las lineas
añadeSeparación:: [[String]] -> [String]
añadeSeparación xss = [concat [x++" " | x <- xs] | xs <- xss]
        -- esta función recursiva me genera el Picture de las lineas.
drawText:: [String] -> Double -> Picture
drawText (x:xs) i
    | length xs == 0 || length x <26 = translated (0) (-i) (lettering (pack x))
    | otherwise = translated (0) (-i) (lettering (pack x)) & (drawText xs (i+1.1))

    -- Aquí generamos ahora todos los recuadros donde contener los diferentes textos.
principalTextRectangle:: Picture
principalTextRectangle = translated (0) (6.2) (colored (light (light purple)) (solidRectangle (38) (4)))

optionsRectangles:: D.OpcionesH -> Picture
optionsRectangles (o1, o2, o3) =translated (-12) (0) (optionRectangle 1 o1) <> translated (0) (0) (optionRectangle 2 o2) <> translated (12) (0) (optionRectangle 3 o3)

optionRectangle:: Integer -> String -> Picture
optionRectangle i option = translated (0) (0.85) (lettering (pack (show i))) & translated (0) (-0.5)(scaled (0.7) (0.7) (lettering (pack  option))) & colored (light (light ( light green))) (solidRectangle (10) (4))

statsRectangle:: (String, Double, Double, Double, Double, Double) -> Picture
statsRectangle principalC = translated (0) (-4) (lettering (pack ("Stats de "++nombre))) &  translated (0) (-6.5) (scaled (0.8) (0.8) texto') & colored (light (light ( light brown))) $ translated (0) (-6) $ solidRectangle (20) (6)
    where   texto' = (lettering (pack ("Luchar: "++(show lucha)++" Sanar: "++ (show sanar)++" Intimidar: "++(show intimidar)++" Felicidad: "++(show felicidad)++ "Vida: "++(show vida))))
            (nombre, lucha, sanar, intimidar, felicidad, vida) = principalC

drawBackground:: Picture
drawBackground = colored (dark blue) $ solidRectangle (50) (50)