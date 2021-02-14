module Draws(

    textDraw,
    combatDraw,
    endDraw,
    resumeDraw,
    intoBDraw,
    initDraw

 ) where 
{-# LANGUAGE OverloadedStrings #-}
import CodeWorld
import Data.Text (pack)
import Datos as D

-- **************************** --
-- DIBUJO de la parte del texto --
-- **************************** --

textDraw:: D.OpcionesH -> (String, Double, Double, Double, Double, Double) -> String -> Picture
textDraw optionsH principalC textH = ((text2Line textH 23 (0,7.5) (0.65, 0.75)) & principalTextRectangle & (colored (light yellow) (blank)) & optionsRectangles optionsH & statsRectangle principalC & (drawBackground blue))
        -- Dibujo de la parte del texto:
        -- Primero las funciones necesarias para imprimir diferentes lineas, ya que el .txt aparece todo como 1.
-- FUNCIONES para tratar texto        
text2Line:: String -> Integer -> (Double, Double) -> (Double, Double) -> Picture
text2Line linea max (coord1,coord2) (scalX, scalY)= (translated (coord1) (coord2) (scaled (scalX) (scalY) (drawText añadirLinea (0) max)))
    where   lineasSeparadas = separate (words linea) max
            añadirLinea = addSpace lineasSeparadas
        -- Separo la linea principal en sublineas de n palabras
separate:: [String] -> Integer -> [[String]]
separate [] _ = [[]]
separate linea max = [(take (fromIntegral max) linea)] ++ (separate (drop (fromIntegral max) linea) max)
        -- le añado la separación ya que al usar las palabras quitamos los espacios y son necesarios, y además lo concatenamos para sacar las lineas
addSpace:: [[String]] -> [String]
addSpace xss = [concat [x++" " | x <- xs] | xs <- xss]

        -- esta función recursiva me genera el Picture de las lineas.
drawText:: [String] -> Double -> Integer -> Picture
drawText (x:xs) i max
    | length xs == 0 || length x < (fromIntegral max) = translated (0) (-i) (lettering (pack x))
    | otherwise = translated (0) (-i) (lettering (pack x)) & (drawText xs (i+1.1) max)

    -- Aquí generamos ahora todos los recuadros donde contener los diferentes textos.
principalTextRectangle:: Picture
principalTextRectangle = translated (0) (6.2) (colored (light (light purple)) (solidRectangle (38) (4)))

optionsRectangles:: D.OpcionesH -> Picture
optionsRectangles (o1, o2, o3) =translated (-12) (0) (optionRectangle 1 o1) <> translated (0) (0) (optionRectangle 2 o2) <> translated (12) (0) (optionRectangle 3 o3)

optionRectangle:: Integer -> String -> Picture
optionRectangle i option = translated (0) (0.85) (lettering (pack (show i))) & text2Line option 6 (0,-0.2) (0.65,0.65) & colored (light (light ( light green))) (solidRectangle (10) (4))

statsRectangle:: (String, Double, Double, Double, Double, Double) -> Picture
statsRectangle principalC = translated (0) (-4) (lettering (pack ("Stats de "++nombre))) &  translated (0) (-6.5) (scaled (0.8) (0.8) texto') & colored (light (light ( light brown))) $ translated (0) (-6) $ solidRectangle (20) (6)
    where   texto' = (lettering (pack ("Luchar: "++(show lucha)++" Sanar: "++ (show sanar)++" Intimidar: "++(show intimidar)++" Felicidad: "++(show felicidad)++ "Vida: "++(show vida))))
            (nombre, lucha, sanar, intimidar, felicidad, vida) = principalC
-- Función para generar un fondo del color deseado.
drawBackground:: Color -> Picture
drawBackground colr= colored (dark colr) $ solidRectangle (100) (100)

-- **************************** --
--       DIBUJO combate         --
-- **************************** --

combatDraw :: (String, Double,Double,Double,Double,Double) -> (String, Double,Double,Double,Double,Double) -> Picture
combatDraw py en = scaled (0.68) (0.63) (translated (0) (5) (colored (blue) (monigote (-10.5)) & colored (dark green) (monigote (10.5)) & actions  & (stats py (-10.5)) & (stats en (10.5)) & ground))


ground :: Picture
ground = colored (green) (translated (0) (-9) (solidRectangle (70) (1.5))) 
         & colored (brown) (translated (0) (-18.3) (solidRectangle (70) (20)))
         & colored (light (light (light blue))) (solidRectangle (80) (80))


attackText :: Picture
attackText = translated (-8) (-13) (lettering(pack ("1.-Atacar")))
             & colored (red) (translated (-8) (-13) (solidRectangle (7) (1.5)))


defText :: Picture
defText = translated (8) (-13) (lettering(pack ("2.-Defenderse")))
          & colored (orange) (translated (8) (-13) (solidRectangle(7) (1.5)))


tnjText :: Picture
tnjText = translated (8) (-16) (lettering(pack $"3.-Bajar moral"))
          & colored (purple) (translated (8) (-16) (solidRectangle(7) (1.5)))

curarseText :: Picture
curarseText = translated (-8) (-16) (lettering(pack $ "4.-Curarse"))
              & colored (pink) (translated (-8) (-16) (solidRectangle(7) (1.5)))

actions :: Picture
actions= attackText & defText & tnjText & curarseText 
         & translated (0) (-14.5) (rectangle(25) (6))

stats :: (String, Double,Double,Double,Double,Double) -> Double -> Picture
stats (n,l,s,tnj,f,v) t = translated (t) (8) (lettering(pack n))
                 & translated (t) (7) (lettering(pack("Ataque: "++show(l))))
                 & translated (t) (6) (lettering(pack("Felicidad: "++show(f))))
                 & translated (t) (5) (lettering(pack("Intimidar: "++show(tnj))))
                 & translated (t) (4) (lettering(pack("Sanar: "++show(s))))
                 & translated (t) (3) (lettering(pack("Vida: "++show(v))))
                 & translated (t) (5.5) (rectangle (7.5) (6))

monigote:: Double -> Picture
monigote t = headP t
        & leftArm t
        & rightArm t
        & body t
        & leftLeg t
        & rightLeg t

headP, leftArm, rightArm, body, leftLeg, rightLeg:: Double -> Picture
headP t = translated (0+t) (0) (solidCircle(1.2))
leftArm t = translated (1.3+t) (-2.4) (rotated (2.35619) (solidRectangle(4) (0.4))) 
rightArm t = translated (-1.2+t) (-2.4) (rotated (0.785398) (solidRectangle(4) (0.4)))
body t = translated (0+t) (-3.4) (rotated (1.5708) (solidRectangle(5) (0.4)))
leftLeg t = translated (1.3+t) (-7) (rotated (2.35619) (solidRectangle(4) (0.4))) 
rightLeg t = translated (-1.2+t) (-7) (rotated (0.785398) (solidRectangle(4) (0.4)))

        -- __________ Transición del combate __________ --

-- **************************** --
--       DIBUJO del inicio      --
-- **************************** --

initDraw:: Picture
initDraw = (translated (0) (7) ( scaled (1.5) (1.5) (lettering (pack "Una Triste Historia")))) <>translated (0) (-2) (actionsInit) <> (drawBackground $ light $ light $ light pink)

actionsInit:: Picture
actionsInit = (textResume "Para Comenzar una partida" (0,4) (1,1)) <> (textResume "pulsa ENTER" (0,3) (1,1)) <> (textResume "Los controles funcionan pulsando " (0,1) (0.9,0.9)) <> (textResume "las teclas deseadas" (0,0) (0.9,0.9)) <> translated (0) (2) (colored (white) (solidRectangle (16) (8)))

-- **************************** --
--       DIBUJO del RESUME      --
-- **************************** --

resumeDraw:: Picture
resumeDraw = colored (light green) (translated (0) (7) ( scaled (1.5) (1.5) (lettering (pack "RESUME")))) <>translated (0) (-1) (actionsRes) <> drawBackground (light (light brown))

actionsRes:: Picture
actionsRes = (textResume "Para:" (-2.5,4) (0.9,0.9)) <> (textResume "Continuar: Esc" (-1,1) (0.8,0.8)) <> (textResume "Nueva partida: R" (-0.8,-1) (0.8,0.8)) <> (textResume "Volver al inicio: Enter" (0,-3) (0.8,0.8)) <>colored (white) (solidRectangle (8) (10))

-- **************************** --
--       DIBUJO del fin         --
-- **************************** --

endDraw:: String -> Color -> Picture
endDraw tex clr = colored (red) (translated (0) (7) ( scaled (1.5) (1.5) (lettering (pack tex)))) <>translated (0) (-1) (actionsEnd) <>drawBackground clr

actionsEnd:: Picture
actionsEnd = (textResume "Para:" (-2.5,4) (0.9,0.9)) <> (textResume "Nueva partida: R" (-0.8,1) (0.8,0.8)) <> (textResume "Volver al inicio: Enter" (0,-1) (0.8,0.8)) <>colored (white) (solidRectangle (8) (10)) 

textResume:: String -> (Double, Double) -> (Double, Double) -> Picture
textResume tex (cX,cY) (sX,sY)= translated (cX) (cY) ( scaled (sX) (sY) (lettering (pack tex)))

-- **************************** --
--   DIBUJO del Entre batalla   --
-- **************************** --

intoBDraw:: String -> String -> Color -> Picture
intoBDraw tex tex2 clr = (translated (0) (7) ( scaled (1.5) (1.5) (lettering (pack tex)))) <>translated (0) (0) (actionsBattle tex2) <>drawBackground clr

actionsBattle:: String -> Picture
actionsBattle tex = (textResume tex (0,0) (1,1)) <> colored (white) (solidRectangle (14) (5))



