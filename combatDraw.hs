import CodeWorld
import Text.CSV
import Text.Printf
import Data.Array
import Data.List
import Data.Text (pack)
import Datos as D

combatDraw :: (Double,Double,Double,Double,Double) -> (Double,Double,Double,Double,Double) -> Picture
combatDraw py en = player & enemy & actions & ground & (playerStats py) & (enemStats en)


ground :: Picture
ground = colored (green) (translated (0) (-9) (solidRectangle (150) (1.5))) 
         & colored (brown) (translated (0) (-58.3) (solidRectangle (150) (100)))


attackText :: Picture
attackText = translated (-8) (-16) (lettering(pack ("1.-Atacar")))
             & colored (red) (translated (-8) (-16) (solidRectangle (7) (1.5)))


defText :: Picture
defText = translated (8) (-16) (lettering(pack ("2.-Defenderse")))
          & colored (orange) (translated (8) (-16) (solidRectangle(7) (1.5)))


tnjText :: Picture
tnjText = translated (8) (-19) (lettering(pack $"3.-Bajar moral"))
          & colored (purple) (translated (8) (-19) (solidRectangle(7) (1.5)))


curarseText :: Picture
curarseText = translated (8) (-19) (lettering(pack $ "4.-Curarse"))
              & colored (pink) (translated (8) (-19) (solidRectangle(7) (1.5)))


actions :: Picture
actions= attackText & defText & tnjText & curarseText 
         & translated (0) (-17.5) (rectangle(25) (6))


player :: Picture
player  = headP
          & leftArmP
          & rightArmP
          & bodyP
          & leftLegP
          & rightLegP


-- Draw player
headP = translated (-5) (0) (solidCircle(1.2))
leftArmP = translated (-3.7) (-2.4) (rotated (135) (solidRectangle(4) (0.4))) 
rightArmP = translated (-6.3) (-2.4) (rotated (45) (solidRectangle(4) (0.4)))
bodyP = translated (-5) (-3.4) (rotated (90) (solidRectangle(5) (0.4)))
leftLegP = translated (-3.7) (-7) (rotated (135) (solidRectangle(4) (0.4))) 
rightLegP = translated (-6.3) (-7) (rotated (45) (solidRectangle(4) (0.4)))


enemy :: Picture
enemy = headE
        & leftArmE
        & rightArmE
        & bodyE
        & leftLegE
        & rightLegE


-- Draw enemy
headE = translated (5) (0) (solidCircle(1.2))
leftArmE = translated (6.3) (-2.4) (rotated (135) (solidRectangle(4) (0.4))) 
rightArmE = translated (3.7) (-2.4) (rotated (45) (solidRectangle(4) (0.4)))
bodyE = translated (5) (-3.4) (rotated (90) (solidRectangle(5) (0.4)))
leftLegE = translated (6.3) (-7) (rotated (135) (solidRectangle(4) (0.4))) 
rightLegE = translated (3.7) (-7) (rotated (45) (solidRectangle(4) (0.4)))



playerStats :: (Double,Double,Double,Double,Double) -> Picture
playerStats (l,f,tnj,s,v) = translated (-8.5) (7) (lettering(pack("Ataque: "++show(l))))
                 & translated (-8.5) (6) (lettering(pack("Felicidad: "++show(f))))
                 & translated (-8.5) (5) (lettering(pack("Intimidar: "++show(tnj))))
                 & translated (-8.5) (4) (lettering(pack("Sanar: "++show(s))))
                 & translated (-8.5) (3) (lettering(pack("Vida: "++show(v))))
                 & translated (-8.5) (5.5) (rectangle (7.5) (6))


enemStats :: (Double,Double,Double,Double,Double) -> Picture
enemStats (l,f,tnj,s,v) = translated (8.5) (7) (lettering(pack("Ataque: "++show(l))))
                 & translated (8.5) (6) (lettering(pack("Felicidad: "++show(f))))
                 & translated (8.5) (5) (lettering(pack("Intimidar: "++show(tnj))))
                 & translated (8.5) (4) (lettering(pack("Sanar: "++show(s))))
                 & translated (8.5) (3) (lettering(pack("Vida: "++show(v))))
                 & translated (8.5) (5.5)(rectangle (7.5) (6))
