combatDraw :: (Double,Double,Double,Double,Double) -> (Double,Double,Double,Double,Double) -> Picture
combatDraw py en = player & enemy & actions & ground & (playerStats py) & (enemStats en)


ground :: Picture
ground = colored (translated(solidRectangle(150,1.5),0,-9.0),green) 
         & colored (translated(solidRectangle(150, 100), 0, -58.3), brown)


attackText :: Picture
attackText = translated(lettering("1.-Atacar"),-8,-16)
             & colored (translated(solidRectangle(7,1.5),-8,-16), red)


defText :: Picture
defText = translated(lettering("2.-Defenderse"), 8,-16)
          & colored (translated(solidRectangle(7,1.5),8,-16), orange)


tnjText :: Picture
tnjText = translated(lettering("3.-Bajar moral"),-8,-19)
          & colored (translated(solidRectangle(7,1.5),-8,-19), purple)


curarseText :: Picture
curarseText = translated(lettering("4.-Curarse"),8,-19)
              & colored (translated(solidRectangle(7,1.5),8,-19), pink)


actions :: Picture
actions= attackText & defText & tnjText & curarseText 
         & colored (translated(rectangle(25,6),0,-17.5), black)


player :: Picture
player  = headP
          & leftArmP
          & rightArmP
          & bodyP
          & leftLegP
          & rightLegP


-- Draw player
headP = translated(solidCircle(1.2), -5, 0)
leftArmP = translated(rotated(solidRectangle(4, 0.4), 135), -3.7, -2.4) 
rightArmP = translated(rotated(solidRectangle(4, 0.4), 45), -6.3, -2.4)
bodyP = translated(rotated(solidRectangle(5, 0.4), 90), -5, -3.4)
leftLegP = translated(rotated(solidRectangle(4, 0.4), 135), -3.7, -7) 
rightLegP = translated(rotated(solidRectangle(4, 0.4), 45), -6.3, -7)


enemy :: Picture
enemy = headE
        & leftArmE
        & rightArmE
        & bodyE
        & leftLegE
        & rightLegE


-- Draw enemy
headE = translated(solidCircle(1.2), 5, 0)
leftArmE = translated(rotated(solidRectangle(4, 0.4), 135), 6.3, -2.4) 
rightArmE = translated(rotated(solidRectangle(4, 0.4), 45), 3.7, -2.4)
bodyE = translated(rotated(solidRectangle(5, 0.4), 90), 5, -3.4)
leftLegE = translated(rotated(solidRectangle(4, 0.4), 135), 6.3, -7) 
rightLegE = translated(rotated(solidRectangle(4, 0.4), 45), 3.7, -7)



playerStats :: (Double,Double,Double,Double,Double) -> Picture
playerStats (l,f,tnj,s,v) = translated(lettering(pack("Ataque: "++show(l))),-8.5,7)
                 & translated(lettering(pack("Felicidad: "++show(f))),-8.5,6)
                 & translated(lettering(pack("Intimidar: "++show(tnj))),-8.5,5)
                 & translated(lettering(pack("Sanar: "++show(s))),-8.5,4)
                 & translated(lettering(pack("Vida: "++show(v))),-8.5,3)
                 & translated(rectangle(7.5,6),-8.5,5.5)


enemStats :: (Double,Double,Double,Double,Double) -> Picture
enemStats (l,f,tnj,s,v) = translated(lettering(pack("Ataque: "++show(l))),8.5,7)
                 & translated(lettering(pack("Felicidad: "++show(f))),8.5,6)
                 & translated(lettering(pack("Intimidar: "++show(tnj))),8.5,5)
                 & translated(lettering(pack("Sanar: "++show(s))),8.5,4)
                 & translated(lettering(pack("Vida: "++show(v))),8.5,3)
                 & translated(rectangle(7.5,6),8.5,5.5)
