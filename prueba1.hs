import Data.Char

historia texto = (textoHistoria, siguientesNodos, habilidades)
    where
        lineas = [l | l <- (lines texto), length l > 1]
        textoHistoria = [hist | linea <- lineas, let hist = (takeWhile (/='%') linea)]
        nodo_habil = [ cosa | linea <- lineas, let cosa = (dropWhile (/='%')  linea)]
        siguientesNodos = [nod | linea <- nodo_habil, let nod = (takeWhile (/='&') linea)]
        habilidades = [hab | linea <- nodo_habil, let hab = (dropWhile (/='&') linea)]

    --let lineas' = [dropWhile (=="#") (words linea) | linea <- lineas] 
    --let solotexto = [tail linea | linea <- lineas] 
    -- let pruebaalgo = [ | linea <- lineas]
    --print (takeWhile' "#" solotexto!!1)
    
main1:: IO ([[Char]], [[Char]], [[Char]])
main1 = do
    r <- readFile "E:\\Universidad\\tercero\\PD\\Practica\\trabajo\\historia.txt"
    let res = historia r
    print res
    return res




