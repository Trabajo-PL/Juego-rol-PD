##                                                                                   Una Triste Historia

A continuación pasaré a hacer tanto una introducción como una descripción del trabajo para Programación Declarativa, realizado por Jose Morera Figueroa y Víctor Rosario Nuñez.

####                                                                                 Pequeña Introducción

El trabajo es un juego de rol con una visualización simple, desarrollado en el lenguaje de programación funcional Haskell. 


![](https://www.google.com/imgres?imgurl=https%3A%2F%2Fsugus.eii.us.es%2Fsugupedia%2Fimages%2Fc%2Fc9%2FHaskell-logo.jpg&imgrefurl=https%3A%2F%2Fsugus.eii.us.es%2Fsugupedia%2Findex.php%3Ftitle%3DArchivo%3AHaskell-logo.jpg&tbnid=UhsG5yO97GKaIM&vet=12ahUKEwim56yg6OTuAhVEpBoKHa1XCV0QMygBegUIARCeAQ..i&docid=TYV36NRXX82w9M&w=431&h=172&q=Haskell%20logo&ved=2ahUKEwim56yg6OTuAhVEpBoKHa1XCV0QMygBegUIARCeAQ)


Como visor gráfico hemos usado Codeworld, este nos ha servido para visualizar de mejor manera tanto el texto de rol de la historia, como las opciones que puede escoger el jugador 
para así seguir un camino u otro dentro de la historia, y por último, los combates que se realizan en el propio juego.

El programa se podría dividir en tres partes:
  1. El main principal en el que se orquesta todo el trabajo y con el cual hemos organizado todas las acciones.
  2. Un sistema de peleas, que tal y como indica su nombre se encarga de la ejecución de las peleas y de modificar a los personajes según sus acciones.
  3. Por último, un sistema de lectura de Datos, con el cual hemos leído toda la historia y las alternativas según las decisiones, de diferentes archivos.

