#                                                                                   Una Triste Historia

Este es un trabajo para la Universidad de Sevilla, concretamente para la asignatura de Programación Declarativa, realizado por Jose Morera Figueroa y Víctor Rosario Nuñez.  
A continuación pasaré a hacer tanto una introducción como una descripción del trabajo.

###                                                                                 Pequeña Introducción

El trabajo es un juego de rol con una visualización simple, desarrollado en el lenguaje de programación funcional __Haskell__. 


![](https://sugus.eii.us.es/sugupedia/images/c/c9/Haskell-logo.jpg)


Como visor gráfico hemos usado __Codeworld__, este nos ha servido para visualizar de mejor manera tanto el texto de rol de la historia, como las opciones que puede escoger el jugador 
para así seguir un camino u otro dentro de la historia, y por último, los combates que se realizan en el propio juego.

El programa se podría dividir en tres partes:
  1. El main principal en el que se orquesta todo el trabajo y con el cual hemos organizado todas las acciones.
  2. Un sistema de peleas, que tal y como indica su nombre se encarga de la ejecución de las peleas y de modificar a los personajes según sus acciones.
  3. Por último, un sistema de lectura de Datos, con el cual hemos leído toda la historia y las alternativas según las decisiones, de diferentes archivos.

