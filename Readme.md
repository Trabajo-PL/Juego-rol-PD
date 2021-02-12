##                                                                                   Una Triste Historia

A continuación pasaré a hacer tanto una introducción como una descripción del trabajo para Programación Declarativa, realizado por Jose Morera Figueroa y Víctor Rosario Nuñez.

####                                                                                 Pequeña Introducción

El trabajo es un juego de rol con una visualización simple, desarrollado en el lenguaje de programación funcional Haskell. 


![](https://www.google.com/imgres?imgurl=https%3A%2F%2Fwww.47deg.com%2Fassets%2Fimg%2Fblog%2Ffeatured_images%2F2019-11-18-setting-up-haskell-environment.png&imgrefurl=https%3A%2F%2Fwww.47deg.com%2Fblog%2Fsetting-up-haskell%2F&tbnid=Gry5K9deyw--xM&vet=12ahUKEwj56-uC5OTuAhVUwIUKHVxiCWUQMygAegUIARCeAQ..i&docid=gEZzA9A-BecnnM&w=1024&h=512&q=Haskell&ved=2ahUKEwj56-uC5OTuAhVUwIUKHVxiCWUQMygAegUIARCeAQ)


Como visor gráfico hemos usado Codeworld, este nos ha servido para visualizar de mejor manera tanto el texto de rol de la historia, como las opciones que puede escoger el jugador 
para así seguir un camino u otro dentro de la historia, y por último, los combates que se realizan en el propio juego.

El programa se podría dividir en tres partes:
  1. El main principal en el que se orquesta todo el trabajo y con el cual hemos organizado todas las acciones.
  2. Un sistema de peleas, que tal y como indica su nombre se encarga de la ejecución de las peleas y de modificar a los personajes según sus acciones.
  3. Por último, un sistema de lectura de Datos, con el cual hemos leído toda la historia y las alternativas según las decisiones, de diferentes archivos.

