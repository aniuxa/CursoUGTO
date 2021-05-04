2+5
5*3

#Para escribir comentarios y que no los lea como operaciones ponemos el símbolo de gato
# Lo podemos hacer para un comentario en una línea o la par de una instrucción
1:5               # Secuencia 1-5

seq(1, 10, 0.5)   # Secuencia con incrementos diferentes a 1

c('a','b','c')  # Vector con caracteres
1:7             # Entero
40<80           # Valor logico
2+2 == 5        # Valor logico
T == TRUE       # T expresion corta de verdadero



x <- 24         # Asignacion de valor 24 a la variable x para su uso posterior (OBJETO)
x/2             # Uso posterior de variable u objeto x
x               # Imprime en pantalla el valor de la variable u objeto
x <- TRUE       # Asigna el valor logico TRUE a la variable x OJO: x toma el ultimo valor que se le asigna
x



y <- c(2,4,6)     # Vector numerico
y <- c('Primaria', 'Secundaria') # Vector caracteres


y[2]              # Acceder al segundo valor del vector y
y[3] <- 'Preparatoria y más' # Asigna valor a la tercera componente del vector
sex <-1:2         # Asigna a la variable sex los valores 1 y 2
names(sex) <- c("Femenino", "Masculino") # Asigna nombres al vector de elementos sexo
sex[2]            # Segundo elemento del vector sex



sum (10,20,30)    # Función suma
rep('R', times=3) # Repite la letra R el numero de veces que se indica
sqrt(9)           # Raiz cuadrada de 9



help(sum)         # Ayuda sobre función sum
example(sum)      # Ejemplo de función sum


ls()
gc()           # Garbage collection, reporta memoria en uso


rm(list=ls())  # Borrar objetos actuales


getwd()           # Directorio actual

setwd("C:/Users/anaes/Dropbox/2021/CursoUGTO")# Cambio de directorio

list.files()      # Lista de archivos en ese directorio


#install.packages("haven", dependencies = TRUE)
library(haven)


lapop2019 <-read_dta("./datos/lapop2019.dta") #checa cómo nos vamos adentro de nuestro directorio



if (!require("pacman")) install.packages("pacman") # instala pacman si se requiere
pacman::p_load(tidyverse,
               skimr,
               esquisse,
               readxl,
               writexl,
               haven, 
               sjlabelled, 
               foreign) #carga los paquetes necesarios para esta práctica



ICE_2018 <- read_excel("./datos/ICE_2018.xlsx", sheet = "para_importar")
#View(ICI_2018)


writexl::write_xlsx(ICE_2018, path = "Mi_Exportación.xlsx")


write_dta(lapop2019, "./datos/mi_exportacion.dta", version = 12)


class(lapop2019) # tipo de objeto
names(lapop2019) # lista las variables
head(lapop2019) # muestra las primeras 6 líneas
table(lapop2019$soct2) # un tabulado simple

