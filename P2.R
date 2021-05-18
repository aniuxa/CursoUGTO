
# =============================================================================#
# Fecha: 2021-05-19 
# Cuestionario nuevo ingreso
# Autores: Aquí sus nombres preciosos
# =============================================================================#


# Directorio - si aplica            --------------------------------------------
setwd("C:\\Users\\anaes\\Dropbox\\2021\\CursoUGTO\\CursoUGTO")


# Paquetes                        ----------------------------------------------
if (!require("pacman")) install.packages("pacman") # instala pacman si se requiere
pacman::p_load(tidyverse, 
               readxl,
               haven, 
               sjlabelled,
               janitor) #carga los paquetes necesarios para esta práctica


# Datos                        ----------------------------------------------
lapop2019 <- read_dta("./datos/lapop2019.dta")

base_genero <- read_excel("./datos/Base-Regimen-de-genero-30.04.21.xlsx")



# Selección                        ----------------------------------------------
#https://www.amelia.mn/Syntax-cheatsheet.pdf
## Subsetting con base

x<-lapop2019$a4
x<-lapop2019[["a4"]]  # ¡Ojo con las comillas! 
x<-lapop2019[,17]
x<-lapop2019[,"a4"]



## Subsetting con dplyr                 -----------------------------------------
x<-lapop2019 %>% 
  select(a4)

# Etiquetas                       ----------------------------------------------

class(lapop2019$q1)
table(lapop2019$q1)
table(as_label(lapop2019$q1))

## Selección inversa

x<-lapop2019 %>% 
  select(-ed)
x<-lapop2019[,-126]

rm(x) #rm sólo bota objetos



lapop2019$ed_2<-lapop2019$ed
lapop2019$ed_2<-NULL

# Subsetting                       ----------------------------------------------



subset1<-lapop2019[lapop2019$ed>4,]


subset2<- lapop2019[, c("q1", "q2", "ed")]



subset3<- lapop2019[(lapop2019$ed>4 & lapop2019$q1==1 ), c("q1", "q2", "ed")]

subset4<-lapop2019 %>% 
  filter(ed>4 & q1==1) %>%
    select(q1, q2, ed)


# Variables nominales                       ----------------------------------------------

lapop2019 %>% count(q1==2) # cuentan los casos que cumplen con la condición "q1==2"


lapop2019 %>% with(table(q1))

lapop2019 %>% mutate(q1=as_label(q1))  %>% tabyl(q1)


lapop2019 %>% mutate(q1=as_label(q1))  %>% tabyl(q1) %>% adorn_totals()


class(lapop2019$q1) # variable sin etiqueta
class(as_label(lapop2019$q1)) # variable con etiqueta

class(as_label(lapop2019$soct2)) 
class(as_label(lapop2019$ed)) 


glimpse(lapop2019$q1)


## Tabulados con janitor -------------------------------------------------------
lapop2019 %>% mutate(q1=as_label(q1)) %>% # cambia los valores de la variable a sus etiquetas
                tabyl(q1) %>% # para hacer la tabla
                adorn_totals() %>% # añade totales
                adorn_pct_formatting()  # nos da porcentaje en lugar de proporción
                
glimpse(lapop2019$soct2)


lapop2019 %>% mutate(soct2=as_label(soct2)) %>% 
                tabyl(soct2)


lapop2019 %>% 
  mutate(soct2=as_label(soct2)) %>% 
                tabyl(soct2, show_missing_levels=F ) %>% # esta opción elimina los valores con 0
                adorn_totals()  

## Tabulados de doble entrada  -------------------------------------------------


lapop2019 %>% 
    mutate(soct2=as_label(soct2)) %>% # para que las lea como factor
    mutate(q1=as_label(q1)) %>% # para que las lea como factor
                tabyl(soct2, q1, show_missing_levels=F ) %>% # incluimos aquí 
                adorn_totals()  

lapop2019 %>%   
    count(soct2==1 & q1==1) # nos da la segunda celda de la izquierda

lapop2019 %>% 
  mutate(soct2=as_label(soct2)) %>% # para que las lea como factor
  mutate(q1=as_label(q1)) %>% # para que las lea como factor
    tabyl(soct2, q1, show_missing_levels=F ) %>% # incluimos aquí q1o
                adorn_totals("col")  


lapop2019 %>% 
  mutate(soct2=as_label(soct2)) %>% # para que las lea como factor
  mutate(q1=as_label(q1)) %>% # para que las lea como factor
                tabyl(soct2, q1, show_missing_levels=F ) %>% # incluimos aquí q1o
                adorn_totals(c("col", "row")) 


lapop2019 %>% 
  mutate(soct2=as_label(soct2)) %>% # para que las lea como factor
  mutate(q1=as_label(q1)) %>% # para que las lea como factor
  tabyl(soct2, q1, show_missing_levels=F ) %>% # incluimos aquí sexo
                adorn_totals(c("col", "row")) %>% 
                adorn_percentages("col") %>% # Divide los valores entre el total de la columna
                adorn_pct_formatting() # lo vuelve porcentaje


lapop2019 %>% 
  mutate(soct2=as_label(soct2)) %>% # para que las lea como factor
  mutate(q1=as_label(q1)) %>% # para que las lea como factor
                tabyl(ed, q1, show_missing_levels=F ) %>% 
                adorn_totals(c("col", "row")) %>% 
                adorn_percentages("row") %>% # Divide los valores entre el total de la fila
                adorn_pct_formatting() # lo vuelve porcentaje

lapop2019 %>% 
  mutate(soct2=as_label(soct2)) %>% # para que las lea como factor
  mutate(q1=as_label(q1)) %>% # para que las lea como factor
                tabyl(soct2, q1, show_missing_levels=F ) %>% # incluimos aquí q1o
                adorn_totals(c("col", "row")) %>% 
                adorn_percentages("all") %>% # Divide los valores entre el total de la población
                adorn_pct_formatting() # lo vuelve porcentaje



# Descriptivos para variables cuantitativas ------------------------------------

## Medidas numéricas básicas  --------------------------------------------------

summary(lapop2019$ed) ## educación

lapop2019 %>% 
  summarise(nombre_indicador=mean(ed, na.rm=T))

## Histogramas  --------------------------------------------------

## ------------------------------------------------------------------------------------------------------------
hist(lapop2019$ed)


## ------------------------------------------------------------------------------------------------------------
hist(lapop2019$ed, 
     main="Histograma de la edad", 
     xlab="Hrs", ylab="Frecuencia") 


## ------------------------------------------------------------------------------------------------------------
hist(lapop2019$ed, 
     main="Histograma de edades",
     xlab="Horas", 
     ylab="Frecuencia", col="deeppink1") 


## ------------------------------------------------------------------------------------------------------------
lapop2019 %>% 
    with(hist(ed)) # con with, para que entienda


## ------------------------------------------------------------------------------------------------------------
lapop2019 %>% 
  filter(!is.na(ed)) %>% # la ventaja de esta forma es que podemos hacer más operaciones
    with(hist(ed, main= "histograma"))

