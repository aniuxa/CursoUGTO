
# =============================================================================#
# Fecha: 2021-06-19 
# Práctica 3
# Autores: Muchos
# =============================================================================#


# Directorio - si aplica            --------------------------------------------
setwd("C:\\Users\\anaes\\Dropbox\\2021\\CursoUGTO\\CursoUGTO")


# Paquetes                        ----------------------------------------------
if (!require("pacman")) install.packages("pacman") # instala pacman si se requiere
pacman::p_load(tidyverse, readxl,haven, sjlabelled,
               janitor, srvyr, esquisse, RColorBrewer) 
#carga los paquetes necesarios para esta práctica


# Datos                        ----------------------------------------------
lapop2019 <- read_dta("./datos/lapop2019.dta")

ICI_2018 <- read_excel("./datos/ICI_2018.xlsx", sheet = "para_importar")
ICI_2018 <- clean_names(ICI_2018) # limpia los nombres


# Gráficos de Base               ----------------------------------------------

plot(as_label(lapop2019$soct2))


barplot(table(as_label(lapop2019$soct2)))


plot(as_label(lapop2019$soct2), 
       main=paste(get_label(lapop2019$soct2)))

hist(ICI_2018$esperanza_de_vida)

boxplot(ICI_2018$esperanza_de_vida)


# Gráficos de ggplot2              ----------------------------------------------
g1<-lapop2019 %>%
  ggplot(aes(as_label(soct2)))

g1 # imprime el lienzo



g1 + geom_bar()


g1 +  geom_bar(aes(
  fill = as_label(q1)
  )) # colorea la geometría

# Esto es equivalente

lapop2019 %>%
  ggplot(aes(as_label(soct2),
             fill = as_label(q1)
             )
         ) + geom_bar()


# Otras variables

g2<-lapop2019 %>%
  ggplot(aes(ed))

g2 # imprime el lienzo

g2 + geom_histogram() 

g2 + geom_histogram(bins=10) 

g2 + geom_density()


# Gráficos bivariados ----

## Gráficos cuali vs cuali -----

g1 +  geom_bar(aes(fill = as_label(q1)),
              position="dodge") #pone las categorías lado a lado y no apiladas


g_bivariado <- g1 +  geom_bar(aes(fill = as_label(q1)),
              position="fill") # cada categoría "llena" a una unidad

g_bivariado



g_bivariado + scale_fill_brewer(palette = "Dark2")

g_bivariado + scale_fill_brewer(palette = "Dark2") + theme_minimal()


## Gráficos cuanti vs cuali -----


ICI_2018 %>% 
  ggplot(aes(indice_de_gini,
             indice_de_vulnerabilidad_a_efectos_del_cambio_climatico)) +
  geom_point()

ICI_2018 %>% 
  ggplot(aes(indice_de_gini,
             indice_de_vulnerabilidad_a_efectos_del_cambio_climatico)) +
  geom_jitter()

# geometría "text"

ICI_2018 %>% 
  ggplot(aes(indice_de_gini,indice_de_vulnerabilidad_a_efectos_del_cambio_climatico)) +
  geom_text(aes(label=indicador))


# geometría "label"

ICI_2018 %>% 
  ggplot(aes(indice_de_gini,indice_de_vulnerabilidad_a_efectos_del_cambio_climatico)) +
  geom_label(aes(label=indicador))


### Más variables ----

etiqueta<-c("No miembro", "Miembro")
ICI_2018<-ICI_2018 %>% 
  set_labels(miembro_de_la_alianza_para_el_gobierno_abierto, labels=etiqueta)
  
ICI_2018<-ICI_2018 %>% 
  dplyr::rename(miembros_al=miembro_de_la_alianza_para_el_gobierno_abierto)



# Introducción de una tercera variable con color

ICI_2018 %>% 
  ggplot(aes(x=indice_de_gini,
             y=indice_de_vulnerabilidad_a_efectos_del_cambio_climatico,
             color=as_label(miembros_al))
         ) +
  geom_point()

# Introducción de una tercera variable con "shape"

ICI_2018 %>% 
  ggplot(aes(x=indice_de_gini,
             y=indice_de_vulnerabilidad_a_efectos_del_cambio_climatico,
             shape=as_label(miembros_al))
  ) +
  geom_point() # ojo, hay un límite para las formas


### Facets ----

ICI_2018 %>% 
  ggplot(aes(x=indice_de_gini,
             y=indice_de_vulnerabilidad_a_efectos_del_cambio_climatico)) +
  geom_point() + facet_wrap(~as_label(miembros_al))


# columnas 
ICI_2018 %>% 
  ggplot(aes(x=indice_de_gini,
             y=indice_de_vulnerabilidad_a_efectos_del_cambio_climatico)) +
  geom_point() + facet_grid(.~as_label(miembros_al))

#filas 
ICI_2018 %>% 
  ggplot(aes(x=indice_de_gini,
             y=indice_de_vulnerabilidad_a_efectos_del_cambio_climatico)) +
  geom_point() +
  facet_grid(as_label(miembros_al)~.)



### Smooth 

ICI_2018 %>% 
  ggplot(aes(x=indice_de_gini,
             y=indice_de_vulnerabilidad_a_efectos_del_cambio_climatico)) +
  geom_point() +
  geom_smooth(method="lm") +
  facet_grid(as_label(miembros_al)~.)


ICI_2018 %>% 
  ggplot(aes(x=indice_de_gini,
             y=indice_de_vulnerabilidad_a_efectos_del_cambio_climatico,
             color=as_label(miembros_al))) +
  geom_text(aes(label=indicador)) +
  geom_smooth(method="lm") + scale_fill_brewer(palette = "Dark2") +
  theme_minimal()




### Más más variables ----

# Introducción de una cuarta variable cuanti
  
ICI_2018 %>% 
  ggplot(aes(x=indice_de_gini,
             y=indice_de_vulnerabilidad_a_efectos_del_cambio_climatico,
             color=as_label(miembros_al))) +
  geom_point(aes(size=crecimiento_del_pib))+ # ojo
  theme_minimal()

# Equivalente  

ICI_2018 %>% 
  ggplot(aes(x=indice_de_gini,
             y=indice_de_vulnerabilidad_a_efectos_del_cambio_climatico,
             color=as_label(miembros_al),
             size=crecimiento_del_pib)) +
  geom_point()+ 
  theme_minimal()

# Agregaremos etiquetas

ICI_2018 %>% 
  ggplot(aes(x=indice_de_gini,
             y=indice_de_vulnerabilidad_a_efectos_del_cambio_climatico,
             color=as_label(miembros_al),
             size=crecimiento_del_pib)) +
  geom_text(aes(label=indicador),
            check_overlap = TRUE)+
  theme_minimal()

## Graficos cuanti vs cuali ----

lapop2019 %>%
  ggplot(aes(ed)) # años de escolaridad


lapop2019 %>%
  ggplot(aes(x=ed, fill=as_label(q1))) 


lapop2019 %>%
  ggplot(aes(x=ed, fill=as_label(q1))) + geom_density()


# Modificamos la opacidad
lapop2019 %>%
  ggplot(aes(x=ed, 
             fill=as_label(q1),
             alpha=I(0.5))) + geom_density() + theme_minimal()

# Indicadores -----

lapop2019 %>% 
 names()

lapop2019 %>% 
  select(starts_with("b")) %>% 
  names()


lapop2019<-lapop2019 %>%
 mutate(index= rowMeans(across(starts_with("b")), na.rm = T)) %>% 
  mutate(index=index/7)

summary(lapop2019$index)

lapop2019 %>% 
  ggplot(aes(index))+ geom_histogram()

lapop2019 %>% 
  ggplot(aes(x=index, y=as_label(idio2)))+ geom_boxplot()+ theme_minimal()


lapop2019 %>% 
  ggplot(aes(x=index, y=as_label(idio2)))+ geom_boxplot()+ theme_minimal()+
  labs(y=paste(get_label(lapop2019$idio2)))

