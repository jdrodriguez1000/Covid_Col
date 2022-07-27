# Activacion librerias
library(dplyr)
library(lubridate)
library(ggplot2)
library(readr)


###  --------- TRANSFORMACON DEL ARCHIVO COVID ORIGINGAL ----------

# Crecacion del archivo transformado 
covid_transformado <- covid_original %>% 
  
# cambio de nombre de columnas
  rename(frepor = "fecha reporte web", 
         idcaso = "ID de caso", 
         fnotif = "Fecha de notificación",
         coddpto = "Código DIVIPOLA departamento", 
         nomdpto = "Nombre departamento", 
         codmpio = "Código DIVIPOLA municipio",
         nommpio = "Nombre municipio", 
         edad = "Edad", 
         mededad= "Unidad de medida de edad", 
         sexo = "Sexo", 
         tipo = "Tipo de contagio", 
         ubicacion = "Ubicación del caso", 
         estado = "Estado", 
         codpais = "Código ISO del país",
         nompais = "Nombre del país", 
         recuperado = "Recuperado", 
         fsintomas = "Fecha de inicio de síntomas", 
         fmuerte = "Fecha de muerte", 
         fdiagnos = "Fecha de diagnóstico", 
         frecuper = "Fecha de recuperación",
         tiprecup = "Tipo de recuperación", 
         codetnia = "Pertenencia étnica", 
         grupetnia = "Nombre del grupo étnico") %>% 
  
# Creacion de las columnas nro de semana y nombre del dia
  mutate(nrosem = isoweek(fnotif)) %>% 
  mutate(diasem = wday(fnotif, label = TRUE))
  


glimpse(covid_transformado)