# Activacion librerias
library(dplyr)
library(lubridate)
library(readr)

# Creacion del archivo con la informacion de contagios
dtf_casos <- covid_transformado %>% 
  select(idcaso, frepor, fnotif, nrosem, diasem, codpais, nompais, coddpto, nomdpto, codmpio, nommpio, edad, sexo,
         estado, nivel, ubicacion, tipo, fsintomas, fdiagnos, frecuper, tiprecup, fmuerte)


# Creacion del archivo con la informacion de la distribucion de dosis
dtf_dosis <- dist_vacunas_transformado %>% 
  select(codres, fresol, año, mes, nrosem, coddpto, nomdpto, laboratorio, dosis, uso, fcorte)

