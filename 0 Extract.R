# ------- CARGA DE ARCHIVOS DE INFORMACION --------

# Activacion de librerias
library(readr)
library(dplyr)

# Carga del set original con los datos de contagios de covid 19 en Colombia
ruta_archivo <- file.choose()
covid_original <- read_csv(ruta_archivo)

# Carga del set original con los datos de distribucion de vacunas contra el covid 19 en Colombia
ruta_archivo1 <- file.choose()
dist_vacunas_original <- read_csv(ruta_archivo1)

1+1
