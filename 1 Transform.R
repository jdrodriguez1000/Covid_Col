# Activacion librerias
library(dplyr)
library(lubridate)
library(ggplot2)
library(readr)


###  ---------                                           ---------------
###  --------- TRANSFORMACON DEL ARCHIVO COVID ORIGINGAL ---------------
###  ---------                                           ---------------


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
  

###  ------  ANALISIS INICIAL DE LA COLUMNA FECHA REPORTE -----------

# Totales por fecha
covid_transformado %>% 
  summarise(total = sum(!is.na(frepor))) %>% 
  View()

# Fecha maxima y minima
covid_transformado %>% 
  summarise(fecha_min = min(frepor),
            fecha_max = max(frepor)) %>% 
  View()

# fechas faltantes
covid_transformado %>% 
  group_by(frepor) %>% 
  summarise(total = sum(is.na(frepor))) %>% 
  arrange(frepor) %>% 
  View()

## ------ ANALISIS INICIAL COLUMNA IDCASO ----------

# Total ID casos
covid_transformado %>% 
  summarise(total = sum(!is.na(idcaso))) %>% 
  View()

# Id minimo - Id maximo
covid_transformado %>% 
  summarise(id_minimo = min(idcaso),
            id_maximo = max(idcaso)) %>% 
  View()



## ------ ANALISIS INICIAL COLUMNA FECHA DE NOTIFICACION -----

# Totales por fecha
covid_transformado %>% 
  summarise(total = sum(!is.na(fnotif))) %>% 
  View()

# Fecha minima y maxima de notificacion
covid_transformado %>% 
  summarise(fecha_minima = min(fnotif),
            fecha_maxima = max(fnotif)) %>% 
  View()


## ------ ANALISIS DE LAS COLUMNAS CODIGO Y NOMBRE DEPARTAMENTO ------

# Totales por codigo departamento
covid_transformado %>% 
  summarise(total = sum(!is.na(coddpto))) %>% 
  View()

# Totales por nombre departamento
covid_transformado %>% 
  summarise(total = sum(!is.na(nomdpto))) %>% 
  View()

# Nombres de departamentos repetidos - Codigos no correctos
covid_transformado %>% 
  group_by(coddpto, nomdpto) %>% 
  summarise(total = sum(!is.na(nomdpto))) %>% 
  arrange(nomdpto) %>% 
  View()

# Se debe actualizar el nombre de los departamentos Caldas, Cartagena, Cundinamarca,
# Santander, Santa Marta DE, Tolima, Barranquilla

# Transformacion de nombres de departamentos
covid_transformado <- covid_transformado %>% 
  mutate(nomdpto = ifelse(nomdpto == "Caldas", "CALDAS", nomdpto)) %>% 
  mutate(nomdpto = ifelse(nomdpto == "CARTAGENA", "BOLIVAR", nomdpto)) %>% 
  mutate(nomdpto = ifelse(nomdpto == "Cundinamarca", "CUNDINAMARCA", nomdpto)) %>% 
  mutate(nomdpto = ifelse(nomdpto == "Santander", "SANTANDER", nomdpto)) %>% 
  mutate(nomdpto = ifelse(nomdpto == "STA MARTA D.E.", "MAGDALENA", nomdpto)) %>%
  mutate(nomdpto = ifelse(nomdpto == "Tolima", "TOLIMA", nomdpto)) %>% 
  mutate(nomdpto = ifelse(nomdpto == "BARRANQUILLA", "ATLANTICO", nomdpto))


## Se debe actualizar los siguientes codigos de departamentos: Atlantico, Bolivar, Magdalena
covid_transformado <- covid_transformado %>% 
  mutate(coddpto = ifelse(coddpto == 8001, 8, coddpto)) %>% 
  mutate(coddpto = ifelse(coddpto == 13001, 13, coddpto)) %>%
  mutate(coddpto = ifelse(coddpto == 47001, 47, coddpto))


## ------ ANALISIS DE LAS COLUMNAS CODIGO Y NOMBRE DE MUNICIPIOS -------

# Total registros por codigo de municipio
covid_transformado %>% 
  summarise(total = sum(!is.na(codmpio))) %>% 
  View()

# Total registro por nombre de municipio
covid_transformado %>% 
  summarise(total = sum(!is.na(nommpio))) %>% 
  View()

# nombres de municipios repetidos o incorrectos - codigos de municipios no correctos
covid_transformado %>% 
  group_by(codmpio, nommpio, nomdpto) %>% 
  summarise(total = sum(!is.na(nommpio))) %>% 
  arrange(nommpio) %>% 
  View()

# Se debe actualizar el nombre de Anserma, Barrancabermeja, Entrerrios, Gachala, Galapa,
# Gameza, Guepsa, Medellin, Momil, Pensilvania, Puerto Colombia, Somondoco

# Actualizacion de nombres de municipios
covid_transformado <- covid_transformado %>% 
  mutate(nommpio = ifelse(nommpio == "Anserma", "ANSERMA", nommpio)) %>% 
  mutate(nommpio = ifelse(nommpio == "barrancabermeja", "BARRANCABERMEJA", nommpio)) %>% 
  mutate(nommpio = ifelse(nommpio == "ENTRERrIOS", "ENTRERRIOS", nommpio)) %>% 
  mutate(nommpio = ifelse(nommpio == "gachala", "GACHALA", nommpio)) %>% 
  mutate(nommpio = ifelse(nommpio == "Galapa", "GALAPA", nommpio)) %>% 
  mutate(nommpio = ifelse(nommpio == "Gameza", "GAMEZA", nommpio)) %>% 
  mutate(nommpio = ifelse(nommpio == "Guepsa", "GUEPSA", nommpio)) %>% 
  mutate(nommpio = ifelse(nommpio == "Medellin", "MEDELLIN", nommpio)) %>% 
  mutate(nommpio = ifelse(nommpio == "MEDELLiN", "MEDELLIN", nommpio)) %>% 
  mutate(nommpio = ifelse(nommpio == "momil", "MOMIL", nommpio)) %>% 
  mutate(nommpio = ifelse(nommpio == "Pensilvania", "PENSILVANIA", nommpio)) %>% 
  mutate(nommpio = ifelse(nommpio == "puerto colombia", "PUERTO COLOMBIA", nommpio)) %>% 
  mutate(nommpio = ifelse(nommpio == "puerto COLOMBIA", "PUERTO COLOMBIA", nommpio)) %>% 
  mutate(nommpio = ifelse(nommpio == "Somondoco", "SOMONDOCO", nommpio))


## ----- ANALISIS INICIAL DE LA COLUMNA EDAD --------

# Total registros con la informacion de la edad
covid_transformado %>% 
  summarise(total = sum(!is.na(edad))) %>% 
  View()


# Edad minima y maxima registrada
covid_transformado %>% 
  summarise(edad_minima = min(edad),
            edad_maxima = max(edad)) %>% 
  View()

## ----- ANALISIS INICIAL DE LA COLUMNA UNIDAD DE MEDIDAD DE EDAD mededad ------

# Total registros con informacion
covid_transformado %>% 
  summarise(total = sum(!is.na(mededad))) %>% 
  View()

# Total registros por unidad de medida de edad 
covid_transformado %>% 
  group_by(mededad) %>% 
  summarise(total = sum(!is.na(mededad))) %>% 
  View()


## ------ ANALISIS INICIAL DE LA COLUMNA SEXO ----------

# Total registros con informacion de edad
covid_transformado %>% 
  summarise(total = sum(!is.na(sexo))) %>% 
  View()

# Total registros por edad
covid_transformado %>% 
  group_by(sexo) %>% 
  summarise(total = sum(!is.na(sexo))) %>% 
  View()

# Se debe transformar la variable sexo f a F y m a M.
covid_transformado <- covid_transformado %>% 
  mutate(sexo = ifelse(sexo == "f", "F", sexo)) %>% 
  mutate(sexo = ifelse(sexo == "m", "M", sexo))


## ----- ANALISIS DE LA COLUMNA TIPO DE CONTAGIO tipo -----

# Total registros en la variable tipo de contagio
covid_transformado %>% 
  summarise(total = sum(!is.na(tipo))) %>% 
  View()


# Informacion almacenada en la variable tipo de contagio
covid_transformado %>% 
  group_by(tipo) %>% 
  summarise(total = sum(!is.na(tipo))) %>% 
  View()

## ------ ANALISIS INICIAL DE LA COLUMNA UBICACION DEL CASO - ubicacion ------

# Total registros en la variable ubicacion
covid_transformado %>% 
  summarise(total = sum(!is.na(ubicacion))) %>% 
  View()

# Informacion registrada en la variable ubicacion
covid_transformado %>% 
  group_by(ubicacion) %>% 
  summarise(total = sum(!is.na(ubicacion))) %>% 
  View()

# Analisis de la variable ubicacion cuando es N/A
covid_transformado %>% 
  filter(ubicacion == "N/A") %>% 
  View()

# Se debe realiza las siguientes transformacion: casa y CASA por Casa,  Fallecido por Cementerio y 
# validar aquellos que ya tiene fecha de muerte realizando el cambio de ubicacion a Cementerio
covid_transformado <- covid_transformado %>% 
  mutate(ubicacion = ifelse(ubicacion == "casa", "Casa", ubicacion)) %>% 
  mutate(ubicacion = ifelse(ubicacion == "CASA", "Casa", ubicacion)) %>% 
  mutate(ubicacion = ifelse(ubicacion == "Fallecido", "Cementerio", ubicacion)) %>% 
  mutate(ubicacion = ifelse(!is.na(fmuerte), "Cementerio", ubicacion))


## ----- ANALISIS INICIAL DE LA VARIABLE ESTADO - nivel --------

# Cambiar el nombre de la columna estado a nivel
covid_transformado <-  covid_transformado %>% 
  rename(nivel = "estado")

# Total registros informados
covid_transformado %>% 
  summarise(total = sum(!is.na(nivel))) %>% 
  View()

# Informacion almacenada en la variable estado
covid_transformado %>% 
  group_by(nivel) %>% 
  summarise(total = sum(!is.na(nivel))) %>% 
  View()


# Se debe actualizar leve y LEVE  a leve.
covid_transformado <- covid_transformado %>% 
  mutate(nivel = ifelse(nivel == "leve", "Leve", nivel)) %>% 
  mutate(nivel = ifelse(nivel == "LEVE", "Leve", nivel))

# Analisis de los N/A
covid_transformado %>% 
  filter(nivel == "N/A") %>% 
  View()

# Transformar los N/A en nivel a Fallecido cuando la ubicacion es cementerio.
covid_transformado <- covid_transformado %>% 
  mutate(nivel = ifelse(ubicacion == "Cementerio", "Fallecido", nivel))
  
  
## ------- ANALISIS INICIAL DE LA VARIABLE CODIGO Y NOMBRE PAIS --------

# Total registros con informacion
covid_transformado %>% 
  summarise(total = sum(!is.na(codpais))) %>% 
  View()

# Total registros sin informacion
covid_transformado %>% 
  summarise(total = sum(is.na(codpais))) %>% 
  View()

# Se debe actualizar el codigo de pais a 170 y el nombre del pais a Colombia.
covid_transformado <- covid_transformado %>% 
  mutate(codpais = 170) %>% 
  mutate(nompais = "COLOMBIA")

## ------- ANALISIS INICIAL DE LA VARIABLE RECUPERADO - estado ------

# Actualizar el nombre de la variable recuperado a estado
covid_transformado <- covid_transformado %>% 
  rename(estado = "recuperado")

# Total registros en la variable estado
covid_transformado %>% 
  summarise(total = sum(!is.na(estado))) %>% 
  View()

# Informacion almacenada en la variable recuperado
covid_transformado %>% 
  group_by(estado) %>% 
  summarise(total = sum(!is.na(estado))) %>% 
  View()

# Se debe actualizar fallecido a Fallecido y actualizar los N/A a Fallecido siempre y cuando el nivel sea Fallecido
covid_transformado <- covid_transformado %>% 
  mutate(estado = ifelse(estado == "fallecido", "Fallecido", estado)) %>% 
  mutate(estado = ifelse(nivel == "Fallecido", "Fallecido", estado))


## -------- ANALISIS INICIAL DE LA VARIABLE FECHA DE SINTOMAS -----

# Total registros informados
covid_transformado %>% 
  summarise(total = sum(is.na(fsintomas))) %>% 
  View()

# analisis de la fecha de sintomas cuando tiene datos faltantes
covid_transformado %>% 
  filter(is.na(fsintomas)) %>% 
  summarise(total = sum(is.na(fsintomas))) %>% 
  View()

# Datos faltantes en la variablbe fecha de sintomas
covid_transformado %>% 
  filter(is.na(fsintomas)) %>% 
  View()

## ----- ANALISIS INICIAL DE LA VARIABLE FECHA DE MUERTE ------

# total registros informados
covid_transformado %>% 
  summarise(total = sum(!is.na(fmuerte))) %>% 
  View()


## ------- ANALISIS INICIAL DE LA VARIABLE FECHA DE DIAGNOSTICO -------

# Total registros informados en la fecha de diagnostico
covid_transformado %>% 
  summarise(total = sum(!is.na(fdiagnos))) %>% 
  View()

# Total registros sin informacion de fecha de diagnostico
covid_transformado %>% 
  summarise(total = sum(is.na(fdiagnos))) %>% 
  View()


## ------- ANALISIS INICIAL DE LA FECHA DE RECUPERACION -------

# Total registros informados en la fecha de recuperacion
covid_transformado %>% 
  summarise(total = sum(!is.na(frecuper))) %>% 
  View()

# Total registros sin informacion en la fecha de recuperacion
covid_transformado %>% 
  summarise(total = sum(is.na(frecuper))) %>% 
  View()


## ------ ANALISIS INICIAL DE LA VARIABLE TIPO DE RECUPERACION -----

# Total registros informados 
covid_transformado %>% 
  summarise(total = sum(!is.na(tiprecup))) %>% 
  View()


# Total registros sin informacion en el tipo de recuperacion
covid_transformado %>% 
  summarise(total = sum(is.na(tiprecup))) %>% 
  View()

# Informacion almacenada en la variable tipo de recuperacion
covid_transformado %>% 
  group_by(tiprecup) %>% 
  summarise(total = sum(!is.na(tiprecup))) %>% 
  View()



## ------- ANALISIS INICIAL DE LA VARIABLE PERTENENCIA ETNICA - codetnia -------

# Total registros informados 
covid_transformado %>% 
  summarise(total = sum(!is.na(codetnia))) %>% 
  View()

# Total registros sin informacion
covid_transformado %>% 
  summarise(total = sum(is.na(codetnia))) %>% 
  View()

# Informacion almacenda en pertencia etnica 
covid_transformado %>% 
  group_by(codetnia) %>% 
  summarise(total = sum(!is.na(codetnia))) %>% 
  View()


## ------ ANALISIS INICIAL DE LA VARIABLE NOMBRE DEL GRUPO ETNICO - grupetnia --------

# Total registros informados
covid_transformado %>% 
  summarise(total = sum(!is.na(grupetnia))) %>% 
  View()

# Total registros sin informacion
covid_transformado %>% 
  summarise(total = sum(is.na(grupetnia))) %>% 
  View()

# Informacion almacenado en la variable grupetnia
covid_transformado %>% 
  group_by(grupetnia) %>% 
  summarise(total = sum(is.na(grupetnia))) %>% 
  View()


## ------ COMANDOS PARA CONOCER LOS NOMBRES Y EL TIPO DE LAS COLUMNAS ------
glimpse(covid_transformado)
glimpse(covid_original)



###  ---------                                                         ---------------
###  --------- TRANSFORMACON DEL ARCHIVO DISTRIBUCION VACUNAS ORIGINAL ---------------
###  ---------                                                         ---------------


# Creacion del archivo distribucion vacunas transformado
dist_vacunas_transformado <- dist_vacunas_original

# Actualizar los nombres de las columnas
dist_vacunas_transformado <- dist_vacunas_transformado %>% 
  rename(codres = "Num_Resolucion", fresol = "Fecha_Resolucion", año = "Año", coddpto = "Cod_Territorio",
         nomdpto = "Nom_Territorio", laboratorio = "Laboratorio_Vacuna", dosis = "Cantidad", uso = "Uso_vacuna",
         fcorte = "fecha_corte")

# Creacion de columnas mes y nrosemana
dist_vacunas_transformado <- dist_vacunas_transformado %>% 
  mutate(nrosem = isoweek(fresol)) %>% 
  mutate(mes = month(fresol, label =TRUE))


## ----- ANALISIS INICIAL DE LA VARIABLE CODIGO DE LA RESOLUCION -------

# Total registros en la variable
dist_vacunas_transformado %>% 
  summarise(total = sum(!is.na(codres))) %>% 
  View()

# Agrupacion de registros por la variable codigo de la resolucion
dist_vacunas_transformado %>% 
  group_by(codres) %>% 
  summarise(total = sum(!is.na(codres))) %>% 
  View()

# Valores minimas y maximas de codigo de resolucion
dist_vacunas_transformado %>% 
  summarise(codigo_minimo = min(codres),
            codigo_maximo = max(codres)) %>% 
  View()


#  -------- ANALISIS INICIAL DE LA VARIABLE FECHA DE RESOLUCION -----

# Total registros informados par la variable
dist_vacunas_transformado %>% 
  summarise(total = sum(!is.na(fresol))) %>% 
  View()

# Fechas maxima y minima registradas
dist_vacunas_transformado %>% 
  summarise(fecha_minima = min(fresol),
            fecha_maxima = max(fresol)) %>% 
  View()


# ------- ANALISIS INIICSL DE LA VARIABLE AÑO --------

# Total  registros informados
dist_vacunas_transformado %>% 
  summarise(total = sum(!is.na(año))) %>% 
  View()

# Registros informados en la variable año
dist_vacunas_transformado %>% 
  group_by(año) %>% 
  summarise(total = sum(!is.na(año))) %>% 
  View()


#  -------- ANALISIS INICIAL DE LAS VARIABLES CODIGOS Y NOMBRES DE DEPARTAMENTO -------

# Registros totales en la variable codigo de departamento
dist_vacunas_transformado %>% 
  summarise(total = sum(!is.na(coddpto))) %>% 
  View()

# Registros totales en la variable nombre de departamento
dist_vacunas_transformado %>% 
  summarise(total = sum(!is.na(nomdpto))) %>% 
  View()

# Registro de codigos y nombres incorrectos o repetidos
dist_vacunas_transformado %>% 
  group_by(coddpto, nomdpto) %>% 
  summarise(total = sum(!is.na(nomdpto))) %>% 
  arrange(nomdpto) %>% 
  View()

# Se debe transformar la siguientes nombres de departamentos: Barranquilla a Atlantico, Bogota DC a Bogota,
# Buenaventura a Valle, Cali a Valle, Cartagena a Bolivar, La Guajira a Guajira, San Andres, Providencia y
# Santa Catalina a San Andres, Santa Martha y Santa Marta a Magadalena, Valle del cauca a Valle
dist_vacunas_transformado <- dist_vacunas_transformado %>% 
  mutate(nomdpto = ifelse(nomdpto == "BARRANQUILLA", "ATLANTICO", nomdpto )) %>% 
  mutate(nomdpto = ifelse(nomdpto == "BOGOTA D.C.", "BOGOTA", nomdpto )) %>% 
  mutate(nomdpto = ifelse(nomdpto == "BUENAVENTURA", "VALLE", nomdpto )) %>% 
  mutate(nomdpto = ifelse(nomdpto == "CALI", "VALLE", nomdpto )) %>% 
  mutate(nomdpto = ifelse(nomdpto == "CARTAGENA", "BOLIVAR", nomdpto )) %>% 
  mutate(nomdpto = ifelse(nomdpto == "LA_GUAJIRA", "GUAJIRA", nomdpto )) %>% 
  mutate(nomdpto = ifelse(nomdpto == "SAN ANDRES, PROVIDENCIA Y SANTA CATALINA", "SAN ANDRES", nomdpto )) %>% 
  mutate(nomdpto = ifelse(nomdpto == "SANTA MARTA", "MAGDALENA", nomdpto )) %>% 
  mutate(nomdpto = ifelse(nomdpto == "SANTA MARTHA", "MAGDALENA", nomdpto )) %>% 
  mutate(nomdpto = ifelse(nomdpto == "VALLE_DEL_CAUCA", "VALLE", nomdpto ))

  
# Se debe actualizar los siguientes codigos de departamentos: Atlantico, Bolivar, Magdalena, Valle
dist_vacunas_transformado <- dist_vacunas_transformado %>% 
  mutate(coddpto = ifelse(coddpto == 8001, 8, coddpto)) %>% 
  mutate(coddpto = ifelse(coddpto == 13001, 13, coddpto)) %>% 
  mutate(coddpto = ifelse(coddpto == 47001, 47, coddpto)) %>% 
  mutate(coddpto = ifelse(coddpto == 76001, 76, coddpto)) %>% 
  mutate(coddpto = ifelse(coddpto == 76109, 76, coddpto)) 


# ------- ANALISIS DE LA VARIABLE LABORATORIO -------

# Total registros en la variable laboratorio
dist_vacunas_transformado %>% 
  summarise(total = sum(!is.na(laboratorio))) %>% 
  View()

# Informacion almacenada en la variable 
dist_vacunas_transformado %>% 
  group_by(laboratorio) %>% 
  summarise(total = sum(!is.na(laboratorio))) %>% 
  View()

# Se debe actualizar el nombre del laboratorio AZTRAZENECA por ASTRAZENECA
dist_vacunas_transformado <- dist_vacunas_transformado %>% 
  mutate(laboratorio = ifelse(laboratorio == "AZTRAZENECA", "ASTRAZENECA", laboratorio))


# ---------- ANALISIS DE LA VARIABLE DOSIS ---------

# Total de registros informados en la variable dosis
dist_vacunas_transformado %>% 
  summarise(total = sum(!is.na(dosis))) %>% 
  View()

# Numero de dosis por laboratorio
dist_vacunas_transformado %>% 
  group_by(laboratorio) %>% 
  summarise(total = sum(!is.na(dosis))) %>% 
  View()


# -------- ANALISIS DE LA VARIABLE USO ---------

# Total registros con informacion en la variable uso
dist_vacunas_transformado %>% 
  summarise(total = sum(!is.na(uso))) %>% 
  View()

# Total registros sin informacion en la variable uso
dist_vacunas_transformado %>% 
  summarise(total = sum(is.na(uso))) %>% 
  View()

# Informacion almacenada en la variable uso
dist_vacunas_transformado %>% 
  group_by(uso) %>% 
  summarise(total = sum(!is.na(uso))) %>% 
  View()


# -------- ANALISIS DE LA VARIABLE FECHA DE CORTE ---------

# Total registros informados
dist_vacunas_transformado %>% 
  summarise(total = sum(!is.na(fcorte))) %>% 
  View()

# Fecha minima y maxima
dist_vacunas_transformado %>% 
  summarise(fecha_minima = min(fcorte),
            fecha_maxima = max(fcorte)) %>% 
  View()



## ------ COMANDOS PARA CONOCER LOS NOMBRES Y EL TIPO DE LAS COLUMNAS ------
glimpse(dist_vacunas_transformado)
glimpse(dist_vacunas_original)

