# Activacion de librerias
library(lubridate)
library(readr)
library(dplyr)
library(ggplot2)
library(tidyverse)

# numero de casos por fecha de reporte
casos_por_reporte <- dtf_casos %>% 
  group_by(frepor) %>% 
  summarise(total_casos = sum(!is.na(frepor))) %>% 
  arrange(desc(frepor))


# Informacion general 
general <- dtf_casos %>% 
  summarise(fecha_reporte = max(frepor, na.rm = TRUE),
            fecha_min_notif = min(fnotif, na.rm = TRUE),
            fecha_max_notif = max(fnotif, na.rm = TRUE),
            casos_reportados = sum(!is.na(idcaso)),
            casos_activos = sum(!is.na(estado) & estado == "Activo"),
            casos_recuperados = sum(!is.na(estado) & estado == "Recuperado"),
            nro_muertes = sum(!is.na(estado) & estado == "Fallecido"))

# Casos nuevos
casos_nuevos <- dtf_casos %>% 
  filter(frepor == max(frepor)) %>% 
  summarise(casos_nuevos = sum(!is.na(frepor)))

  
# Numero de casos por año y acumulados
casos_por_año <-dtf_casos %>% 
  group_by(year(fnotif)) %>% 
  summarise(total_casos = sum(!is.na(fnotif))) %>% 
  ungroup() %>% 
  mutate(casos_acumulados = cumsum(total_casos)) 
 colnames(casos_por_año) <- c("año", "total_casos", "casos_acumulados")


# Informacion general por departamento
general_dpto <- dtf_casos %>% 
  group_by(coddpto, nomdpto) %>% 
  summarise(fecha_reporte = max(frepor, na.rm = TRUE),
            fecha_min_notif = min(fnotif, na.rm = TRUE),
            fecha_max_notif = max(fnotif, na.rm = TRUE),
            casos_reportados = sum(!is.na(idcaso)),
            casos_activos = sum(!is.na(estado) & estado == "Activo"),
            casos_recuperados = sum(!is.na(estado) & estado == "Recuperado"),
            nro_muertes = sum(!is.na(estado) & estado == "Fallecido"))

# Casos nuevos por departamento
casos_nuevos_dpto <- dtf_casos %>% 
  filter(frepor == max(frepor, na.rm = TRUE)) %>% 
  group_by(coddpto, nomdpto) %>% 
  summarise(casos_nuevos = sum(!is.na(frepor))) %>% 
  arrange(desc(casos_nuevos))

# Informacion general a nivel de municipio
general_mpio <- dtf_casos %>% 
  group_by(nomdpto, codmpio, nommpio) %>% 
  summarise(fecha_reporte = max(frepor, na.rm = TRUE),
            fecha_min_notif = min(fnotif, na.rm = TRUE),
            fecha_max_notif = max(fnotif, na.rm = TRUE),
            casos_reportados = sum(!is.na(idcaso)),
            casos_activos = sum(!is.na(estado) & estado == "Activo"),
            casos_recuperados = sum(!is.na(estado) & estado == "Recuperado"),
            nro_muertes = sum(!is.na(estado) & estado == "Fallecido"))

# Casos nuevos a nivel de municipio
casos_nuevos_mpio <- dtf_casos %>% 
  filter(frepor == max(frepor, na.rm = TRUE)) %>% 
  group_by(nomdpto, codmpio, nommpio) %>% 
  summarise(casos_nuevos = sum(!is.na(frepor)))


# Informacion general para un departamento especifico y sus municipios
general_esp_dpto <- dtf_casos %>% 
  filter(nomdpto == "SANTANDER") %>% 
  group_by(nomdpto, codmpio, nommpio) %>% 
  summarise(fecha_reporte = max(frepor, na.rm = TRUE),
            fecha_min_notif = min(fnotif, na.rm = TRUE),
            fecha_max_notif = max(fnotif, na.rm = TRUE),
            casos_reportados = sum(!is.na(idcaso)),
            casos_activos = sum(!is.na(estado) & estado == "Activo"),
            casos_recuperados = sum(!is.na(estado) & estado == "Recuperado"),
            nro_muertes = sum(!is.na(estado) & estado == "Fallecido")) %>% 
  arrange(desc(casos_reportados))


# Casos nuevos para un departamento especifico y sus municipios
casos_nuevos_esp_dpto <- dtf_casos %>% 
  filter(nomdpto == "ANTIOQUIA" & frepor == max(frepor, na.rm = TRUE)) %>%
  group_by(nomdpto, codmpio, nommpio) %>% 
  summarise(casos_nuevos = sum(!is.na(frepor))) %>% 
  arrange(desc(casos_nuevos)) 


# Casos totales por año por departamento
casos_año_dpto <- dtf_casos %>% 
  group_by(coddpto, nomdpto, year(fnotif)) %>% 
  summarise(total_casos = sum(!is.na(idcaso))) %>% 
  ungroup() %>% 
  mutate(acumulados = cumsum(total_casos))
colnames(casos_año_dpto) <- c("Codigo", "Departamento", "Año", "Casos_reportados", "Casos_acumulados")


# Casos totales por mes
casos_mes <- dtf_casos %>% 
  group_by(year(fnotif), month(fnotif)) %>% 
  summarise(casos_totales = sum(!is.na(idcaso))) %>% 
ungroup() %>% 
  mutate(acumulados = cumsum(casos_totales))


# Casos totales por semana
casos_semana  <- dtf_casos %>% 
  group_by(year(fnotif), nrosem) %>% 
  summarise(casos_totales = sum(!is.na(idcaso))) %>% 
  ungroup() %>% 
  mutate(acumulados = cumsum(casos_totales))


# Casos totales por dia
casos_dia  <- dtf_casos %>% 
  group_by(fnotif) %>% 
  summarise(casos_totales = sum(!is.na(idcaso))) %>% 
  ungroup() %>% 
  mutate(acumulados = cumsum(casos_totales))
colnames(casos_dia) <- c("Fecha", "Casos_totales", "Casos_acumulados")


# Casos totales dia por departamento
casos_dia_dpto <- dtf_casos %>% 
  filter(nomdpto=="ANTIOQUIA") %>% 
  group_by(nomdpto, fnotif) %>% 
  summarise(total_casos = sum(!is.na(idcaso))) %>% 
ungroup() %>% 
  mutate(acumulados = cumsum(total_casos))
colnames(casos_dia_dpto) <- c("Departamento", "Fecha_notificacion", "Total_casos", "Casos_acumulados")


# Casos diarios para un municipio especifico
casos_dia_mpio <- dtf_casos %>% 
  filter(nomdpto == "ANTIOQUIA" & nommpio == "MEDELLIN") %>% 
  group_by(nomdpto, nommpio, fnotif) %>% 
  summarise(total_casos = sum(!is.na(idcaso))) %>% 
  ungroup() %>% 
  mutate(casos_acum = cumsum(total_casos))
  colnames(casos_dia_mpio) <- c("Departamento", "Municipio", "Fecha_notificacion", "Casos_registrados", "Casos_acumulados")
  

# Numero de muertes por año
muertes_año <- dtf_casos %>% 
  filter(estado == "Fallecido") %>% 
  group_by(year(fmuerte)) %>% 
  summarise(total_muertes = sum(!is.na(fmuerte))) %>% 
  ungroup() %>% 
    mutate(acumulados = cumsum(total_muertes))
  colnames(muertes_año) <- c("Año", "Fallecimientos", "Muertes_acumuladas")
  
# Número de muertes por mes
muertes_mes <- dtf_casos %>% 
  filter(estado == "Fallecido") %>% 
  group_by(year(fmuerte), month(fmuerte)) %>% 
  summarise(total = sum(!is.na(fmuerte))) %>% 
  ungroup() %>% 
  mutate(acumulados = cumsum(total))
colnames(muertes_mes) <- c("Año", "Mes", "Fallecimientos", "Muertes_acumuladas")
muertes_mes <- (unite(muertes_mes, año_mes, Año, Mes, sep = "-"))

  
# Número de muertes por semana
muertes_semana <- dtf_casos %>% 
    filter(estado == "Fallecido") %>% 
    group_by(year(fmuerte), nrosem) %>% 
    summarise(total = sum(!is.na(fmuerte))) %>% 
    ungroup() %>% 
    mutate(acumulados = cumsum(total))
  colnames(muertes_semana) <- c("Año", "Nro_semana", "Fallecimientos", "Muertes_acumuladas")
muertes_semana <- unite(muertes_semana, año_semana, Año, Nro_semana, sep = "_")

  
# Número de muertes diarias
muertes_dia <- dtf_casos %>% 
  filter(estado == "Fallecido") %>% 
  group_by(fmuerte) %>% 
  summarise(total = sum(!is.na(fmuerte))) %>% 
  ungroup() %>% 
  mutate(acumulados = cumsum(total))
colnames(muertes_dia) <- c("Fecha", "Fallecimientos", "Muertes_acumuladas")

  
# Numero de muertos por año y por departamento
muertes_año_dpto <- dtf_casos %>% 
  filter(estado == "Fallecido") %>% 
  group_by(coddpto, nomdpto, year(fmuerte)) %>% 
  summarise(total = sum(!is.na(fmuerte))) %>% 
  ungroup() %>% 
  mutate(casos_acum = cumsum(total))
  colnames(muertes_año_dpto) <- c("Codigo", "Departamento", "Año", "Fallecimientos", "Muertes_acumuladas")
  
# Número de muertes por semana y por departamento


