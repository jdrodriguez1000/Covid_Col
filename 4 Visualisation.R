# Activacion de librerias
library(lubridate)
library(readr)
library(dplyr)
library(ggplot2)
library(forcats)

# Grafico de total de casos por año
ggplot(casos_por_año, aes(año, total_casos)) +
  geom_col(fill = "#D4E6F1") +
  theme_classic() +
  labs(title = "Casos por año",
       x= "Años",
       y ="Numero de casos",
       caption = "INS Colombia") +
  geom_text(aes(label = total_casos, vjust= 1.5, hjust=0.5))

# Grafico de casos acumulados por año 
ggplot(casos_por_año, aes(año, casos_acumulados)) +
  geom_col(fill= "#D98880") +
  theme_classic() +
  labs(title = "Casos acumulados por año",
       x= "Años",
       y ="Numero de casos acumulados",
       caption = "INS Colombia") +
  geom_text(aes(label = casos_acumulados, vjust= 1.5, hjust=0.5))


# Grafico de casos totales por dia
ggplot(casos_dia, aes(Fecha, Casos_totales)) +
  geom_line(color = "#6C3483") + 
  theme_classic() +
  labs(title = "Distribución de casos diarios",
       x = "Fechas",
       y = "Número de casos",
       caption = "INS Colombia") +
  theme(axis.text.x = element_text(angle = 90, size = 7)) +
  theme(axis.text.y = element_text(size = 7)) 


# Gráfico caumulado de casos totales por dia
ggplot(casos_dia, aes(Fecha, Casos_acumulados)) +
  geom_line(color = "#6C3483") + 
  theme_classic() +
  labs(title = "Distribución de casos diarios",
       x = "Fechas",
       y = "Número de casos",
       caption = "INS Colombia") +
  theme(axis.text.x = element_text(angle = 90, size = 7)) +
  theme(axis.text.y = element_text(size = 7)) 


# Gráfico total casos por departamento
ggplot(casos_año_dpto, aes(Departamento, Casos_reportados)) +
  geom_col(fill = "#BB8FCE") +
  theme_classic() +
  coord_cartesian(ylim = c(0, 2000000), expand = TRUE)+
  theme(axis.text.x = element_text(angle = 90, size = 6)) +
  labs(title = "Casos totales por departamento",
       x= "Departamentos",
       y ="Numero de casos",
       caption = "INS Colombia") 

#Grafico total de casos reportados por departamento y año
ggplot(casos_año_dpto) +
  geom_col(mapping = aes(x=Departamento, y=Casos_reportados, fill= Año)) +
  theme_classic() +
  coord_cartesian(ylim = c(0, 1900000), expand = TRUE)+
  theme(axis.text.x = element_text(angle = 90, size = 7)) +
  theme(axis.text.y = element_text(size = 6)) +
  labs(title = "Casos totales por departamento",
       x= "Departamentos",
       y ="Numero de casos",
       caption = "INS Colombia") 


# Grafico de casos totales por dia para un departamento
ggplot(casos_dia_dpto, aes(Fecha_notificacion, Total_casos)) +
  geom_line(color = "#16A085") +
  theme_classic() +
  labs(title = "Casos diarios departamento de Antioquia",
       x = "Fechas",
       y = "Casos registrados",
       caption = "INS Colombia") + 
  theme(axis.text.x = element_text(angle = 90, size = 7)) +
  theme(axis.text.y = element_text(size = 7))   

# Grafico de casos acumulados diarios para un departamento
ggplot(casos_dia_dpto, aes(Fecha_notificacion, Casos_acumulados)) +
  geom_line(color = "#16A085") +
  theme_classic() +
  labs(title = "Casos acumulados diarios departamento de Antioquia",
       x = "Fechas",
       y = "Casos acuumulados",
       caption = "INS Colombia") + 
  theme(axis.text.x = element_text(angle = 90, size = 7)) +
  theme(axis.text.y = element_text(size = 7))   


# grafico de casos diarios para un municipio
ggplot(casos_dia_mpio, aes(Fecha_notificacion, Casos_registrados)) +
  geom_line(color ="#D35400") +
  theme_classic() +
  labs(title = "Casos diarios registrados municipio de Sabaneta",
       x = "Fechas",
       y = "Casos registrados",
       caption = "INS Colombia") +
  theme(axis.text.x = element_text(angle = 90, size = 7)) +
  theme(axis.text.y = element_text(size = 7))  

# Gráfico de casos acumulados para un municipio
ggplot(casos_dia_mpio, aes(Fecha_notificacion, Casos_acumulados)) +
  geom_line(color ="#2471A3") +
  theme_classic() +
  labs(title = "Casos acumulados diarios municipio de Sabaneta",
       x = "Fechas",
       y = "Casos acumuluados",
       caption = "INS Colombia") +
  theme(axis.text.x = element_text(angle = 90, size = 7)) +
  theme(axis.text.y = element_text(size = 7)) 


# Grafico de muertes por año
ggplot(muertes_año, aes(Año, Fallecimientos)) +
  geom_col(fill = "#F06292") +
  theme_bw() +
  labs(title = "Numero de fallecimientos por año",
       x = "Años",
       y = "Fallecimientos",
       caption = "INS Colombia") +
  theme(axis.text.x = element_text(size = 7)) +
  theme(axis.text.y = element_text(size = 7)) +
  geom_text(aes(label = Fallecimientos, vjust= 1.5, hjust=0.5))


# Grafico de muertes acumuladas por año
ggplot(muertes_año, aes(Año, Muertes_acumuladas)) +
  geom_col(fill = "#F06292") +
  theme_bw() +
  labs(title = "Fallecimientos acumulados por año",
       x = "Años",
       y = "Fallecimientos",
       caption = "INS Colombia") +
  theme(axis.text.x = element_text(size = 7)) +
  theme(axis.text.y = element_text(size = 7)) +
  geom_text(aes(label = Muertes_acumuladas, vjust= 1.5, hjust=0.5))


# Grafico numero de muertes diarias
ggplot(muertes_dia, aes(Fecha, Fallecimientos)) +
  geom_line(color= "#CC0033") +
  theme_bw() +
  labs(title = "Comportamiento de muertes por dia",
       x = "Fechas",
       y = "Número de fallecimientos",
       caption = "INS Colombia") +
  theme(axis.text.x = element_text(angle = 90, size = 7)) +
  theme(axis.text.y = element_text(size = 7))

# Grafico de muertes acumuladas diarias
ggplot(muertes_dia, aes(Fecha, Muertes_acumuladas)) +
  geom_line(color= "#CC0033") +
  theme_bw() +
  labs(title = "Comportamiento de muertes acumuladas por dia",
       x = "Fechas",
       y = "Fallecimientos",
       caption = "INS Colombia") +
  theme(axis.text.x = element_text(angle = 90, size = 7)) +
  theme(axis.text.y = element_text(size = 7)) 

# Comportamiento de muertes por mes
ggplot(muertes_mes, aes(año_mes, Fallecimientos)) +
  geom_col(fill = "#90CAF9") +
  theme_bw() + 
  labs(title = "Comportamiento de muertes por mes",
       x = "Meses",
       y = "Fallecimientos",
       caption = "INS Colombia") +
  theme(axis.text.x = element_text(angle = 90, size = 7)) +
  theme(axis.text.y = element_text(size = 7)) +
  geom_text(aes(label = Fallecimientos, vjust= 1.5, hjust=0.5), size =3)


# Número de casos por rangos de edad
ggplot(dtf_casos, aes(edad)) +
  geom_histogram(bins = 20, alpha=0.5, color = "#AEB6BF", fill="#D7BDE2") +
  theme_bw() +
  labs(title = "Histograma de casos reportados por edad",
       x = "Edad",
       y = "Casos",
       caption = "INS Colombia") 


# Histograma de casos por mes
ggplot(dtf_casos, aes(fnotif)) +
  geom_histogram(bins = 30, alpha=0.5, color = "#AEB6BF", fill="#D7BDE2") +
  theme_bw() +
  labs(title = "Histograma de casos reportados por fecha",
       x = "Fecha",
       y = "Casos",
       caption = "INS Colombia") +
  theme(axis.text.x = element_text(angle = 90, size = 6))


# Grafico de boxplot
ggplot(dtf_casos, aes(sexo, edad, color = sexo)) +
  geom_boxplot() + 
  theme_bw() +
  labs(title = "Comportamiento de contagios por sexo y edad",
       x = " Sexo",
       y = "Edad",
       caption = "INS Colombia")
  