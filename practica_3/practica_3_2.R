# Script Practice 3_2 cargar datos observados  Boyaca
# Author: Rodriguez-Espinoza J.
# Repository: https://github.com/jrodriguez88/cropmodel_colombia2020
# 2020


# Cargar paquetes
library(tidyverse)
library(data.table)
library(lubridate)
library(skimr)
source("https://raw.githubusercontent.com/jrodriguez88/aquacrop-R/master/make_weather_aquacrop.R", encoding = "UTF-8")

#Localidad
localidad <- "toca_24035040"
latitud <- 5.579444444
longitud <- -73.20794444
altitud <- 2700

#Directorios de trabajo
directorio <- paste0(getwd(), "/practica_3/")
directorio_resultados <- paste0(directorio, "/data/") ; dir.create(directorio_resultados)

# Leer datos observados
data_clima <- read_csv("practica_3/data/24035040_la_copa.csv")
data_clima_nasa <- get_data_nasapower(c("PRECTOT", "T2M_MAX", "T2M_MIN"), 19920101, 20191231, latitud, longitud)

data_clima_nasa %>% setNames(c("date", "rain", "tmax", "tmin")) %>%
  pivot_longer(-date) %>% mutate(source = "nasa") %>% bind_rows(
    data_clima %>% pivot_longer(-date) %>% mutate(source = "ideam")) %>%
  ggplot(aes(date, value, color = source, alpha =  0.5)) + geom_line() + facet_grid(name ~ .) +
  theme_bw()


# Descarga datos nasapower
# Summary 
skim(data_clima)
skim(data_clima_nasa)

#Plot comparar datos
data_clima %>%  
  group_by(year = year(date), month = month(date)) %>%
  summarise(rain = sum(rain), 
            tmin = mean(tmin), 
            tmax = mean(tmax)) %>%
  ungroup() %>% gather(var, value, -c(year, month)) %>% mutate(source = "IDEAM") %>% 
  bind_rows(
    data_clima_nasa %>% setNames(c("date", "rain", "tmax", "tmin")) %>%
      group_by(year = year(date), month = month(date)) %>%
      summarise(rain = sum(rain), 
                tmin = mean(tmin), 
                tmax = mean(tmax)) %>%
      ungroup() %>% gather(var, value, -c(year, month)) %>% mutate(source = "NASA")) %>%
  ggplot(aes(factor(month), value, fill = source)) + 
  geom_boxplot() + 
  facet_wrap(~var, scales = "free") + 
  labs(x = "mes", title = paste("promedios mesuales de", localidad)) +
  theme_bw()

# Covertir a Aquacrop
make_weather_aquacrop(directorio_resultados, localidad, data_clima, latitud, altitud)





