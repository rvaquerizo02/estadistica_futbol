# Extracciones para análisis de tiros a puerta

library(worldfootballR)
library(tidyverse)
library(lubridate)

# Solo big 5 de fotmob

big5 <- c(47,55,54,87,53)

# Código sin calidad
# Ojo que deja de funcionar con el cambio de año
extraccion <- function(mes) {
  mes2 = ifelse(mes==12, 1, mes+1)

  lista <- seq(as.Date(paste0("2022/",mes,"/1")),
               as.Date(paste0("2023/",mes2,"/01"))-1, "days")
  lista <- format(lista, format='%Y%m%d')
  results <- fotmob_get_matches_by_date(date=lista) %>% filter(match_league_id %in% big5)
  match_details <- fotmob_get_match_details(results$match_id)
  return(match_details)}

# detalles08 <- extraccion(8)
# detalles09 <- extraccion(9)
# detalles10 <- extraccion(10)
# detalles11 <- extraccion(11)
detalles12 <- extraccion(12)

# No está automatizado ni parametrizado. Se van acumulando meses

tiros <- readRDS('./data/tiros_202211.rds')
tiros <- rbind.data.frame(tiros, detalles12)

# Está pensado para una primera ejecución
#tiros <- rbind.data.frame(detalles08, detalles09, detalles10, detalles11)
# saveRDS(tiros, './data/tiros_202211.rds')

saveRDS(tiros, './data/tiros_202212.rds')
