library(worldfootballR)
library(tidyverse)
library(stringr)
library(gganimate)

# Toda la información la extraemos de FBRef
# Extraemos los partidos

partidos <- data.frame(url=fb_match_urls(country = "ESP", gender = "M",
                          season_end_year = c(2015:2023), tier = "1st"))

partidos <- partidos %>% filter(grepl("Real-Madrid",url) >0 )

# Alineaciones partido a partido. Este proceso es largo
# No lo ejecutéis, está en el directorio data

alineaciones <- tibble()

for (i in seq(1:nrow(partidos))) {
  ax <- fb_match_lineups(partidos[i,1])
  alineaciones <- rbind.data.frame(alineaciones, ax)
}


# saveRDS(alineaciones, './data/alineaciones_madrid.rds')
alineaciones <- readRDS('./data/alineaciones_madrid.rds')

# Para identificar el conjunto local se emplea la url del partido
alineaciones <- alineaciones %>% mutate(local =substr(MatchURL,30, length(MatchURL)),
                                        local = substr(local, str_locate(local, "/")[,1]+1, length(MatchURL)),
                                        local = str_replace(local,'El-Derbi-Madrileno-',''),
                                        local = str_replace(local, 'El-Clasico-',''))

# Podemos buscar longitudes extremas
table(nchar(alineaciones$local))

alineaciones <- alineaciones %>%
  mutate(equipo = case_when(
    substr(local,1,11)=='Real-Madrid' & Home_Away=='Home' ~ "RM",
    substr(local,1,11) != 'Real-Madrid' & Home_Away=='Away' ~ "RM",
    TRUE ~ 'Rival'  ))

alineaciones_RM <- alineaciones %>% filter(equipo=="RM")

# Validación

table(alineaciones_RM$Player_Name)

# borra <- alineaciones_RM %>% filter(Player_Name=='Fernando Pacheco')

# Vamos a sacar las alineaciones titulares solamente
alineaciones_RM <- alineaciones_RM %>% filter(Starting=='Pitch') %>%
  mutate(Pais = ifelse(Nation=='ESP', 'Nacional', 'No nacional'))

alineaciones_RM <- alineaciones_RM %>% group_by(Dia = as.Date(Matchday),Pais) %>%
  summarise(conteo=n()) %>% as_tibble()

p <- ggplot(alineaciones_RM) +
  geom_line(aes(x=Dia, y=conteo, color = Pais)) +
  labs(x = "Día de partido", y = "Número de jugadores") +
  transition_reveal(Dia) + theme_classic()

animacion2 <- animate(p, end_pause = 25, fps=5, height = 800, width =800)
anim_save("animacion2.gif", animacion2)


