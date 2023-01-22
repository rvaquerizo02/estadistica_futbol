# Librerías de statsbomb
# devtools::install_github("statsbomb/SDMTools")
# devtools::install_github("statsbomb/StatsBombR")


library(tidyverse)
library(StatsBombR)
library(lubridate)


# Ha dejado de funcionar
StatsBombData <- free_allevents()

comp <- FreeCompetitions()

partidos_messi <- comp %>%
  filter(competition_id == 11) %>%
  FreeMatches()

messi_data <- StatsBombFreeEvents(MatchesDF = partidos_messi)

# saveRDS(messi_data,'./data/messi_data.rds')
messi_data2 <- readRDS('./data/messi_data.rds')

entrenadores <- partidos_messi %>% mutate(entrenador_bcn = case_when(
  home_team.home_team_name=='Barcelona' ~ as.character(map(home_team.managers,2)),
  away_team.away_team_name=='Barcelona' ~ as.character(map(away_team.managers,2)))) %>%
  dplyr::select(match_id, match_date,entrenador_bcn) %>%
  arrange(match_date)

fechas_entrenadores <- entrenadores %>% group_by(entrenador_bcn) %>%
  summarise(f_inicio = min(match_date),f_fin=max(match_date)) %>%
  filter(entrenador_bcn !='NULL') %>% mutate(
    f_inicio = as.Date(f_inicio), f_fin=as.Date(f_fin)  ) %>% arrange(f_inicio) %>%
  mutate(entrenador_bcn2=paste0(row_number(), ". ", entrenador_bcn))

entrenadores <- entrenadores %>%
  left_join(select(fechas_entrenadores, entrenador_bcn, entrenador_bcn2)) %>%
  select(-entrenador_bcn) %>% rename(entrenador_bcn=entrenador_bcn2)

#Tenemos valores perdidos

entrenadores2 <- entrenadores %>% filter(is.na(entrenador_bcn)) %>%
  select(-entrenador_bcn) %>%
  mutate(match_date=as.Date(match_date)) %>% crossing(fechas_entrenadores) %>%
  filter(match_date>=f_inicio & match_date<=f_fin) %>%
  select(-f_inicio, -f_fin, -entrenador_bcn) %>%
  rename(entrenador_bcn=entrenador_bcn2)

entrenadores <- entrenadores %>% filter(!is.na(entrenador_bcn))

entrenadores <- rbind.data.frame(entrenadores, entrenadores2)

# borra <- anti_join( partidos_messi,entrenadores,by = c("match_id"))


intervenciones_messi <- messi_data %>% filter(player.id==5503) %>%
  mutate(tiro=ifelse(type.name=='Shot',1,0),
         gol=ifelse(shot.outcome.name=='Goal',1,0)) %>%
  group_by(match_id) %>%
  summarise(intervenciones = n(),
            tiros=sum(tiro, na.rm = T),
            goles=sum(gol, na.rm = T))

intervenciones_messi <- intervenciones_messi %>% left_join(entrenadores)

table(entrenadores$entrenador_bcn)

intervenciones_messi %>% filter(!is.na(entrenador_bcn)) %>%
  ggplot(aes(y=entrenador_bcn, x=goles, color=entrenador_bcn, fill=entrenador_bcn)) +
  geom_violin( alpha=0.3) +geom_jitter()

entrenadores <- partidos_messi %>% mutate(entrenador = case_when(
  home_team.home_team_name=='Barcelona' ~ map(home_team.managers,3),
  away_team.away_team_name=='Barcelona' ~ map(away_team.managers,3),
  TRUE ~ "Error"))

borra <- messi_data_raw %>% filter(match_id==3773631)
