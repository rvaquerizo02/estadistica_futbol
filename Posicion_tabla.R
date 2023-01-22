# Librerías

library(tidyverse)
library(lubridate)
library(ggrepel)
library(rvest)

# Reflexion estatica sobre la posición en la tabla
# Lectura de datos de fbref
ub ="https://fbref.com/en/comps/12/La-Liga-Stats"
liga=read_html(ub,as.data.frame=TRUE,stringAsFactors=TRUE) %>%
  html_nodes("table") %>% .[[1]] %>% html_table(fill=TRUE)

liga <- liga %>% mutate(descenso = case_when(
    Rk >= 17 ~ "Descenso",
    TRUE ~ "No descenso"))

liga %>% ggplot(aes(x=xGA, y=GA, label=Squad, color=Rk)) + geom_point()  +
  geom_text_repel(size = 3) +
  scale_color_gradient2(midpoint = 12, low = "blue", mid = "grey",
                        high = "red", space = "Lab" ) +
  theme_light() + ggtitle('Ocasiones en contra vs Goles en contra')

liga %>% ggplot(aes(x=xGA, y=xG, label=Squad, color=Rk)) + geom_point()  +
  geom_text_repel(size = 3) +scale_color_gradient2(midpoint = 12, low = "blue", mid = "grey",
                                                   high = "red", space = "Lab" ) +
  theme_light() + ggtitle('Ocasiones en contra vs Ocasiones a favor')

# Mejoramos los nombres de los equipos para hacer cruces
liga <- liga %>%
  mutate(Squad=chartr("áéíóú", "aeiou", Squad)) %>%
  mutate(Squad=case_when(
    Squad=='Betis' ~ 'Real Betis',
    Squad=='Valladolid' ~ 'Real Valladolid',
    TRUE ~ Squad))

# Lista con los equipos en descenso
Descenso <- liga[liga$Rk>=17,]$Squad

# Disponemos de un programa extracciones_big5 que nos permite ver los tiros a puerta.

tiros <- readRDS( './data/tiros_202212.rds')

# Filtramos sólo La Liga
laliga <- tiros %>% filter(league_name=='LaLiga')

# Estudiamos las ocasiones que hacen a cada equipo
df <- laliga %>%
  mutate(Equipo = case_when(
    team_id==home_team_id ~ home_team,
    TRUE ~ away_team),
    xg = expected_goals,
    Equipo_contrario = case_when(
      team_id==away_team_id ~ home_team,
      TRUE ~ away_team)) %>%
  select(Equipo,Equipo_contrario, xg, event_type, last_name, situation)

# Total de tiros recibidos
df %>% group_by(Equipo_contrario) %>% summarise(tiros=n()) %>%
  mutate(Equipo_contrario = fct_reorder(Equipo_contrario, tiros),
         En_descenso = case_when(
           Equipo_contrario %in% Descenso ~ 'En descenso',
           TRUE ~ 'No descenso')) %>%
  ggplot(aes(x=Equipo_contrario, y=tiros, fill=En_descenso)) +
  geom_bar(stat = "identity" , alpha=0.5, show.legend = FALSE) +
  coord_flip() +  theme_light() + ggtitle("Tiros recibidos")

# Tiros salvados
df %>% filter(event_type=='AttemptSaved') %>% group_by(Equipo_contrario)  %>%
  summarise(tiros=n()) %>%
  mutate(Equipo_contrario = fct_reorder(Equipo_contrario, tiros),
         En_descenso = case_when(
           Equipo_contrario %in% Descenso ~ 'En descenso',
           TRUE ~ 'No descenso')) %>%
  ggplot(aes(x=Equipo_contrario, y=tiros,  fill=En_descenso)) +
  geom_bar(stat = "identity" , alpha=0.5,  show.legend = FALSE) +
  coord_flip() +  theme_light() + ggtitle("Tiros salvados")

# % de tiros salvados
df %>% group_by(Equipo_contrario) %>%
  summarise(tiros=n(), salvados=sum(ifelse(event_type=='AttemptSaved',1,0))) %>%
  mutate(pct_paradas = round(salvados/tiros,3),
         Equipo_contrario = fct_reorder(Equipo_contrario, pct_paradas),
         En_descenso = case_when(
           Equipo_contrario %in% Descenso ~ 'En descenso',
           TRUE ~ 'No descenso')) %>%
  ggplot(aes(x=Equipo_contrario, y=pct_paradas, fill=En_descenso)) +
  geom_bar(stat = "identity" , alpha=0.5, show.legend = FALSE) +
  coord_flip() +  theme_light()

# Se une tabla de ranking con tiros
ntiros <- df %>% group_by(Equipo_contrario) %>% summarise(tiros=n())

ntiros <- ntiros %>% left_join(select(liga, Rk, Squad), by=c("Equipo_contrario"="Squad"))

ntiros %>% mutate(En_descenso = case_when(
                    Equipo_contrario %in% Descenso ~ 'En descenso',
                    TRUE ~ 'No descenso')) %>%
  ggplot(aes(x=Rk, y=tiros, label=Equipo_contrario, color=En_descenso)) +
  geom_point()  +
  geom_text_repel(size = 3) +  theme_light() + ggtitle("Tiros frente a clasificación")

df %>% ggplot(aes(x=xg)) + geom_histogram() +  theme_light()

df %>% group_by(Equipo_contrario) %>% filter(xg>=0.20 & situation != 'Penalty') %>%
  summarise(tiros=n()) %>%
  mutate(Equipo_contrario = fct_reorder(Equipo_contrario, tiros),
         En_descenso = case_when(
           Equipo_contrario %in% c('Cadiz', 'Elche', 'Sevilla', 'Celta Vigo') ~ 'En descenso',
           TRUE ~ 'No descenso')) %>%
  ggplot(aes(x=Equipo_contrario, y=tiros, fill=En_descenso)) +
  geom_bar(stat = "identity" , alpha=0.5, show.legend = FALSE) +
  coord_flip() +  theme_light() + ggtitle('Ocasiones claras (sin penaltis)')


