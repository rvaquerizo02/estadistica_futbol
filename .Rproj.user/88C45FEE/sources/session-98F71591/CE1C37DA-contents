
# Librerías

library(tidyverse)
library(lubridate)
library(ggrepel)
library(rvest)


ub2 ="https://www.transfermarkt.es/laliga/startseite/wettbewerb/ES1"

precio <- ub2 %>%
  read_html()%>%
  html_nodes(xpath='//*[@id="yw1"]') %>%
  html_table()

precio <- precio[[1]]
