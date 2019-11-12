## PACOTES ####
library(gridExtra)
library(readr)
library(data.table)
library(dplyr)
library(formattable)
library(tidyr)
library(ggplot2)
library(rvest)
library(xml2)
library(ggalt)

## CÓDIGO ####

medias_12novembro <- read_html(
  paste0("https://www.basketball-reference.com/leagues/NBA_2020_per_game.html")
) %>% html_nodes("table") %>% html_table() %>% as.data.frame

attach(medias_12novembro)

PTS <- as.numeric(PTS)
MP <- as.numeric(MP)
mediasordenadas <- medias_12novembro[order(- PTS), ]                 
media_acima_20 <- mediasordenadas[1:27, ]

ggplot(medias_12novembro, aes(x = MP, y = PTS)) +
  geom_point(na.rm = T) + geom_text_repel(aes(label = ifelse(PTS >= 20, as.character(Player), "")), na.rm = T) +
  geom_encircle(data = media_acima_20, na.rm = T, colour="red", size = 1) +
  xlab("Minutos por jogo") + ylab("Pontos por jogo") + theme_minimal() +
  labs(title = "Nova era ofensiva na NBA",
       subtitle = "Em 12 de novembro temos 27 jogadores com mais de 20 pontos por jogo, seria 30 o novo 20?", 
       caption = "Dados: Basketball-Reference
                  Visualização: @cestade7")
