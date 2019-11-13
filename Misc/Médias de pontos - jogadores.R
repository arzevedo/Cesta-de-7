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

## C?DIGO #### 

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
                  Visualiza??o: @cestade7")



# COMPARANDO ANOS COM ANOS ANTERIORES ----
library(janitor);library(gganimate)
library(rvest);library(tidyverse)
all_players_seasons <- list()

for(i in 2005:2019){
  
  link <- read_html(paste0("https://widgets.sports-reference.com/wg.fcgi?css=1&site=bbr&url=%2Fleagues%2FNBA_",
                           i,
                           "_per_game.html&div=div_per_game_stats"))
  
  jogadores <- link %>% 
    html_nodes("table") %>% html_table() %>% 
    as.data.frame() %>% as_tibble() %>% 
    filter(!(Player == "Player")) %>%
    janitor::clean_names() %>% 
    mutate_at(vars(- player, - pos, - tm),
              as.numeric) %>% 
    mutate(season = i)
    
  cat(i, "\n")
  
  Sys.sleep(10) # Pegando leve
  
  all_players_seasons[[i - 2005 + 1]] <- jogadores
  
}

player_per_game <- map_df(all_players_seasons, bind_rows)

BRRR::skrrrahh()

pts_gm <- player_per_game %>% # filter(season > 2017) %>% 
  ggplot(aes(x = mp, y = pts, label = paste(player, season))) +
  ggrepel::geom_text_repel(data = . %>% group_by(season) %>% 
                             top_n(n = 1, wt = pts),
                           color = "firebrick4",seed = 20, size = 7, max.iter = 1) +
  #ggforce::geom_mark_circle(aes(color = pts > 20)) +
  xlab("Minutos por jogo") + ylab("Pontos por jogo") + theme_minimal() +
  labs(title = "Evolução da quantidade de pontos por jogo através dos anos",
       subtitle = "Ano: {frame_time}",
       caption = "Dados: Basketball-Reference
                  Visualizacão: @cestade7") +
  theme(legend.title = element_blank(), plot.subtitle = element_text(face = "bold"),
        legend.text = element_text(face = 'bold', size = 12),
        axis.title = element_text(size = 14),
        legend.position = c(.1, .95)) +
  guides(size = "none") +
  transition_time(
    season
  ) + 
  #ease_aes('cubic-in-out') +
  ease_aes('linear') +
  shadow_mark(colour = "gray70", size = 2) +
  geom_point(
    aes(color = pts > 20,
        size = pts > 20)
  ) +
  scale_color_manual(values = c("FALSE" = "gray10", "TRUE" = "firebrick4"),
                     labels = c("TRUE" = "+ 20 pontos por jogo",
                                "FALSE" = "- 20 pontos por jogo"))

anim_save(pts_gm, nframes = 150,
          filename = "pts_temp_final_opt2.gif",
          height = 800, width = 800)
BRRR::skrrrahh()