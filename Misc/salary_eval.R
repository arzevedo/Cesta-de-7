library(rvest)
library(tidyverse)

salarios <- list()
for(i in 2013:2019){
  season_int <- paste0(i - 1, "-", i)
  sal <- read_html(paste0("https://hoopshype.com/salaries/players/",
                          season_int,"/")) %>% 
    html_nodes("table") %>% 
    html_table(header = TRUE) %>% as.data.frame() %>% 
    as_tibble() %>%  
    mutate(season = i) %>% select(-4) %>%
    rename(salaries = 3)
  
  salarios[[i - 2013 + 1]] <- sal
  
  Sys.sleep(15) #Be kind always (smile face motherfucker)
  
}

df_all_variables %>% 
  mutate(names_wo_dots = player_name %>% str_replace_all("\\.","")) %>% 
  left_join(y = salaries, by = c("names_wo_dots" = "Player",
                                 "season" = "season")) %>% select(names_wo_dots,player_name,
                                                                  salaries) %>% View()


salaries <- salarios %>% 
  map_df(bind_cols) %>% select(-1) %>% 
  mutate(salaries = salaries %>% 
           str_replace("\\$", "") %>% 
           str_replace_all(",", "") %>% 
           as.numeric())

#write_csv(salaries, "nba_players_salaries_14_19.csv")
salaries <- read_csv("nba_players_salaries_14_19.csv")
df_all_variables %>% 
  left_join(y = salaries, by = c("player_name" = "Player",
                                 "season" = "season")) %>%
  filter(mp >= quantile(mp, .25)) %>% 
  ggplot(aes(salaries, raptor_defense, label = paste(player_name, season))) +
  ggthemes::theme_fivethirtyeight() +
  geom_smooth(method = "lm", se = FALSE) +
  geom_point() +
  #geom_vline(xintercept = quantile(salaries, .7)) +
  facet_wrap(~ season_type) +
  ggrepel::geom_text_repel(data = . %>% filter(raptor_defense >= 7|
                                               raptor_defense <= -7))+
  scale_x_continuous(labels = scales::dollar) + 
  labs(y = "Raptor Defense") + 
  theme(axis.title = element_text(size = 10),
        axis.title.x = element_blank())

df_all_variables %>% 
  left_join(y = salaries, by = c("player_name" = "Player",
                                 "season" = "season")) %>%
  filter(mp >= quantile(mp, .25)) %>% 
  ggplot(aes(salaries, raptor_offense, label = paste(player_name, season))) +
  ggthemes::theme_fivethirtyeight() +
  geom_smooth(method = "lm", se = FALSE) +
  geom_point() +
  facet_wrap(~ season_type) +
  ggrepel::geom_text_repel(data = . %>% filter(raptor_offense >= 8|
                                               raptor_offense <= -8))+
  scale_x_continuous(labels = scales::dollar) + 
  labs(y = "Raptor offense") +
  theme(axis.title = element_text(size = 10),
        axis.title.x = element_blank())
