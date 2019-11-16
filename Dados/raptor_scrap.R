library(tidyverse);library(rvest)
link_five <- read_html("https://projects.fivethirtyeight.com/2020-nba-player-ratings/")

five_df <- link_five %>% 
  html_nodes("table") %>% html_table(fill = TRUE) %>% 
  as.data.frame() %>% as_tibble() %>% 
  janitor::clean_names()

colnames(five_df) <- c("player", "team", "pos", "mp", "box_sco_off",
                       "box_sco_def", "box_sco_tot", "on_off_off",
                       "on_off_def", "on_off_tot", "ovr_off", "ovr_def",
                       "ovr_tot", "war", "null")

five_df <- five_df %>% select(-null) %>% 
  filter(player != "Player") %>% 
  mutate_at(vars(mp:war), as.numeric) %>% 
  separate(col = pos, 
           into = c("pri_pos", "sec_pos", "ter_pos"),
           sep = ", ",convert = TRUE)


