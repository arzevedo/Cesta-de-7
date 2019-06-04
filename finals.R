library(tidyverse)
library(rvest)
library(janitor)

#WRANGLING ------------
link <- read_html("https://www.basketball-reference.com/playoffs/2019-nba-finals-warriors-vs-raptors.html")

finals <- link %>% 
  html_nodes("table") %>% 
  .[5:6] %>% 
  html_table(fill = TRUE) 
finals_gsw <- finals[[2]] %>% as.data.frame()
finals_tor <- finals[[1]] %>% as.data.frame()

colnames(finals_gsw) <- finals_gsw[1,]
colnames(finals_tor) <- finals_gsw[1,]

finals_gsw <- finals_gsw %>% 
  .[-nrow(finals_gsw),] %>% 
  clean_names() %>% 
  as_tibble() %>% 
  mutate_at(vars(-player), list(as.numeric)) %>% 
  .[-1,] %>% 
  rename_at(vars(contains("_2")), funs(str_replace(., "_2", "_pergame"))) %>% 
  mutate(tm = "gsw")
  
finals_tor <- finals_tor %>% 
  .[-nrow(finals_tor),] %>% 
  clean_names() %>% 
  as_tibble() %>% 
  mutate_at(vars(-player), list(as.numeric)) %>% 
  .[-1,] %>% 
  rename_at(vars(contains("_2")), funs(str_replace(., "_2", "_pergame"))) %>% 
  mutate(tm = "tor")

finals_2019_basic <- bind_rows(finals_gsw, finals_tor)

tor_adv <- read_html("https://widgets.sports-reference.com/wg.fcgi?css=1&site=bbr&url=%2Fplayoffs%2F2019-nba-finals-warriors-vs-raptors.html&div=div_TORadvanced") %>% 
  html_nodes("table") %>% 
  html_table() %>% 
  as.data.frame()
gsw_adv <- read_html("https://widgets.sports-reference.com/wg.fcgi?css=1&site=bbr&url=%2Fplayoffs%2F2019-nba-finals-warriors-vs-raptors.html&div=div_GSWadvanced") %>% 
  html_nodes("table") %>% 
  html_table() %>% 
  as.data.frame()

colnames(gsw_adv) <- gsw_adv[1,]
colnames(tor_adv) <- tor_adv[1,]

gsw_adv <- gsw_adv %>% 
  .[-nrow(gsw_adv),] %>% 
  clean_names() %>% 
  as_tibble() %>% 
  mutate_at(vars(-player), list(as.numeric)) %>% 
  .[-1,] %>% 
  rename_at(vars(contains("_2")), funs(str_replace(., "_2", "_pergame"))) %>% 
  mutate(tm = "gsw")

tor_adv <- tor_adv %>% 
  .[-nrow(tor_adv),] %>% 
  clean_names() %>% 
  as_tibble() %>% 
  mutate_at(vars(-player), list(as.numeric)) %>% 
  .[-1,] %>% 
  rename_at(vars(contains("_2")), funs(str_replace(., "_2", "_pergame"))) %>% 
  mutate(tm = "tor")

finals_2019_adv <- bind_rows(gsw_adv, tor_adv)


finals_data <- inner_join(finals_2019_basic, finals_2019_adv)
#ANAL---------
  