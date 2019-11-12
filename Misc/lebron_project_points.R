library(tidyverse);library(rvest);library(randomForest)
lebron <- list()
for(i in 2004:2020){
  
  link <- read_html(paste0("https://www.basketball-reference.com/players/j/jamesle01/gamelog/", i))
  
  data_lebron <- link %>% html_nodes(xpath = '//*[@id="pgl_basic"]') %>% 
    html_table() %>% as.data.frame() %>% as_tibble() %>% 
    filter(Age != "Age") %>% 
    mutate_all(as.character)
  
  lebron[[i - 2004 + 1]] <- data_lebron
  
  Sys.sleep(5) 
}

lebron_all_games <- map_df(lebron, bind_rows) %>% 
  dplyr::select(- Rk, - GS, - ncol(.), - Age) %>% 
  mutate(Date = lubridate::as_date(Date),
         id = 1:nrow(.),
         MP = lubridate::ms(MP) %>% lubridate::minute(),
         Var.8 = gsub("[^a-zA-Z]", "", Var.8)) %>% 
  mutate_at(vars(Tm, Var.6, Opp, Var.8), as.factor) %>% 
  mutate_at(vars(G, FG, FGA, FG., X3P, X3PA,  X3P., FT, FTA, 
                 FT., ORB, DRB, TRB, AST, STL, BLK, TOV, PF, 
                 PTS, GmSc), as.numeric) %>% 
  replace(., is.na(.), 0) %>% 
  mutate(Var.6 = ifelse(Var.6 == "@", "a", "h") %>% as.factor()
         )

library(fable)
library(GGally)

lebron_all_games %>%
  GGally::ggpairs(columns = 8:25)

teste <- lebron_all_games %>%
  select(id, PTS, FGA, FG.,MP) %>% 
  as_tsibble(index = id) %>% 
  model(
    snaive = ARIMA(PTS)
  ) %>%
  forecast(h = 49) 

lebron_all_games %>% 
  select(id, PTS) %>% 
  pull(PTS) %>% sum() + sum(teste$PTS)
## 33643

lebron_all_games %>% 
  select(id, PTS) %>% 
  mutate(pts_car = cumsum(PTS)) %>% 
  ggplot() +
  geom_line(aes(id, PTS)) #+
  #scale_y_log10()+
  #scale_x_log10()
