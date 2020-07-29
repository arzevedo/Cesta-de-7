library(tidyverse)
library(rvest)
library()
update_war <- read_html("https://projects.fivethirtyeight.com/2020-nba-player-ratings/") %>% 
  html_nodes("table") %>% html_table(fill = TRUE) %>% 
  as.data.frame() %>% as_tibble()
colnames(update_war) <- update_war[1,]

update_war <- update_war[-1, ] %>% janitor::clean_names() %>% 
  mutate(season = 2020, war = as.numeric(war)) %>% 
  select(player, season, war) 

hist_war <- read_csv(
  "https://raw.githubusercontent.com/fivethirtyeight/data/master/nba-raptor/modern_RAPTOR_by_player.csv"
  ) %>% 
  select(player = player_name, season, war = war_total)

full_war <- bind_rows(hist_war, update_war)

years <- c("", "2013-2014/", "2014-2015/", "2015-2016/", "2016-2017/",
           "2017-2018/", "2018-2019/")
sal_list <- list()
  
for(ano in years){
  sal <- read_html(paste0("https://hoopshype.com/salaries/players/",
                          ano)) %>% 
    html_nodes("table") %>% html_table() %>% as.data.frame()
  
  colnames(sal) <- sal[1,]
  
  df <- sal[-1, ] %>% janitor::clean_names() %>% as_tibble() %>% 
    select(2, salaries = 3) %>% 
    mutate(season = ano)
  
  sal_list[[ano]] <- df
  Sys.sleep(5)
}

salaries <- map_df(sal_list, bind_rows) %>% 
  mutate(
    season = case_when(
      season == "" ~ "2020",
      TRUE ~ season
    )
  ) %>% 
  mutate(
    season = str_replace(season, ".*-", "") %>% 
      str_replace("/", ""),
    salaries = str_replace_all(salaries, "[[:punct:]]", "") %>% 
      str_remove("\\$") %>% as.numeric()
  )

df_war_sal <- full_war %>%
  mutate(season = as.character(season)) %>% 
  inner_join(salaries)

aux_plot <- df_war_sal %>% 
  #group_by(season) %>% 
  #mutate(season_median = median(salaries)) %>% 
  mutate(
    classss = case_when(
      salaries >= 17e6 & war <= 1.5 ~ "STINKERS",
      war >= 7 & salaries <= 10e6 ~ "YOUNG STARS",
      war >= 2.6 & salaries <= 2e6 ~ "GEMS",
      war >= 8 & salaries > 25e6 ~ "SUPER STARS",
      TRUE                       ~ "OTHERS"
      
    )
  ) %>% 
  ggplot(aes(x = salaries, y = war, group = player, color = classss)) +
  geom_point() +
  #facet_wrap(~season) +
  scale_x_continuous(labels = scales::dollar) +
  facet_wrap(~season)

library(gganimate)

plot_war <- df_war_sal %>% 
  mutate(season = as.numeric(season)) %>% 
  ggplot(aes(x = salaries, y = war, group = player)) +
  geom_point() +
  #facet_wrap(~season) +
  scale_x_continuous(labels = scales::dollar) +
  labs(title = 'Season: {frame_time}', x = 'Salary', y = 'WAR') +
  transition_time(season) +
  ease_aes('linear')

gganimate::animate(plot_war)
