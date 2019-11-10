# PACOTES ----

library(rvest);library(tidyverse)

# Identificacao dos jogadores 2020 ----

pl_id <- read_html("https://www.basketball-reference.com/leagues/NBA_2020_per_game.html") %>% 
  html_nodes("table") %>% html_table() %>% as.data.frame() %>% 
  as_tibble() %>% 
  filter(Rk != "Rk") %>% 
  group_by(Player) %>% 
  mutate(
    player_id =
      Player %>% str_replace_all(pattern = "\\.", replacement = "") %>% 
              str_to_lower() %>% str_split(" ") %>% unlist() %>% str_sub(start = 1,end = 5) %>% 
              paste0(str_sub(.[1], start = 1, end = 2)) %>% .[2] %>% 
              stringi::stri_trans_general("Latin-ASCII") 
    
  ) %>% ungroup() %>% select(Player, player_id, Pos, Age, Tm, G)

shot_get_20 <- function(player_id){

  pl_shot_data_raw <- read_html(
    paste0("https://www.basketball-reference.com/players/",
           player_id %>% str_sub(start = 1, end = 1),
           "/",
           player_id,
           "01/shooting/2020")
    ) %>%
    html_nodes("div")
  
  shot_data <- pl_shot_data_raw %>% str_split(pattern = " ") %>% unlist() %>% 
    .[
      pl_shot_data_raw %>% str_split(pattern = " ") %>% unlist() %>% 
        str_detect("style=")
      ] %>% str_replace('style=\"', "") %>% str_replace(';\"', "") %>%
    str_replace("top:", "") %>% str_replace("left:", "") %>% str_replace_all("px","") %>% 
    tibble(pos = .) %>% 
    separate(col = pos, into = c("y", "x"),
             sep = ";") %>% 
    mutate_all(as.numeric) %>% drop_na() %>% 
    mutate(result = 
             pl_shot_data_raw %>% str_split(pattern = " ") %>% unlist() %>% 
             .[
               pl_shot_data_raw %>% str_split(pattern = " ") %>% unlist() %>%
                 str_detect(pattern = "<br>Missed") |
                 pl_shot_data_raw %>% str_split(pattern = " ") %>% unlist() %>%
                 str_detect(pattern = "<br>Made")
               ] %>% str_replace("remaining<br>", "")
           
    )  
  
  return(shot_data)
  
}
  
shot_get_20(player_id = pl_id$player_id[10]) %>% 
  write_csv("shot_selection/um_exemplo_grego.csv")
