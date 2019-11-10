# Load packages ----
library(tidyverse)
library(rvest)
# Proces ----
all_players_seasons <- list()
aux_data <- list()
# REGULAR SEASON ----
for(i in 2009:2019){
  
  link <- read_html(paste0("https://www.basketball-reference.com/leagues/NBA_", i,"_advanced.html#advanced_stats::none")) #Lendo o HTML direto da WEB
  link_total <- read_html(paste0("https://widgets.sports-reference.com/wg.fcgi?css=1&site=bbr&url=%2Fleagues%2FNBA_", i,"_totals.html&div=div_totals_stats"))
  link_poss <- read_html(paste0("https://widgets.sports-reference.com/wg.fcgi?css=1&site=bbr&url=%2Fleagues%2FNBA_", i,"_per_poss.html&div=div_per_poss_stats"))
  link_starters <- read_html(paste0("https://widgets.sports-reference.com/wg.fcgi?css=1&site=bbr&url=%2Fleagues%2FNBA_", i,"_per_game.html&div=div_per_game_stats"))
  
  player_star <- as.data.frame(html_table(html_nodes(link_starters,"table"))) %>% 
    filter(!(Player == "Player")) %>% 
    select(Player,GS) %>% 
    mutate_at(vars(-Player), funs(as.numeric)) %>%
    tbl_df() %>% 
    rename(jogador_chek = Player, games_started = GS)
  player_total <- as.data.frame(html_table(html_nodes(link_total,"table")))
  player_poss <- as.data.frame(html_table(html_nodes(link_poss,"table")))
  players_adv <- as.data.frame( # Transformando em um data frame...
    html_table( # ...Uma tabela...
      html_nodes(x = link, css = "table") # ...Que possui o link "x" e o formato css "table"
    )
  )
  
  player_total <- player_total %>% 
    filter(!(Player == "Player")) %>% 
    data.table::setnames(
      old = player_total %>% names(),
      new = player_total %>% names() %>% paste0("_do_total")
    ) %>% 
    select(Player_do_total,FG_do_total:PTS_do_total) %>% 
    mutate_at(vars(-Player_do_total), funs(as.numeric)) %>% 
    tbl_df()
  
  player_poss <- player_poss %>% 
    filter(!(Player == "Player")) %>% 
    data.table::setnames(
      old = player_poss %>% names(),
      new = player_poss %>% names() %>% paste0("_do_poss")
    ) %>% 
    select(Player_do_poss,FG_do_poss:DRtg_do_poss) %>% 
    mutate_at(vars(-Player_do_poss), funs(as.numeric)) %>% 
    tbl_df()
  
  players_adv <- players_adv %>% # Pegando o dataframe
    filter(!str_detect(Rk, "Rk")) %>% # Retirando as linhas que possuem a repetição do nome das colunas
    select(-c(Var.25,Var.20)) %>% # Retirando as colunas "fantasmas", provinientes da formatação da página
    mutate_at(vars(-Player, -Pos, -Tm), funs(as.numeric)) %>% # Transformando as variaveis que não são fatores ou caracteres em valores numericos
    tbl_df() # Transformando tudo isso em tibble
  
  jogadores <- bind_cols(
    players_adv,player_star %>% 
      select(games_started),
    player_poss, player_total
  ) %>% 
    select(-Player_do_poss,-Player_do_total,-Var.30_do_poss) %>% 
    rename(eFG = eFG._do_total, ORtg = ORtg_do_poss, DRtg = DRtg_do_poss) %>% 
    mutate(season = i)
  
  cat(i, "\n")
  
  all_players_seasons[[i - 2009 + 1]] <- jogadores
  
}
# PLAYERS Ht and We ----
for(let in letters[-c(24,25)]){ # desconsiderando x e y (vazio)
  table_aux_info <- read_html(
    paste0("https://www.basketball-reference.com/players/", let, "/")
  ) %>% 
    html_nodes("table") %>% 
    html_table() %>% as.data.frame() %>% as_tibble()
  
  aux_data[[let]] <- table_aux_info
}
# POST SEASON ----
all_players_post_seasons <- list()
for(i in 2009:2019){
  
  link <- read_html(paste0("https://www.basketball-reference.com/playoffs/NBA_", i, "_advanced.html#advanced_stats::none")) #Lendo o HTML direto da WEB
  link_total <- read_html(paste0("https://widgets.sports-reference.com/wg.fcgi?css=1&site=bbr&url=%2Fplayoffs%2FNBA_", i,"_totals.html&div=div_totals_stats"))
  link_poss <- read_html(paste0("https://widgets.sports-reference.com/wg.fcgi?css=1&site=bbr&url=%2Fplayoffs%2FNBA_", i,"_per_poss.html&div=div_per_poss_stats"))
  link_starters <- read_html(paste0("https://widgets.sports-reference.com/wg.fcgi?css=1&site=bbr&url=%2Fplayoffs%2FNBA_", i,"_per_game.html&div=div_per_game_stats"))
  
  player_star <- as.data.frame(html_table(html_nodes(link_starters,"table"))) %>% 
    filter(!(Player == "Player")) %>% 
    select(Player,GS) %>% 
    mutate_at(vars(-Player), funs(as.numeric)) %>%
    tbl_df() %>% 
    rename(jogador_chek = Player, games_started = GS)
  player_total <- as.data.frame(html_table(html_nodes(link_total,"table")))
  player_poss <- as.data.frame(html_table(html_nodes(link_poss,"table")))
  players_adv <- as.data.frame( # Transformando em um data frame...
    html_table( # ...Uma tabela...
      html_nodes(x = link, css = "table") # ...Que possui o link "x" e o formato css "table"
    )
  )
  
  player_total <- player_total %>% 
    filter(!(Player == "Player")) %>% 
    data.table::setnames(
      old = player_total %>% names(),
      new = player_total %>% names() %>% paste0("_do_total")
    ) %>% 
    select(Player_do_total,FG_do_total:PTS_do_total) %>% 
    mutate_at(vars(-Player_do_total), funs(as.numeric)) %>% 
    tbl_df()
  
  player_poss <- player_poss %>% 
    filter(!(Player == "Player")) %>% 
    data.table::setnames(
      old = player_poss %>% names(),
      new = player_poss %>% names() %>% paste0("_do_poss")
    ) %>% 
    select(Player_do_poss,FG_do_poss:DRtg_do_poss) %>% 
    mutate_at(vars(-Player_do_poss), funs(as.numeric)) %>% 
    tbl_df()
  
  players_adv <- players_adv %>% # Pegando o dataframe
    filter(!str_detect(Rk, "Rk")) %>% # Retirando as linhas que possuem a repetição do nome das colunas
    select(-c(Var.25,Var.20)) %>% # Retirando as colunas "fantasmas", provinientes da formatação da página
    mutate_at(vars(-Player, -Pos, -Tm), funs(as.numeric)) %>% # Transformando as variaveis que não são fatores ou caracteres em valores numericos
    tbl_df() # Transformando tudo isso em tibble
  
  jogadores <- bind_cols(
    players_adv,player_star %>% 
      select(games_started),
    player_poss, player_total
  ) %>% 
    select(-Player_do_poss,-Player_do_total,-Var.30_do_poss) %>% 
    rename(eFG = eFG._do_total, ORtg = ORtg_do_poss, DRtg = DRtg_do_poss) %>% 
    mutate(season = i)
  
  cat(i, "\n")
  
  all_players_post_seasons[[i - 2009 + 1]] <- jogadores
  
}

post_season <- map_df(all_players_post_seasons, bind_rows) %>% 
  mutate(season_type = "PO")

# Data bind ----
df_players <- map_df(all_players_seasons, bind_rows) %>% 
  mutate(season_type = "RS") %>% 
  mutate(Player = str_replace(Player, "\\*", "")) %>% 
  bind_rows(
    map_df(all_players_post_seasons, bind_rows) %>% 
      mutate(season_type = "PO") %>% 
      mutate(Player = str_replace(Player, "\\*", ""))
  ) %>% 
  left_join(
    map_df(aux_data, bind_rows) %>% 
      rename(pos_carer = Pos) %>% 
      select(-From, -To, -Colleges) %>% 
      mutate(Player = str_replace(Player, "\\*", "")),
    by = c("Player")
  ) %>% 
  separate(col = Ht, into = c("fet", "inc"), sep = "-") %>% 
  mutate(Ht = (2.54 * 12 * as.numeric(fet) +
              (2.54 * as.numeric(inc)))) %>% 
  select(-inc, -fet, -Birth.Date) 



write_csv(df_players, "nba_players_data.csv")
df_players <- read_csv("nba_players_data.csv")

# get HW (deprecated) ----
get_player_HW <- function(first_letter_surname, full_surname_2letters_name){
  link <- paste0("https://www.basketball-reference.com/players/",
                 first_letter_surname,"/",
                 full_surname_2letters_name,"01.html") %>% list()
  info <- map(read_html) %>% 
    map(html_nodes(xpath = '//*[@id="meta"]/div[2]')) %>%
    html_text() %>% 
    str_extract(pattern = ".*lb") %>% str_trim()
  return(info)
}
#df_players %>% 
#  group_by(Player) %>% 
#  mutate(
#    first_letter_surname = Player %>% 
#      str_split(" ") %>% unlist() %>% str_extract(".") %>%
#      .[2] %>% str_to_lower(),
#    size_first_name = Player %>% 
#      str_to_lower() %>% str_split(" ") %>% unlist() %>% 
#      .[1] %>% str_length(),
#    size_surname = Player %>% 
#      str_to_lower() %>% str_split(" ") %>% unlist() %>% 
#      .[2] %>% str_length()
#  ) %>% 
#  mutate(
#    get_by_name = Player %>% str_replace_all(pattern = "\\.", replacement = "") %>% 
#      str_to_lower() %>% str_split(" ") %>% unlist() %>% str_sub(start = 1,end = 5) %>% 
#      paste0(str_sub(.[1], start = 1, end = 2)) %>% .[2] %>% 
#      stringi::stri_trans_general("Latin-ASCII") 
#  ) %>% ungroup() %>% 
#  select(Player,first_letter_surname:get_by_name)-> players_names
#
#players_names %>% 
#  mutate(vamola = get_player_HW(first_letter_surname = first_letter_surname,
#                                full_surname_2letters_name = get_by_name))
#
#players_HW <- players_names %>% 
#  distinct() %>% 
#  filter(Player != "Jeff Ayres") %>% 
#  mutate(
#    link_to_hw = paste0(
#      "https://www.basketball-reference.com/players/",
#      first_letter_surname,"/",
#      get_by_name,"01.html"
#    ) 
#  ) %>% pull(link_to_hw) %>% 
#  map(read_html) %>% map(html_node, xpath = '//*[@id="meta"]/div[2]') %>% 
#  map_chr(html_text) %>% str_extract(pattern = ".*lb") %>% str_trim() 
