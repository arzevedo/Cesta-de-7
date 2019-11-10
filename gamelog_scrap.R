library(rvest)
library(tidyverse)
library(lubridate)
library(gghighlight)

nba_teams_names <- c("ATL","BOS","BRK","CHO","CHI","CLE","DAL","DEN","DET","GSW",
               "HOU","IND","LAC","LAL","MEM","MIA","MIL","MIN","NOP","NYK",
               "OKC","ORL","PHI","PHO","POR","SAC","SAS","TOR","UTA","WAS")

tables <- list()
tables_ano <- list()


for(k in 2015:2019){
for(i in nba_teams_names){
  
  game_log <- read_html(
    paste0("https://www.basketball-reference.com/teams/", i,"/", k,"/gamelog/#tgl_basic::none")
    ) %>% 
    html_nodes("table") %>% 
    html_table() %>% 
    as.data.frame() %>% 
    select(-Var.25, -Var.1) %>% 
    filter(!(Team %in% c("Team", "FG"))) %>% 
    mutate(Var.4 = factor(case_when(
      .$Var.4 == ""  ~ "C",
      .$Var.4 == "@" ~ "F"
    )),
    Var.3 = ymd(.$Var.3)) %>% 
    mutate_at(vars(Var.7:Opponent.15), list(as.numeric)) %>% 
    mutate(team_check = paste(i),
           season = paste(k))
  
  tables[[i]] <- game_log
  
}
  tables_n <- do.call("rbind", tables)
  
  tables_ano[[k]] <- tables_n
}

nba_teamss <- do.call("rbind", tables_ano) %>% 
  rename(game = Var.2, date = Var.3, 
         home_away = Var.4, opp = Var.5,
         result = Var.6, tm_score = Var.7,
         opp_score = Var.8, tm_fg = Team,
         tm_fga = Team.1, tm_fgp = Team.2,
         tm_3pt = Team.3, tm_3pta = Team.4,
         tm_3ptp = Team.5, tm_ft = Team.6,
         tm_fta = Team.7, tm_ftp = Team.8,
         tm_orb = Team.9, tm_trb = Team.10,
         tm_ast = Team.11, tm_stl = Team.12,
         tm_blk = Team.13, tm_tov = Team.14,
         tm_pf = Team.15, opp_fg = Opponent,
         opp_fga = Opponent.1,
         opp_fgp = Opponent.2,
         opp_3pt = Opponent.3, 
         opp_3pta = Opponent.4,
         opp_3ptp = Opponent.5,
         opp_ft = Opponent.6, 
         opp_fta = Opponent.7,
         opp_ftp = Opponent.8,
         opp_orb = Opponent.9,
         opp_trb = Opponent.10,
         opp_ast = Opponent.11,
         opp_stl = Opponent.12,
         opp_blk = Opponent.13,
         opp_tov = Opponent.14,
         opp_pf = Opponent.15)
