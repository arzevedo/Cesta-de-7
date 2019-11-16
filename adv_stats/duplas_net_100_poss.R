library(tidyverse);library(rvest);library(janitor)

nbaTeams <- c("ATL","BOS","BRK","CHO","CHI","CLE","DAL","DEN","DET","GSW",
             "HOU","IND","LAC","LAL","MEM","MIA","MIL","MIN","NOP","NYK",
             "OKC","ORL","PHI","PHO","POR","SAC","SAS","TOR","UTA","WAS")

duplas_nba <- list()

for(teams in nbaTeams){
  duplas <- read_html(
    paste0(
      "https://www.basketball-reference.com/teams/", teams, "/2020/lineups/#lineups_2-man_::none"
    )
  ) %>% html_nodes(xpath = '//comment()') %>% html_text() %>%   
    paste(collapse = '') %>% read_html() %>%   
    html_table() %>% .[4] %>% 
    as.data.frame()
  
  colnames(duplas) <- duplas[1,]
  duplas <- duplas[-1,]
  
  duplas <- duplas %>% janitor::clean_names()
  
  
  duplas <- duplas %>% 
    mutate(lineup = str_replace_all(lineup, pattern = " ", replacement = "") %>% 
             str_replace_all("\\|", "-")) %>%
    drop_na() %>% 
    mutate(mp = lubridate::ms(mp))%>%
    select(-rk) %>% 
    mutate_at(vars(mp:pf), as.numeric) %>% 
    mutate(team = teams)
  
  duplas_nba[[teams]] <- duplas
  
  cat(teams,"\n")
  
  Sys.sleep(15)
  
}

BRRR::skrrrahh()

df_duplas <- map_df(duplas_nba, bind_rows) %>% as_tibble() %>% 
  filter(lineup != "TeamAverage")

# Verificando distribuicao dos minutos
df_duplas %>% 
 ggplot(aes(mp)) +
  geom_histogram(bins = 20)
# Um exemplo das duplas mais eficientes por time
df_duplas %>% 
  group_by(team) %>% 
  top_n(n = 2, wt = e_fg_percent)
