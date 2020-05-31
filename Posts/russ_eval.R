master_list <- list()

for(i in c("2019-20", "2018-19", "2017-18", "2016-17", "2015-16", "2014-15", "2013-14")){
  
  link_shoot <- paste0("https://stats.nba.com/stats/playerdashptshots?DateFrom=&DateTo=&GameSegment=&LastNGames=0&LeagueID=00&Location=&Month=0&OpponentTeamID=0&Outcome=&PerMode=PerGame&Period=0&PlayerID=201566&Season=", 
                       i,
                       "&SeasonSegment=&SeasonType=Regular+Season&TeamID=0&VsConference=&VsDivision=")
  
  
  shoot <- httr::GET(url = link_shoot,
                     
                     httr::add_headers(.headers = c('Accept'= 'application/json, text/plain, */*','Accept-Encoding'='gzip, deflate, br',
                                                    'Accept-Language'='en-US,en;q=0.9','Connection'='keep-alive','Host'= 'stats.nba.com',
                                                    'Referer'='https://stats.nba.com/','Sec-Fetch-Mode'='cors','Sec-Fetch-Site'= 'same-origin',
                                                    'User-Agent'= 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) 
          Chrome/79.0.3945.130 Safari/537.36','x-nba-stats-origin'= 'stats','x-nba-stats-token'= 
                                                      'true'))
                     
  )
  
  shot_json <- fromJSON(shoot %>% read_html() %>% html_text())
  
  master_list[[i]] <- map(shot_json$resultSets$rowSet, tidyr::as_tibble)
  cat(i, "\n")
  Sys.sleep(10)
  
}

russ_thund <- map_dfr(master_list[2:7], bind_rows) %>% 
  select_all(~shot_json$resultSets$headers[1] %>% unlist()) %>% 
  mutate(year = c(
    rep("18-19", 27), rep("17-18", 27), rep("16-17", 27), rep("15-16", 27),
    rep("14-15", 27), rep("13-14", 27)
  )
  ) %>% janitor::clean_names() %>% 
  filter(shot_type != "Overall") %>% select(-(1:3)) %>% 
  mutate_at(vars(-shot_type, - year),as.numeric)

russ_mvp <- russ_thund %>% filter(year == "16-17")

russ_hou <- map_dfr(master_list[1], bind_rows) %>% 
  select_all(~shot_json$resultSets$headers[1] %>% unlist()) %>% 
  mutate(year = "19-20") %>% 
  janitor::clean_names() %>% 
  filter(shot_type != "Overall") %>% select(-(1:3)) %>% 
  mutate_at(vars(-shot_type, -year),as.numeric)
