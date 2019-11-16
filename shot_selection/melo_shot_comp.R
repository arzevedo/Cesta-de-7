library(rvest);library(tidyverse)

melo <- shot_get(player_id = 'anthoca', ano = 2018) %>% 
  group_by(
    meu_grupo = x + y, x, y
  ) %>% 
  count(result, sort = TRUE) %>% 
  group_by(result) %>% 
  mutate(freq = n / sum(n)) %>% 
  ungroup() %>% 
  filter(result == "Made") %>% 
  select(meu_grupo, x, y, freq)

atual <- shot_get(player_id = "hoodro", ano = 2020) %>% 
  bind_rows(
    shot_get(player_id = "bazemke", ano = 2020),
    shot_get(player_id = "tollian", ano = 2020)
  ) %>% 
  group_by(
    meu_grupo = x + y, x, y
  ) %>% 
  count(result, sort = TRUE) %>% 
  group_by(result) %>% 
  mutate(freq = n / sum(n)) %>% 
  ungroup() %>% 
  filter(result == "Made") %>% 
  select(meu_grupo, freq) %>% rename(freq_atual = freq)

melo %>%  
  ggplot(aes(x,y, colour = freq)) +
  geom_point()
