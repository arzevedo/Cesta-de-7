library(tidyverse);library(rvest)

pl_data <- read_html("https://www.basketball-reference.com/leagues/NBA_2020_advanced.html") %>% 
  html_nodes("table") %>% 
  html_table() %>% as.data.frame() %>% as_tibble() %>% 
  janitor::clean_names() %>% 
  select(-rk) %>% 
  mutate_at(vars(-player, -pos, -tm), as.numeric) %>% 
  filter(player != "Player")

pl_data %>% 
  filter(mp > quantile(mp,.25)) %>% 
  group_by(tm) %>% 
  top_n(n = 2, wt = per) %>% 
  summarise(
    dupla = paste0(player, collapse = "\n"),
    per_comb = sum(per)
  ) %>% 
  arrange(desc(per_comb)) %>% 
  mutate(dupla = fct_reorder(dupla, -per_comb)) %>% 
  top_n(10, per_comb) %>% 
  ggplot(aes(x = dupla, y = per_comb, label = dupla)) +
  geom_col() +
  geom_label() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
