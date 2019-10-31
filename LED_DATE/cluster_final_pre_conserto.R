# Clustering 
library(caret)

imp.geral <- read_csv("most_imp_geral.csv")
imp_pesos <- pull(read_csv("most_imp_geral.csv"), x)
most_imp_vars <- pull(read_csv("most_imp_geral.csv"), X1)[1:45]

df_all_variables %>% 
  group_by(season) %>% 
  filter(poss >= quantile(poss, .25)) %>% 
  ungroup() %>% 
  filter(season_type == "RS") %>% 
  dplyr::select(most_imp_vars) %>%
  dplyr::select_if(is.numeric) -> df_precluster

set.seed(2019)
factor_teste <- factoextra::fviz_nbclust(x = df_precluster, k.max = 15, nboot = 99, method = "gap_stat",
                                         FUNcluster = kmeans, nstart = 25)+labs(title = "nstart = 25")
# 11 pra nstart=1 e 9 pra nstart=25
BRRR::skrrrahh(26)
# Clustering with K-means
set.seed(2019)

clustering_k_12 <- df_precluster %>% 
  scale() %>% kmeans(9, nstart = 25)

mds_preimp <- df_precluster %>% dist(method = "euclidean") %>% cmdscale(k = 2)

bind_cluster <- cbind(df_precluster, le_clust = as.factor(clustering_k_12$cluster), as_tibble(mds_preimp)) 


full_cluster_data <- bind_cluster %>% 
  bind_cols(df_all_variables %>% 
              group_by(season) %>% 
              filter(poss >= quantile(poss, .25)) %>% 
              ungroup() %>% 
              filter(season_type == "RS")) %>% 
  mutate(pos_2 = case_when(
    Pos %in% c("PG", "SG") ~ "Guards",
    Pos %in% c("SF", "PF") ~ "Wings",
    TRUE                   ~ "Centers"
  )) %>% 
  mutate(Pos = factor(Pos, levels = c("PG", "SG", "SF", "PF", "C")))
full_cluster_data %>% 
  ggplot(aes(V1, V2, colour = as.factor(Pos) # ou H_clusterize 
             )) +
  geom_point(alpha = .8)+
  facet_wrap(~ paste("K means",le_clust), scales = "free")+
  ggrepel::geom_text_repel(data = . %>% group_by(le_clust, season) %>% top_n(1, poss), inherit.aes = FALSE,
            aes(x = V1, y = V2, label = paste(player_name, season)))+
  scale_colour_brewer(type = "qual", palette = 6)+
  #ggforce::geom_mark_ellipse(aes(label = H_clusterize), n = 1)+ 
  theme_bw()+
  labs(colour = "Posições\nClássicas", title = "Comparando Cluster hierárquico com K-means")

# Kmeans 1 = high usage center (finisher)
# Kmeans 2 = offensive wing(?)
# Kmeans 3 = Pessoal mais defensivo (de perimetro) do que ofensivo 
# Kmeans 4 = Offensive scorers (redundante)
# Kmeans 5 = Paint defenders
# Kmeans 6 = defensive centers
# Kmeans 7 = Shooters
# Kmeans 8 = 
# Kmeans 9 = 3 & D

full_cluster_data <- full_cluster_data %>% 
  mutate(clust_disc = factor(
    case_when(
      le_clust == 1 ~ "Paint finisher",
      le_clust == 2 ~ "All around offense",
      le_clust == 3 ~ "Perimeter defense",
      le_clust == 4 ~ "All-Star scorers",
      le_clust == 5 ~ "2-Way paint players",
      le_clust == 6 ~ "Paint defenders",
      le_clust == 7 ~ "Rotation 3 point shooter",
      le_clust == 8 ~ "General rotation",
      TRUE          ~ "3 Point shooter specialist & defensive player"
    )
  ))

full_cluster_data %>% 
  ggplot(aes(V1, V2, colour = Pos # ou H_clusterize 
  )) +
  geom_point(alpha = .8)+
  facet_wrap(~ clust_disc, scales = "free")+
  ggrepel::geom_text_repel(data = . %>% group_by(le_clust, season) %>% top_n(1, poss), inherit.aes = FALSE,
                           aes(x = V1, y = V2, label = paste(player_name, season)),
                           size = 4)+
  scale_colour_brewer(type = "qual", palette = 6)+
  #ggforce::geom_mark_ellipse(aes(label = H_clusterize), n = 1)+ 
  theme_bw()+
  labs(colour = "Posições\nClássicas", 
       title = "MDS das variaveis selecionadas pela RF onde a  cor representa as posições clássicas\ne as facetas os agrupamentos funcionais",
       subtitle = "Os jogadores com mais posses jogadas estão indicados em suas respectivas temporadas.",
       x = "MDS1", y = "MDS2")+
  theme(strip.text.x = element_text(size = 11, face = "bold"))

ggsave("MDS_clust_pos.png", width = 13, height = 9, dpi = 400)

full_cluster_data %>% ggplot(aes(raptor_box_offense))+
  geom_histogram()+facet_wrap(~clust_disc, scales = "free_y")+theme_bw()+
  geom_vline(xintercept = 0, color = "firebrick3")+
  labs(title = "Distribuição do raptor box offense nos clusters",
       subtitle = "Saca os scorers 🔥🔥🔥🔥🔥🔥🔥")
full_cluster_data %>% ggplot(aes(raptor_box_defense))+
  geom_histogram()+facet_wrap(~clust_disc, scales = "free_y")+theme_bw()+
  geom_vline(xintercept = 0, color = "firebrick3")+
  labs(title = "Distribuição do raptor box defense nos clusters",
       subtitle = "Saca os scorers 🔥🔥🔥🔥🔥🔥🔥")

full_cluster_data %>% ggplot(aes(eFG))+
  geom_histogram()+
  facet_wrap(~clust_disc, scales = "free_y")+
  theme_bw()+
  geom_vline(xintercept = mean(full_cluster_data$eFG), color = "firebrick3")+
  labs(title = "Distribuição do eFG nos clusters",
       subtitle = "Confirmando o bad offense")


full_cluster_data %>% ggplot(aes(X3PAr))+
  geom_histogram()+
  facet_wrap(~clust_disc, scales = "free_y")+
  theme_bw()+
  geom_vline(xintercept = mean(full_cluster_data$X3PAr), color = "firebrick3")+
  labs(title = "Distribuição do %3pr nos clusters",
       subtitle = "Confirmando o bad offense")

full_cluster_data %>% group_by(clust_disc) %>%
  summarise(media_var_3pts = mean(X3PAr)/ var(X3PAr),
            media_ast = mean(AST.)/var(AST.),
            media_var_2pts = mean(X2PA_do_poss)/var(X2PA_do_poss))

full_cluster_data %>% group_by(clust_disc, season) %>%
  count() %>% 
  filter(season %in% c(2014,2019)) %>% group_by(season) %>% 
  mutate(freq = n/sum(n)) %>% 
  ggplot(aes(season, freq, colour = clust_disc))+
  geom_line(size = 3, linetype = 6)+ geom_point(size = 6)+
  ggrepel::geom_text_repel(
    data = . %>% filter(season == 2014),
    aes(season, freq, label = paste0(round(freq*100), "%")),
    nudge_x = -.4, show.legend=FALSE
    ) +
  ggrepel::geom_text_repel(
    data = . %>% filter(season == 2019),
    aes(season, freq, label = paste0(round(freq*100), "%")),
    nudge_x = +.4, show.legend=FALSE
    )+
  scale_colour_brewer(type = "qual", palette = 3)+
  scale_x_continuous(labels = c(2014,2019), breaks = c(2014, 2019),
                     limits = c(2013,2022))+
  scale_y_continuous(labels = scales::percent, limits = c(0, .23))+
  labs(title = "Evolução da classificação funcional através das temporadas",
       subtitle = "Porcentagem de jogadores classificados funcionalmente nas temporadas\n2013-2014 e 2018-2019",
       y = NULL, x = NULL, 
       colour = "Classificação funcional")+
  theme_minimal()+theme(legend.position=c(.9,.75))

ggsave(filename = "slope_chart_pos.png", width = 7, height = 10, dpi = 400)
