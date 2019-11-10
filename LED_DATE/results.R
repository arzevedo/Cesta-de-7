library(tidyverse)
library(randomForest)
all_rap <- list.files(path = "nba-raptor", pattern = "*.csv",full.names = TRUE)

modr_rap_tm <- read_csv(all_rap[4])

df_players <- read_csv("nba_players_data.csv")

df_all_variables <- modr_rap_tm %>% 
  inner_join(df_players %>% 
               mutate(Player = stringi::stri_trans_general(Player,"Latin-ASCII")),
             by = c("player_name" = "Player",
                    "season" = "season",
                    "team" = "Tm",
                    "season_type", "season_type")) %>% 
  replace(., is.na(.), 0)

# RF setup----

rf_input <- df_all_variables %>% 
  mutate_if(is.character, as.factor) %>% 
  dplyr::select(- player_id) %>% 
  filter(poss >= quantile(poss, .25)) # Filtering players greather then the 1st quartile of minutes

set.seed(2019)

rf_train <- rf_input %>% 
  sample_frac(size = .7)

rf_test <- rf_input %>% 
  filter(!(player_name %in% rf_train$player_name))

# Class Positions----
set.seed(2019)
rf_class_model <- randomForest(
  
  ntree = 10000,
  
  x = rf_train %>% 
    dplyr::select(-player_name,
           -Pos, - pos_carer),
  
  y = rf_train$Pos,
  
  ytest = rf_test$Pos,
  
  proximity = TRUE
  
)

rf_output <- cbind(rf_train, pred = rf_class_model$predicted)

rf_output %>% dplyr::select(player_name,Pos,pred) %>% filter(Pos != pred) %>% View()

rf_class_model$importance %>% as.data.frame() %>% 
  rownames_to_column() %>% write_csv("rf_class_model_importance_full_dataset.csv")
rf_class_model$importance %>% as.data.frame() %>% 
  rownames_to_column() %>% 
  mutate(rowname = fct_reorder(rowname, MeanDecreaseGini)) %>% 
  ggplot(aes(x = rowname, y = MeanDecreaseGini))+
  geom_col()+
  coord_flip()
ggsave(filename = "class_position.png", width = 500, height = 1200, units = "mm", limitsize = FALSE)

rf_class_model_adv <- randomForest(
  
  ntree = 10000,
  
  x = rf_train %>% 
    dplyr::select(-player_name,
           -Pos, - pos_carer) %>% 
    dplyr::select(season:VORP),
  
  y = rf_train$Pos,
  
  ytest = rf_test$Pos,
  
  proximity = TRUE
  
)

rf_class_model_adv$importance %>% as.data.frame() %>% 
  rownames_to_column() %>% write_csv("rf_class_model_importance_adv_dataset.csv")

rf_output <- cbind(rf_train, pred = rf_class_model_adv$predicted)

rf_output %>% dplyr::select(player_name,Pos,pred) %>% filter(Pos != pred) %>% View()

class_prox_geral <- rf_class_model_adv$proximity %>%
  dist(method = "euclidean") %>% vegan::wcmdscale(k = 2)
# KMEANS
pos_k_means <- class_prox_geral %>% kmeans(centers = 5)


plotly::ggplotly(rf_train %>% 
                   bind_cols(class_prox_geral %>% as_tibble()) %>% 
                   ggplot() +
                   geom_point(aes(x = V1, y = V2, colour = Pos, label = player_name, text = season)) +
                   geom_point(data = as.data.frame(pos_k_means$centers),
                              aes(V1, V2), size = 5)+
                   labs(title = "MDS distance full data")
)


class_prox_adv <- rf_class_model_adv$proximity %>%
  dist(method = "euclidean") %>% vegan::wcmdscale(k = 2)
# KMEANS
pos_k_means_adv <- class_prox_adv %>% kmeans(centers = 5)


plotly::ggplotly(rf_train %>% 
                   bind_cols(class_prox_adv %>% as_tibble()) %>% 
                   ggplot() +
                   geom_point(aes(x = V1, y = V2, colour = Pos, label = player_name, text = season)) +
                   geom_point(data = as.data.frame(pos_k_means_adv$centers),
                              aes(V1, V2), size = 5)+
                   labs(title = "MDS distance advanced data")
)

# An unsupervised approach ----
# General
set.seed(2019)
unsup_rf <- randomForest(
  rf_input %>% 
    dplyr::select(season:games_started) %>% 
    dplyr::select( - MP, - Rk),
  ntree = 10000,
  proximity = TRUE
)
#save(unsup_rf, file = "unsup_rf.RData")
load("unsup_rf.RData")
unsup_importance <- importance(unsup_rf) %>% as.data.frame() %>% 
  rownames_to_column("variable") %>%
  rename(importance = MeanDecreaseGini) %>% 
  arrange(desc(importance))
#write_csv(unsup_importance, "unsup_importance.csv")

unsup_rf_prox <- unsup_rf$proximity %>%
  dist(method = "euclidean") %>% vegan::wcmdscale(k = 2)


plotly::ggplotly(rf_input %>% bind_cols(unsup_rf_prox %>% as_tibble()) %>%
                   ggplot(aes(x = V1, y = V2, colour = raptor_total, 
                              label = player_name, text = raptor_total)) +
                   geom_point()+
                   #scale_colour_brewer(type = "qual", 12)+
                   scale_colour_gradient2(low = "#ef8a62", high = "#67a9cf")+
                   theme_dark())
# Creio que não vamos usar isso.
# Esta agrupando por variaveis que fazem sentido e outras que nao eram pra agrupar.
# Ex: os melhores(direita) jogadores estão separados dos piores(esquerda) por WS e raptor_total,  
# Mas tambem estao separados por numero de jogos que eles comecaramm

# Ploting both ----
unsup_rf$importance %>% as.data.frame() %>% 
  rownames_to_column() %>% 
  rename(unsup_gini = MeanDecreaseGini) %>% 
  inner_join(
    rf_class_model$importance %>% as.data.frame() %>% 
      rename(sup_gini = MeanDecreaseGini) %>% 
      rownames_to_column(),
    by = c("rowname")
  ) %>% 
  ggplot(aes(x = unsup_gini, y = sup_gini, label = rowname)) +
  ggrepel::geom_label_repel()

# With the 538 metrics ----
five38 <- rf_input %>%
  select(player_name:pace_impact)

unsup_rf_five38 <- randomForest(
  five38 %>% 
    select(-player_name),
  ntree = 10000,
  proximity = TRUE
)

five38_unsup_rf_prox <- unsup_rf_five38$proximity %>%
  dist(method = "euclidean") %>% vegan::wcmdscale(k = 2)


plotly::ggplotly(rf_input %>% bind_cols(five38_unsup_rf_prox %>% as_tibble()) %>%
                   ggplot(aes(x = V1, y = V2, colour = pos_team, label = player_name, text = season)) +
                   geom_point())
# Feature selection ----
# Here we dont't want to know the proximity 
p <- ncol(rf_train %>% 
            select(-player_name,
                   -pos_team, - pos_careear))

runs <- 30 
nTREE <- 10000
mTRY <- sqrt(p)

IMPS_ACC <- matrix(0,nrow = p, ncol = runs)
IMPS_GINI <- IMPS_ACC

for (k in 1:runs){
  set.seed(k + 2019)
  model.rep <- randomForest(x = rf_train %>%
                              select(-player_name,
                                     -pos_team, - pos_careear),
                            ntree = nTREE, mtry = mTRY,
                            ytest = rf_test$pos_team,
                            y = rf_train$pos_team,
                            importance = TRUE)
  Imp <- importance(model.rep)  
  IMPS.ACC[,k]=Imp[,"MeanDecreaseAccuracy"] 
  IMPS.GINI[,k]=Imp[,"MeanDecreaseGini"]
  cat("running",k,"... \n")
  
}

# Random forest class Pos for each season and season type ----
all_season_rf <- rf_input %>%
  select(-player_name) %>% 
  group_nest(season, season_type) %>% pull(data) %>% 
  lapply(randomForest, 
         ntree = 10000,
         
         data = .,
         
         
         y = .$Pos,
         
         proximity = TRUE
         
         )
set.seed(2019)
all_season_rf <- rf_input %>%
  select(-player_name) %>% 
  group_by(season, season_type) %>%
  do(model = randomForest(
    data = .[,-24],
    Pos ~ .,
    ntree = 10000,
    proximity = TRUE
    )
    ) 
all_season_rf %>% ungroup() %>% rowwise() %>%  
  mutate(
    importancia = list(
      importance(model) %>% as.data.frame() %>%
        rownames_to_column("variable")
    ),
    OOB = model[4] %>% as.data.frame() %>%
      summarise(OOB_mean = mean(err.rate.OOB)) %>% 
      pull(OOB_mean)
         ) %>% View

all_season_rf$model[[12]][4] %>% as.data.frame() %>% summarise(media = mean(err.rate.OOB))
