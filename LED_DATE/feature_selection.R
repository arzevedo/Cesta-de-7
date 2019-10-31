####FEATURE SELECTION
####If functional variables
library(tidyverse)
library(rpart)
library(randomForest)

all_rap <- list.files(path = "nba-raptor", pattern = "*.csv",full.names = TRUE)
df_players <- read_csv("nba_players_data.csv")
modr_rap_tm <- read_csv(all_rap[4])

df_all_variables <- modr_rap_tm %>% 
  inner_join(df_players %>% 
               mutate(Player = stringi::stri_trans_general(Player,"Latin-ASCII")),
             by = c("player_name" = "Player",
                    "season" = "season",
                    "team" = "Tm",
                    "season_type", "season_type")) %>% 
  replace(., is.na(.), 0)

rf_input <- df_all_variables %>% 
  mutate_if(is.character, as.factor) %>% 
  select(- player_id) %>% 
  group_by(season) %>% 
  filter(poss >= quantile(poss, .25)) %>%  # Filtering players greather then the 1st quartile of possetions
  ungroup() %>% 
  select(- Rk, - MP, - Wt, - Ht)

set.seed(2019)

rf_train <- rf_input %>% 
  sample_frac(size = .7)

rf_test <- rf_input %>% 
  filter(!(player_name %in% rf_train$player_name))


p <- ncol(rf_train %>% 
            select(-player_name,
                   -Pos, - pos_carer))

runs <- 30 
nTREE <- 10000
mTRY <- sqrt(p)

IMPS_ACC <- matrix(0,nrow = p, ncol = runs)
IMPS_GINI <- IMPS_ACC

for (k in 1:runs){
  set.seed(k + 2019)
  model.rep <- randomForest(x = rf_train %>%
                              select(-player_name,
                                     -Pos, - pos_carer),
                            ntree = nTREE, mtry = mTRY,
                            ytest = rf_test$Pos,
                            y = rf_train$Pos,
                            importance = TRUE)
  Imp <- importance(model.rep)  
  IMPS_ACC[,k]=Imp[,"MeanDecreaseAccuracy"] 
  IMPS_GINI[,k]=Imp[,"MeanDecreaseGini"]
  cat("running",k,"... \n")
  
}

nomes.var <- names(Imp[,"MeanDecreaseAccuracy"])
Imp.geral <- apply(IMPS_ACC,1,mean)
names(Imp.geral) <- nomes.var
Var.Imp.geral <- apply(IMPS_ACC,1,sd)
names(Var.Imp.geral) <- nomes.var

imp.geral <- sort(Imp.geral,decreasing = T)
var.imp.geral <- Var.Imp.geral[names(imp.geral)] 

id <- 1:length(var.imp.geral)
fit <- rpart(var.imp.geral ~ id)
pred <- predict(fit,data=seq(1,p))
thr <- min(pred)

lv <- var.imp.geral>=thr
corte <- min(which(lv == FALSE))
corte.imp <- min(imp.geral[1:corte])


imp.geral <- sort(Imp.geral,decreasing = T)
vars <- names(imp.geral[1:corte])
write.csv(vars, "variables.csv")
n.var <- length(vars)
OOB <- matrix(0, nrow = n.var, ncol = runs)
n.var <- 6
vars <- pull(
  read_csv("variables.csv"),x
)
for (k in 1:runs) {
  for (i in 1:n.var){
    set.seed(k + 2019)
    dados.mod <- rf_train[c("Pos",vars[1:i])]
    model2 <- randomForest(data = dados.mod, 
                           ntree = nTREE, mtry = n.var/3,
                           x = dados.mod,
                           ytest = rf_test$Pos,
                           y = dados.mod$Pos,
                           importance = TRUE
                           )
    OOB[i,k] <- 1-sum(diag(model2$confusion))/sum(model2$confusion)
  }
  cat("running",k,"... \n")
}


OOBs <- apply(OOB,1,mean)

OOBS.min.position <- which(OOBs == min(OOBs))


trh2 <- mean(OOB[OOBS.min.position,])+sd(OOB[OOBS.min.position,])


ind.thr <- trh2 > OOBs
corte.final <- min(which(ind.thr == TRUE))



most_important_cat_vars <- vars[1:corte.final]

imp.geral <- sort(Imp.geral,decreasing = T)

write.csv(most_important_cat_vars, "most_imp.csv")
write.csv(imp.geral, "most_imp_geral.csv")