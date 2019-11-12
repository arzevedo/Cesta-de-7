#Setup####
library(mgcv)
library(reshape2)
library(ggradar)
library(png)
library(scales)
library(cowplot)
library(magick)
library(ggrepel)
library(ggridges)
library(tidyverse)
library(norm)
library(plotly)
library(ggExtra)
library(chron)
library(httr)
library(gghighlight)
library(rvest)

#team_palete_1 <- c("ATL"="#DF393E", "BOS"="#223C2B", "BRK"="#FEFEFE", "CHI"="#CD1141", "CHO"="#008CA8",
#                   "CLE"="#6A0832", "DAL"="#006BB6", "DEN"="#0D213F", "DET"="#C8102E", "GSW"="#006AB6",
#                   "HOU"="#CD1141", "IND"="#002D62", "LAC"="#FEFEFE", "LAL"="#542582", "MEM"="#5D76A9",
#                   "MIA"="#98002E", "MIL"="#00471A", "MIN"="#001641", "NOP"="#032B5B", "NYK"="#F48427",
#                   "OKC"="#284E7B", "ORL"="#0076BF", "PHI"="#ED174B", "PHO"="#F8A01B", "POR"="#D7373D",
#                   "SAC"="#592C81", "SAS"="#C4CDD3", "TOR"="#000000", "UTA"="#00471A", "WAS"="#E51937")
#team_palete_2 <- c("ATL"="#FBF1F1", "BOS"="#FEFEFE", "BRK"="#000000", "CHI"="#000000", "CHO"="#FEFEFE",
#                   "CLE"="#FDB71A", "DAL"="#C5CED3", "DEN"="#FFC626", "DET"="#002D62", "GSW"="#FCB827",
#                   "HOU"="#FEFEFE", "IND"="#FCBA30", "LAC"="#006AB6", "LAL"="#BA8D25", "MEM"="#F5B112",
#                   "MIA"="#000000", "MIL"="#FEFEFE", "MIN"="#719BFF", "NOP"="#B4975A", "NYK"="#006BB6",
#                   "OKC"="#F05033", "ORL"="#C0C9CF", "PHI"="#006BB6", "PHO"="#E46020", "POR"="#FEFEFE",
#                   "SAC"="#C8CDCF", "SAS"="#000000", "TOR"="#CD1844", "UTA"="#FAA11B", "WAS"="#002A5C")

link <- read_html("https://www.basketball-reference.com/leagues/NBA_2019_advanced.html#advanced_stats::none") #Lendo o HTML direto da WEB
link_total <- read_html("https://widgets.sports-reference.com/wg.fcgi?css=1&site=bbr&url=%2Fleagues%2FNBA_2019_totals.html&div=div_totals_stats")
link_poss <- read_html("https://widgets.sports-reference.com/wg.fcgi?css=1&site=bbr&url=%2Fleagues%2FNBA_2019_per_poss.html&div=div_per_poss_stats")
link_starters <- read_html("https://widgets.sports-reference.com/wg.fcgi?css=1&site=bbr&url=%2Fleagues%2FNBA_2019_per_game.html&div=div_per_game_stats")

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
  rename(eFG = eFG._do_total, ORtg = ORtg_do_poss, DRtg = DRtg_do_poss)
rooks_adv <- jogadores %>% 
  filter(Player %in% rooks_18)
#####LOADING END

defesa <- players_adv %>%
  filter((MP) >= quantile(MP, 0.25)) %>%
  filter(Pos %in% c("PG", "SG", "SF", "PF", "C")) %>% 
  ggplot(aes(x = TRB., y = DBPM, 
             color = factor(Pos,levels = c("PG", "SG", "SF", "PF", "C"))))+
  geom_point()+
  labs(x = "Total Rebound Percentage",
       title = "How big is the big man today ?",
       subtitle = "On a defensive spectrum",
       y = "Defensive Box Plus/Minus",
       color = "Position",
       caption = "Dados: Basketball reference\nVisualização: @cestade7\n13/11/18")

ggdraw()+
  draw_image("cesta_7_basq.png", scale = 0.20, x=0.42,y=0.41)+
  draw_plot(defesa)

players_adv %>% 
  filter(MP >= quantile(MP, 0.25)) %>%
  select(DRB.,DBPM) %>% 
  kmeans(centers = 3) -> km

centroids <- data.frame(km$centers)
centroids <- data.frame(centroids, Cluster = factor(c("Armador",
                                                      "Ala",
                                                      "Pivo"),
                                                    levels = c("Armador",
                                                               "Ala",
                                                               "Pivo")))

defesa_kmedias <- players_adv %>%
  filter(MP >= quantile(MP, 0.25)) %>%
  ggplot(aes(x = DRB., y = DBPM, 
             color = factor(Pos,levels = c("PG", "SG", "SF", "PF", "C"))))+
  geom_point()+
  scale_point_shape_discrete(name = "Position",solid = FALSE)+
  geom_point(data = centroids, aes(x = DRB., y = DBPM,shape = Cluster), size = 8,
             inherit.aes = FALSE)+
  labs(x = "Defensive Rebound Percentage",
       subtitle = "Cluster de defensores na NBA por k-médias",
       title = "Vamos classificar a NBA em 3 posições para analisar a defesa",
       color = "Position",
       y = "Defensive Box Plus/Minus",
       caption = paste0("Dados: Basketball reference\nVisualização: @cestade7\n",
                        "Atualizado: ",format(Sys.Date(), "%d/%m")))+
  scale_color_manual(values = c("PG" = "#F75654", "SG" = "#49B5FF", "SF" = "#FFFE0D",
                                "PF" = "#B2B26B", "C" = "#B20300"))


ggdraw()+
  draw_image("cesta_7_basq.png", scale = 0.20, x=0.42,y=0.41)+
  draw_plot(defesa_kmedias)


players_adv %>% 
  filter(!(Tm == "TOT")) %>% 
  filter(MP >= quantile(MP, 0.25)) %>% 
  ggplot(aes(x = PER, y = USG., color = Pos))+
  geom_point()+
  facet_wrap(~Pos,nrow = 1)

usg_per_team <- players_adv %>% 
  filter(!(Tm == "TOT")) %>% 
  filter(Tm == "BOS") %>% 
  filter(MP >= quantile(MP, 0.25)) %>% 
  ggplot(aes(x = PER, y = USG., label = Player))+
  geom_label(size = 4.8, alpha =0.55)+
  labs(
    title = "Como os jogadores do Celtics estão sendo utilizados ?\n",
    x = "Player Efficiency Rating",
    y = "USG%",
    caption = paste0("Dados: Basketball reference\nVisualização: @cestade7\n",
                               "Atualizado: ",format(Sys.Date(), "%d/%m"))
  )+
  #facet_wrap(~Tm, nrow = 5)+
  coord_flip()+
  theme(plot.background = element_blank(),panel.background = element_blank(),panel.grid = element_blank())

ggdraw()+
  draw_image("logo_times/BOS.png", scale = 1)+
  draw_image("brad.jpg", scale = 0.5, x = 0.45)+
  draw_plot(usg_per_team)+
  draw_image("cesta_7_basq.png", scale = 0.14, x=-0.45,y=-0.422)

players_adv %>% 
  filter(MP >= quantile(MP, 0.25)) %>%
  filter(Tm != "TOT") %>% 
  ggplot(aes(x = USG., y = PER, label = Player))+
  geom_label_repel()+
  facet_wrap(~Tm)


rooks_adv %>% 
  filter(MP >= quantile(MP, 0.25)) %>% 
  ggplot(aes(x = MP/G, y = WS.48, label = Player,fill=Tm,color=Tm))+
  ggrepel::geom_label_repel()+
  scale_fill_manual(values = team_palete_1, guide = FALSE)+
  scale_color_manual(values = team_palete_2, guide = FALSE)

jogadores %>% 
  filter(MP >= quantile(MP, 0.25)) %>% 
  filter(Tm == "LAC") %>%  
  View()


jogadores %>% 
  filter(MP >= quantile(MP, 0.25),
         Tm != "TOT") %>% 
  ggplot(aes(ORtg,-DRtg,label = Player))+
  geom_label(color = "blue4")+
  labs(x = "Rtg Off", y = "-Rtg Def")+
  gghighlight(Pos == "DAL")+
  geom_hline(yintercept = -median(jogadores$DRtg), color = "red2")+
  geom_vline(xintercept = mean(jogadores$DRtg), color = "red3")

  

sabo <- jogadores %>% 
  filter(MP >= quantile(MP, 0.25),
         Tm == "IND") %>% 
  ggplot(aes(reorder(Player,MP/G),WS))+
  geom_col(fill = "#F1B331", alpha = 0.7)+
  labs(title = "Sabonis em quadra! Favorito demais pra 6º homem do ano!",
       subtitle = "Jogadores ordenados por quantidade de minutos por jogo\n",
       y = "Win Share",
       caption = paste0("Dados: Basketball reference\nVisualização: @cestade7\n  ",
                        "Atualizado: ",format(Sys.Date(), "%d/%m")))+
  coord_flip()+
  theme(axis.title.y = element_blank(),plot.subtitle = element_text(hjust = 0.5))

ggdraw()+
  draw_plot(sabo)+
  draw_image(image = "img_jog/sabo.png",x=0.17,scale = 0.7,y=-0.07)+
  draw_image("cesta_7_basq.png", scale = 0.20, x=0.45,y=0.39)

# ALGO COM 6º HOMEM
jogadores %>% 
  filter(MP >= quantile(MP, 0.25),
         Tm != "TOT",
         G > 10) %>% 
  group_by(Tm) %>% 
  filter(games_started/G <= 0.2) %>% 
  arrange(Tm,MP/G) %>% 
  top_n(1) %>% 
  ggplot(aes(WS.48,VORP,label=Player, color=Tm,fill=Tm))+
  ggrepel::geom_label_repel(fontface = "bold",,seed = 10)+
  labs(title = "Relacionando o value over replacement com o win share dos 6º Homens da NBA")+
  scale_fill_manual(values = team_palete_1,guide=FALSE)+
  scale_color_manual(values = team_palete_2,guide=FALSE)



plotly::ggplotly(jogadores %>% 
  filter(MP >= quantile(MP, 0.25),
         Tm != "TOT") %>% 
  ggplot(aes(FG._do_total,USG.,label = Player, fill = Tm, color = Tm))+
  geom_point()+
  scale_fill_manual(values = team_palete_1,guide=FALSE)+
  scale_color_manual(values = team_palete_2,guide=FALSE))

novat <- rooks_adv %>% 
  filter(MP >= quantile(MP, 0.25),
         MP/G > 5) %>% 
  ggplot(aes(PER,VORP,label = Player,fill=MP/G))+
  geom_smooth(method="gam",color = "gold3",fill="yellow",se = 0.1)+
  geom_label_repel(color = "white")+
  labs(title = "Comparando VORP e PER dos rookies",
       subtitle = paste0(format(Sys.Date(), "%d de %B"),"\n"),
       caption = "Dados: Basketball reference\nVisualização: @cestade7",
       x="Player Efficiency Rating", y="Value Over Replacement Player")+
  guides(fill = guide_colourbar(ticks = FALSE,barheight = 20,barwidth = 3,
                                label.hjust = 1,title.hjust = 0.2))+
  scale_fill_gradient(low = "black",high = "orange","Minutos por jogo",
                      breaks=c(5,10,15,20,25,30))+
  #scale_fill_manual(values = team_palete_1,guide=FALSE)+
  #scale_color_manual(values = team_palete_2,guide=FALSE)+
  theme(plot.subtitle = element_text(hjust=0.5))

ggdraw()+
  draw_plot(novat)+
  draw_image("cesta_7_basq.png", scale = 0.17, x=0.45,y=0.425)

#ANIMANDO

cesta_logo <- readPNG("cesta_7_basq.png")
cesta_logo <- grid::rasterGrob(cesta_logo,interpolate = TRUE)

nba_hand <- jogadores %>% 
  filter(Tm != "TOT",
         MP >= quantile(MP, 0.25)) %>% 
  ggplot(aes(USG.,TOV.,label = Player,group = Tm,fill = Tm,color = Tm))+
  annotation_custom(cesta_logo, xmin = 30, xmax = 45, ymin = 25,ymax = 30)+
  #annotation_custom(cesta_logo, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)+
  geom_label_repel(seed = 2018)+
  scale_fill_manual(values = team_palete_1,guide=FALSE)+
  scale_color_manual(values = team_palete_2,guide=FALSE)+
  labs(x="Porcentagem de Uso",
       caption = "Dados: Basketball reference\nVisualização: @cestade7",
       y="Porcentagem de Turnover")+
  transition_states(states = Tm, transition_length = 30,
                    state_length = 1)+
  ggtitle('Quais são os jogadores com as melhores mãos da NBA?\nTime: {closest_state}',
          subtitle = paste0(format(Sys.Date(), "%d de %B"),"\n"))+
  theme(plot.subtitle = element_text(hjust = 0.5,face = "bold"),
        plot.title = element_text(hjust = 0.5),axis.line = element_blank(),axis.ticks = element_blank(),
        panel.grid.major =  element_line(colour = "gray90",size = 1))


animate(nba_hand, fps = 1.5) %>% 
  anim_save(filename = "nba_hand.gif")

jogadores %>% 
  filter(Tm != "TOT",
         MP >= quantile(MP, 0.25)) %>% 
  ggplot(aes(USG.,eFG*100,label = Player,group = Tm,fill = Tm,color = Tm))+
  annotation_custom(cesta_logo, xmin = 30, xmax = 45, ymin = 25,ymax = 30)+
  #annotation_custom(cesta_logo, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)+
  geom_label_repel(seed = 2018)+
  scale_fill_manual(values = team_palete_1,guide=FALSE)+
  scale_color_manual(values = team_palete_2,guide=FALSE)+
  labs(x="Porcentagem de Uso",
       caption = "Dados: Basketball reference\nVisualização: @cestade7",
       y="eFG")+
  transition_states(states = Tm, transition_length = 30,
                    state_length = 1)+
  ggtitle('Quais são os jogadores com as melhores mãos da NBA?\nTime: {closest_state}',
          subtitle = paste0(format(Sys.Date(), "%d de %B"),"\n"))+
  theme(plot.subtitle = element_text(hjust = 0.5,face = "bold"),
        plot.title = element_text(hjust = 0.5),axis.line = element_blank(),axis.ticks = element_blank(),
        panel.grid.major =  element_line(colour = "gray90",size = 1))

####QUEM É P MVP DESSA PORRA ?

mvp_cesta <- jogadores %>% 
  select(-Pos,-Tm,-Rk,Player) %>%
  mutate_at(vars(-Player),funs(rescale)) %>% 
  filter(Player %in% c("Giannis Antetokounmpo","LeBron James","Joel Embiid",
                       "Stephen Curry","Kawhi Leonard","James Harden")) %>% 
  select(1,6,16:18,20,24) %>% 
  ggradar(group.point.size = 5,grid.label.size = 0.01,
          axis.labels = c("True Shooting %","% de Uso","Offensive \nWin Share",
                          "Defensive Win Share","Win Share\npor 48 min","VORP"),
          plot.title = "Quem são os candidatos a MVP do cesta de 7?",
          group.colours = c("green4","#CE1141","blue","red3","#542582","#FCB827"))+
  labs(subtitle = paste0("\nAdicionando a Barba a equação\n",format(Sys.Date(), "%d de %B"),"\n"),
       caption = "Dados: Basketball reference\nVisualização: @cestade7")+
  theme(plot.title = element_text(size = 25,face = "bold"),plot.background = element_blank(),
        panel.background = element_blank(),
        plot.subtitle = element_text(size = 12,hjust = 0.5,face = "bold"),
        plot.caption = element_text(size = 10))

ggdraw()+
  draw_image("cesta_7_basq.png", scale = 0.06, x=0.13,y=-0.031)+
  draw_plot(mvp_cesta)+
  draw_image("cesta_7_basq.png", scale = 0.17, x=-0.35,y=0.24)

top_roks <- rooks_adv %>% 
  select(-Pos,-Tm,-Rk,Player) %>%
  mutate_at(vars(-Player),funs(rescale)) %>% 
  filter(Player %in% c("Deandre Ayton","Jaren Jackson","Luka Doncic",
                       "Aaron Holiday","Trae Young")) %>% 
  select(1,6,16:18,20,24) %>% 
  ggradar(group.point.size = 5,grid.label.size = 0.01,
          axis.labels = c("True Shooting %","% de Uso","Offensive \nWin Share",
                          "Defensive Win Share","Win Share\npor 48 min","VORP"),
          plot.title = "Comparando os stats avançados de alguns rookies",
          group.colours = c("#032B5B","#F8A01B","#5D76A9","#23892F","red"))+
  labs(subtitle = paste0("One of these things is not like the other",
                         "\n",format(Sys.Date(), "%d de %B"),"\n"),
       caption = "Dados: Basketball reference\nVisualização: @cestade7")+
  theme(plot.title = element_text(size = 25,face = "bold"),plot.background = element_blank(),
        panel.background = element_blank(),
        plot.subtitle = element_text(size = 10,hjust = 0.5,face = "bold"),
        plot.caption = element_text(size = 10))

ggdraw()+
  draw_image("cesta_7_basq.png", scale = 0.06, x=0.097,y=-0.031)+
  draw_plot(top_roks)+
  draw_image("cesta_7_basq.png", scale = 0.17, x=-0.25,y=0.3)


jogadores %>% 
  select(-Pos,-Tm,-Rk,Player) %>%
  mutate_at(vars(-Player),funs(rescale)) %>% 
  filter(Player %in% c("Jrue Holiday","Anthony Davis","Julius Randle",
                       "E'Twaun Moore","Nikola Mirotic")) %>% 
  select(1,6,16:18,20,24) %>% 
  ggradar(group.point.size = 5,grid.label.size = 0.01,
          axis.labels = c("True Shooting %","% de Uso","Offensive \nWin Share",
                          "Defensive Win Share","Win Share\npor 48 min","VORP"),
          plot.title = "Comparando o gap que AD faz no seu time",
          group.colours = c("#032B5B","#F8A01B","#5D76A9","#23892F","red"))+
  labs(subtitle = paste0("One of these things is not like the other",
                         "\n",format(Sys.Date(), "%d de %B"),"\n"),
       caption = "Dados: Basketball reference\nVisualização: @cestade7")+
  theme(plot.title = element_text(size = 25,face = "bold"),plot.background = element_blank(),
        panel.background = element_blank(),
        plot.subtitle = element_text(size = 10,hjust = 0.5,face = "bold"),
        plot.caption = element_text(size = 10))







jogadores %>% 
  filter(Tm == "LAL",
         MP >= quantile(MP,0.25)) %>% 
  #filter(Player %in% c("Giannis Antetokounmpo","LeBron James","Joel Embiid",
  #                     "Stephen Curry","Kawhi Leonard","James Harden")) %>% 
  select(Player,PER,ORtg:PTS_do_total) %>% 
  melt() %>% 
  ggplot(aes(y = Player, x = variable, fill = value))+
  geom_raster()+
  #geom_tile(colour="white",
   #         width=.9, height=.9)+
  scale_fill_gradient(low = "#542582",high = "#BA8D25")+
theme(axis.text.x = element_text(angle = 45, hjust = 1))
