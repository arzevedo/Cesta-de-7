#Setup####
library(mgcv)
library(png)
library(scales)
library(cowplot)
library(ggridges)
library(tidyverse)
library(norm)
library(plotly)
library(ggExtra)
library(chron)
library(httr)
library(gghighlight)
library(rvest)
library(gganimate)
library(stattleshipR)
library(ggimage)
library(ggrepel)
library(rsvg)

nbaTeams=c("ATL","BOS","BKN","CHA","CHI","CLE","DAL","DEN","DET","GSW",
           "HOU","IND","LAC","LAL","MEM","MIA","MIL","MIN","NOP","NYK",
           "OKC","ORL","PHI","PHX","POR","SAC","SAS","TOR","UTA","WAS")
#teamImg=sprintf("https://i.cdn.turner.com/nba/nba/.element/img/4.0/global/logos/512x512/bg.white/svg/%s.svg",nbaTeams)
#teamImg=c(teamImg,"https://images-na.ssl-images-amazon.com/images/I/51ciAXMo1TL._SY886_.jpg")

#####LOADING BEGIN
link_1 <- read_html("https://widgets.sports-reference.com/wg.fcgi?css=1&site=bbr&url=%2Fleagues%2FNBA_2020.html&div=div_team-stats-per_game")
link_2 <- read_html("https://widgets.sports-reference.com/wg.fcgi?css=1&site=bbr&url=%2Fleagues%2FNBA_2020.html&div=div_opponent-stats-per_game")
link_3 <- read_html("https://widgets.sports-reference.com/wg.fcgi?css=1&site=bbr&url=%2Fleagues%2FNBA_2020.html&div=div_misc_stats")
link_4 <- read_html("https://widgets.sports-reference.com/wg.fcgi?css=1&site=bbr&url=%2Fleagues%2FNBA_2020.html&div=div_team_shooting")

nba_tea_sho <- as.data.frame(html_table(html_nodes(link_4,"table"))) %>% 
  select(Var.2,Var.6:X3.Pt.Field.Goals.2) %>% 
  slice(3:32) %>% 
  rename(
    team_chek = Var.2, distance_avg = Var.6, pct_fgA_d_2pt = Var.7,
    pct_fgA_d_0to3 = Var.8, pct_fgA_d_3to10 = Var.9,
    pct_fgA_d_10to16 = Var.10, pct_fgA_d_16to3pt = Var.11,
    pct_fgA_d_3pt = Var.12, fgP_d_2pt = Var.13, fgP_d_0to3 = Var.14,
    fgP_d_3to10 = Var.15, fgP_d_10to16 = Var.16,fgP_d_16to3pt = Var.17,fgP_d_3pt=Var.18,
    pct_2pt_ast_d = X2.Pt.Field.Goals, pct_fgA_dunk = X2.Pt.Field.Goals.1,
    dunk = X2.Pt.Field.Goals.2, pct_fgA_layups = X2.Pt.Field.Goals.3,
    layups = X2.Pt.Field.Goals.4, pct_3pt_ast_d = X3.Pt.Field.Goals,
    pct_corner_3_atemp = X3.Pt.Field.Goals.1, 
    corner_3_pct = X3.Pt.Field.Goals.2
  ) %>% 
  mutate_at(vars(-team_chek), funs(as.numeric)) %>% 
  tbl_df()

nba_tea_pre <- as.data.frame(html_table(html_nodes(link_1,"table"))) %>% 
  dplyr::arrange(Team)
nba_opo_pre <- as.data.frame(html_table(html_nodes(link_2,"table"))) %>% 
  rename_all(tolower) %>% 
  dplyr::arrange(team)
mais_adv <- as.data.frame(html_table(html_nodes(link_3,"table"))) %>%
  slice(2:31) %>% 
  select(-Var.13, -Var.26, -Var.27, -Var.28) %>% 
  dplyr::arrange(Var.2) %>% 
  rename(Rank = Var.1, Age = Var.3, W = Var.4, L = Var.5, PW = Var.6, PL = Var.7, MOV = Var.8, SOS = Var.9, SRS = Var.10, 
         ORtg = Var.11, DRtg = Var.12, Pace = Var.14, FTr = Var.15, THREE_PAr = Var.16, TS_pct = Var.17, off_eFG_prc = Offense.Four.Factors,
         off_TOV_prc = Offense.Four.Factors.1, off_ORB_pct = Offense.Four.Factors.2, off_FT_FGA = Offense.Four.Factors.3,
         def_eFG_prc = Defense.Four.Factors, def_TOV_prc = Defense.Four.Factors.1, def_ORB_pct = Defense.Four.Factors.2,
         def_FT_pct = Defense.Four.Factors.3) %>% 
  mutate_at(vars(-Var.2), funs(as.numeric)) %>% 
  tbl_df()


times_nba <- bind_cols(nba_tea_pre,
                       nba_opo_pre) %>% 
  filter(!(Team == "League Average")) %>% 
  mutate(logo = paste0("logo_times/",nbaTeams,".png")) %>% 
  bind_cols(
    mais_adv 
  ) %>% 
  select(-Rk, -rk, -Var.2, -team, -g, -mp) %>% 
  bind_cols(nba_tea_sho)


#####LOADING END
times_nba %>% 
  ggplot(aes(x = X3P., y = x3p., label = Team))+
  geom_label() 


tres_pts <- times_nba %>%
  ggplot(aes(x = X3P., y = x3p.)) +
  geom_vline(xintercept = mean(times_nba$X3P.), linetype = "dashed") +
  geom_hline(yintercept = mean(times_nba$x3p.), linetype = "dashed") +
  geom_image(data = times_nba, aes(image = times_nba$logo), size = 0.12,
             by = "width")+
  coord_fixed()+
  theme_void()+
  labs(x="Porcentagem média da bola de 3 do time",
       caption = paste0("Dados: Basketball reference\nVisualização: @cestade7\n",
                        "Atualizado: ",format(Sys.Date(), "%d/%m")),
       title = "Como os times estão chutando e defendendo a bola de três pontos na\ntemporada 18-19 ?",
       y="Porcentagem média da bola de 3 do oponente")+
  scale_x_continuous(breaks = c(min(times_nba$X3P.),mean(times_nba$X3P.),
                                max(times_nba$X3P.)),
                     #limits = c(0.25, 0.48),
                     labels = scales::percent_format(accuracy = 1))+
  scale_y_continuous(breaks = c(min(times_nba$x3p.),mean(times_nba$x3p.),
                                max(times_nba$x3p.)),
                     #limits = c(0.25, 0.45),
                     labels = scales::percent_format(accuracy = 1))+
  #annotate(geom = "text", x = 0.38, y = 0.40, label = "Times que arremessam, em média, tão bem \nquanto seus adversários",
  #         size = 3.2)+
  #annotate(geom = "text", x = 0.332, y = 0.40, label = "Times que acertam, em média, menos que\n seus adversários",
  #         size = 3.2)+
  #annotate(geom = "text", x = 0.33, y = 0.31, label = "Times que arremessam, em média, tão mal \nquanto seus adversários",
  #         size = 3.2)+
  #annotate(geom = "text", x = 0.39, y = 0.31, label = "Times que acertam, em média, mais que\n seus adversários",
  #         size = 3.2)+
  theme(
    axis.line = element_line(arrow = arrow()),axis.title = element_text(face = "bold"),
    axis.ticks = element_line(),axis.title.y = element_text(angle = 90,face = "bold"),
    title = element_text(face = "bold"),
    axis.text = element_text()
  )


ggdraw()+
  draw_image("cesta_7_basq.png", scale = 0.20, x=0.35,y=0.41)+
  draw_plot(tres_pts)


  
times_nba %>% 
  ggplot(aes(off_eFG_prc,-def_eFG_prc))+
  geom_image(data = times_nba, aes(image = times_nba$logo), size = 0.12,
             by = "width")+coord_fixed()
  #geom_vline(xintercept = mean(times_nba$def_eFG_prc), linetype = "dashed") +
  #geom_hline(yintercept = mean(times_nba$off_eFG_prc), linetype = "dashed") 
  
times_nba %>% 
  ggplot(aes(W, PW))+
  geom_image(data = times_nba, aes(image = times_nba$logo), size = 0.12,
             by = "width")+coord_fixed()
  geom_smooth()
  labs(x = "Vitórias", y = "Vitórias Pitagóricas")
  

times_nba %>% 
  mutate(PW_d = PW - W) %>% 
  select(Team, PW_d, W) %>%
  mutate(supos_vic = PW_d + W) %>% 
  arrange(desc(PW_d)) %>% View("Diferenca de vitoria pitagorica")

times_nba %>% 
  ggplot(aes(Pace/100,TS_pct))+
  geom_image(data = times_nba, aes(image = times_nba$logo), size = 0.12,
             by = "width")+
  coord_fixed()+
  geom_smooth(method = "lm",se=0,color = "red")

times_nba %>% 
  ggplot(aes(Pace, pct_fgA_dunk,label = Team))+
  geom_label()
times_nba %>% 
  ggplot(aes(Pace, distance_avg,label = Team))+
  geom_label_repel()+
  labs(y = "Distância Média dos arremessos")

net_nba <- times_nba %>% 
  mutate(eff_def = -DRtg) %>% 
  ggplot(aes(y=-DRtg,x=ORtg,colour = W-L))+
  geom_segment(
    x = mean(times_nba$ORtg), yend=max(-times_nba$DRtg)+0.7,
    color = "#EF4120",
    y=min(-times_nba$DRtg)-1, xend=mean(times_nba$ORtg),
    size = 1, arrow = arrow(length = unit(0.2, "inches"),type = "closed")
  ) +
  geom_segment(
    x = min(times_nba$ORtg)-1, xend = max(times_nba$ORtg)+0.76,
    color = "#EF4120",
    y = mean(-times_nba$DRtg), yend = mean(-times_nba$DRtg),
    size = 1, arrow = arrow(length = unit(0.2, "inches"),type = "closed")
  ) +
  geom_image(data = times_nba, aes(image = times_nba$logo), size = 0.12,
             by = "width")+
  coord_fixed()+
  geom_abline(slope = -1, color="red", intercept = 0,
              linetype="dashed", size=0.5)+
  annotate("text",label = "NET Positivo",angle = -45,
           x=104,y=-103.65,color = "#EF4120")+
  annotate("text",label = "NET Negativo",angle = -45,
           x=103.7,y=-103.95,color = "#601212")+
  annotate("text",label = "Piores Ratings Ofensivos",
           x = 102.7,y=-109.5,color = "#601212")+
  annotate("text",label = "Melhores Ratings Defensivos",
           x = 112.51,y=-103,color = "#EF4120")+
  labs(title = "M A P A   D A   E F I C I Ê N C I A   N A   N B A",
       subtitle = paste0(format(Sys.Date(), "%d de %B"),"\n"),
       caption = "Dados: Basketball reference\nVisualização: @cestade7")+
  theme_void()+
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 15),
        plot.subtitle = element_text(hjust = 0.5, face = "bold", size = 10),
        panel.background = element_blank())
ggdraw()+
  draw_plot(net_nba)+
  draw_image("cesta_7_basq.png", scale = 0.20, x=0.35,y=0.41)

net_sos <- times_nba %>% 
  filter(W/G > 0.5) %>% 
  ggplot(aes(reorder(Team,ORtg-DRtg), SOS))+
  geom_col(fill="#EF4120")+
  labs(subtitle = paste0("Apenas times com campanhas positivas ordenados por NET Rating\n",
                         format(Sys.Date(), "%d de %B"),"\n"),
       title = "Os melhores times tem vida mais fácil?",
       x="Melhores NET Ratings",
       y = "Calendário mais difícil",caption = "Dados: Basketball reference\nVisualização: @cestade7")+
  coord_flip()+
  scale_y_continuous(breaks = c(min(times_nba$SOS),-1,-0.5,0,0.5,1))+
  theme(
    axis.line = element_line(arrow = arrow()),axis.title = element_text(face = "bold",hjust = 1),
    axis.ticks = element_blank(),axis.title.y = element_text(angle = 90,face = "bold"),
    title = element_text(face = "bold"), plot.caption = element_text(face = "plain"),
    plot.subtitle = element_text(size = 14,face = "bold",hjust = 0.5),
    axis.text.y = element_text(face = "bold")
  )+
  gghighlight(SOS >0, unhighlighted_colour = "#121212")

ggdraw()+
  draw_plot(net_sos)+
  draw_image("cesta_7_basq.png", scale = 0.20, x=0.44,y=0.41)


net_sos_im <- times_nba %>% 
  ggplot(aes(SOS,ORtg-DRtg))+
  geom_hline(yintercept = 0,linetype = 2)+
  geom_vline(xintercept = 0,linetype = 2)+
  geom_image(data = times_nba, aes(image = times_nba$logo), size = 0.12,
             by = "width")+
  labs(x = "Calendários mais difíceis",
       title = "Os melhores times tem vida mais fácil?",
       subtitle = paste0('Conseguimos contar na mão quantos times do oeste estão do lado "fácil" do gráfico\n',
                         format(Sys.Date(), "%d de %B")),
       caption = "Dados: Basketball reference\nVisualização: @cestade7",
       y = "Melhores NET Rating")+
  scale_x_continuous(breaks = c(min(times_nba$SOS), -1, 0, 1, max(times_nba$SOS)),
                     labels = c("-1,32","-1","0","1","1,75"))+
  scale_y_continuous(breaks = c(min(times_nba$SRS), -5, 0, 5, max(times_nba$SRS)),
                     labels = c("-9,33","-5","0","5","8,31"))+
  theme(
    axis.line = element_line(arrow = arrow()),axis.title = element_text(face = "bold",hjust = 1),
    axis.ticks = element_blank(),axis.title.y = element_text(angle = 90,face = "bold"),
    title = element_text(face = "bold"), plot.caption = element_text(face = "plain"),
    plot.subtitle = element_text(size = 10,face = "bold",hjust = 0.5)
  )
ggdraw()+
  draw_image("cesta_7_basq.png", scale = 0.15, x=-0.42,y=-0.41)+
  draw_plot(net_sos_im)
  
times_nba %>% 
  ggplot(aes(Pace,ORtg-DRtg))+
  geom_image(data = times_nba, aes(image = times_nba$logo), size = 0.12,
             by = "width")+
  geom_smooth(method = "loess")
  



ggplot(short , aes(x = penetration, y = scc)) +
  geom_raster(aes(fill = pi0), interpolate=TRUE) +
  scale_fill_gradient2(low="navy", mid="white", high="red", 
                       midpoint=0, limits=range(short$pi0))
times_nba %>% 
  dplyr
  select(Team,FG:PTS) %>% 
  melt() %>% colnames()
  ggplot(aes(y = Team, x = variable, fill = value))+
  geom_raster()+
  geom_tile(colour="white",
            width=.9, height=.9)+
  scale_fill_distiller(palette = "RdPu", trans = "log10")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
