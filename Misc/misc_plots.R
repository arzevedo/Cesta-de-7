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
