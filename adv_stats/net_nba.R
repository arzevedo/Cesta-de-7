library(ggimage)
library(ggforce)

net_nba <- times_nba %>% 
  mutate(eff_def = -DRtg,
         team_rank = case_when(
           ORtg >= (times_nba %>% top_n(wt = ORtg, n = 3) %>% pull(ORtg) %>% min()) ~ "Melhores Ataques",
           ORtg <= (times_nba %>% top_n(wt = -ORtg, n = 3) %>% pull(ORtg) %>% max()) ~ "Piores Ataques",
           DRtg <= (times_nba %>% top_n(wt = -DRtg, n = 3) %>% pull(DRtg) %>% max()) ~ "Melhores Defesas",
           DRtg >= (times_nba %>% top_n(wt = DRtg, n = 3) %>% pull(DRtg) %>% min()) ~ "Piores Defesas",
           TRUE ~ "ignore"
         )) %>%
  ggplot(aes(y=-DRtg,x=ORtg))+
  #ggforce::geom_mark_ellipse(
  #  aes(description = "Melhores Ataques",
  #      filter = Team %in% c("Dallas Mavericks","Los Angeles Clippers",
  #                           "Milwaukee Bucks")), fill = "#EF4120", alpha = .5,
  #  label.fontsize = 15, label.colour = "#EF4120", label.fontface = "bold"
  #)+
  #ggforce::geom_mark_circle(
  #  aes(description = "Melhores Defesas",
  #      filter = Team %in% c("Los Angeles Lakers",
  #                           "Oklahoma City Thunder", "Utah Jazz" )), 
  #  fill = "#EF4120", alpha = .5,
  #  label.fontsize = 15, label.colour = "#EF4120", label.fontface = "bold"
  #)+
  #ggforce::geom_mark_rect(
  #  aes(description = "Piores Ataques",
  #      filter = Team %in% c("Memphis Grizzlies", "New York Knicks",
  #                           "Orlando Magic")), 
  #  fill = "#601212", alpha = .5,
  #  label.fontsize = 15, label.colour = "#601212", label.fontface = "bold"
  #)+
  ggforce::geom_mark_circle(
    aes(fill = team_rank, label = team_rank,
        filter = team_rank != "ignore"), con.cap = 0,label.buffer = unit(40, "mm"),
    alpha = .5,show.legend = FALSE,
    label.fontsize = 15, label.colour = "#601212", label.fontface = "bold",expand = .02
  )+
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
  geom_image(aes(image = logo), size = 0.10,
             by = "width")+
  geom_abline(slope = -1, color="red", intercept = 0,
              linetype="dashed", size=0.5)+
  annotate("text",label = "NET Positivo",angle = -40,size=5,
           x=98,y=-97.5,color = "#EF4120")+
  annotate("text",label = "NET Negativo",angle = -40,size=5,
           x=97.85,y=-98.4,color = "#601212")+
  #annotate("text",label = "Piores Ratings Ofensivos",,size=8,
  #         x = 102.7,y=-118.5,color = "#601212")+
  #annotate("text",label = "Melhores Ratings Defensivos",size=8,
  #         x = 112.51,y=-100,color = "#EF4120")+
  labs(title = "M A P A   D A   E F I C I Ê N C I A   N A   N B A",
       subtitle = paste0(format(Sys.Date(), "%d de %B"),"\n"),
       caption = "Dados: Basketball reference\nVisualização: @cestade7")+
  scale_fill_manual(
    values = c(
      "Melhores Ataques" = "#50FF51",
      "Piores Ataques"= "#FF221B",
      "Melhores Defesas" = "#50FF51",
      "Piores Defesas" = "#FF221B",
      "ignore" = "#40F500"
    )
  ) +
  theme_void()+
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 15),
        plot.caption = element_text(face = "bold", size = 10),
        plot.subtitle = element_text(hjust = 0.5, face = "bold", size = 12),
        panel.background = element_blank())
#library(cowplot)
ggdraw()+
  draw_plot(net_nba)+
  draw_image("cesta_7_basq.png", scale = 0.20, x=0.38,y=0.41)

ggsave(paste0("quatro_teste_net",Sys.Date(),".png"), width = 10, height = 10, dpi = 450)

