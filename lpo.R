library(trend)
library(ggridges)
library(gganimate)

team_palete_1 <- c("ATL"="#DF393E", "BOS"="#223C2B", "BRK"="#FEFEFE", "CHI"="#CD1141", "CHO"="#008CA8",
                   "CLE"="#6A0832", "DAL"="#006BB6", "DEN"="#0D213F", "DET"="#002D62", "GSW"="#006AB6",
                   "HOU"="#CD1141", "IND"="#002D62", "LAC"="#FEFEFE", "LAL"="#BA8D25", "MEM"="#5D76A9",
                   "MIA"="#98002E", "MIL"="#00471A", "MIN"="#001641", "NOP"="#032B5B", "NYK"="#F48427",
                   "OKC"="#F05033", "ORL"="#0076BF", "PHI"="#ED174B", "PHO"="#E46020", "POR"="#D7373D",
                   "SAC"="#592C81", "SAS"="#C4CDD3", "TOR"="#000000", "UTA"="#FAA11B", "WAS"="#E51937")

nba_teamss <- nba_teamss %>% 
  mutate(poss = 0.5 * ((tm_fga + 0.4 * tm_fta - 1.07 * (tm_orb/ (tm_orb + opp_trb - opp_orb)) * 
                          (tm_fga - tm_fg) + tm_tov) + 
                         (opp_fga + 0.4 * opp_fta - 1.07 * (opp_orb/(opp_orb + opp_trb - opp_orb)) * 
                            (opp_fga - opp_fg) + opp_tov)),
         team_tre_rt = tm_3pta/tm_fga,
         opp_tre_rt = opp_3pta/opp_fga,
         division = factor(
           case_when(
             team_check %in% c("TOR", "PHI", "BOS", "NYK", "BRK") ~ "ATLANTIC",
             team_check %in% c("MIL", "IND", "DET", "CHI", "CLE") ~ "CENTRAL",
             team_check %in% c("ORL", "CHO", "MIA", "WAS", "ATL") ~ "SOUTHEAST",
             team_check %in% c("DEN", "POR", "UTA", "OKC", "MIN") ~ "NORTHWEST",
             team_check %in% c("GSW", "PHO", "LAL", "LAC", "SAC") ~ "PACIFIC",
             team_check %in% c("HOU", "SAS", "MEM", "NOP", "DAL") ~ "SOUTHWEST",
             TRUE                                                 ~ "NON-identified"
           ), 
           levels = c("ATLANTIC", "CENTRAL", "SOUTHEAST",
                      "NORTHWEST", "PACIFIC", "SOUTHWEST"))) %>% 
  mutate(conference = factor(
    case_when(
      division %in% c("ATLANTIC", "CENTRAL", "SOUTHEAST")  ~ "Eastern Conference", 
      division %in% c("NORTHWEST", "PACIFIC", "SOUTHWEST") ~ "Western Conference",
      TRUE                                                 ~ "NON-identfied"
    )
  )) %>% 
  as_tibble()


die_welle <- nba_teamss %>% 
  mutate(team_check = fct_reorder(team_check, team_tre_rt),
         season = factor(season)) %>% 
  ggplot(aes(y = team_check, x = team_tre_rt, fill = team_check))+
  geom_density_ridges(alpha = .5)+
  scale_x_continuous(labels = scales::percent)+
  guides(fill = NULL)+
  scale_fill_manual(values = team_palete_1)+
  labs(x = expression(paste(frac(3-Point~Attempts, Field~Goal~Attempts))), 
       caption = "Season: {closest_state}\nVisualization: @RiversArthur\nData: Basketball Reference",
       title = "DIE WELLE\nor", 
       y = NULL,
       subtitle = 
         expression(paste("(THE ", italic("UNEXPECTED"), " VIRTUE OF SHOOTING FROM BEYOND THE ARC)"))
       )+
  theme_minimal() +
  theme(legend.position = "none",plot.title = element_text(hjust = 0.5),plot.tag = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5,face = "bold"),
        plot.caption = element_text(size = 10), axis.text = element_text(face = "bold"))+
  transition_states(
    season,
    transition_length = 2,
    state_length = 3
    )

anim_save(die_welle+
            labs(tag = "Director's cut"),
          filename = "die_welle_or_shutup_and_shoot_beyond_the_arc_directorscut.gif",
          height = 800, width = 800)

anim_save(die_welle +
            facet_wrap(~division, scales = "free_y"),
          filename = "die_welle_or_shutup_and_shoot_beyond_the_arc.gif",
          height = 800, width = 800)

nba_teamss %>% 
  filter(team_check %in% c("HOU", "LAL")) %>% 
  ggplot(aes(team_tre_rt, opp_tre_rt, fill = team_check, colour = team_check))+
  geom_density2d(size = 1)+
  geom_point(color = "white", alpha = .3)+
  scale_colour_manual(values = c("HOU" = "red", "LAL" = "gold3"))+
  theme_void()+
  theme(panel.background = element_rect(fill = "blue"))

nba_teamss %>% filter(team_check %in% c("DET", "HOU", "LAL", "GSW", "UTA")#,
                     #season == 2019
                     ) %>% 
  ggplot(aes(x = date, y = poss, colour = team_check, fill = team_check))+
  geom_smooth()+geom_point()+
  #geom_vline(xintercept = as.numeric(
  #  as.Date("2019-02-15")
  #), 
  #linetype=4)+
  #geom_vline(xintercept = as.numeric(
  #  as.Date("2019-02-17")
  #), 
  #linetype=4)+
  guides(colour = FALSE)+
  scale_colour_manual(values = c("DET" = "#A1B324", "HOU" = "#75B7FF", "LAL" = "#F2FF9C", "GSW" = "#CC7068", "UTA" = "#B3261B"))+
  scale_fill_manual(values = c("DET" = "#A1B324", "HOU" = "#75B7FF", "LAL" = "#F2FF9C", "GSW" = "#CC7068", "UTA" = "#B3261B"))+
  theme_minimal() 
  


nba_teamss %>% 
  group_by(team_check) %>% 
  summarise(mk_rand_fta = mk.test(team_tre_rt, alternative = "greater")$p.value) %>% 
  arrange(mk_rand_fta)

nba_teamss %>% 
  ggplot(aes(x = season, y = tm_tov))+
  geom_boxplot()+
  facet_wrap(~team_check)

nba_teamss %>% filter(team_check == "SAC") %>% 
  pull(team_tre_rt) %>% 
  plot(type = "l")



lm(tm_score ~ team_tre_rt + tm_ast + tm_tov + poss,
   nba_teamss)
glm(tm_score ~ team_tre_rt + tm_ast + tm_tov + poss, data = nba_teamss)


nba_teamss %>% 
  ggplot(aes(x = team_tre_rt + tm_ast + tm_tov + poss, y = tm_score, colour = result))+
  geom_point(alpha = .4)+
  facet_wrap(~team_check)+
  geom_smooth(method = "lm")
