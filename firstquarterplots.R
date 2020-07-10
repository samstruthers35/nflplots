#Load in Packages
pacman::p_load(concaveman,ggforce,devtools,extrafont,ggbump,here, ggridges,plotly, glue, gganimate,googleVis, lubridate, rvest, stringr,tidyverse,nflfastR,ggrepel,ggimage,ggthemes,sf,RANN, magick)

#Load in Anthony Reinhard's New Logos Function
helmet_url = function(x) ifelse(is.na(x),NA,paste0('https://raw.githubusercontent.com/ajreinhard/data-viz/master/helmet_left/',x,'.png'))
ESPN_logo_url = function(x) ifelse(is.na(x),NA,ifelse(x %in% c('WAS','KC'),paste0('https://raw.githubusercontent.com/ajreinhard/data-viz/master/alt-logo/',x,'.png'),paste0('https://a.espncdn.com/i/teamlogos/nfl/500/',x,'.png')))

#Pull First Quarters from 2019
first_quarters <- all_nfl_pbp %>% filter(wp>=.20,wp<=.8,season_type == 'REG',qtr ==1, play ==1, !is.na(epa), season ==2019)
first_quarter_teams <- first_quarters %>% group_by(posteam, season) %>%  summarize(plays = n(),first_quarter_epa_per_play = mean(epa))
#Pull Rest of Quarters
rest_of_quarters <- all_nfl_pbp %>% filter(season_type == 'REG',qtr >=2,wp>=.20,wp<=.8, half_seconds_remaining >120, play ==1, !is.na(epa), season ==2019)
rest_of_quarters_teams <- rest_of_quarters %>% group_by(posteam) %>%  summarize(plays = n(),other_quarters_epa_per_play = mean(epa))

#Join for Plots
first_quarter_teams <- inner_join(first_quarter_teams, rest_of_quarters_teams, by =c('posteam'))
#Add in Logos
first_quarter_teams <- first_quarter_teams %>%  mutate(helmet_url = helmet_url(posteam), espn_new_logo = ESPN_logo_url(posteam))

#Put Together Plot
first_quarter_teams %>% ggplot(aes(x = first_quarter_epa_per_play, y = other_quarters_epa_per_play)) + 
  geom_image(aes(image = espn_new_logo)) +
  theme_clean() +
  geom_smooth(method = 'lm', color = 'black', se = FALSE) +
  theme(text = element_text(),
        plot.title = element_text(size = 12, family = "Trebuchet MS", hjust =.5),
        plot.subtitle = element_text(size = 12,family = "Trebuchet MS",
                                     color = "grey20", hjust =.5),
        axis.title = element_text(size = 12,family = "Trebuchet MS",color = "grey20"),
        axis.text = element_text(size = 10, face = "bold"),
        panel.grid.minor = element_blank(),
        legend.text = element_text(size = 8),
        legend.direction = "horizontal",
        legend.title = element_blank(), 
        legend.position = 'none', 
        plot.caption = element_text(size = 10,family = "Trebuchet MS",
                                    color = "grey20", hjust = .5)) +  
  labs(title = "1st Quarter Performance vs Rest of Game Performance",
       subtitle = "2019 Season (WP 20-80%, >2 Minutes remaining in Half)",
       x = "1st Quarter EPA per Play",
       y = "2nd-4th Quarter EPA per Play",
       caption = 'Data from @nflfastR. Plot by @Sam_S35'
  ) +
  geom_mark_hull(aes(filter = posteam %in% c('BUF',
                                             "LAC", 
                                            "HOU"),
                    label = "Slow Starters",
                    description = "These Teams were usually extremely inefficient in the first quarter, but got better as the game went on"),
                label.family = "Trebuchet MS", label.fontsize = c(8, 6),
                label.colour = "grey20", label.fill = "#cce5cc",fill = 'yellow', alpha = .1) +
  geom_mark_hull(aes(filter = posteam %in% c("KC", 
                                             "BAL"),
                     label = "Super Fast Starters",
                     description = "These Teams came out guns blazing, establishing themeselves early in games"),
                 label.family = "Trebuchet MS", label.fontsize = c(8, 6),
                 label.colour = "grey20", label.fill = "#cce5cc",fill = 'green', alpha = .1) +
  geom_mark_hull(aes(filter = posteam %in% c( "TB",
                                             "CAR",
                                             'CIN',
                                             'WAS'),
                     label = "Never Got it Going",
                     description = "These teams could not figure out how to move the ball efficiently, regardless of the Quarter"),
                 label.family = "Trebuchet MS", label.fontsize = c(8, 6),con.cap = unit(1, 'mm'),
                 label.colour = "grey20", label.fill = "#cce5cc",fill = 'red', alpha = .1) +
  geom_mark_hull(aes(filter = posteam %in% c("DEN", 
                                             "IND",
                                             'DET',
                                             'NE',
                                             'ARI'),
                     label = "Solid First Quarters",
                     description = "These teams had good First Quarter performances, while their efficiency slightly dropped in other quarters"),
                 label.family = "Trebuchet MS", label.fontsize = c(8, 6),
                 label.colour = "grey20", label.fill = "#cce5cc",fill = 'darkgrey', alpha = .2) +
  coord_cartesian(xlim = c(-.4,.4), ylim = c(-.3,.25))
ggsave('documents/slowstarters.png',dpi =1200)



