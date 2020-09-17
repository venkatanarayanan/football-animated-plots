library(tidyverse)
library(ggsoccer)
library(ggtext)
library(gganimate)
library(magrittr)
library(extrafont)

# filtering the data for the specific passing sequence
everton_buildup <- spurs_everton %>%
  filter(minute == 3, teamId == 31, second < 40) %>%
  select(playerId, teamId, minute, second, x,y,action)


# add spurs location data - approximations 
everton_buildup <- rbind(everton_buildup, c(92508, 30, 3 , 16.5, 25.4, 75, NA, NA, NA))
everton_buildup <- rbind(everton_buildup, c(92508, 30, 3 , 17.5, 20.4, 90, NA, NA, NA))
everton_buildup <- rbind(everton_buildup, c(101859, 30, 3 , 16.5, 35.4, 70, NA, NA, NA))
everton_buildup <- rbind(everton_buildup, c(101859, 30, 3 , 16.8, 27, 80, NA, NA, NA))
everton_buildup <- rbind(everton_buildup, c(131519, 30, 3, 13.5, 20.4, 36.7, NA, NA, NA))
everton_buildup <- rbind(everton_buildup, c(131519, 30, 3, 15.5, 11.4, 36.7, NA, NA, NA))
everton_buildup <- rbind(everton_buildup, c(83532, 30, 3, 8, 20.4, 57.3, NA, NA, NA))
everton_buildup <- rbind(everton_buildup, c(83532, 30, 3, 16, 20.4, 64.2, NA, NA, NA))

# add player names and team names
everton_buildup %<>%
  mutate(team_name = ifelse(teamId == 31, "Everton", "Spurs"),
         plyer_name = ifelse(playerId == 107941, "Keane",
                      ifelse(playerId == 92508, "Moura",
                      ifelse(playerId == 89925, "Digne",
                      ifelse(playerId == 318432, "Mina",
                      ifelse(playerId == 110189, "Pickford",
                      ifelse(playerId == 83532, "Kane",
                      ifelse(playerId == 131519, "Alli",
                      ifelse(playerId == 317804, "Richarlison",
                      ifelse(playerId == 116317, "Doucoure",
                      ifelse(playerId == 118123, "Gomes",
                      ifelse(playerId == 	31826, "Coleman",
                      ifelse(playerId == 101859, "Hojbjerg",
                      "James")))))))))))))

# arrange the data frame in sequenetial order
everton_buildup %<>% arrange(second)

# build the plot
spurs_press <- ggplot(everton_buildup) +
  annotate_pitch(limits = FALSE,
                 colour = "navajowhite3",
                 fill = "gray22") +
  # geom_point(aes(x = x, y = y, group = seq_along(second))) +
  geom_point(aes(x = x, y = y,
                 fill = team_name),
             pch = 21,
             size = 6) +
  # geom_line(aes(x = x, y = y)) +
  # geom_point(aes(x = ifelse(teamId == 26, 100 - x + 5, x + 5),
  #                y = ifelse(teamId == 26, 100 - y + 5, y + 5),
  #                fill = teamId),
  #            pch = 21,
  #            size = 6) +
  # geom_text(aes(label = playerId,  x = x, y = y)) +
  scale_fill_manual(values = c("red1", "goldenrod1")) +
  transition_time(second) +
  shadow_mark(past = T, futue = F) +
  geom_text(aes(label = plyer_name, y = y + 3, x = x),
            color = "ivory1",
            fontface = "bold",
            family = "Georgia",
            size = 3) +
  coord_cartesian(clip = "off") +
  labs(title = "Spurs' disorganized Pressing & Everton Playing out from the back - 13/09/2020",
       subtitle = "Spurs manager Jose Mourinho mentioned his players were lazy in pressing and he may have been right\nThey were lazy, there was no organisation in pressing.Here is an instance from very early in the game.\nKane and Alli allow too much time forMina and Keane to exchange passes.Hojbjerg presses\naggressively and ends in no man's land.",
       caption = "Animation by Venkatanarayanan / @VenkyReddevil") +
  theme(plot.background = element_rect(fill = "gray22"),
        panel.background = element_rect(fill = "gray22"),
        panel.border = element_rect(fill = NA, color = "gray22", size = 1),
        panel.spacing = unit(2.5, "lines"),
        text = element_text(color = "ivory1", family = "Georgia", face = "bold"),
        strip.text = element_text(color = "ivory1", family = "Georgia",
                                  face = "bold", size = 18),
        strip.background = element_rect(fill = "gray22"),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none",
        legend.title = element_blank(),
        legend.key = element_blank(),
        legend.background = element_rect(fill = "gray22"),
        legend.text = element_text(size = 18,
                                   family = "Georgia",
                                   face = "bold",
                                   color = "ivory1"),
        plot.title = element_text(size = 20, face = "bold",
                                  family = "Georgia", hjust = 0.5,
                                  margin = margin(10, 0, 10,0, unit = "pt")),
        plot.subtitle = element_text(size = 12, face = "bold.italic",
                                         family = "Georgia", hjust = 0.5),
        plot.caption = element_text(face = "bold.italic", family="Georgia",
                                    size = 14))

# create animation
plot <- animate(spurs_press,
                width = 900,
                height = 600)

