library("tidyverse")
library("gganimate")

#download data
source("R/download_data.R")

cases <- DownloadData()
  
#correct location of French Polynesia (should be corrected in source soon - issue #641)
cases <- cases %>% 
  mutate(Long = case_when(
    `Province/State` == "French Polynesia" & Long > 0 ~ -Long, 
    TRUE ~ Long))

#west bank fix
cases %>% filter(cases < 0)
cases %>% filter(`Country/Region` == "West Bank and Gaza") %>% tail()

cases <- cases %>% 
  mutate(cases = if_else(cases < 0, true = lag(cases), false = cases)) 

#map
cases_to_map <- cases %>% 
  filter(date == "2020-03-10" | date == max(date)) %>% 
  mutate(when = if_else(date == "2020-03-10", "march10", "latest")) %>%
  select(-date) %>% 
  pivot_wider(names_from = when, values_from = cases) %>% 
  mutate(five = case_when(
    march10 >= 5 ~ "by 10 March",
    latest >= 5 ~ "by now", 
    TRUE ~ "not yet"
  )) %>% 
  arrange(desc(five))

mp <- map_data("world")
case_map <- ggplot(cases_to_map, aes(x = Long, y = Lat, colour = five)) +
  geom_map(map = mp, data = mp, aes(map_id = region), inherit.aes = FALSE, fill = "grey80", colour = "grey40") +
  geom_point() +
  coord_quickmap() +
  labs(colour = "Reached five cases") +
  theme(legend.position = "bottom",
        axis.title = element_blank())

case_map


#animation
date_format <- "%d %B"
date_levels <- format(
  x = seq(min(cases$date), max(cases$date), by = "1 day"), 
  date_format)

case_map_animate <- cases %>% 
  filter(cases > 0) %>% 
  arrange(cases) %>% 
  mutate(date2 = format(date, date_format),
         date2 = factor(date2, levels = date_levels)) %>% 
  ggplot(aes(x = Long, y = Lat, colour = cases > 5)) +
  geom_map(map = mp, data = mp, aes(map_id = region), inherit.aes = FALSE, fill = "grey85", colour = "grey40") +
  geom_point() +
  coord_quickmap() +
  scale_color_brewer(palette = "Set1", direction = -1, labels = c("One to four cases", "Five or more cases")) +
  labs(colour = "") +
  transition_states(date2,
                    transition_length = 0,
                    state_length = 1) +
  ggtitle('{closest_state}') + 
  theme_bw(base_size = 16) +
  theme(legend.position = "bottom",
        axis.title = element_blank(), 
        axis.text = element_blank(), 
        panel.grid = element_blank()
        )

nsteps <- n_distinct(cases$date) 
pause <- 5
animate(case_map_animate, fps = 5, nframes = nsteps + pause,  height = 260, width = 480, end_pause = pause)
anim_save(filename = "animated_cases.gif")
