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
case_map_animate <- cases %>% arrange(cases) %>% ggplot(aes(x = Long, y = Lat, colour = cases > 5)) +
  geom_map(map = mp, data = mp, aes(map_id = region), inherit.aes = FALSE, fill = "grey80", colour = "grey40") +
  geom_point() +
  coord_quickmap() +
  labs(colour = "Reached five cases") +
  transition_states(date,
                    transition_length = 0,
                    state_length = 3) +
  ggtitle('Now showing {closest_state}') + 
  theme_bw() +
  theme(legend.position = "bottom",
        axis.title = element_blank())

case_map_animate

