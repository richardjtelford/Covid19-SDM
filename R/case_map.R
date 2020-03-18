library("tidyverse")

#download data
source("R/download_data.R")

cases <- DownloadData()
  
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
