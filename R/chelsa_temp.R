library(tidyverse)

temp_feb <- raster::raster("data/CHELSA_temp10_02_1979-2013_V1.2_land.tif")

loc <- cases %>% select(Lat, Long)
sp::coordinates(loc) <- ~ Long + Lat
 
cases_temp <- cases %>% 
  mutate(feb = raster::extract(temp_feb, loc))

mp <- map_data("world")
temp_map <- ggplot(cases_temp, aes(x = Long, y = Lat, colour = feb)) +
  geom_map(map = mp, data = mp, aes(map_id = region), inherit.aes = FALSE, fill = "grey80", colour = "grey40") +
  geom_point() +
  coord_quickmap() +
  scale_colour_viridis_c() +
  theme(legend.position = "bottom",
        axis.title = element_blank())

temp_map


