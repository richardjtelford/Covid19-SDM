library(tidyverse)

#data from 
#https://envidatrepo.wsl.ch/uploads/chelsa/chelsa_V1/climatologies/temp/integer/temp/
temp_feb <- raster::raster("data/CHELSA_temp10_02_1979-2013_V1.2_land.tif")

loc <- cases %>% select(Lat, Long)
sp::coordinates(loc) <- ~ Long + Lat
 
cases_temp <- cases %>% 
  mutate(feb = raster::extract(temp_feb, loc), #spot temp, fast
        
         feb = feb/10)


feb = ifelse(
  test = is.na(feb), 
  yes = raster::extract(temp_feb, loc, buffer = 1000, fun = mean, na.rm = TRUE),
  no = feb),#try to catch missing data with buffer

mp <- map_data("world")
temp_map <- ggplot(cases_temp %>% filter(Long > 150), aes(x = Long, y = Lat, colour = is.na(feb))) +
  geom_map(map = mp, data = mp, aes(map_id = region), inherit.aes = FALSE, fill = "grey80", colour = "grey40") +
  geom_point() +
  coord_quickmap() +
#  scale_colour_viridis_c() +
  theme(legend.position = "bottom",
        axis.title = element_blank())
temp_map

cases_temp %>% filter(is.na(feb)) %>% 
  select(1:4, last_col(1)) %>% 
  view
