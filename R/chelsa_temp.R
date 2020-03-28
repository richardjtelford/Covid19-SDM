library(tidyverse)

cases <- DownloadData()

#data from 
#https://envidatrepo.wsl.ch/uploads/chelsa/chelsa_V1/climatologies/temp/integer/temp/
temp_feb <- raster::raster("data/CHELSA_temp10_02_1979-2013_V1.2_land.tif")

#correct location of French Polynesia (should be corrected in source soon - issue #641)
cases <- cases %>% 
  mutate(Long = case_when(
    `Province/State` == "French Polynesia" & Long > 0 ~ -Long, 
    TRUE ~ Long))

#make spatial points df of cases (should use sf but cannot install)
loc <- cases %>%
  select(Lat, Long) %>% 
  distinct() %>% 
  ungroup()
loc_sp <- loc %>% 
  select(Lat, Long)
sp::coordinates(loc_sp) <- ~ Long + Lat
 
#extract temperatures
loc_temp <- loc %>% 
  mutate(feb = raster::extract(temp_feb, loc_sp)) #spot temp, fast
        
#with buffer for sites in ocean 40km is minumum that gets all (Bahamas is worst)
loc_sp2 <- loc_sp[is.na(loc_temp$feb)]

loc_temp2 <- loc_temp %>% 
  filter(is.na(feb)) %>%
  select(-feb) %>% 
  mutate(feb_buffered = raster::extract(temp_feb, loc_sp2, buffer = 40000, fun = mean, na.rm = TRUE))

loc_temp2

#combine spot and buffered data
loc_temp <- left_join(loc_temp, loc_temp2) %>% 
  mutate(feb = coalesce(feb, feb_buffered)) %>% 
  select(-feb_buffered) %>% 
  mutate(feb = feb/10)

#map
mp <- map_data("world")
temp_map <- ggplot(loc_temp, aes(x = Long, y = Lat, colour = feb)) +
  geom_map(map = mp, data = mp, aes(map_id = region), inherit.aes = FALSE, fill = "grey80", colour = "grey40") +
  geom_point() +
  coord_quickmap() +
  scale_colour_viridis_c() +
  theme(legend.position = "bottom",
        axis.title = element_blank())
temp_map

#join with cases
cases_temp <- cases %>% left_join(loc_temp) %>% 
  filter(date == "2020-03-10" | date == max(date)) 

#plot cases by temperature
cases_temp %>% 
  ggplot(aes(x = feb, y = log(cases), colour = as.factor(date))) +
  geom_smooth() + 
  geom_point()

#plot change in cases by temperature
cases_temp %>% 
  pivot_wider(names_from = date, values_from = cases) %>% 
  rename(march10 = last_col(1), latest = last_col()) %>% 
  ggplot(aes(x = feb, y = log1p(latest) - log1p(march10), colour = log10(march10))) +
  geom_point() +
  scale_colour_viridis_c() 

#presence absence analysis

cases_temp %>% 
  mutate(present = if_else(cases < 5, 0, 1), 
         date = format(date, "%b %d")) %>% 
  ggplot(aes(x = feb, y = present)) +
  geom_point() +
#  geom_smooth(method = "glm", formula = y ~ splines::ns(x), method.args = list( family = "binomial")) +
   #geom_smooth(method = "glm", formula = y ~ poly(x, 2), method.args = list( family = "binomial")) +  
  geom_smooth(method = "gam", method.args = list( family = "binomial")) +
  facet_wrap(~date)
  


#growth vs temperature
cases %>% 
  left_join(loc_temp) %>%
  filter(max(cases) > 450) %>% 
  ggplot(aes(date, cases, colour = feb, group = paste(`Province/State`, `Country/Region`))) + 
  geom_line() +
  scale_colour_viridis_c() +
  coord_cartesian(ylim = c(0, 500))

cases_100_400 <- cases %>% 
  left_join(loc_temp) %>%
  group_by(feb, Lat, Long, add = TRUE) %>% 
  filter(min(cases) < 100, max(cases) > 400) %>% 
  nest() %>% 
  mutate(time = map_dbl(data, ~diff(approx(x = .x$cases, y = .x$date, xout = c(100, 400))$y))) 
  
cases_100_400 %>% ggplot(aes(x = feb, y = time)) +
  geom_point()

#######  Ficetola &  Rubolini
  
r50 <- cases %>% 
  left_join(loc_temp) %>% 
  filter(cases >= 50) %>%
  #get first date above 50 and 5 days later
  filter(date == min(date) | date == min(date + 5)) %>% 
  #check at least 2 dates (ie day 5 is present)
  filter(n() == 2) %>% 
  group_by(feb, .add = TRUE) %>% 
  summarise(r50 = (log(max(cases)) - log(min(cases))) / 5)
  

ggplot(r50, aes(x = feb, y = r50)) + 
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x + I(x^2))


r50 %>% filter(r50 > 0.4) %>% 
  arrange(-r50)
