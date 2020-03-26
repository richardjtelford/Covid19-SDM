library(raster)
library(hypervolume)
library(tidyverse)
library(ggthemes)
library(HH)

#function to measure niche volume
niche_volume <- function(x, dims, bandwidth){
  
  #building hypervolume
  hv <- hypervolume::hypervolume_gaussian(
    data = x[, dims],
    samples.per.point = ceiling(((10^(3 + sqrt(ncol(x))))/nrow(x))/5),
    kde.bandwidth = bandwidth,
    sd.count = 3,
    quantile.requested = 0.95,
    quantile.requested.type = "probability",
    chunk.size = 2500,
    verbose = FALSE
  )

  #getting volume
  hv.volume <- as.numeric(hypervolume::get_volume(hv))
  
  return(hv.volume)
  
}


#getting covidData
covid <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")

#to long format
covid <- covid %>% 
  tidyr::pivot_longer(matches("^\\d"), names_to = "date", values_to = "cases") %>% 
  dplyr::mutate(date = lubridate::mdy(date)) %>% 
  dplyr::group_by(`Province/State`, `Country/Region`) %>% 
  as.data.frame() %>% 
  dplyr::filter(cases > 0)

#importing environmental data
environment <- raster::stack(
  list.files(
    "/home/blas/Desktop/chelsa", #CHELSA folder with bio1 bio4 bio5 bio6 bio12 bio15 at 1km resolution
    full.names = TRUE
    )
  )

#extracting environmental data for covid locations
covid.environment <- raster::extract(
  x = environment, 
  y = covid[, c("Long", "Lat")], 
  df = TRUE
  )

#checking multicollinearity (the niche volume analysis does not deal well with correlated variables)
covid.environment$ID <- NULL

HH::vif(covid.environment)
cor(na.omit(covid.environment))
covid.environment$bio1 <- NULL #removed because temperature extremes seem to be more relevant to Araujo's model

HH::vif(covid.environment)
cor(na.omit(covid.environment))
covid.environment$bio4 <- NULL #same as bio1

HH::vif(covid.environment)
cor(na.omit(covid.environment))

#preparing the data for analysis
covid <- 
  covid %>% 
  dplyr::bind_cols(covid.environment) %>% 
  dplyr::select(-`Province/State`) %>% 
  na.omit() %>% 
  dplyr::mutate_at(colnames(covid.environment), scale) %>% #scaling environmental data (requirement of hypervolume)
  dplyr::mutate_at(c("Long", "Lat"), scale) %>% #scaling coordinates
  dplyr::rename(x = Long, y = Lat)

#output df
output.df <- data.frame(
  date = unique(covid$date),
  n.1 = NA,
  n.5 = NA,
  water.1 = NA,
  water.5 = NA,
  temperature.1 = NA,
  temperature.5 = NA,
  water_temperature.1 = NA,
  water_temperature.5 = NA,
  geo.1 = NA,
  geo.5 = NA,
  stringsAsFactors = FALSE
)

#niche dimensions
water.dims <- c("bio12", "bio15")
temperature.dims <- c("bio5", "bio6")
water.temperature.dims <- c(water.dims, temperature.dims)
geo.dims <- c("x", "y")

#estimating bandwidths for the whole data (bandwidth needs to be constant across realizations)
water.bandwidth <- hypervolume::estimate_bandwidth(
  data = covid[, water.dims],
  method = "silverman"
)

temperature.bandwidth <- hypervolume::estimate_bandwidth(
  data = covid[, temperature.dims],
  method = "silverman"
)

water.temperature.bandwidth <- hypervolume::estimate_bandwidth(
  data = covid[, water.temperature.dims],
  method = "silverman"
)

geo.bandwidth <- hypervolume::estimate_bandwidth(
  data = covid[, geo.dims],
  method = "silverman"
)

#iterating through dates
for(i in 1:nrow(output.df)){
  
  #getting date
  date.i <- output.df[i, "date"]
  
  #subset
  covid.i <- covid[covid$date == date.i, ]
  
  #number of locations
  output.df[i, "n.1"] <- nrow(covid.i)
  
  #niche volume water
  output.df[i, "water.1"] <- niche_volume(
    x = covid.i,
    dims = water.dims,
    bandwidth = water.bandwidth
  )
  
  #niche volume temperature
  output.df[i, "temperature.1"] <- niche_volume(
    x = covid.i,
    dims = temperature.dims,
    bandwidth = temperature.bandwidth
  )
  
  #niche volume water temperature
  output.df[i, "water_temperature.1"] <- niche_volume(
    x = covid.i,
    dims = water.temperature.dims,
    bandwidth = water.temperature.bandwidth
  )
  
  #niche volume geo
  output.df[i, "geo.1"] <- niche_volume(
    x = covid.i,
    dims = geo.dims,
    bandwidth = geo.bandwidth
  )
  
  #subseting to > 5
  covid.i <- filter(covid.i, cases > 5)
  
  #number of cases
  output.df[i, "n.5"] <- nrow(covid.i)
  
  #niche volume water
  output.df[i, "water.5"] <- niche_volume(
    x = covid.i,
    dims = water.dims,
    bandwidth = water.bandwidth
  )
  
  #niche volume temperature
  output.df[i, "temperature.5"] <- niche_volume(
    x = covid.i,
    dims = temperature.dims,
    bandwidth = temperature.bandwidth
  )
  
  #niche volume water temperature
  output.df[i, "water_temperature.5"] <- niche_volume(
    x = covid.i,
    dims = water.temperature.dims,
    bandwidth = water.temperature.bandwidth
  )
  
  #niche volume water temperature
  output.df[i, "geo.5"] <- niche_volume(
    x = covid.i,
    dims = geo.dims,
    bandwidth = geo.bandwidth
  )
  
}
 
#saving outcomes
save(covid, output.df, file = "covid_niche_stability.RData")

#to long format
output.df.long <-
  output.df %>% 
  pivot_longer(cols = 4:11, names_to = "variable", values_to = "niche.volume") %>% 
  as.data.frame() 

#separating variable and min.cases
output.df.long <- 
  output.df.long %>% 
  separate(col = variable, into = c("variable", "min.cases"), sep = "\\.")

#plot
ggplot(
  data = output.df.long, 
  aes(x = date, y = niche.volume, color = min.cases)
  ) + 
  geom_path() +
  facet_wrap("variable", scales = "free_y") + 
  ggthemes::theme_tufte()

