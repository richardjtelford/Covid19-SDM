library(raster)
library(sf)

outputDataFolder <- Sys.getenv("WORKSPACE_COVIDSDMRESPONSE")

# Download the Natural Earth dataset for country administrative boundaries
tempNatEarthLoc <- tempdir()
download.file("https://www.naturalearthdata.com/http//www.naturalearthdata.com/download/110m/cultural/ne_110m_admin_0_countries.zip", paste(tempNatEarthLoc, "NatEarthshp.zip", sep = "/"))
unzip(paste(tempNatEarthLoc, "NatEarthshp.zip", sep = "/"), exdir = paste(tempNatEarthLoc, "NatEarth", sep = "/"))
natEarthData <- st_read(paste(tempNatEarthLoc, "NatEarth", sep = "/"), "ne_110m_admin_0_countries")
natEarthData <- natEarthData[, c("ADMIN", "ADM0_A3", "REGION_UN", "CONTINENT", "geometry")]
colnames(natEarthData) <- c("Name", "CountryCode", "UNRegion", "Continent", "geometry")

monthText <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")
# The location of the confirmed cases data
covidDataLoc <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv"
# Retrieve the covid data from the repository
inCon <- url(covidDataLoc)
covidData <- read.csv(inCon)
# Slightly change the column names
colnames(covidData) <- gsub("^X", "Date_", gsub(".", "_", colnames(covidData), fixed = TRUE), perl = TRUE)
# French Polynesia coordinates were inverted in previous versions of the dataset (issue #641)
covidData[covidData$Province_State == "French Polynesia", "Long"] <- ifelse(
  covidData[covidData$Province_State == "French Polynesia", "Long"] > 0,
  -covidData[covidData$Province_State == "French Polynesia", "Long"],
  covidData[covidData$Province_State == "French Polynesia", "Long"])
# Convert the data into a long form
longFormCovidData <- do.call(rbind, lapply(X = gsub("^Date_", "", colnames(covidData)[grepl("^Date_", colnames(covidData), perl = TRUE)], perl = TRUE), FUN = function(curDate, covidData, monthText) {
  cbind(
    covidData[, !grepl("^Date_", colnames(covidData), perl = TRUE), drop = FALSE],
    data.frame(
      date = rep(strptime(curDate, format = "%m_%d_%y"), nrow(covidData)),
      monthText = factor(rep(monthText[as.integer(gsub("_\\d+_\\d+$", "", curDate, perl = TRUE))], nrow(covidData)), levels = monthText),
      cases = covidData[, paste("Date_", curDate, sep = "")],
      siteID = 1:nrow(covidData)
    )
  )
}, covidData = covidData, monthText = monthText))

# Set the locations of the CHELSA data
chelsaDataLoc <- "https://envidatrepo.wsl.ch/uploads/chelsa/chelsa_V1/climatologies"
allChelsaDataLocs <- setNames(c(
  paste(chelsaDataLoc, "/prec/CHELSA_prec_", formatC(1:12, width = 2, format = "d", flag = "0"), "_V1.2_land.tif", sep = ""),
  paste(chelsaDataLoc, "/temp/integer/temp/CHELSA_temp10_", formatC(1:12, width = 2, format = "d", flag = "0"), "_1979-2013_V1.2_land.tif", sep = ""),
  paste(chelsaDataLoc, "/temp/integer/tmax/CHELSA_tmax10_", formatC(1:12, width = 2, format = "d", flag = "0"), "_1979-2013_V1.2_land.tif", sep = ""),
  paste(chelsaDataLoc, "/temp/integer/tmin/CHELSA_tmin10_", formatC(1:12, width = 2, format = "d", flag = "0"), "_1979-2013_V1.2_land.tif", sep = "")
), c(
  paste("precip", monthText, sep = ""),
  paste("tempMean", monthText, sep = ""),
  paste("tempMax", monthText, sep = ""),
  paste("tempMin", monthText, sep = "")
))
# Import the CHELSA data (using the data at the 1km resolution requires a lot of computational resources so downscaled to 0.5 degree resolution instead)
aggFactor <- c(60)   # Aggregation factor (in number of cells)
chelsaData <- setNames(lapply(X = allChelsaDataLocs, FUN = function(curLoc, aggFactor) {
  cat("Currently processing and aggregating data at location ", curLoc, " ...\n", sep = "")
  aggregate(raster(curLoc), fact = aggFactor, fun = mean, na.rm = TRUE)
}, aggFactor = aggFactor), names(allChelsaDataLocs))
chelsaData <- raster::stack(chelsaData)

# Check the covid cases to ensure that their coordinates do not fall into the ocean (move it to the nearest cell centroid with data if that is the case).
# It is unclear whether Araujo and Naimi did this (if they did not then they would have lost some Pacific island case data).
climValsAtCoords <- extract(chelsaData, longFormCovidData[, c("Long", "Lat")])
isInNoDataLoc <- apply(X = as.matrix(climValsAtCoords), FUN = anyNA, MARGIN = 1)
longFormCovidData <- cbind(longFormCovidData, data.frame(
  latCorrected = longFormCovidData$Lat,
  longCorrected = longFormCovidData$Long
))
validCentroids <- st_as_sf(as.data.frame(coordinates(chelsaData)[!apply(X = as.matrix(values(chelsaData)), FUN = anyNA, MARGIN = 1), ]), coords = c(1, 2), crs = crs(chelsaData))
distMat <- as.matrix(st_distance(
  st_as_sf(longFormCovidData[isInNoDataLoc, c("longCorrected", "latCorrected")], coords = 1:2, crs = crs(chelsaData)),
  validCentroids
))
longFormCovidData[isInNoDataLoc, c("longCorrected", "latCorrected")] <- st_coordinates(validCentroids)[apply(X = distMat, FUN = which.min, MARGIN = 1), ]
# Remove zero cases from the dataset and convert to an sf object
longFormCovidData <- st_as_sf(longFormCovidData[!is.na(longFormCovidData$cases) & longFormCovidData$cases > 0, ], coords = c("longCorrected", "latCorrected"), crs = crs(chelsaData))

# Ensure that the admin data is on the same projection as the point location data
natEarthData <- st_transform(natEarthData, crs = crs(longFormCovidData))

# Save the data
writeRaster(chelsaData, filename = paste(outputDataFolder, "climateData.tif", sep = "/"), options = "INTERLEAVE=BAND", overwrite = TRUE)
saveRDS(list(
  covidData = longFormCovidData,
  adminData = natEarthData,
  climateLayers = names(chelsaData)
), file = paste(outputDataFolder, "vectorData.rds", sep = "/"))
