library(raster)
library(sf)
library(ggplot2)

# Import the data
inputDataFolder <- Sys.getenv("WORKSPACE_COVIDSDMRESPONSE")
covidData <- readRDS(paste(inputDataFolder, "vectorData.rds", sep = "/"))

# Create a set of dates to demonstrate the distribution of the COVID-19 coronavirus at
# calculationDates <- strptime(c("15-02-2020", "15-03-2020"), format = "%d-%m-%Y")
calculationDates <- seq(min(covidData$covidData$date), max(covidData$covidData$date), 60 * 60 * 24 * 7)

# Arrange the data in a series of time-slices arranged according to the calculation dates above
dateStackCases <- lapply(X = calculationDates, FUN = function(curDate, covidData) {
  # Initialise an empty data frame
  nearestCases <- as.data.frame(matrix(character(), nrow = 0, ncol = ncol(covidData) + 1, dimnames = list(NULL, c(colnames(covidData), "usedInSDM"))))
  if(any(curDate - covidData$date >= 0)) {
    # Create a data frame of number of cases nearest in the past to the current calculation date
    nearestCases <- do.call(rbind, lapply(X = unique(covidData$siteID[curDate - covidData$date >= 0]), FUN = function(curID, curDate, covidData) {
      idData <- covidData[covidData$siteID == curID, ]
      # Calcualte the difference in time between the current calculation date and the dates in the covid data
      timeDiff <- curDate - idData$date
      timeDiff <- ifelse(timeDiff < 0, Inf, timeDiff)
      idData[min(timeDiff, na.rm = TRUE) == timeDiff, , drop = FALSE]
    }, curDate = curDate, covidData = covidData))
    nearestCases <- st_sf(cbind(as.data.frame(nearestCases), data.frame(
      usedInSDM = factor(ifelse(nearestCases$cases >= 5, "used", "notUsed"), levels = c("used", "notUsed"), labels = c("5 or more cases", "fewer than 5 cases"))
    )), crs = crs(covidData))
  }
  nearestCases
}, covidData = covidData$covidData)
names(dateStackCases) <- paste("caseData", as.character(calculationDates), sep = "_")

# Function to plot the CORONA virus cases
casePlot <- function(dateSliceData, adminBorders, plotSave = NULL) {
  outGrob <- ggplot(data = adminBorders) + geom_sf(fill = rgb(255, 248, 220, maxColorValue = 255)) + geom_sf(data = dateSliceData, aes(col = usedInSDM), show.legend = FALSE) + theme_classic()
  if(!is.null(plotSave)) {
    ggsave(plotSave, outGrob, device = "svg")
  }
  outGrob
}

# Create a time series of the CORONA virus cases plots
caseFigures <- setNames(lapply(X = names(dateStackCases), FUN = function(curSliceName, dateStackCases, adminBorders, outLoc) {
  casePlot(dateStackCases[[curSliceName]], adminBorders, paste(outLoc, "/", curSliceName, ".svg", sep = ""))
}, dateStackCases = dateStackCases, adminBorders = covidData$adminData, outLoc = inputDataFolder), names(dateStackCases))
