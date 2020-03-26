library(grid)
library(ggplot2)
library(gridExtra)
library(raster)

# Import the case figure grobs
inputDataFolder <- paste(Sys.getenv("WORKSPACE_COVIDSDMRESPONSE"), "AnalysisOutputs", sep = "/")
caseFigures <- readRDS(paste(inputDataFolder, "caseFigures.rds", sep = "/"))

# Case dates to create a composite of
caseDates <- names(caseFigures)[c(1, 8, 9)]

# Function to relabel Norwegian month names to English (having problems with different locales on my different computers)
monthRelabel <- function(inText) {
  curText <- inText
  curText <- gsub("januar", "January", curText, fixed = TRUE)
  curText <- gsub("februar", "February", curText, fixed = TRUE)
  curText <- gsub("mars", "March", curText, fixed = TRUE)
  curText <- gsub("april", "April", curText, fixed = TRUE)
  curText <- gsub("mai", "May", curText, fixed = TRUE)
  curText <- gsub("juni", "June", curText, fixed = TRUE)
  curText <- gsub("juli", "July", curText, fixed = TRUE)
  curText <- gsub("august", "August", curText, fixed = TRUE)
  curText <- gsub("september", "September", curText, fixed = TRUE)
  curText <- gsub("oktober", "October", curText, fixed = TRUE)
  curText <- gsub("november", "November", curText, fixed = TRUE)
  curText <- gsub("desember", "December", curText, fixed = TRUE)
  curText
}

# Make a graphical object for the text labels
labelGrob <- lapply(X = caseDates, FUN = function(caseStr) {
  textGrob(monthRelabel(format(as.Date(gsub("caseData_", "",caseStr, perl = TRUE)), "%d %B %Y")))
})
# Make a time series of the case data
casesGrob <- do.call(grid.arrange, append(
  append(labelGrob, lapply(X = caseFigures[caseDates], FUN = function(curGrob) {
    curGrob + theme(plot.margin = unit(c(0.0, 0.0, 0.0, 0.0), "cm"))
  })), list(
    nrow = 2, ncol = length(caseDates), heights = c(0.2, 1)
  )
))
ggsave(plot = casesGrob, filename = paste(inputDataFolder, "casesTimeSeries.svg", sep = "/"), width = 19, height = 3)
ggsave(plot = casesGrob, filename = paste(inputDataFolder, "casesTimeSeries.jpeg", sep = "/"), width = 19, height = 3)

# Make a time series of the GAM predictions
monthNames <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")
predictionGrobs <- setNames(lapply(
  X = monthNames,
  FUN = function(curMonth, inputDataFolder, caseDates, labelGrob, monthNames) {
    # Get a data frame of predictions made for the current month from the different case data cutoff dates
    predictFrame <- do.call(rbind, lapply(X = caseDates, FUN = function(curCaseDate, curMonth, inputDataFolder, monthNames) {
      # Retrieve the prediction stack for the model outputs
      curPredStack <- stack(paste(inputDataFolder, "/", curCaseDate, "_predictionStack.tif", sep = ""))
      names(curPredStack) <- monthNames
      # Retrieve the raster layer for the particular month
      monthLayer <- curPredStack[[curMonth]]
      cbind(
        as.data.frame(coordinates(monthLayer)),
        setNames(as.data.frame(monthLayer), "predictVal"),
        data.frame(cutoffDate = rep(
          monthRelabel(format(as.Date(gsub("caseData_", "", curCaseDate, perl = TRUE)), "%d %B %Y")),
          ncell(monthLayer))))
    }, curMonth = curMonth, inputDataFolder = inputDataFolder, monthNames = monthNames))
    # Create the facet plot
    predictGrob <- ggplot(predictFrame, aes(x, y)) + geom_tile(aes(fill = predictVal)) + facet_grid(. ~ cutoffDate) +
      theme_classic() + theme(legend.title = element_blank(), axis.line = element_blank(), axis.text = element_blank(), axis.title = element_blank(), axis.ticks = element_blank(), strip.background = element_blank()) +
      scale_fill_gradient(low = rgb(252, 230, 201, maxColorValue = 255), high = rgb(255, 0, 0, maxColorValue = 255), na.value = rgb(255, 255, 255, maxColorValue = 255))
    ggsave(plot = predictGrob, filename = paste(inputDataFolder, "/predictTimeSeries", curMonth, ".jpeg", sep = ""), width = 20, height = 4)
    predictGrob
  }, inputDataFolder = inputDataFolder, caseDates = caseDates, labelGrob = labelGrob, monthNames = monthNames
), monthNames)

saveRDS(list(
  casesGrob = casesGrob,
  predictionGrobs = predictionGrobs
), file = paste(inputDataFolder, "collatedFigures.rds", sep = "/"))