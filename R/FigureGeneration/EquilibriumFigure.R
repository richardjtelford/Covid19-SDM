library(raster)
library(sf)
library(ggplot2)
library(mgcv)

# Import the data
inputDataFolder <- paste(Sys.getenv("WORKSPACE_COVIDSDMRESPONSE"), "AnalysisOutputs", sep = "/")
vectorData <- readRDS(paste(inputDataFolder, "vectorData.rds", sep = "/"))
# monthText <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")
#vectorData$climateLayers <- c(
#  paste("precip", monthText, sep = ""),
#  paste("tempMean", monthText, sep = ""),
#  paste("tempMax", monthText, sep = ""),
#  paste("tempMin", monthText, sep = "")
# )
climateData <- stack(paste(inputDataFolder, "climateData.tif", sep = "/"))
names(climateData) <- vectorData$climateLayers

# Create a set of dates to model the distribution of the COVID-19 coronavirus at
# calculationDates <- strptime(c("15-02-2020", "15-03-2020"), format = "%d-%m-%Y")
calculationDates <- seq(min(vectorData$covidData$date), max(vectorData$covidData$date), 60 * 60 * 24 * 7)

# Create a data set presences and absences for model building at different stages of the outbreak
occSlices <- setNames(lapply(X = calculationDates, FUN = function(curDate, covidData, climateData) {
  cat("Processing occurrences for data up until", as.character(curDate), "...\n")
  # monthText <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")
  # Filter the data so only the records before the current calculation date are included
  filteredData <- covidData[covidData$date <= curDate, ]
  # Break the cases on a month-by-month basis so matching the timings of the outbreaks with the respective climate layer
  occFrame <- do.call(rbind, lapply(X = as.character(unique(filteredData$monthText)), FUN = function(curMonth, filteredData, climateData) {
    # Get the data that is present in the current month
    monthlyData <- filteredData[as.character(filteredData$monthText) == curMonth, ]
    # Go through each site ID and retrieve the row with the highest number of cases in the dataset (representing the peak of the infection in that month)
    highestCases <- do.call(rbind, lapply(X = unique(monthlyData$siteID), FUN = function(curID, monthlyData) {
      outFrame <- monthlyData[monthlyData$siteID == curID, , drop = FALSE]
      outFrame[which.max(outFrame$cases)[1], , drop = FALSE]
    }, monthlyData = monthlyData))
    # Retrieve the climate data just for this month (plus rename columns)
    monthlyClimateData <- as.data.frame(climateData)[, grepl(paste(curMonth, "$", sep = ""), names(climateData), perl = TRUE)]
    names(monthlyClimateData) <- gsub(curMonth, "", names(monthlyClimateData), fixed = TRUE)
    # Initialise an occurrence status variable
    occurrenceStatus <- rep(0, nrow(monthlyClimateData))
    occurrenceStatus[
      # If there is more than five cases recorded then there is a 'presence' for this month.  It appears that Araujo and Naimi have simply used the raw
      # number of cases as input here (which can never go down) so once a 'presence' is registered for one month then is registered for all future months.
      # Obviously this is terrible but I'm just recreating what they appear to have done...
      cellFromXY(climateData, st_coordinates(highestCases[highestCases$cases >= 5, ]))
    ] <- 1
    # Attach the occurrence status to the climate data frame
    monthlyClimateData <- cbind(data.frame(
      presAbs = occurrenceStatus
    ), monthlyClimateData)
    # Finally remove NA values from the dataset
    monthlyClimateData[!apply(X = as.matrix(monthlyClimateData), FUN = anyNA, MARGIN = 1), ]
  }, filteredData = filteredData, climateData = climateData))
  # Use the combined occurrence frame of cases over all the months observed so far
  occFrame
}, covidData = vectorData$covidData, climateData = climateData), paste("caseData", as.character(calculationDates), sep = "_"))

# Function to create a GAM for a given slice of occurrence data
gamSlice <- function(occSlice) {
  inFormText <- paste("presAbs ~ ", paste("s(", colnames(occSlice)[colnames(occSlice) != "presAbs"], ")", collapse = " + ", sep = ""), sep = "")
  gam(as.formula(inFormText), family = binomial("logit"), data = occSlice)
}
# Apply function to create a list of GAM model outputs
covidGAMs <- setNames(lapply(X = occSlices, FUN = gamSlice), paste("caseData", as.character(calculationDates), sep = "_"))

# Function to plot prediction for each month
makePredictions <- function(modelList, climateData, outLocation) {
  # Make a list of stacks of predictions (one stack for each date cutoff)
  setNames(lapply(X = names(modelList), FUN = function(curElName, modelList, climateData, outLocation) {
    # Retrieve the current model
    curEl <- modelList[[curElName]]
    # Retrieve the cutoff date for the case-data associated with the current model
    curDate <- gsub("^caseData_", "", curElName, perl = TRUE)
    cat("Processing predictions from model with case data up until", curDate, "...\n")
    # Create a prediction for each month
    monthNames <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")
    climateFrame <- as.data.frame(climateData)
    predStack <- lapply(X = monthNames,
      FUN = function(curMonth, curEl, climateData, climateExtent, climateCRS, climateRows, climateCols) {
        # Retrieve the data frame for the current month
        curClimate <- climateData[, grepl(paste(curMonth, "$", sep = ""), names(climateData), perl = TRUE)]
        colnames(curClimate) <- gsub(paste(curMonth, "$", sep = ""), "", colnames(curClimate), perl = TRUE)
        # Retrieve the prediction for the current climate data
        modPreds <- predict(curEl, newdata = curClimate, type = "response")
        # Create a raster of the current prediction (using the climate data as a template)
        raster(nrows = climateRows, ncols = climateCols, ext = climateExtent, crs = climateCRS, vals = as.double(modPreds))
      }, curEl = curEl, climateData = climateFrame, climateExtent = extent(climateData), climateCRS = crs(climateData),
        climateRows = nrow(climateData), climateCols = ncol(climateData))
    names(predStack) <- monthNames
    predStack <- stack(predStack)
    # Add some extra anlyses if the output location is set
    if(!is.null(outLocation)) {
      # Write the output
      writeRaster(predStack, filename = paste(outLocation, "/caseData_", curDate, "_predictionStack.tif", sep = ""), options = "INTERLEAVE=BAND", overwrite = TRUE)
      lapply(X = monthNames, FUN = function(curMonth, predFrame) {
        curPredFrame <- predFrame
        colnames(curPredFrame)[curMonth == colnames(curPredFrame)] <- "preds"
        # Plot the prediction for the current month
        grobOut <- ggplot(data = curPredFrame, aes(x, y)) + geom_tile(aes(fill = preds)) + theme_classic() +
          theme(legend.title = element_blank(), axis.line = element_blank(), axis.text = element_blank(), axis.title = element_blank()) +
          scale_fill_gradient(low = rgb(252, 230, 201, maxColorValue = 255), high = rgb(255, 0, 0, maxColorValue = 255), na.value = rgb(255, 255, 255, maxColorValue = 255))
        ggsave(paste(outLocation, "/caseData_", curDate, "_prediction", curMonth, ".jpeg", sep = ""), grobOut, device = "jpeg")
      }, predFrame = cbind(
        # Create a data frame of the predictions along with their coordinates
        as.data.frame(matrix(coordinates(predStack), ncol = 2, dimnames = list(NULL, c("x", "y")))),
        as.data.frame(predStack))
      )
      # Retrieve some summary statistics of the climate values
      climateValueRange <- sapply(X = attr(terms(curEl), "term.labels"), FUN = function(curTerm, climateFrame) {
        climVals <- as.matrix(climateFrame[, grepl(paste("^", curTerm, sep = ""), colnames(climateFrame), perl = TRUE)])
        setNames(c(min(climVals, na.rm = TRUE), max(climVals, na.rm = TRUE), mean(climVals, na.rm = TRUE)), c("min", "max", "mean"))
      }, climateFrame = climateFrame)
      # Create a data frame to make a prediction for the creation of response curves
      responseFrame <- do.call(rbind, lapply(X = colnames(climateValueRange), FUN = function(curTerm, climateValueRange) {
        # Initialise a data frame of mean values
        initialFrame <- do.call(rbind, replicate(200, climateValueRange["mean", , drop = FALSE], simplify = FALSE))
        # Create a sequence of test values
        testValues <- seq(climateValueRange["min", curTerm], climateValueRange["max", curTerm], length.out = 200)
        initialFrame[, curTerm] <- testValues
        cbind(initialFrame, data.frame(
          testValue = testValues,
          testVariable = factor(rep(curTerm, 200), colnames(climateValueRange))
        ))
      }, climateValueRange = climateValueRange))
      # Make preditions for the response curve
      responsePred <- predict(curEl, newdata = responseFrame, type = "response")
      responseFrame <- cbind(responseFrame, data.frame(
        occProb = responsePred
      ))
      # Create a grob for the response curves
      responseGrob <- ggplot(data = responseFrame, aes(testValue, occProb)) + geom_line() + facet_grid(. ~ testVariable, scales = "free") +
        xlab("Covariate value") + ylab("Probability of occurrence") + theme_classic()
      ggsave(paste(outLocation, "/caseData_", curDate, "_responseCurves.svg", sep = ""), responseGrob, device = "svg", width = 20.8, height = 7.33)
    }
    predStack
  }, modelList = modelList, climateData = climateData, outLocation = outLocation), names(modelList))
}
# Create a list of predictions from the fitted models
covidGAMPredRaster <- makePredictions(covidGAMs, climateData, inputDataFolder)

# Save the fitted GAM model objects
saveRDS(covidGAMs, file = paste(inputDataFolder, "/fittedGAMs.rds", sep = ""))