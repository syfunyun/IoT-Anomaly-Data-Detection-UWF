# Final Experiment Spring 2024

# Sy Fontenot UWF
# jdf52@students.uwf.edu
# Code updated: 5/8/2024
# Comments updated: 9/9/2024
# Epislon implementation: 10/8/2024

library(ggplot2)
library(lattice)
library(caret)

# Read the data
source("read_data.R")

# Load functions and parameters
source("start_5.R")

# Create a data frame to store the raw dataset
df = getDataUpdated()

# Select the following sensors from the dataset and their timestamps
dfSensors = df %>% select(WHE, RSE, MHE, BME, HPE, Year, Month, MonthCount, DayCount)

# Variable list used for the experiments
varList  <- c("WHE", "RSE", "MHE", "BME", "HPE")
varCount <- length(varList)

# Confidence percentage to determine the assumed percentage of correctly labeled T2 values
confidencePercentage <- 0.90

# Choose which algorithm heuristic (A/B)
algorithm <- "B"

# Set an epsilon value as the default algorithm function value
defaultEpsilon <- 0.001

# Start and print time updates while debugging
startTime <- Sys.time()
printTimeUpdate <- function(message = FALSE) {
  if (message)
    print(message)
  print(Sys.time() - startTime)
}

# Calculate the four-month mean vectors and covariance matrices for the reference
# Note: A single reference calculation includes previous month and the three month window of the previous year (4 months)
# Note: This means that our dataset can only calculate the last 11 months of the second year, thus there are 11 references
colMeansMonthly <- double()
covarianceMatricesMonthly <- double()
for (month in 14:24) {
  monthCountRange <- c(month - 1, month - 13, month - 12, month - 11)
  colMeansMonthly <- c(colMeansMonthly, list(colMeans(dfSensors[dfSensors$MonthCount %in% monthCountRange, 1:varCount])))
  covarianceMatricesMonthly <- c(covarianceMatricesMonthly, list(cov(dfSensors[dfSensors$MonthCount %in% monthCountRange, 1:varCount])))
}

# Return the daily T2 values of the reference given the current MonthCount (only works from 14-24)
getT2ReferenceValues <- function(currentMonthCount) {
  if (!(currentMonthCount %in% 14:24))
    stop("getT2ReferenceValues : currentMonthCount not in range")
  
  # Calculate daily mean vectors of reference
  monthCountRange <- c(currentMonthCount - 1, currentMonthCount - 13, currentMonthCount - 12, currentMonthCount - 11)
  colMeansByDayCount <- aggregate(dfSensors[dfSensors$MonthCount %in% monthCountRange, 1:varCount], list(dfSensors[dfSensors$MonthCount %in% monthCountRange, "DayCount"]), mean)
  
  # Calculate the daily T2 values of the reference
  values <- double()
  currentIndex <- currentMonthCount - 13
  for (currentDay in colMeansByDayCount[[1]]) {
    colMeansDaily <- as.numeric(colMeansByDayCount[colMeansByDayCount[1] == currentDay, 2:(varCount + 1)])
    values <- c(values, t2(colMeansDaily, colMeansMonthly[[currentIndex]], covarianceMatricesMonthly[[currentIndex]]))
  }
  
  return(values)
}

# Return the daily T2 values of the individual MonthCount (only works from 14-24)
getT2MonthCountValues <- function(currentMonthCount) {
  if (!(currentMonthCount %in% 14:24))
    stop("getT2MonthCountValues : currentMonthCount not in range")
  
  # Calculate daily mean vectors of reference
  colMeansByDayCount <- aggregate(dfSensors[dfSensors$MonthCount == currentMonthCount, 1:varCount], list(dfSensors[dfSensors$MonthCount == currentMonthCount, "DayCount"]), mean)
  
  # Calculate the daily T2 values of the reference
  values <- double()
  currentIndex <- currentMonthCount - 13
  for (currentDay in colMeansByDayCount[[1]]) {
    colMeansDaily <- as.numeric(colMeansByDayCount[colMeansByDayCount[1] == currentDay, 2:(varCount + 1)])
    values <- c(values, t2(colMeansDaily, colMeansMonthly[[currentIndex]], covarianceMatricesMonthly[[currentIndex]]))
  }
  
  return(values)
}

# Return the daily means with a data shift on the current MonthCount
# Note: Since we are checking the three-month window with current MonthCount on the end, we omit the first and last reference months (giving 9 possible windows, 16-24)
# Note: The standard deviation shift is calculated based on the reference for the given MonthCount
getShiftedDailyMeans <- function(targetVar, data, currentMonthCount, range, shiftFactor) {
  if (!(currentMonthCount %in% 16:24))
    stop("getShiftedDailyMeans : currentMonthCount not in range")
  
  # Calculate anomaly data with current MonthCount data shift
  dfAnomaly <- data
  currentMonthCountRange <- c(currentMonthCount - 1, currentMonthCount - 13, currentMonthCount - 12, currentMonthCount - 11)
  referenceData <- dfSensors[dfSensors$MonthCount %in% currentMonthCountRange, targetVar]
  shift <- sd(referenceData, na.rm = FALSE) * shiftFactor
  dfAnomaly[dfAnomaly$MonthCount == currentMonthCount, targetVar] %+=% shift
  
  # Daily means data with shifted MonthCount data
  dailyMeans <- double()
  for (currentDay in range)
    dailyMeans <- c(dailyMeans, mean(dfAnomaly[dfAnomaly$DayCount == currentDay, targetVar]))
  
  return(dailyMeans)
}

# Return the daily T2 values of the three-month window with the current MonthCount shifted
# Note: Since we are checking the three-month window with current MonthCount on the end, we omit the first and last reference months (giving 9 possible windows, 16-24)
# Note: The standard deviation shift is calculated based on the reference for the given MonthCount
getT2AnomalyChart <- function(targetVar, data, currentMonthCount, range, shiftFactor) {
  if (!(currentMonthCount %in% 16:24))
    stop("getT2AnomalyChart : currentMonthCount not in range")
  
  # Calculate anomaly data with current MonthCount data shift
  dfAnomaly <- data
  currentMonthCountRange <- c(currentMonthCount - 1, currentMonthCount - 13, currentMonthCount - 12, currentMonthCount - 11)
  referenceData <- dfSensors[dfSensors$MonthCount %in% currentMonthCountRange, targetVar]
  shift <- sd(referenceData, na.rm = FALSE) * shiftFactor
  dfAnomaly[dfAnomaly$MonthCount == currentMonthCount, targetVar] %+=% shift
  
  # Calculate the daily T2 values for the anomaly
  T2ChartAnomaly <- double()
  for (currentDay in range) {
    currentIndex <- data[data$DayCount == currentDay, "MonthCount"][1] - 13
    colMeansDailyAnomaly <- colMeans(dfAnomaly[dfAnomaly$DayCount == currentDay, 1:varCount])
    T2ChartAnomaly <- c(T2ChartAnomaly, t2(colMeansDailyAnomaly, colMeansMonthly[[currentIndex]], covarianceMatricesMonthly[[currentIndex]]))
  }
  
  return(T2ChartAnomaly)
}

# Return the daily T2 values of the three-month window with data shift over the full window
# Note: Since we are checking the three-month window with current MonthCount on the end, we omit the first and last reference months (giving 9 possible windows, 16-24)
# Note: The standard deviation shift is calculated based on the reference for the given MonthCount
getFullT2ChartAnomaly <- function(targetVar, data, currentMonthCount, range, shiftFactor) {
  if (!(currentMonthCount %in% 16:24))
    stop("getFullT2ChartAnomaly : currentMonthCount not in range")
  
  # Get the index for the reference data
  currentIndex <- currentMonthCount - 13
  
  # Calculate anomaly data with full-window data shift
  dfAnomaly <- data
  currentMonthCountRange <- c(currentMonthCount - 1, currentMonthCount - 13, currentMonthCount - 12, currentMonthCount - 11)
  referenceData <- dfSensors[dfSensors$MonthCount %in% currentMonthCountRange, targetVar]
  shift <- sd(referenceData, na.rm = FALSE) * shiftFactor
  dfAnomaly[, targetVar] %+=% shift
  
  # Calculate the daily T2 chart for the anomaly
  T2ChartAnomaly <- double()
  for (currentDay in range) {
    colMeansDailyAnomaly <- colMeans(dfAnomaly[dfAnomaly$DayCount == currentDay, 1:varCount])
    T2ChartAnomaly <- c(T2ChartAnomaly, t2(colMeansDailyAnomaly, colMeansMonthly[[currentIndex]], covarianceMatricesMonthly[[currentIndex]]))
  }
  
  return(T2ChartAnomaly)
}

# Calculate the 11 UCLs for the 11 references given the confidence percentage
# Note: This starts with MonthCount 14 (May)
ucls <- double()
for (monthCount in 14:24)
  ucls <- c(ucls, as.numeric(quantile(getT2ReferenceValues(monthCount), probs = confidencePercentage)))

# Return the accuracy of a T2 chart (the percentage of correctly labeled data)
# Note: UCLs by day is taken rather than the previous current UCL in order to compare data relative to which month they are in, not just the middle month
getAccuracy <- function(targetVar, data, monthCount, range, chart, uclByDay, shiftFactor) {
  
  # Calculate the T2 anomaly chart
  anomalyChart <- getT2AnomalyChart(targetVar, data, monthCount, range, shiftFactor)
  
  # Combine anomaly chart and UCLs by day in order to call the sapply function on the paired data
  if (length(anomalyChart) != length(uclByDay))
    stop("getAccuracy : lists not equal size")
  pairedChart <- double()
  for (pair in 1:length(anomalyChart))
    pairedChart <- c(pairedChart, list(c(anomalyChart[pair], uclByDay[pair])))
  
  # Boolean list where anomalies are predicted in relation to a given UCL
  perdicted <- sapply(pairedChart, function(x) x[1] > x[2])
  
  # Return the accuracy from the confusion matrix
  return(results(chart, anomalyChart, perdicted)$overall[1])
}


# This algorithm is a take on the Bisection Method

# The assumption is that there are two measurable states 'above' and 'below' and unknown point.
#   Since we prefer to overestimate rather than under estimate, we will attempt to minimize the value in the 'above' state (minimize upper bound).
#   There are two algorithms, each having a different condition to determine states and state changes:
#     A: (Experimental) The current monthly shift is applied to the corresponding month, running the given experiment (the 3-month window)
#        and comparing the accuracy with the given confidence percentage.
#     B: (Theoretical) The current monthly shift is applied to the given portion of data (the 3-month window), and the quantile of the
#        inverse confidence percentage (1 - p) is used when comparing to the given UCL.
#       Note: The window over which the data is taken does not take into account the different UCLs that each data point corresponds to,
#             just the one given as a parameter.
# The algorithm operates in two phases: (1) finding the initial upper bound and (2) the bisection.
#   The initial upper bound phase simply finds an upper bound.
#     We start with 0.0 as our lower bound.
#     Starting with 1.0, the current value doubles until the current state is the 'above' state.
#     The lower bound is set as the current value before it is doubled.
#     Once an 'above' state is found, then the bisection begins.
#   The bisection phase is given a maximum amount of overshoots.
#     When in the 'above' state, the current value becomes the upper bound, and the next value becomes the midway point to the lower bound.
#     Similarly, when in the 'below' state, the current value becomes the lower bound, and the next value becomes the midway point to the upper bound.
#     Once the upper and lower bound are separated by less than some epsilon value, then the function returns the current upper bound

# Algorithm A
# Def: LOW SHIFT is the lowest calculated positive shift where the resulting T2 experiment chart returns an accuracy at least the confidence percentage.
findShiftFactorA <- function(targetVar, data, monthCount, range, chart, uclByDay, epsilon = defaultEpsilon) {
  
  # Parameter
  currentFactor <- 1.0
  
  # Bounds
  factorMax <- NaN
  factorMin <- 0.0
  
  # Find a starting upper bound to begin the bisection algorithm with
  while (is.nan(factorMax)) {
    # Calculate the accuracy of the T2 chart after the shifted data
    acc <- getAccuracy(targetVar, data, monthCount, range, chart, uclByDay, currentFactor)
    if (acc < confidencePercentage)
      currentFactor <- currentFactor * 2
    else
      factorMax <- currentFactor
  }
  
  # Find the lowest upper bound with a resolution matching the maximum overshoot count given
  while (factorMax - factorMin > epsilon) {
    # Calculate the accuracy of the T2 chart after the shifted data
    acc <- getAccuracy(targetVar, data, monthCount, range, chart, uclByDay, currentFactor)
    
    # Bisection
    if (acc < confidencePercentage) {
      factorMin <- currentFactor
      currentFactor <- mean(c(currentFactor, factorMax))
    }
    else {
      factorMax <- currentFactor
      currentFactor <- mean(c(factorMin, currentFactor))
    }
    
    print(paste(factorMin, factorMax)) # DELETE ME
  }
  
  return(factorMax)  
}

# Algorithm B
# Def: LOW SHIFT is the lowest calculated positive shift where the resulting T2 values over the given window are above the UCL appropriate to the confidence percentage.
# Note: The window over which the data is taken does not take into account the different UCLs that each data point corresponds to, just the one given as a parameter.
findShiftFactorB <- function(targetVar, data, monthCount, range, ucl, epsilon = defaultEpsilon) {
  
  # Parameter
  currentFactor <- 1.0
  
  # Bounds
  factorMax <- NaN
  factorMin <- 0.0
  
  # Find a starting upper bound to begin the bisection algorithm with
  while (is.nan(factorMax)) {
    # Calculate the quantile for full daily anomaly T2 values in accordance with the confidence percentage
    # Note: The percentile is inverted since T2 anomalies should reside above the line rather than below
    quant <- quantile(getFullT2ChartAnomaly(targetVar, data, monthCount, range, currentFactor), probs = 1 - confidencePercentage)
    if (quant < ucl)
      currentFactor <- currentFactor * 2
    else
      factorMax <- currentFactor
  }
  
  # Find the lowest upper bound with a resolution matching the maximum overshoot count given
  while (factorMax - factorMin > epsilon) {
    # Calculate the quantile for full daily anomaly T2 values in accordance with the confidence percentage
    # Note: The percentile is inverted since T2 anomalies should reside above the line rather than below
    quant <- quantile(getFullT2ChartAnomaly(targetVar, data, monthCount, range, currentFactor), probs = 1 - confidencePercentage)
    
    # Bisection
    if (quant < ucl) {
      factorMin <- currentFactor
      currentFactor <- mean(c(currentFactor, factorMax))
    }
    else {
      factorMax <- currentFactor
      currentFactor <- mean(c(factorMin, currentFactor))
    }
    
    print(paste(factorMin, factorMax)) # DELETE ME
  }
  
  return(factorMax)  
}


# List of lists of low shifts of T2 charts for each variable
allLowShifts <- double()

# List of lists of T2 charts for each variable
allT2Charts <- double()
allT2LowCharts <- double()
allT2MediumCharts <- double()
allT2HighCharts <- double()

# List of lists of accuracies of T2 charts for each variable
allLowAccuracies <- double()
allMediumAccuracies <- double()
allHighAccuracies <- double()

# List of lists of daily means (shifted and unshifted) for each variable
allDailyMeansLists  <- double()
allLowShiftLists    <- double()
allMediumShiftLists <- double()
allHighShiftLists   <- double()

# Calculate the results of the experiments (low shifts, charts, accuracies, and shifted data)
for (var in 1:varCount) {
  
  # Get the name of the variable being used
  focusVar <- varList[var]
  
  # Store the low shift results of all end months for the current variable
  lowShifts <- double()
  
  # Store the T2 charts for the current variable
  T2Charts <- double()
  T2LowCharts    <- double()
  T2MediumCharts <- double()
  T2HighCharts   <- double()
  
  # Store the accuracies of T2 charts for the current variable
  lowAccuracies    <- double()
  mediumAccuracies <- double()
  highAccuracies   <- double()
  
  # Store the daily means (shifted and unshifted) for the current variable
  dailyMeansList  <- double()
  lowShiftList    <- double()
  mediumShiftList <- double()
  highShiftList   <- double()
  
  # Store the day range over which the current T2 chart is calculated
  dayRanges <- double()
  
  # Iterate through the 9 end reference MonthCounts (16-24)
  for (monthCount in 16:24) {
    
    # Get the three-month window around the end month
    monthCountWindow  <- c(monthCount - 2, monthCount - 1, monthCount)
    
    # Store the data associated with the three-month window
    dfWindow <- dfSensors[dfSensors$MonthCount %in% monthCountWindow, ]
    dfWindowTest <- dfSensors[dfSensors$MonthCount == monthCount, ]
    
    # Get the current window's day range
    dayRange <- min(dfWindow[dfWindow$MonthCount == monthCountWindow[1], "DayCount"]):max(dfWindow[dfWindow$MonthCount == monthCountWindow[3], "DayCount"])
    dayRanges <- c(dayRanges, list(dayRange))
    dayRangeTest <- min(dfWindowTest[, "DayCount"]):max(dfWindowTest[, "DayCount"])
    
    # Store UCL by day in parallel with dayRange
    uclByDay <- double()
    for (day in dayRange)
      uclByDay <- c(uclByDay, ucls[dfWindow[dfWindow$DayCount == day, "MonthCount"][1] - 13])
    
    # Calculate the T2 chart of the true reference data
    # Note: The shift factor is 0.00, resulting in no shift and thus no anomaly despite the function's name
    T2Chart <- getT2AnomalyChart(focusVar, dfWindow, monthCount, dayRange, 0.00)
    
    # Use the UCL of the reference corresponding to the current MonthCount
    # Note: Since there are 11 UCLs for the 11 references and we are using the 9 end MonthCounts, then we want MonthCount 16 -> UCL index 3 and 24 -> 11 (subtract 13)
    currentUCL <- ucls[monthCount - 13]
    
    # Calculate the shifts in accordance with the confidence percentage
    if (algorithm == "A")
      shiftLow <- findShiftFactorA(focusVar, dfWindow, monthCount, dayRange, T2Chart, uclByDay)
    else if (algorithm == "B")
      shiftLow <- findShiftFactorB(focusVar, dfWindow, monthCount, dayRange, currentUCL)
    #shiftLow <- findShiftFactorB(focusVar, dfWindowTest, monthCount, dayRangeTest, currentUCL)
    else
      stop("shiftLow : please choose a correct algorithm (A/B)")
    
    shiftMedium <- 1.5 * shiftLow
    shiftHigh   <- 2.0 * shiftLow
    
    # Store the calculated low shift
    # Note: The medium and high shifts are scalars of the low shifts
    lowShifts <- c(lowShifts, shiftLow)
    
    # Store the calculated T2 charts
    T2Charts <- c(T2Charts, list(T2Chart))
    T2LowCharts    <- c(T2LowCharts,    list(getT2AnomalyChart(focusVar, dfWindow, monthCount, dayRange, shiftLow)))
    T2MediumCharts <- c(T2MediumCharts, list(getT2AnomalyChart(focusVar, dfWindow, monthCount, dayRange, shiftMedium)))
    T2HighCharts   <- c(T2HighCharts,   list(getT2AnomalyChart(focusVar, dfWindow, monthCount, dayRange, shiftHigh)))
    
    # Store the calculated accuracies
    lowAccuracies    <- c(lowAccuracies,    getAccuracy(focusVar, dfWindow, monthCount, dayRange, T2Chart, uclByDay, shiftLow))
    mediumAccuracies <- c(mediumAccuracies, getAccuracy(focusVar, dfWindow, monthCount, dayRange, T2Chart, uclByDay, shiftMedium))
    highAccuracies   <- c(highAccuracies,   getAccuracy(focusVar, dfWindow, monthCount, dayRange, T2Chart, uclByDay, shiftHigh))
    
    # Store the calculated daily means
    dailyMeansList  <- c(dailyMeansList,  list(getShiftedDailyMeans(focusVar, dfWindow, monthCount, dayRange, 0.00)))
    lowShiftList    <- c(lowShiftList,    list(getShiftedDailyMeans(focusVar, dfWindow, monthCount, dayRange, shiftLow)))
    mediumShiftList <- c(mediumShiftList, list(getShiftedDailyMeans(focusVar, dfWindow, monthCount, dayRange, shiftMedium)))
    highShiftList   <- c(highShiftList,   list(getShiftedDailyMeans(focusVar, dfWindow, monthCount, dayRange, shiftHigh)))
    
    # Print update to the console
    print(paste(focusVar, "Month:", monthCount))
  }
  
  # Store the calculated list of low shifts
  # Note: The medium and high shifts are scalars of the low shifts
  allLowShifts <- c(allLowShifts, list(lowShifts))
  
  # Store the calculated list of T2 charts
  allT2Charts <- c(allT2Charts, list(T2Charts))
  allT2LowCharts    <- c(allT2LowCharts, list(T2LowCharts))
  allT2MediumCharts <- c(allT2MediumCharts, list(T2MediumCharts))
  allT2HighCharts   <- c(allT2HighCharts, list(T2HighCharts))
  
  # Store the calculated list of accuracies
  allLowAccuracies    <- c(allLowAccuracies,    list(lowAccuracies))
  allMediumAccuracies <- c(allMediumAccuracies, list(mediumAccuracies))
  allHighAccuracies   <- c(allHighAccuracies,   list(highAccuracies))
  
  # Store the calculated list of daily means
  allDailyMeansLists  <- c(allDailyMeansLists,  list(dailyMeansList))
  allLowShiftLists    <- c(allLowShiftLists,    list(lowShiftList))
  allMediumShiftLists <- c(allMediumShiftLists, list(mediumShiftList))
  allHighShiftLists   <- c(allHighShiftLists,   list(highShiftList))
}

# Print results of experiment
for (var in 1:varCount) {
  
  # Get the name of the variable being used
  focusVar <- varList[var]
  
  # Display monthly shift and accuracy results
  # Iterate through the 9 end reference MonthCounts (16-24)
  for (monthCount in 16:24) {
    print(paste(focusVar, "Month:", monthCount))
    print(paste("shiftLow:   ", allLowShifts[[var]][[monthCount - 15]]))
    print(paste("shiftMedium:", allLowShifts[[var]][[monthCount - 15]] * 1.5))
    print(paste("shiftHigh:  ", allLowShifts[[var]][[monthCount - 15]] * 2.0))
    print(paste("Low Accuracy:   ", allLowAccuracies[[var]][[monthCount - 15]]))
    print(paste("Medium Accuracy:", allMediumAccuracies[[var]][[monthCount - 15]]))
    print(paste("High Accuracy:  ", allHighAccuracies[[var]][[monthCount - 15]]))
  }
}

# Print accuracy meta data
for (var in 1:varCount) {
  
  # Get the name of the variable being used
  focusVar <- varList[var]
  
  # Display aggregate accuracy results
  print(paste("Variable:", focusVar))
  print(paste("Low accuracy mean:   ", mean(allLowAccuracies[[var]])))
  print(paste("Medium accuracy mean:", mean(allMediumAccuracies[[var]])))
  print(paste("High accuracy mean:  ", mean(allHighAccuracies[[var]])))
  print(paste("Low accuracy sd:     ", sd(allLowAccuracies[[var]])))
  print(paste("Medium accuracy sd:  ", sd(allMediumAccuracies[[var]])))
  print(paste("High accuracy sd:    ", sd(allHighAccuracies[[var]])))
}

# Print low shift meta data
for (var in 1:varCount) {
  
  # Get the name of the variable being used
  focusVar <- varList[var]
  
  # Display results
  print(paste("Variable:", focusVar))
  print(paste("Low shift mean:", mean(allLowShifts[[var]])))
  print(paste("Low shift sd:  ", sd(allLowShifts[[var]])))
}

# Output validation: is the target month coherent with its reference data?
# Iterate through the 9 end reference MonthCounts (16-24)
for (monthCount in 16:24) {
  ucl <- ucls[monthCount - 14]
  correct <- sapply(getT2MonthCountValues(monthCount), function(x) x < ucl)
  accuracy <- sum(correct) / length(correct)
  print(paste("Month:", monthCount, "| Validation Accuracy:", accuracy))
}

## Side-by-side charts (T2 values & Daily means)
for (var in 1:varCount) {
  
  # Get the name of the variable being used
  focusVar <- varList[var]
  
  # Get charts
  charts <- allT2Charts[[var]]
  lowCharts    <- allT2LowCharts[[var]]
  mediumCharts <- allT2MediumCharts[[var]]
  highCharts   <- allT2HighCharts[[var]]
  
  # Get daily means
  means <- allDailyMeansLists[[var]]
  lowMeans    <- allLowShiftLists[[var]]
  mediumMeans <- allMediumShiftLists[[var]]
  highMeans   <- allHighShiftLists[[var]]
  
  # Grid for plots
  par(mfrow = c(1, 2))
  
  # Plot the daily T2 values for each experiment
  for (chartCount in 1:9) {
    
    # Save figures
    setEPS()
    postscript(paste("EPSChart/", focusVar, "Chart", chartCount + 15, ".eps", sep = ""))
    
    # T2 CHART
    
    # Plot setup
    days <- dayRanges[[chartCount]]
    labelScale <- 1.0 # 2.0 for poster
    lineWidth  <- 1.0 # 3.0 for poster
    plot(charts[[chartCount]], xaxt = "n", main = paste(focusVar, "T2 over Months", chartCount + 13, "to", chartCount + 15),
         xlab = "Day", ylab = "T2 Value", type = "n", ylim = c(0, max(highCharts[[chartCount]])),
         cex.lab = 1.0, cex.axis = 1.0, cex.main = labelScale)
    axis(1, at = 1:length(days), labels = days, cex.axis = 1.0)
    
    # Get UCL for each month in the experiment
    graphUCLMonth0 <- ucls[chartCount]
    graphUCLMonth1 <- ucls[chartCount + 1]
    graphUCLMonth2 <- ucls[chartCount + 2]
    
    # Display the monthly UCLs
    segments(x0 = 1, y0 = graphUCLMonth0, x1 = length(days) / 3, y1 = graphUCLMonth0, col = "deepskyblue")
    segments(x0 = length(days) / 3, y0 = graphUCLMonth1, x1 = length(days) * 2 / 3, y1 = graphUCLMonth1, col = "deepskyblue")
    segments(x0 = length(days) * 2 / 3, y0 = graphUCLMonth2, x1 = length(days), y1 = graphUCLMonth2, col = "deepskyblue")
    
    # Display the T2 values
    lines(highCharts[[chartCount]],   lwd = lineWidth, col = "darkgoldenrod1", type = "o")
    lines(mediumCharts[[chartCount]], lwd = lineWidth, col = "chocolate1", type = "o")
    lines(lowCharts[[chartCount]],    lwd = lineWidth, col = "brown1", type = "o")
    lines(charts[[chartCount]], lwd = lineWidth, col = "black", type = "o")
    
    # Legend
    legend("topleft", legend = c("Daily T2", "High T2", "Medium T2", "Low T2", "UCL"), 
           col = c("black", "darkgoldenrod1", "chocolate1", "brown1", "deepskyblue"), 
           lwd = lineWidth, lty = 1, bg = "transparent", cex = labelScale)
    
    # Close files
    dev.off()
    
    # MEANS CHART
    
    # Save figures
    setEPS()
    postscript(paste("EPSMeans/", focusVar, "Means", chartCount + 15, ".eps", sep = ""))
    
    # Plot setup
    days <- dayRanges[[chartCount]]
    labelScale <- 1.0 # 2.0 for poster
    lineWidth  <- 1.0 # 3.0 for poster
    plot(means[[chartCount]], xaxt = "n", main = paste(focusVar, "SHIFT over Months", chartCount + 13, "to", chartCount + 15),
         xlab = "Day", ylab = "Daily Mean", type = "n", ylim = c(min(means[[chartCount]]), max(highMeans[[chartCount]])),
         cex.lab = 1.0, cex.axis = 1.0, cex.main = labelScale)
    axis(1, at = 1:length(days), labels = days, cex.axis = 1.0)
    
    # Display the daily means
    lines(highMeans[[chartCount]],   lwd = lineWidth, col = "darkgoldenrod1", type = "o")
    lines(mediumMeans[[chartCount]], lwd = lineWidth, col = "chocolate1", type = "o")
    lines(lowMeans[[chartCount]],    lwd = lineWidth, col = "brown1", type = "o")
    lines(means[[chartCount]], lwd = lineWidth, col = "black", type = "o")
    
    # Legend
    legend("topleft", legend = c("Daily Means", "High Shift", "Medium Shift", "Low Shift"),
           col = c("black", "darkgoldenrod1", "chocolate1", "brown1"),
           lwd = lineWidth, lty = 1, bg = "transparent", cex = labelScale)
    
    # Close files
    dev.off()
  }
}

# Charts separated

### # Display T2 charts
### for (var in 1:varCount) {
###   
###   # Get the name of the variable being used
###   focusVar <- varList[var]
###   
###   # Get charts
###   charts <- allT2Charts[[var]]
###   lowCharts    <- allT2LowCharts[[var]]
###   mediumCharts <- allT2MediumCharts[[var]]
###   highCharts   <- allT2HighCharts[[var]]
###   
###   # Grid for plots
###   par(mfrow = c(1, 1))
###   
###   # Plot the daily T2 values for each experiment
###   for (chartCount in 1:9) {
###     
###     # Plot setup
###     days <- dayRanges[[chartCount]]
###     labelScale <- 0.7 # 2.0 for poster
###     lineWidth  <- 1.0 # 3.0 for poster
###     plot(charts[[chartCount]], xaxt = "n", main = paste(focusVar, "T2 over Months", chartCount + 13, "to", chartCount + 15),
###          xlab = "Day", ylab = "T2 Value", type = "n", ylim = c(0, max(highCharts[[chartCount]])),
###          cex.lab = 1.5, cex.axis = 1.5, cex.main = labelScale)
###     axis(1, at = 1:length(days), labels = days, cex.axis = 1.5)
###     
###     # Get UCL for each month in the experiment
###     graphUCLMonth0 <- ucls[chartCount]
###     graphUCLMonth1 <- ucls[chartCount + 1]
###     graphUCLMonth2 <- ucls[chartCount + 2]
###     
###     # Display the monthly UCLs
###     segments(x0 = 1, y0 = graphUCLMonth0, x1 = length(days) / 3, y1 = graphUCLMonth0, col = "deepskyblue")
###     segments(x0 = length(days) / 3, y0 = graphUCLMonth1, x1 = length(days) * 2 / 3, y1 = graphUCLMonth1, col = "deepskyblue")
###     segments(x0 = length(days) * 2 / 3, y0 = graphUCLMonth2, x1 = length(days), y1 = graphUCLMonth2, col = "deepskyblue")
###     
###     # Display the T2 values
###     lines(highCharts[[chartCount]],   lwd = lineWidth, col = "darkgoldenrod1", type = "o")
###     lines(mediumCharts[[chartCount]], lwd = lineWidth, col = "chocolate1", type = "o")
###     lines(lowCharts[[chartCount]],    lwd = lineWidth, col = "brown1", type = "o")
###     lines(charts[[chartCount]], lwd = lineWidth, col = "black", type = "o")
###     
###     # Legend
###     legend("topleft", legend = c("Daily T2", "High T2", "Medium T2", "Low T2", "UCL"), 
###            col = c("black", "darkgoldenrod1", "chocolate1", "brown1", "deepskyblue"), 
###            lwd = lineWidth, lty = 1, bg = "transparent", cex = labelScale)
###   }
### }
### 
### # Display the data and anomalies
### for (var in 1:varCount) {
###   
###   # Get the name of the variable being used
###   focusVar <- varList[var]
###   
###   # Get daily means
###   means <- allDailyMeansLists[[var]]
###   lowMeans    <- allLowShiftLists[[var]]
###   mediumMeans <- allMediumShiftLists[[var]]
###   highMeans   <- allHighShiftLists[[var]]
###   
###   # Grid for plots
###   par(mfrow = c(1, 1))
###   
###   # Plot the daily means for each experiment
###   for (chartCount in 1:9) {
###     
###     # Plot setup
###     days <- dayRanges[[chartCount]]
###     labelScale <- 0.7 # 2.0 for poster
###     lineWidth  <- 1.0 # 3.0 for poster
###     plot(means[[chartCount]], xaxt = "n", main = paste(focusVar, "SHIFT over Months", chartCount + 13, "to", chartCount + 15),
###          xlab = "Day", ylab = "Daily Mean", type = "n", ylim = c(min(means[[chartCount]]), max(highMeans[[chartCount]])),
###          cex.lab = 1.5, cex.axis = 1.5, cex.main = labelScale)
###     axis(1, at = 1:length(days), labels = days, cex.axis = 1.5)
###     
###     # Display the daily means
###     lines(highMeans[[chartCount]],   lwd = lineWidth, col = "darkgoldenrod1", type = "o")
###     lines(mediumMeans[[chartCount]], lwd = lineWidth, col = "chocolate1", type = "o")
###     lines(lowMeans[[chartCount]],    lwd = lineWidth, col = "brown1", type = "o")
###     lines(means[[chartCount]], lwd = lineWidth, col = "black", type = "o")
###     
###     # Legend
###     legend("topleft", legend = c("Daily Means", "High Shift", "Medium Shift", "Low Shift"),
###            col = c("black", "darkgoldenrod1", "chocolate1", "brown1"),
###            lwd = lineWidth, lty = 1, bg = "transparent", cex = labelScale)
###   }
### }
