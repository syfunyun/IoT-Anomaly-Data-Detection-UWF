# Final Experiment

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

# Start and print time updates while debugging
startTime <- Sys.time()
printTimeUpdate <- function(message = FALSE) {
  if (message)
    print(message)
  print(Sys.time() - startTime)
}

# DELETE ME??? # Calculate the daily mean vectors over the two years
# DELETE ME??? colMeansDaily <- double()
# DELETE ME??? for (d in 1:(nrow(dfSensors) / 1440))
# DELETE ME???   colMeansDaily <- c(colMeansDaily, list(colMeans(dfSensors[dfSensors$DayCount == d, 1:varCount])))

# Calculate the three-month mean vectors and covariance matrices for the reference
# Note: A single reference calculation includes the three months over both years (6 months)
# Note: The indices are stored corresponding to the Month and NOT the MonthCount! 
colMeansMonthly <- double()
covarianceMatricesMonthly <- double()
for (month in 1:12) {
  monthRange <- (c(month - 1, month, month + 1) - 1) %% 12 + 1
  colMeansMonthly <- c(colMeansMonthly, list(colMeans(dfSensors[dfSensors$Month %in% monthRange, 1:varCount])))
  covarianceMatricesMonthly <- c(covarianceMatricesMonthly, list(cov(dfSensors[dfSensors$Month %in% monthRange, 1:varCount])))
}

# Return the daily T2 values of the reference given the inner month 
getT2ReferenceChart <- function(innerMonthReference) {
  # Calculate daily mean vectors within the reference
  monthRange <- (c(innerMonthReference - 1, innerMonthReference, innerMonthReference + 1) - 1) %% 12 + 1
  colMeansByDayCount <- aggregate(dfSensors[dfSensors$Month %in% monthRange, 1:varCount], list(dfSensors[dfSensors$Month %in% monthRange, "DayCount"]), mean)
  
  # Calculate the daily T2 values from the reference
  chart <- double()
  for (currentDay in colMeansByDayCount[[1]]) {
    colMeansDaily <- as.numeric(colMeansByDayCount[colMeansByDayCount[1] == currentDay, 2:(varCount + 1)])
    chart <- c(chart, t2(colMeansDaily, colMeansMonthly[[innerMonthReference]], covarianceMatricesMonthly[[innerMonthReference]]))
  }
  
  return(chart)
}

# Return the daily means with a data shift on the inner month
getInnerShiftedDailyMeans <- function(targetVar, data, monthCount, range, shiftFactor) {
  
  # Calculate anomaly data with inner-month data shift
  dfAnomaly <- data
  shift <- sd(data[[targetVar]], na.rm = FALSE) * shiftFactor
  dfAnomaly[dfAnomaly$MonthCount == monthCount, targetVar] %+=% shift
  
  # Daily means data with shifted inner month
  dailyMeans <- double()
  for (currentDay in range)
    dailyMeans <- c(dailyMeans, mean(dfAnomaly[dfAnomaly$DayCount == currentDay, targetVar]))
  
  return(dailyMeans)
}

# Return the daily T2 values of the three-month window with the specified inner-month data shift
getT2AnomalyChart <- function(targetVar, data, monthCount, range, shiftFactor) {
  # Convert month count to corresponding month in dataset to properly access the reference means and covariance matrices
  month <- (monthCount + 2) %% 12 + 1
  
  # Calculate anomaly data with inner-month data shift
  dfAnomaly <- data
  shift <- sd(data[[targetVar]], na.rm = FALSE) * shiftFactor
  dfAnomaly[dfAnomaly$MonthCount == monthCount, targetVar] %+=% shift
  
  # Calculate the daily T2 values for the anomaly
  T2ChartAnomaly <- double()
  for (currentDay in range) {
    colMeansDailyAnomaly <- colMeans(dfAnomaly[dfAnomaly$DayCount == currentDay, 1:varCount])
    T2ChartAnomaly <- c(T2ChartAnomaly, t2(colMeansDailyAnomaly, colMeansMonthly[[month]], covarianceMatricesMonthly[[month]]))
  }
  
  return(T2ChartAnomaly)
}

# Return the daily T2 values of the three-month window with data shift over the full window
getFullT2ChartAnomaly <- function(targetVar, data, monthCount, range, shiftFactor) {
  # Convert month count to corresponding month in dataset to properly access the reference means and covariance matrices
  month <- (monthCount + 2) %% 12 + 1
  
  # Calculate anomaly data with full-window data shift
  dfAnomaly <- data
  shift <- sd(data[[targetVar]], na.rm = FALSE) * shiftFactor
  dfAnomaly[, targetVar] %+=% shift
  
  # Calculate the daily T2 chart for the anomaly
  T2ChartAnomaly <- double()
  for (currentDay in range) {
    colMeansDailyAnomaly <- colMeans(dfAnomaly[dfAnomaly$DayCount == currentDay, 1:varCount])
    T2ChartAnomaly <- c(T2ChartAnomaly, t2(colMeansDailyAnomaly, colMeansMonthly[[month]], covarianceMatricesMonthly[[month]]))
  }
  
  return(T2ChartAnomaly)
}

# Calculate the twelve UCLs for the twelve three-month references given the confidence percentage
# Note: This starts with month 4 (April)
ucls <- double()
for (month in (1:12 + 2) %% 12 + 1)
  ucls <- c(ucls, as.numeric(quantile(getT2ReferenceChart((month + 2) %% 12 + 1), probs = confidencePercentage)))

# Return the accuracy of a T2 chart (the percentage of correctly labeled data)
getAccuracy <- function(targetVar, data, monthCount, range, chart, ucl, shiftFactor) {
  # Calculate the T2 anomaly chart
  anomalyChart <- getT2AnomalyChart(targetVar, data, monthCount, range, shiftFactor)
  
  # Return the accuracy from the confusion matrix
  return(results(chart, anomalyChart, ucl)$overall[1])
}

# Algorithm to find the low shift of a T2 chart
# Def: LOW SHIFT is the lowest calculated positive shift where resulting T2 values are labeled appropriate to the confidence percentage
findShiftFactor <- function(targetVar, data, monthCount, range, ucl) {
  
  # THIS IS CALLED BISECTION METHOD!!!!!!!!
  
  # This algorithm uses somewhat of a Binary Fractal Search on the positive reals.
  # The assumption is that there are two measurable states above and below and unknown point.
  #   Since we prefer to overestimate rather than under estimate, we will attempt to minimize the above state.
  # The algorithm operates in two phases: the Initial Upper Bound, and the Binary Fractal.
  #   The Initial Upper Bound phase simply finds an upper bound as we only know a lower bound for the positive reals.
  #     Starting with 1.0 and until the current state is the above state, the current value doubles.
  #     Once an above state is found, this current state becomes our first upper bound, and the Binary Fractal phase begins.
  #   The Binary Fractal phase is given a maximum amount of overshoots.
  #     An overshoot is when the current state differs from the previous state.
  #     When in the above state, the current value becomes the upper bound, and the next value becomes the midway point to the lower bound.
  #     Similarly, when in the below state, the current value becomes the lower bound, and the next value becomes the midway point to the upper bound.
  #     Once the maximum amount of overshoots occur, then the function returns the current upper bound
  
  # Parameters
  overshoots    <- 0
  currentFactor <- 1.0
  maxOvershoots <- 10
  
  # Bounds
  factorMax <- NaN
  factorMin <- 0.0
  
  # Find the lowest upper bound with a resolution matching the maximum overshoot count given
  while (overshoots < maxOvershoots) {
    # Calculate the quantile for full daily anomaly T2 values in accordance with the confidence percentage
    # Note: The percentile is inverted since T2 anomalies should reside above the line rather than below
    quant <- quantile(getFullT2ChartAnomaly(targetVar, data, monthCount, range, currentFactor), probs = 1 - confidencePercentage)
    
    # Find starting state
    if (is.nan(factorMax) & factorMin == 0)
      previousState <- quant >= ucl
    
    # Binary Fractal Search
    if (quant >= ucl) {
      factorMax <- currentFactor
      currentFactor <- mean(c(factorMin, currentFactor))
      if (!previousState)
        overshoots %+=% 1
    }
    else {
      factorMin <- currentFactor
      if (previousState)
        overshoots %+=% 1
      
      # Initial Upper Bound phase
      if (is.nan(factorMax))
        currentFactor <- currentFactor * 2
      # Binary Fractal phase
      else
        currentFactor <- mean(c(currentFactor, factorMax))
    }
    
    # Store current state as previous state for next iteration
    previousState <- quant >= ucl
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
  
  # Store the low shift results of all inner months for the current variable
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
  
  # Iterate through the inner months of the two years in the dataset
  for (monthCount in 2:23) {
    currentMonth <- (monthCount + 2) %% 12 + 1
    
    # Get the three-month window arount the inner month
    monthCountWindow  <- c(monthCount - 1, monthCount, monthCount + 1)
    
    # Store the data associated with the three-month window
    dfWindow <- dfSensors[dfSensors$MonthCount %in% monthCountWindow, ]
    
    # Get the current window's day range
    dayRange <- min(dfWindow[dfWindow$MonthCount == monthCountWindow[1], "DayCount"]):max(dfWindow[dfWindow$MonthCount == monthCountWindow[3], "DayCount"])
    dayRanges <- c(dayRanges, list(dayRange))
    
    # Calculate the T2 chart of the true reference data
    # Note: The shift factor is 0.00, resulting in no shift and thus no anomaly despite the function's name
    T2Chart <- getT2AnomalyChart(focusVar, dfWindow, monthCount, dayRange, 0.00)
    
    # Use the previously calculated UCL of the reference window associated with the current window
    currentUCL <- ucls[(monthCount - 4) %% 12 + 1]
    
    # Calculate the shifts in accordance with the confidence percentage
    shiftLow    <- findShiftFactor(focusVar, dfWindow, monthCount, dayRange, currentUCL)
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
    lowAccuracies    <- c(lowAccuracies,    getAccuracy(focusVar, dfWindow, monthCount, dayRange, T2Chart, currentUCL, shiftLow))
    mediumAccuracies <- c(mediumAccuracies, getAccuracy(focusVar, dfWindow, monthCount, dayRange, T2Chart, currentUCL, shiftMedium))
    highAccuracies   <- c(highAccuracies,   getAccuracy(focusVar, dfWindow, monthCount, dayRange, T2Chart, currentUCL, shiftHigh))
    
    # Store the calculated daily means
    dailyMeansList  <- c(dailyMeansList,  list(getInnerShiftedDailyMeans(focusVar, dfWindow, monthCount, dayRange, 0.00)))
    lowShiftList    <- c(lowShiftList,    list(getInnerShiftedDailyMeans(focusVar, dfWindow, monthCount, dayRange, shiftLow)))
    mediumShiftList <- c(mediumShiftList, list(getInnerShiftedDailyMeans(focusVar, dfWindow, monthCount, dayRange, shiftMedium)))
    highShiftList   <- c(highShiftList,   list(getInnerShiftedDailyMeans(focusVar, dfWindow, monthCount, dayRange, shiftHigh)))
    
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
  
  # Iterate through the inner months of the two years in the dataset
  for (monthCount in 2:23) {
    print(paste(focusVar, "Month:", monthCount))
    print(paste("shiftLow:   ", allLowShifts[[var]][[monthCount - 1]]))
    print(paste("shiftMedium:", allLowShifts[[var]][[monthCount - 1]] * 1.5))
    print(paste("shiftHigh:  ", allLowShifts[[var]][[monthCount - 1]] * 2.0))
    print(paste("Low Accuracy:   ", allLowAccuracies[[var]][[monthCount - 1]]))
    print(paste("Medium Accuracy:", allMediumAccuracies[[var]][[monthCount - 1]]))
    print(paste("High Accuracy:  ", allHighAccuracies[[var]][[monthCount - 1]]))
  }
}

# Print accuracy meta data
for (var in 1:varCount) {
  # Get the name of the variable being used
  focusVar <- varList[var]

  print(paste("Variable:", focusVar))
  print(paste("Low accuracy mean:   ", mean(allLowAccuracies[[var]])))
  print(paste("Medium accuracy mean:", mean(allMediumAccuracies[[var]])))
  print(paste("High accuracy mean:  ", mean(allHighAccuracies[[var]])))
  print(paste("Low accuracy standard deviation:   ", sd(allLowAccuracies[[var]])))
  print(paste("Medium accuracy standard deviation:", sd(allMediumAccuracies[[var]])))
  print(paste("High accuracy standard deviation:  ", sd(allHighAccuracies[[var]])))
}

# Display T2 charts ############################################################# COMMENT!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! #####################################################################
for (var in 1:varCount) {
  # Get the name of the variable being used
  focusVar <- varList[var]
  
  charts <- allT2Charts[[var]]
  lowCharts    <- allT2LowCharts[[var]]
  mediumCharts <- allT2MediumCharts[[var]]
  highCharts   <- allT2HighCharts[[var]]
  
  # Grid for plots
  par(mfrow = c(1, 1))
  
  for (chartCount in 1:22) {
    graphUCL <- ucls[(chartCount - 3) %% 12 + 1]
    # Plot the T2 statistics of the first year
    days <- dayRanges[[chartCount]]
    labelScale <- 0.7 # 2.0 for poster
    lineWidth  <- 1.0 # 3.0 for poster
    plot(charts[[chartCount]], xaxt = "n", main = paste(focusVar, "T2 over Months", chartCount, "to", chartCount + 2), xlab = "Day", ylab = "T2 Value", type = "n", ylim = c(0, max(highCharts[[chartCount]])), cex.lab = 1.5, cex.axis = 1.5, cex.main = labelScale)
    axis(1, at = 1:length(days), labels = days, cex.axis = 1.5)
    abline(h = graphUCL, lwd = lineWidth, col = "deepskyblue")
    lines(highCharts[[chartCount]],   lwd = lineWidth, col = "darkgoldenrod1", type = "o")
    lines(mediumCharts[[chartCount]], lwd = lineWidth, col = "chocolate1", type = "o")
    lines(lowCharts[[chartCount]],    lwd = lineWidth, col = "brown1", type = "o")
    lines(charts[[chartCount]], lwd = lineWidth, col = "black", type = "o")
    legend("topleft", legend = c("Daily T2", "High T2", "Medium T2", "Low T2", "UCL"), col = c("black", "darkgoldenrod1", "chocolate1", "brown1", "deepskyblue"), lwd = lineWidth, lty = 1, bg = "transparent", cex = labelScale)
  }
}

# Display the anomalies
for (var in 1:varCount) {
  # Get the name of the variable being used
  focusVar <- varList[var]
  
  means <- allDailyMeansLists[[var]]
  lowMeans    <- allLowShiftLists[[var]]
  mediumMeans <- allMediumShiftLists[[var]]
  highMeans   <- allHighShiftLists[[var]]
  
  # Grid for plots
  par(mfrow = c(1, 1))
  
  for (chartCount in 1:22) {
    # Plot the T2 statistics of the first year
    days <- dayRanges[[chartCount]]
    labelScale <- 0.7 # 2.0 for poster
    lineWidth  <- 1.0 # 3.0 for poster
    plot(means[[chartCount]], xaxt = "n", main = paste(focusVar, "SHIFT over Months", chartCount, "to", chartCount + 2), xlab = "Day", ylab = "Daily Mean", type = "n", ylim = c(min(means[[chartCount]]), max(highMeans[[chartCount]])), cex.lab = 1.5, cex.axis = 1.5, cex.main = labelScale)
    axis(1, at = 1:length(days), labels = days, cex.axis = 1.5)
    lines(highMeans[[chartCount]],   lwd = lineWidth, col = "darkgoldenrod1", type = "o")
    lines(mediumMeans[[chartCount]], lwd = lineWidth, col = "chocolate1", type = "o")
    lines(lowMeans[[chartCount]],    lwd = lineWidth, col = "brown1", type = "o")
    lines(means[[chartCount]], lwd = lineWidth, col = "black", type = "o")
    legend("bottomleft", legend = c("Daily Means", "High Shift", "Medium Shift", "Low Shift"), col = c("black", "darkgoldenrod1", "chocolate1", "brown1"), lwd = lineWidth, lty = 1, bg = "transparent", cex = labelScale)
  }
}
