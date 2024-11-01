# Accuracy - SD Plot

# TODO: Comment new code

library(ggplot2)
library(lattice)
library(caret)

# Read the data
source("read_data.R")

# Load functions and parameters
source("start_5.R")

# Create a data frame to store the raw data set
df = getData()

# Select the following sensors from the data set along with each datum's Month and Year
df_sensors = df %>% select(WHE, RSE, MHE, BME, HPE, Year, Month, Week)

# Organize the list of variables being used
var_list    <- c("WHE", "RSE", "MHE", "BME", "HPE")
var_count   <- length(var_list)

# CHANGE ANOMALY TYPE: "SHIFT" OR "BIAS"
anomaly_type <- "SHIFT"

all_var_matrices <- list()
all_max_factors <- double()

# Start and print time updates
start_time <- Sys.time()
printTimeUpdate <- function(message = FALSE) {
  if (message)
    print(message)
  print(Sys.time() - start_time)
}

# List of covariance matrices for each month over two years
covariance_matrix_month_1st <- double() # First year
covariance_matrix_month_2nd <- double() # Second year
for (m in 1:12) {
  covariance_matrix_month_1st <- c(covariance_matrix_month_1st, list(cov(df_sensors[df_sensors$Month == m & df_sensors$Week %/% 100 == 1, 1:var_count])))
  covariance_matrix_month_2nd <- c(covariance_matrix_month_2nd, list(cov(df_sensors[df_sensors$Month == m & df_sensors$Week %/% 100 == 2, 1:var_count])))
}

# List of column means for each month over two years
colMeans_month_1st <- double() # First year
colMeans_month_2nd <- double() # Second year
for (m in 1:12) {
  colMeans_month_1st <- c(colMeans_month_1st, list(colMeans(df_sensors[df_sensors$Month == m & df_sensors$Week %/% 100 == 1, 1:var_count])))
  colMeans_month_2nd <- c(colMeans_month_2nd, list(colMeans(df_sensors[df_sensors$Month == m & df_sensors$Week %/% 100 == 2, 1:var_count])))
}

# List of column means for each week over two years
# Note: this does not actually optimize the code as a whole since each list is only used once
colMeans_week_1st <- double() # First year
colMeans_week_2nd <- double() # Second year
for (w in 1:52) {
  colMeans_week_1st <- c(colMeans_week_1st, list(colMeans(df_sensors[df_sensors$Week == (w + 100), 1:var_count])))
  colMeans_week_2nd <- c(colMeans_week_2nd, list(colMeans(df_sensors[df_sensors$Week == (w + 200), 1:var_count])))
}

for (cur_var in 1:5) {
  
  all_var_matrices[[cur_var]] <- list()
  
  # CHANGE SENSOR INDEX: WHE:1, RSE:2, MHE:3, BME:4, HPE:5
  anomaly_var <- cur_var
  focus_var   <- var_list[anomaly_var]
  
  # # Randomize
  # df_sensors[anomaly_var] <- rnorm(n = dim(df_sensors[anomaly_var])[1], mean = 0, sd = 1)
  
  # for (i in 1:var_count) {
  #   # # Randomize each variable
  #   # df_sensors[i] <- rnorm(n = dim(df_sensors[i])[1], mean = 0, sd = 1)
  #   
  #   # Normalize each variable
  #   df_sensors[i] <- (df_sensors[i]] - colMeans(df_sensors[i])) / apply(df_sensors[i], 2, sd)
  # }
  
  # Parameters
  step_count <- 100
  
  success_count  <- 0
  failure_count  <- 0
  current_factor <- 1 # Starting factor
  failure_max    <- 10
  
  factor_max <- NaN
  factor_min <- NaN
  
  # Find starting factor
  while (TRUE) {
    
    # Print updates to console
    print(paste(focus_var, "Current:", current_factor, "Min:", factor_min, "Max:", factor_max, "Failures:", failure_count, "Successes:", success_count))
    
    # === From MAIN CODE === #
    
    # Store anomaly data
    df_weekly_anomaly <- df_sensors
    
    # Calculate a given factor of the standard deviation
    sd_factor <- sd(df_sensors[[focus_var]], na.rm = FALSE) * current_factor
    quant <- quantile(df_sensors[[focus_var]], probs = c(.25, .75), na.rm = FALSE)
    lower <- quant[1] - sd_factor # Lower Range
    upper <- quant[2] + sd_factor # Upper Range
    bias  <- quantile(df_sensors[[focus_var]], probs = c(.5), na.rm = FALSE) + sd_factor
    
    # Insert anomaly into the later half of the year
    # Note: the year in the data set starts with April (month 4), thus 10-12 and 1-3 are later
    if (anomaly_type == "SHIFT")
      df_weekly_anomaly[df_weekly_anomaly$Month %in% c(1:3, 10:12), focus_var] %+=% sd_factor
    if (anomaly_type == "BIAS")
      df_weekly_anomaly[df_weekly_anomaly$Month %in% c(1:3, 10:12), focus_var] <- bias # upper
    
    # Store the weekly T2 statistics for each year
    T2_1st <- double() # First year
    T2_2nd <- double() # Second year
    
    # Store the weekly T2 statistics for each year with the anomaly
    T2_1st_anomaly <- double() # First year
    T2_2nd_anomaly <- double() # Second year
    
    # Loop through the weeks in a year
    for (week in 1:52) {
      # Store a window of values for the current week of each year with the anomaly
      week_data_1st_anomaly <- df_weekly_anomaly[df_weekly_anomaly$Week == (week + 100), ] # First year
      week_data_2nd_anomaly <- df_weekly_anomaly[df_weekly_anomaly$Week == (week + 200), ] # Second year
      
      # Find the month of the beginning of each week for each year
      month_1st <- week_data_1st_anomaly[1, "Month"] # First year
      month_2nd <- week_data_2nd_anomaly[1, "Month"] # Second year
      
      # Calculate the current week's T2 statistics compared to the month it starts in for each year
      T2_1st <- c(T2_1st, t2(colMeans_week_1st[[week]],
                             colMeans_month_2nd[[month_1st]],
                             covariance_matrix_month_2nd[[month_1st]]))
      T2_2nd <- c(T2_2nd, t2(colMeans_week_2nd[[week]],
                             colMeans_month_1st[[month_2nd]],
                             covariance_matrix_month_1st[[month_2nd]]))
      
      # Calculate the current week's T2 statistics compared to the month it starts in for each year with the anomaly
      T2_1st_anomaly <- c(T2_1st_anomaly, t2(colMeans(week_data_1st_anomaly[1:var_count]),
                                             colMeans_month_2nd[[month_1st]],
                                             covariance_matrix_month_2nd[[month_1st]]))
      T2_2nd_anomaly <- c(T2_2nd_anomaly, t2(colMeans(week_data_2nd_anomaly[1:var_count]),
                                             colMeans_month_1st[[month_2nd]],
                                             covariance_matrix_month_1st[[month_2nd]]))
    }
    
    # Calculate the weekly means of the target variable
    mean   <- aggregate(df_sensors[anomaly_var], list(df_sensors$Week), mean)
    mean_anomaly <- aggregate(df_weekly_anomaly[, anomaly_var], list(df_weekly_anomaly$Week), mean)
    
    # Calculate Upper Control Limit (UCL)
    UCL <- quantile(c(T2_1st, T2_2nd), probs = 1.00)
    
    # Calculate Confusion Matrix of the results for each year
    confusion_matrix <- results(c(T2_1st, T2_2nd), c(T2_1st_anomaly, T2_2nd_anomaly), UCL) # Total
    
    # === ==== ==== ==== === #
    
    # Binary searching algorithm
    # [INSERT EXPLANATION HERE]
    if (failure_count == 0) {
      if (confusion_matrix$overall[1] == 1) {
        factor_max <- current_factor
        if (is.nan(factor_min))
          current_factor <- current_factor / 2
        else
          current_factor <- mean(c(factor_min, current_factor))
        success_count %+=% 1
      }
      else if (success_count == 0) {
        factor_min <- current_factor
        current_factor <- current_factor * 2
      }
      else {
        factor_min <- current_factor
        current_factor <- mean(c(current_factor, factor_max))
        failure_count <- 1
      }
    }
    else if (failure_count < failure_max) {
      if (confusion_matrix$overall[1] == 1) {
        factor_max <- current_factor
        current_factor <- mean(c(factor_min, current_factor))
        success_count %+=% 1
      }
      else {
        factor_min <- current_factor
        current_factor <- mean(c(current_factor, factor_max))
        failure_count %+=% 1
      }
    }
    if (failure_count == failure_max) {
      max_factor <- factor_max
      all_max_factors <- c(all_max_factors, max_factor)
      print(paste(focus_var, "Factor:", max_factor, "Min:", factor_min, "Max:", factor_max, "Failures:", failure_count, "Successes:", success_count))
      break
    }
  }
  
  printTimeUpdate()
  
  # List of UCL quantiles to simulate
  ucls <- c(1.00, 0.98, 0.95, 0.90)
  
  # Store the confusion matrices of each SD-dependent model
  #accuracies <- list()
  confusion_matrices <- list()
  
  for (n in 1:length(ucls))
    confusion_matrices[[n]] <- list()
    #accuracies[[n]] <- double()
  
  # Iterate to max factor by a given amount of steps
  for (n in c(-(floor((step_count * 1.1) + 0.5):1), 1:floor((step_count * 1.1) + 0.5))) {
    # CHANCE SD ANOMALY FACTOR
    anomaly_factor <- (all_max_factors[[cur_var]] / step_count) * n
    
    # === From MAIN CODE === #
    
    # Store anomaly data
    df_weekly_anomaly <- df_sensors
    
    # Calculate a given factor of the standard deviation
    sd_factor <- sd(df_sensors[[focus_var]], na.rm = FALSE) * anomaly_factor
    quant <- quantile(df_sensors[[focus_var]], probs = c(.25, .75), na.rm = FALSE)
    lower <- quant[1] - sd_factor # Lower Range
    upper <- quant[2] + sd_factor # Upper Range
    bias  <- quantile(df_sensors[[focus_var]], probs = c(.5), na.rm = FALSE) + sd_factor
    
    # Insert anomaly into the later half of the year
    # Note: the year in the data set starts with April (month 4), thus 10-12 and 1-3 are later
    if (anomaly_type == "SHIFT")
      df_weekly_anomaly[df_weekly_anomaly$Month %in% c(1:3, 10:12), focus_var] %+=% sd_factor
    if (anomaly_type == "BIAS")
      df_weekly_anomaly[df_weekly_anomaly$Month %in% c(1:3, 10:12), focus_var] <- bias # upper
    
    # Store the weekly T2 statistics for each year
    T2_1st <- double() # First year
    T2_2nd <- double() # Second year
    
    # Store the weekly T2 statistics for each year with the anomaly
    T2_1st_anomaly <- double() # First year
    T2_2nd_anomaly <- double() # Second year
    
    # Loop through the weeks in a year
    for (week in 1:52) {
      # Store a window of values for the current week of each year with the anomaly
      week_data_1st_anomaly <- df_weekly_anomaly[df_weekly_anomaly$Week == (week + 100), ] # First year
      week_data_2nd_anomaly <- df_weekly_anomaly[df_weekly_anomaly$Week == (week + 200), ] # Second year
      
      # Find the month of the beginning of each week for each year
      month_1st <- week_data_1st_anomaly[1, "Month"] # First year
      month_2nd <- week_data_2nd_anomaly[1, "Month"] # Second year
      
      # Calculate the current week's T2 statistics compared to the month it starts in for each year
      T2_1st <- c(T2_1st, t2(colMeans_week_1st[[week]],
                             colMeans_month_2nd[[month_1st]],
                             covariance_matrix_month_2nd[[month_1st]]))
      T2_2nd <- c(T2_2nd, t2(colMeans_week_2nd[[week]],
                             colMeans_month_1st[[month_2nd]],
                             covariance_matrix_month_1st[[month_2nd]]))
      
      # Calculate the current week's T2 statistics compared to the month it starts in for each year with the anomaly
      T2_1st_anomaly <- c(T2_1st_anomaly, t2(colMeans(week_data_1st_anomaly[1:var_count]),
                                             colMeans_month_2nd[[month_1st]],
                                             covariance_matrix_month_2nd[[month_1st]]))
      T2_2nd_anomaly <- c(T2_2nd_anomaly, t2(colMeans(week_data_2nd_anomaly[1:var_count]),
                                             colMeans_month_1st[[month_2nd]],
                                             covariance_matrix_month_1st[[month_2nd]]))
    }
    
    # Print updates to console
    #print(paste(focus_var, "Step:", n, "/", floor((step_count * 1.1) + 0.5)))
    
    # Calculate the weekly means of the target variable
    mean   <- aggregate(df_sensors[anomaly_var], list(df_sensors$Week), mean)
    mean_anomaly <- aggregate(df_weekly_anomaly[, anomaly_var], list(df_weekly_anomaly$Week), mean)
    
    for (u in 1:length(ucls)) {
      # Calculate Upper Control Limit (UCL)
      UCL <- quantile(c(T2_1st, T2_2nd), probs = ucls[u])
      
      # Calculate Confusion Matrix of the results for each year
      confusion_matrix <- results(c(T2_1st, T2_2nd), c(T2_1st_anomaly, T2_2nd_anomaly), UCL) # Total
      #accuracies[[u]] <- c(accuracies[[u]], confusion_matrix$overall[1])
      confusion_matrices[[u]] <- c(confusion_matrices[[u]], list(confusion_matrix))
    }
    
    # === ==== ==== ==== === #
  }
  
  all_var_matrices[[cur_var]] <- c(all_var_matrices[[cur_var]], confusion_matrices)
  
  printTimeUpdate()
}

# Print when done
print("DONE!")

# Loop through each variable to display
for (n in 1:5) {
  # CHANGE SENSOR INDEX: WHE:1, RSE:2, MHE:3, BME:4, HPE:5
  anomaly_var <- n
  var_list    <- c("WHE", "RSE", "MHE", "BME", "HPE")
  focus_var   <- var_list[anomaly_var]
  
  # List of lists of values to display
  display <- list()
  for (u in 1:length(ucls)) {
    # Change what to extract from the Confusion Matrices
    display[[u]] <- lapply(all_var_matrices[[n]][[u]], function(x) x$overall[1])
    
    # Reduce list of lists of lists into a list of lists of values
    display[[u]] <- Reduce(function(x, y) c(x, y), display[[u]])
  }
  
  # Find indices of Low and Medium shift factor from the 90th UCL and the High from the 100th UCL
  # EXPLAIN EACH
  high <- floor((step_count * 1.1) + 0.5) + step_count
  medium <- length(display[[4]])
  while (display[[4]][medium - 1] == display[[4]][medium])
    medium %+=% -1
  low <- length(display[[4]])
  while (display[[4]][low - 1] > 0.90)
    low %+=% -1
  print(paste(focus_var, "Indices for", "High:", high, "Medium:", medium, "Low:", low))
  print(paste(focus_var, "SD Factors for", c("High:", "Medium:", "Low:"), (all_max_factors[[n]] / step_count) * (c(high, medium, low) - floor((step_count * 1.1) + 0.5))))
  print(paste(focus_var, "Accuracies for UCL", ucls[2], c("High:", "Medium:", "Low:"), display[[2]][c(high, medium, low)]))
  print(paste(focus_var, "Accuracies for UCL", ucls[3], c("High:", "Medium:", "Low:"), display[[3]][c(high, medium, low)]))
  print(paste(focus_var, "Accuracies for UCL", ucls[4], c("High:", "Medium:", "Low:"), display[[4]][c(high, medium, low)]))
  
  # Grid for plots
  par(mfrow = c(1, 1))
  
  # Plot the Confusion Matrices variables
  steps <- c(1:(2 * floor((step_count * 1.1) + 0.5)))
  plot(display[[1]], xaxt = "n", main = paste(focus_var, anomaly_type, "Accuracy vs Standard Deviation Factor"), xlab = "SD Anomaly Factor", ylab = "Model Accuracy", type = "o", ylim = c(0.4, 1), cex.lab = 0.7, cex.axis = 0.7, cex.main = 0.7)
  axis(1, at = steps, labels = (all_max_factors[[n]] / step_count) * c(-(floor((step_count * 1.1) + 0.5):1), 1:floor((step_count * 1.1) + 0.5)), cex.axis = 0.7)
    abline(v = high, col = "red")
  abline(v = medium, col = "red")
  abline(v = low, col = "red")
  # High
  abline(h = display[[2]][high], col = "pink")
  abline(h = display[[3]][high], col = "orange")
  abline(h = display[[4]][high], col = "lightblue")
  # Medium
  abline(h = display[[2]][medium], col = "pink")
  abline(h = display[[3]][medium], col = "orange")
  abline(h = display[[4]][medium], col = "lightblue")
  # Low
  abline(h = display[[2]][low], col = "pink")
  abline(h = display[[3]][low], col = "orange")
  abline(h = display[[4]][low], col = "lightblue")
  lines(display[[2]], col = "purple", type = "o")
  lines(display[[3]], col = "brown", type = "o")
  lines(display[[4]], col = "blue", type = "o")
  legend("bottomleft", legend = c(paste("UCL", format(ucls, nsmall = 2)), paste(c("HIGH", "MEDIUM", "LOW"), format((all_max_factors[[n]] / step_count) * (c(high, medium, low) - floor((step_count * 1.1) + 0.5)), nsmall = 4))), col = c("black", "purple", "brown", "blue", "red", "red", "red"), lty = 1, bg = "transparent", cex = 0.7)
}
