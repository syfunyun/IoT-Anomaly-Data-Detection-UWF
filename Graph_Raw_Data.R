# Print out variable data

# Read the data
source("read_data.R")

# Load functions and parameters
source("start_5.R")

df = getData()

# CHANGE ANOMALY TYPE: "SHIFT" OR "BIAS"
anomaly_type <- "SHIFT"

# CHANGE TEST TYPE: "MONTHLY" OR "WEEKLY"
test_type <- "WEEKLY"

# Show covariance window
test <- FALSE

# SD factor shifts from Accuracy_SD_Plot.R binary fractal algorithm
shift_list <- list(c(0.0144285559654236, 0.0108214169740677, 0.00721427798271179),
                c(0.124534606933594, 0.0323789978027344, 0.0273976135253906),
                c(0.062255859375, 0.0161865234375, 0.01431884765625),
                c(3.8970947265625, 1.32501220703125, 1.16912841796875),
                c(3.09064865112305, 0.958101081848144, 0.865381622314453))

for (anomaly_var in 1:5) {
  # Variable indices: WHE:1, RSE:2, MHE:3, BME:4, HPE:5
  var_list    <- c("WHE", "RSE", "MHE", "BME", "HPE")
  var_count   <- length(var_list)
  focus_var   <- var_list[anomaly_var]
  
  # each sensor data for the entire data
  df_sensors = df %>% select(WHE, RSE, MHE, BME, HPE, Year, Month, Day, Hour, Minute, Week)
  
  # List of overlapping shifted data values
  df_graphed_anomaly_list <- double()
  
  # List of in- and out-of-control data
  df_in_control_list <- double()
  df_out_of_control_list <- double()
  
  # Mean Block graph parameters
  par(mfrow = c(1, 1))
  blocksOf <- 1440 # 10080
  sorted <- FALSE # CHANGE TO 'TRUE' OR 'FALSE'
  
  # Calculate the means for the given block size
  df_graphed <- .colMeans(df_sensors[[focus_var]], blocksOf, length(df_sensors[[focus_var]]) / blocksOf)
  
  for (shift_level in 1:3) {
    # Store anomaly data
    df_anomaly <- df_sensors
    
    # Calculate a given factor of the standard deviation
    sd_factor <- sd(df_sensors[[focus_var]], na.rm = FALSE) * shift_list[[anomaly_var]][shift_level]
    iqr   <- IQR(df_sensors[[focus_var]], na.rm = FALSE)
    quant <- quantile(df_sensors[[focus_var]], probs = c(.25, .75), na.rm = FALSE)
    lower <- quant[1] - sd_factor # Lower Range
    upper <- quant[2] + sd_factor # Upper Range
    bias  <- quantile(df_sensors[[focus_var]], probs = c(.5), na.rm = FALSE) + sd_factor
    
    if (test_type == "MONTHLY") {
      # Insert anomaly into the later last 11 months of the year
      # Note: the year in the data set starts with April (month 4), thus 5-12 and 1-3 are last 11 months
      if (anomaly_type == "SHIFT")
        df_anomaly[df_anomaly$Month %in% c(1:3, 5:12), focus_var] %+=% sd_factor
      if (anomaly_type == "BIAS")
        df_anomaly[df_anomaly$Month %in% c(1:3, 5:12), focus_var] <- bias # upper
      
      df_in_control_list     <- df_anomaly[df_anomaly$Month %in% 4, focus_var]
      df_out_of_control_list <- df_anomaly[df_anomaly$Month %in% c(1:3, 5:12), focus_var]
    }
    else if (test_type == "WEEKLY") {
      # Insert anomaly into the later half of the year
      # Note: the year in the data set starts with April (month 4), thus 10-12 and 1-3 are later
      if (anomaly_type == "SHIFT")
        df_anomaly[df_anomaly$Month %in% c(1:3, 10:12), focus_var] %+=% sd_factor
      if (anomaly_type == "BIAS")
        df_anomaly[df_anomaly$Month %in% c(1:3, 10:12), focus_var] <- bias # upper
      
      df_in_control_list     <- c(df_in_control_list, list(df_anomaly[df_anomaly$Month %in% 4:9, focus_var]))
      df_out_of_control_list <- c(df_out_of_control_list, list(df_anomaly[df_anomaly$Month %in% c(1:3, 10:12), focus_var]))
    }
    
    df_graphed_anomaly_list <- c(df_graphed_anomaly_list, list(.colMeans(df_anomaly[[focus_var]], blocksOf, length(df_anomaly[[focus_var]]) / blocksOf)))
  }

  # # Histogram parameters
  # par(mfrow = c(1, 2))
  # blocksOf <- 1440
  # 
  # # Histogram of raw data
  # df_select <- df_sensors[[focus_var]]
  # hist(df_select, breaks = length(df_select) / blocksOf, main = paste(focus_var, "by Blocks of", blocksOf), xlab = "Data Before Anomaly", cex.lab = 0.7, cex.axis = 0.7, cex.main = 0.7)
  # abline(v = iqr, col = "blue")
  # abline(v = mean(df_select), col = "red")
  # legend("topright", legend = c("IQR", "Mean"), col = c("blue", "red"), lty = 1, bg = "transparent", cex = 0.7)
  # 
  # # Histogram after anomaly
  # df_select = df_anomaly[[focus_var]]
  # hist(df_select, breaks = length(df_select) / blocksOf, main = paste(focus_var, "by Blocks of", blocksOf), xlab = "Data After Anomaly", cex.lab = 0.7, cex.axis = 0.7, cex.main = 0.7)
  # abline(v = iqr, col = "blue")
  # abline(v = mean(df_sensors[[focus_var]]), col = "red")
  # abline(v = mean(df_select), col = "orange")
  # legend("topright", legend = c("IQR", "Mean", "Anomaly Mean"), col = c("blue", "red", "orange"), lty = 1, bg = "transparent", cex = 0.7)
  
  df_test <- double()
  temp <- .colMeans(df_sensors[[focus_var]], 43200, length(df_sensors[[focus_var]]) / 43200)
  for (l in 1:length(df_graphed))
    df_test <- c(df_test, temp[ceiling(l / length(df_graphed) * 24)])
  
  # Sort data if specified
  if (sorted) {
    df_graphed <- sort(df_graphed)
    for (shift_level in 1:3)
      df_graphed_anomaly_list[[shift_level]] <- sort(df_graphed_anomaly_list[[shift_level]])
  }
  
  # Print means
  print(paste(focus_var, "Raw Mean:", mean(df_sensors[[focus_var]])))
  #print(paste(focus_var, "In-Control Mean:", mean(df_in_control_list)))
  #print(paste(focus_var, "Out-of-Control Mean:", mean(df_out_of_control_list)))
  #print(paste(focus_var, "Control Mean Difference:", mean(df_out_of_control_list) - mean(df_in_control_list)))
  print(paste(focus_var, "Raw SD:", sd(df_sensors[[focus_var]])))
  #print(paste(focus_var, "In-Control SD:", sd(df_in_control_list)))
  #print(paste(focus_var, "Out-of-Control SD:", sd(df_out_of_control_list)))
  cat("\n")
  
  # Plot raw means and anomaly means overlaid
  data <- c(1:length(df_graphed))
  plot(df_graphed, xaxt = "n", main = paste(focus_var, "by Mean Blocks of", blocksOf), xlab = "Time", ylab = "Value", type = "n", ylim = c(min(0, df_graphed, df_graphed_anomaly_list[[1]]), max(df_graphed)), cex.lab = 0.7, cex.axis = 0.7, cex.main = 0.7) # df_graphed_anomaly_list[[1]])), cex.lab = 0.7, cex.axis = 0.7, cex.main = 0.7)
  axis(1, at = data, labels = data, cex.axis = 0.7)
  if (test) lines(df_test, col = "grey", type = "o")
  #lines(df_graphed_anomaly_list[[1]], col = "darkgoldenrod1", type = "o")
  #lines(df_graphed_anomaly_list[[2]], col = "chocolate1", type = "o")
  #(df_graphed_anomaly_list[[3]], col = "red2", type = "o")
  lines(df_graphed, col = "black", type = "o")
  abline(v = 457, col = "magenta")
  abline(h = iqr, col = "blue")
  abline(h = mean(df_sensors[[focus_var]]), col = "red")
  #abline(h = mean(df_in_control_list), col = "green")
  #abline(h = mean(df_out_of_control_list), col = "magenta")
  #abline(h = mean(df_sensors[[focus_var]]) + c(-sd(df_sensors[[focus_var]]), sd(df_sensors[[focus_var]])), col = "red", lty = 2, lwd = 1)
  legend("topleft", legend = c("Raw Data", "High Shift", "Medium Shift", "Low Shift", "IQR", "Raw Mean"), col = c("black", "darkgoldenrod1", "chocolate1", "red2", "blue", "red"), lty = 1, bg = "transparent", cex = 0.7)
  #legend("topleft", legend = c("Raw Data", "Anomaly-Injected Data", "IQR", "Raw Mean", "Raw SD"), col    = c("black", "purple", "blue", "red", "red"), lty = c(1, 1, 1, 1, 2), bg = "transparent", cex = 0.7)
  
  # Anomaly plot (HIGH Shift)
  data <- c(1:length(df_graphed_anomaly_list[[1]]))
  plot(df_graphed_anomaly_list[[1]], xaxt = "n", main = paste(focus_var, "High Shift by Mean Blocks of", blocksOf), xlab = "Time", ylab = "Value", type = "n", ylim = c(min(0, df_graphed, df_graphed_anomaly_list[[1]]), max(df_graphed, df_graphed_anomaly_list[[1]])), cex.lab = 0.7, cex.axis = 0.7, cex.main = 0.7)
  axis(1, at = data, labels = data, cex.axis = 0.7)
  if (test) lines(df_test, col = "grey", type = "o")
  lines(df_graphed, col = "black", type = "o")
  lines(df_graphed_anomaly_list[[1]], col = "darkgoldenrod1", type = "o")
  abline(h = mean(df_in_control_list[[1]]), col = "green")
  abline(h = mean(df_out_of_control_list[[1]]), col = "magenta")
  #abline(h = mean(df_in_control_list) + c(-sd(df_in_control_list), sd(df_in_control_list)), col = "green", lty = 2, lwd = 1)
  #abline(h = mean(df_out_of_control_list) + c(-sd(df_out_of_control_list), sd(df_out_of_control_list)), col = "orange", lty = 2, lwd = 1)
  legend("topleft", legend = c("Raw Data", "High Shift", "In-Control Mean", "Out-of-Control Mean"),col = c("black", "darkgoldenrod1", "green", "magenta"), lty = 1, bg = "transparent", cex = 0.7)
  #legend("topleft", legend = c("Raw Data", "Anomaly-Injected Data", "In-Control Mean", "In-Control SD", "Out-of-Control Mean", "Out-of-Control SD"), col = c("black", "purple", "green", "green", "orange", "orange"), lty = c(1, 1, 1, 2, 1, 2), bg = "transparent", cex = 0.7)
  
  # Anomaly plot (MEDIUM Shift)
  data <- c(1:length(df_graphed_anomaly_list[[2]]))
  plot(df_graphed_anomaly_list[[2]], xaxt = "n", main = paste(focus_var, "Medium Shift by Mean Blocks of", blocksOf), xlab = "Time", ylab = "Value", type = "n", ylim = c(min(0, df_graphed, df_graphed_anomaly_list[[2]]), max(df_graphed, df_graphed_anomaly_list[[2]])), cex.lab = 0.7, cex.axis = 0.7, cex.main = 0.7)
  axis(1, at = data, labels = data, cex.axis = 0.7)
  if (test) lines(df_test, col = "grey", type = "o")
  lines(df_graphed, col = "black", type = "o")
  lines(df_graphed_anomaly_list[[2]], col = "chocolate1", type = "o")
  abline(h = mean(df_in_control_list[[2]]), col = "green")
  abline(h = mean(df_out_of_control_list[[2]]), col = "magenta")
  #abline(h = mean(df_in_control_list) + c(-sd(df_in_control_list), sd(df_in_control_list)), col = "green", lty = 2, lwd = 1)
  #abline(h = mean(df_out_of_control_list) + c(-sd(df_out_of_control_list), sd(df_out_of_control_list)), col = "orange", lty = 2, lwd = 1)
  legend("topleft", legend = c("Raw Data", "Medium Shift", "In-Control Mean", "Out-of-Control Mean"),col = c("black", "chocolate1", "green", "magenta"), lty = 1, bg = "transparent", cex = 0.7)
  #legend("topleft", legend = c("Raw Data", "Anomaly-Injected Data", "In-Control Mean", "In-Control SD", "Out-of-Control Mean", "Out-of-Control SD"), col = c("black", "purple", "green", "green", "orange", "orange"), lty = c(1, 1, 1, 2, 1, 2), bg = "transparent", cex = 0.7)
  
  # Anomaly plot (LOW Shift)
  data <- c(1:length(df_graphed_anomaly_list[[3]]))
  plot(df_graphed_anomaly_list[[3]], xaxt = "n", main = paste(focus_var, "Low Shift by Mean Blocks of", blocksOf), xlab = "Time", ylab = "Value", type = "n", ylim = c(min(0, df_graphed, df_graphed_anomaly_list[[3]]), max(df_graphed, df_graphed_anomaly_list[[3]])), cex.lab = 0.7, cex.axis = 0.7, cex.main = 0.7)
  axis(1, at = data, labels = data, cex.axis = 0.7)
  if (test) lines(df_test, col = "grey", type = "o")
  lines(df_graphed, col = "black", type = "o")
  lines(df_graphed_anomaly_list[[3]], col = "red2", type = "o")
  abline(h = mean(df_in_control_list[[3]]), col = "green")
  abline(h = mean(df_out_of_control_list[[3]]), col = "magenta")
  #abline(h = mean(df_in_control_list) + c(-sd(df_in_control_list), sd(df_in_control_list)), col = "green", lty = 2, lwd = 1)
  #abline(h = mean(df_out_of_control_list) + c(-sd(df_out_of_control_list), sd(df_out_of_control_list)), col = "orange", lty = 2, lwd = 1)
  legend("topleft", legend = c("Raw Data", "Low Shift", "In-Control Mean", "Out-of-Control Mean"),col = c("black", "red2", "green", "magenta"), lty = 1, bg = "transparent", cex = 0.7)
  #legend("topleft", legend = c("Raw Data", "Anomaly-Injected Data", "In-Control Mean", "In-Control SD", "Out-of-Control Mean", "Out-of-Control SD"), col = c("black", "purple", "green", "green", "orange", "orange"), lty = c(1, 1, 1, 2, 1, 2), bg = "transparent", cex = 0.7)
}

