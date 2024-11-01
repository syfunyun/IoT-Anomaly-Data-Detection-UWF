###########################
# EXPLAIN THE CODE HERE!! #
###########################

# Read the data
source("read_data.R")

# Load functions and parameters
source("start_5.R")

# Create a data frame to store the raw data set
df = getData()

# == Sy Fontenot Refactored == #

# CHANGE ANOMALY TYPE: "SHIFT" OR "BIAS"
anomaly_type <- "SHIFT"

# CHANGE SENSOR INDEX: WHE:1, RSE:2, MHE:3, BME:4, HPE:5
anomaly_var <- 5
var_list    <- c("WHE", "RSE", "MHE", "BME", "HPE")
var_count   <- length(var_list)
focus_var   <- var_list[anomaly_var]

# Select the following sensors from the data set along with each datum's Month and Year
df_sensors = df %>% select(WHE, RSE, MHE, BME, HPE, Year, Month)

# Calculate IQR and Lower and Upper ranges
iqr   <- IQR(df[[focus_var]], na.rm = FALSE)
quant <- quantile(df[[focus_var]], probs = c(.25, .75), na.rm = FALSE)
lower <- quant[1] - 1.5 * iqr # Lower Range
upper <- quant[2] + 1.5 * iqr # Upper Range

# Partition years into month-long lists
monthly_part_1st <- list() # First year
monthly_part_2nd <- list() # Second year

# Calculate monthly means by year
mean_1st <- double() # First year
mean_2nd <- double() # Second year

# Loop through 12 months
for (n in 0:11) {
  # Shift the month count by 3 to align with the data which begins in April
  month <- 1 + (n + 3) %% 12
  year  <- 2012 + (n + 3) %/% 12
  
  # Add add data in current month to a list
  monthly_part_1st <- c(monthly_part_1st, list(df_sensors[df_sensors$Month == month & df_sensors $ Year == year, ]))
  monthly_part_2nd <- c(monthly_part_2nd, list(df_sensors[df_sensors$Month == month & df_sensors $ Year == year + 1, ]))
  
  # Collect a list of monthly means for each year
  mean_1st <- c(mean_1st, colMeans(df_sensors[df_sensors$Month == month & df_sensors$Year == year, 1:var_count]))
  mean_2nd <- c(mean_2nd, colMeans(df_sensors[df_sensors$Month == month & df_sensors$Year == year + 1, 1:var_count]))
}

# Turn the lists into matrices
mean_1st <- matrix(mean_1st, ncol = var_count, byrow = TRUE)
mean_2nd <- matrix(mean_2nd, ncol = var_count, byrow = TRUE)

# Calculate the T2 statistic for each year
T2_1st <- apply(mean_1st, 1, t2, colMeans(df_sensors[1:var_count]), cov(df_sensors[1:var_count])) # First year
T2_2nd <- apply(mean_2nd, 1, t2, colMeans(df_sensors[1:var_count]), cov(df_sensors[1:var_count])) # Second year

# Monthly partitions after anomaly
monthly_part_1st_anomaly <- monthly_part_1st # First year
monthly_part_2nd_anomaly <- monthly_part_2nd # Second year

# Insert anomaly into the later 11 months of each list (Note: from May to March)
for (n in 2:12) {
  if (anomaly_type == "SHIFT") { # Data Shift
    monthly_part_1st_anomaly[[n]][anomaly_var] %+=% iqr
    monthly_part_2nd_anomaly[[n]][anomaly_var] %+=% iqr
  }
  else if (anomaly_type == "BIAS") { # Data Bias
    monthly_part_1st_anomaly[[n]][anomaly_var] <- upper
    monthly_part_2nd_anomaly[[n]][anomaly_var] <- upper
  }
}

# Calculate yearly means after the anomaly
mean_1st_anomaly <- double() # First year
mean_2nd_anomaly <- double() # Second year

# Loop through 12 months
for (n in 1:12) {
  # Collect a list of monthly means for each year after the anomaly
  mean_1st_anomaly <- c(mean_1st_anomaly, colMeans(monthly_part_1st_anomaly[[n]][1:var_count]))
  mean_2nd_anomaly <- c(mean_2nd_anomaly, colMeans(monthly_part_2nd_anomaly[[n]][1:var_count]))
}

# Turn the lists into matrices after the anomaly
mean_1st_anomaly <- matrix(mean_1st_anomaly, ncol = var_count, byrow = TRUE)
mean_2nd_anomaly <- matrix(mean_2nd_anomaly, ncol = var_count, byrow = TRUE)

# Calculate the T2 statistic for each year after the anomaly
T2_1st_anomaly <- apply(mean_1st_anomaly, 1, t2, colMeans(df_sensors[1:var_count]), cov(df_sensors[1:var_count]))
T2_2nd_anomaly <- apply(mean_2nd_anomaly, 1, t2, colMeans(df_sensors[1:var_count]), cov(df_sensors[1:var_count]))

# Calculate the Upper Control Limit (UCL)
UCL <- quantile(c(T2_1st, T2_2nd), probs = c(0.95))

# Grid for plots
par(mfrow = c(1, 2))

# Plot the T2 statistics of the first year
first_year <- c("4/12", "5/12", "6/12", "7/12", "8/12", "9/12", "10/12", "11/12", "12/12", "1/13", "2/13", "3/13")
plot(T2_1st, xaxt = "n", main = paste(focus_var, anomaly_type, "T2 of First Year (2012/13)"), xlab = "Month/Year", ylab = "T2", type = "o", ylim = c(min(0, T2_1st, T2_1st_anomaly), max(0, T2_1st, T2_1st_anomaly, UCL)), cex.lab = 0.7, cex.axis = 0.7, cex.main = 0.7)
axis(1, at = 1:12, labels = first_year, cex.axis = 0.7)
lines(T2_1st_anomaly, col = "red", type = "o")
abline(h = UCL, col = "blue")
legend("right", legend = c("T2 (Original)", "T2 (Anomaly)", "UCL"), col = c("black", "red", "blue"), lty = 1, bg = "transparent", cex = 0.7)

# Box Plots of each month in the first year
box_data <- Reduce(function(x, y) merge(x, y, all = TRUE), monthly_part_1st_anomaly) 
box_data$Month <- factor(box_data$Month, levels = c(4, 5, 6, 7, 8, 9, 10, 11, 12, 1, 2, 3))
boxplot(box_data[[focus_var]] ~ Month, data = box_data, main = paste(focus_var, anomaly_type, "Box Plot First Year (2012/13)"), ylab = focus_var, ylim = c(min(0, box_data[[focus_var]]), quantile(box_data[[focus_var]], probs = 0.95)), cex.lab = 0.7, cex.axis = 0.7, cex.main = 0.7)
lines(mean_1st_anomaly[, anomaly_var], col = "red", type = "o")
legend("bottomright", legend = c("Average"), col = "red", lty = 1, pch = 1, bg = "transparent", cex = 0.7)

# Plot the T2 statistics of the second year
second_year <- c("4/13", "5/13", "6/13", "7/13", "8/13", "9/13", "10/13", "11/13", "12/13", "1/14", "2/14", "3/14")
plot(T2_2nd, xaxt = "n", main = paste(focus_var, anomaly_type, "T2 of Second Year (2013/14)"), xlab = "Month/Year", ylab = "T2", type = "o", ylim = c(min(0, T2_2nd, T2_2nd_anomaly), max(0, T2_2nd, T2_2nd_anomaly, UCL)), cex.lab = 0.7, cex.axis = 0.7, cex.main = 0.7)
axis(1, at = 1:12, labels = second_year, cex.axis = 0.7)
lines(T2_2nd_anomaly, col = "red", type = "o")
abline(h = UCL, col = "blue")
legend("right", legend = c("T2 (Original)", "T2 (Anomaly)", "UCL"), col = c("black", "red", "blue"), lty = 1, bg = "transparent", cex = 0.7)

# Box Plots of each month in the second year
box_data <- Reduce(function(x, y) merge(x, y, all = TRUE), monthly_part_2nd_anomaly) 
box_data$Month <- factor(box_data$Month, levels = c(4, 5, 6, 7, 8, 9, 10, 11, 12, 1, 2, 3))
boxplot(box_data[[focus_var]] ~ Month, data = box_data, main = paste(focus_var, anomaly_type, "Box Plot Second Year (2013/14)"), ylab = focus_var, ylim = c(min(0, box_data[[focus_var]]), max(box_data[[focus_var]])), cex.lab = 0.7, cex.axis = 0.7, cex.main = 0.7)
lines(mean_1st_anomaly[, anomaly_var], col = "red", type = "o")
legend("bottomright", legend = c("Average"), col = "red", lty = 1, pch = 1, bg = "transparent", cex = 0.7)
