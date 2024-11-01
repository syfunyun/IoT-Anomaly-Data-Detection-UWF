###########################
# EXPLAIN THE CODE HERE!! #
###########################

library(ggplot2)
library(lattice)
library(caret)

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
anomaly_var <- 1
var_list    <- c("WHE", "RSE", "MHE", "BME", "HPE")
var_count   <- length(var_list)
focus_var   <- var_list[anomaly_var]

# CHANCE SD ANOMALY FACTOR
anomaly_factors <- c(0.0548994541168213, 0.0148228526115417, 0.0120778799057007)
#anomaly_factors <- c(0.124534606933594, 0.0323789978027344, 0.0273976135253906)
#anomaly_factors <- c(0.062255859375, 0.0161865234375, 0.01431884765625)
#anomaly_factors <- c(3.8970947265625, 1.32501220703125, 1.16912841796875)
#anomaly_factors <- c(3.09064865112305, 0.958101081848144, 0.865381622314453)

# Select the following sensors from the data set along with each datum's Month and Year
df_sensors = df %>% select(WHE, RSE, MHE, BME, HPE, Year, Month, Week)

# # Normalize each variable
# Note: Nothing seems to change, even if just 1 variable is transformed.
# for (v in var_list)
#   df_sensors[[v]] <- (df_sensors[[v]] - mean(df_sensors[[v]])) / sd(df_sensors[[v]])

# Store the weekly T2 statistics for each year
T2_1st <- double() # First year
T2_2nd <- double() # Second year

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
for (week in 1:52) {
  colMeans_week_1st <- c(colMeans_week_1st, list(colMeans(df_sensors[df_sensors$Week == (week + 100), 1:var_count])))
  colMeans_week_2nd <- c(colMeans_week_2nd, list(colMeans(df_sensors[df_sensors$Week == (week + 200), 1:var_count])))
  
  # Find the month this current week is in
  month <- df_sensors[df_sensors$Week %% 100 == week, "Month"][1]
  
  # Calculate the current week's T2 statistics compared to the month it starts in for each year
  # Note: this only needs to be calculated once, so it was removed from the following t2 calls
  T2_1st <- c(T2_1st, t2(colMeans_week_1st[[week]],
                         colMeans_month_2nd[[month]],
                         covariance_matrix_month_2nd[[month]]))
  T2_2nd <- c(T2_2nd, t2(colMeans_week_2nd[[week]],
                         colMeans_month_1st[[month]],
                         covariance_matrix_month_1st[[month]]))
}

# List of T2 calculations over the low, medium, and high standard deviation shifts
T2_1st_anomaly_list <- double() # First year
T2_2nd_anomaly_list <- double() # Second year

for (anomaly_factor in anomaly_factors) {
  # Store anomaly data
  df_weekly_anomaly <- df_sensors
  
  # Calculate a given factor of the standard deviation
  sd_factor <- sd(df_sensors[[focus_var]], na.rm = FALSE) * anomaly_factor
  # quant <- quantile(df_sensors[[focus_var]], probs = c(.25, .75), na.rm = FALSE)
  # lower <- quant[1] - sd_factor # Lower Range
  # upper <- quant[2] + sd_factor # Upper Range
  
  # Insert anomaly into the later half of the year
  # Note: the year in the data set starts with April (month 4), thus 10-12 and 1-3 are later
  if (anomaly_type == "SHIFT")
    df_weekly_anomaly[df_weekly_anomaly$Month %in% c(1:3, 10:12), focus_var] %+=% sd_factor
  # if (anomaly_type == "BIAS")
  #   df_weekly_anomaly[df_weekly_anomaly$Month %in% c(2, 4), focus_var] <- upper
  
  # Store the weekly T2 statistics for each year with the anomaly
  T2_1st_anomaly <- double() # First year
  T2_2nd_anomaly <- double() # Second year
  
  # tempMonthRange <- c(11)
  # cov_temp <- cov(df_sensors[df_sensors$Month %in% tempMonthRange & df_sensors$Week %/% 100 == 1, 1:3])
  
  # Loop through the weeks in a year
  for (week in 1:52) {
    # Store a window of values for the current week of each year with the anomaly
    week_data_1st_anomaly <- df_weekly_anomaly[df_weekly_anomaly$Week == (week + 100), ] # First year
    week_data_2nd_anomaly <- df_weekly_anomaly[df_weekly_anomaly$Week == (week + 200), ] # Second year
    
    # Find the month of the beginning of each week for each year
    month_1st <- week_data_1st_anomaly[1, "Month"] # First year
    month_2nd <- week_data_2nd_anomaly[1, "Month"] # Second year
    
    # # Calculate the current week's T2 statistics compared to the month it starts in for each year
    # T2_1st <- c(T2_1st, t2(colMeans(week_data_1st[1:3]),
    #                        0,
    #                        cov_temp))
    # T2_2nd <- c(T2_2nd, t2(colMeans(week_data_2nd[1:3]),
    #                        0,
    #                        cov_temp))
    # 
    # # Calculate the current week's T2 statistics compared to the month it starts in for each year with the anomaly
    # T2_1st_anomaly <- c(T2_1st_anomaly, t2(colMeans(week_data_1st_anomaly[1:3]),
    #                                        0,
    #                                        cov_temp))
    # T2_2nd_anomaly <- c(T2_2nd_anomaly, t2(colMeans(week_data_2nd_anomaly[1:3]),
    #                                        0,
    #                                        cov_temp))
    
    # Calculate the current week's T2 statistics compared to the month it starts in for each year with the anomaly
    T2_1st_anomaly <- c(T2_1st_anomaly, t2(colMeans(week_data_1st_anomaly[1:var_count]),
                                           colMeans_month_2nd[[month_1st]],
                                           covariance_matrix_month_2nd[[month_1st]]))
    T2_2nd_anomaly <- c(T2_2nd_anomaly, t2(colMeans(week_data_2nd_anomaly[1:var_count]),
                                           colMeans_month_1st[[month_2nd]],
                                           covariance_matrix_month_1st[[month_2nd]]))
  }
  
  # Add the current T2 chart to the list
  T2_1st_anomaly_list <- c(T2_1st_anomaly_list, list(T2_1st_anomaly))
  T2_2nd_anomaly_list <- c(T2_2nd_anomaly_list, list(T2_2nd_anomaly))
}

# # Calculate the weekly means of the target variable
# DELETE ME?
# mean <- aggregate(df_sensors[anomaly_var], list(df_sensors$Week), mean)
# mean_anomaly <- aggregate(df_weekly_anomaly[, anomaly_var], list(df_weekly_anomaly$Week), mean)

# Calculate Upper Control Limits (UCL)
# List of UCL quantiles to simulate
ucls_qunatiles <- c(0.98, 0.95, 0.90)
ucls <- quantile(c(T2_1st, T2_2nd), probs = ucls_qunatiles)

# # Calculate Confusion Matrix of the results for each year
# confusion_matrix_1st <- results(T2_1st, T2_1st_anomaly, UCL) # First year
# confusion_matrix_2nd <- results(T2_2nd, T2_2nd_anomaly, UCL) # Second year
# confusion_matrix_total <- results(c(T2_1st, T2_2nd), c(T2_1st_anomaly, T2_2nd_anomaly), UCL) # Total
# confusion_matrix_total

# Grid for plots
par(mfrow = c(1, 1))

# Plot the T2 statistics of the first year
week <- 1:104
max_x <- max(ucls, T2_1st, T2_2nd, T2_1st_anomaly_list[[1]], T2_2nd_anomaly_list[[1]], T2_1st_anomaly_list[[2]], T2_2nd_anomaly_list[[2]], T2_1st_anomaly_list[[3]], T2_2nd_anomaly_list[[3]])
plot(c(T2_1st, T2_2nd), xaxt = "n", main = paste(focus_var, anomaly_type, "T2 Over Two Years"), xlab = "Week", ylab = "T2", type = "n", ylim = c(0, max_x), cex.lab = 0.7, cex.axis = 0.7, cex.main = 0.7)
axis(1, at = week, labels = week, cex.axis = 0.7)
lines(c(T2_1st_anomaly_list[[1]], T2_2nd_anomaly_list[[1]]), col = "darkgoldenrod1", type = "o")
lines(c(T2_1st_anomaly_list[[2]], T2_2nd_anomaly_list[[2]]), col = "chocolate1", type = "o")
lines(c(T2_1st_anomaly_list[[3]], T2_2nd_anomaly_list[[3]]), col = "brown1", type = "o")
abline(h = ucls[1], col = "royalblue")
abline(h = ucls[2], col = "dodgerblue")
abline(h = ucls[3], col = "deepskyblue")
lines(c(T2_1st, T2_2nd), col = "black", type = "o")
legend("topleft", legend = c("True Data", paste("Shift", format(round(anomaly_factors, 6), nsmall = 6)), paste("UCL", format(ucls_qunatiles, nsmall = 2))), col = c("black", "darkgoldenrod1", "chocolate1", "brown1", "royalblue", "dodgerblue", "deepskyblue"), lty = 1, bg = "transparent", cex = 0.7)

# # Box Plots of each week in the first year
# box_data <- df_weekly_anomaly[df_weekly_anomaly$Week %in% 101:152, ]
# boxplot(box_data[[focus_var]] ~ Week, data = box_data, xaxt = "n", main = paste(focus_var, anomaly_type, "Box plot First Year (2012/13)"), ylim = c(min(box_data[[focus_var]]), max(quantile(box_data[[focus_var]], probs = 0.95), mean_anomaly[2:53, 2])), cex.lab = 0.7, cex.axis = 0.7, cex.main = 0.7)
# lines(mean_anomaly[2:53, 2], col = "red", type = "o")
# axis(1, at = week, labels = week)
# legend("bottomright", legend = c("Average"), col = "red", lty = 1, pch = 1,  cex = 0.7)
# 
# # Box Plots of each week in the second year
# box_data <- df_weekly_anomaly[df_weekly_anomaly$Week %in% 101:152, ]
# boxplot(box_data[[focus_var]] ~ Week, data = box_data, xaxt = "n", main = paste(focus_var, anomaly_type, "Box plot second Year (2012/13)"), ylim = c(min(box_data[[focus_var]]), max(quantile(box_data[[focus_var]], probs = 0.95), mean_anomaly[2:53, 2])), cex.lab = 0.7, cex.axis = 0.7, cex.main = 0.7)
# lines(mean_anomaly[2:53, 2], col = "red", type = "o")
# axis(1, at = week, labels = week)
# legend("bottomright", legend = c("Average"), col = "red", lty = 1, pch = 1,  cex = 0.7)
