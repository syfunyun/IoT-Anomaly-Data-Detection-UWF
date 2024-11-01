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
anomaly_var <- 1
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

# Create a list of lists by month and year
monthly_list <- double()

# Create a list to store the means by month
monthly_mean <- double()

# Loop through 24 months
for (n in 0:23) {
  # Shift the month count by 3 to align with the data which begins in April
  month <- 1 + (n + 3) %% 12
  year  <- 2012 + (n + 3) %/% 12
  
  # Collect the monthly data into a list
  monthly_list <- c(monthly_list, list(df_sensors[df_sensors$Month == month & df_sensors$Year == year, ]))
  
  # Calculate the monthly means
  monthly_mean <- c(monthly_mean, colMeans(monthly_list[[n + 1]][1:var_count]))
}

# Turn the lists into matrices
monthly_mean <- matrix(monthly_mean, ncol = var_count, byrow = TRUE)

# Create a data frame for all four seasons over the two years
df_spring <- df %>% filter(str_detect(Season, "Spring")) %>% select(WHE, RSE, MHE, BME, HPE, Year, Month)
df_summer <- df %>% filter(str_detect(Season, "Summer")) %>% select(WHE, RSE, MHE, BME, HPE, Year, Month)
df_fall   <- df %>% filter(str_detect(Season, "Fall"  )) %>% select(WHE, RSE, MHE, BME, HPE, Year, Month)
df_winter <- df %>% filter(str_detect(Season, "Winter")) %>% select(WHE, RSE, MHE, BME, HPE, Year, Month)

# Break each seasonal data frame into two year-unique data frame
# Note: this excludes Springs 2012 & 2014 since neither are complete in the data
df_summer_12 <- monthly_list[3:6]
df_fall_12   <- monthly_list[6:9]
df_winter_12 <- monthly_list[9:12]
df_spring_13 <- monthly_list[12:15]
df_summer_13 <- monthly_list[15:18]
df_fall_13   <- monthly_list[18:21]
df_winter_13 <- monthly_list[21:24]

# Calculate the T2 statistic for each month in each year-unique season
T2_summer_12 <- apply(monthly_mean[3:6,   1:var_count], 1, t2, colMeans(df_summer[1:var_count]), cov(df_summer[1:var_count]))
T2_fall_12   <- apply(monthly_mean[6:9,   1:var_count], 1, t2, colMeans(df_fall  [1:var_count]), cov(df_fall  [1:var_count]))
T2_winter_12 <- apply(monthly_mean[9:12,  1:var_count], 1, t2, colMeans(df_winter[1:var_count]), cov(df_winter[1:var_count]))
T2_spring_13 <- apply(monthly_mean[12:15, 1:var_count], 1, t2, colMeans(df_spring[1:var_count]), cov(df_spring[1:var_count]))
T2_summer_13 <- apply(monthly_mean[15:18, 1:var_count], 1, t2, colMeans(df_summer[1:var_count]), cov(df_summer[1:var_count]))
T2_fall_13   <- apply(monthly_mean[18:21, 1:var_count], 1, t2, colMeans(df_fall  [1:var_count]), cov(df_fall  [1:var_count]))
T2_winter_13 <- apply(monthly_mean[21:24, 1:var_count], 1, t2, colMeans(df_winter[1:var_count]), cov(df_winter[1:var_count]))

# Create seasonal data frames to store the anomaly
df_summer_12_anomaly <- df_summer_12
df_fall_12_anomaly   <- df_fall_12  
df_winter_12_anomaly <- df_winter_12
df_spring_13_anomaly <- df_spring_13
df_summer_13_anomaly <- df_summer_13
df_fall_13_anomaly   <- df_fall_13  
df_winter_13_anomaly <- df_winter_13

# Insert anomaly into the later 3 months of each 4-month season
for (n in 2:4) {
  if (anomaly_type == "SHIFT") { # Data Shift
    df_summer_12_anomaly[[n]][anomaly_var] %+=% iqr
    df_fall_12_anomaly  [[n]][anomaly_var] %+=% iqr
    df_winter_12_anomaly[[n]][anomaly_var] %+=% iqr
    df_spring_13_anomaly[[n]][anomaly_var] %+=% iqr
    df_summer_13_anomaly[[n]][anomaly_var] %+=% iqr
    df_fall_13_anomaly  [[n]][anomaly_var] %+=% iqr
    df_winter_13_anomaly[[n]][anomaly_var] %+=% iqr
  }
  else if (anomaly_type == "BIAS") { # Data Bias
    df_summer_12_anomaly[[n]][anomaly_var] <- upper
    df_fall_12_anomaly  [[n]][anomaly_var] <- upper
    df_winter_12_anomaly[[n]][anomaly_var] <- upper
    df_spring_13_anomaly[[n]][anomaly_var] <- upper
    df_summer_13_anomaly[[n]][anomaly_var] <- upper
    df_fall_13_anomaly  [[n]][anomaly_var] <- upper
    df_winter_13_anomaly[[n]][anomaly_var] <- upper
  }
}

# Store the means of each month in each seasonal data frame after the anomaly
mean_summer_12_anomaly <- double()
mean_fall_12_anomaly   <- double()
mean_winter_12_anomaly <- double()
mean_spring_13_anomaly <- double()
mean_summer_13_anomaly <- double()
mean_fall_13_anomaly   <- double()
mean_winter_13_anomaly <- double()

# Loop through all 4 months in each season
for (n in 1:4) {
  # Calculate the means of each month in each seasonal data frame
  mean_summer_12_anomaly <- c(mean_summer_12_anomaly, colMeans(df_summer_12_anomaly[[n]][1:var_count]))
  mean_fall_12_anomaly   <- c(mean_fall_12_anomaly,   colMeans(df_fall_12_anomaly  [[n]][1:var_count]))
  mean_winter_12_anomaly <- c(mean_winter_12_anomaly, colMeans(df_winter_12_anomaly[[n]][1:var_count]))
  mean_spring_13_anomaly <- c(mean_spring_13_anomaly, colMeans(df_spring_13_anomaly[[n]][1:var_count]))
  mean_summer_13_anomaly <- c(mean_summer_13_anomaly, colMeans(df_summer_13_anomaly[[n]][1:var_count]))
  mean_fall_13_anomaly   <- c(mean_fall_13_anomaly,   colMeans(df_fall_13_anomaly  [[n]][1:var_count]))
  mean_winter_13_anomaly <- c(mean_winter_13_anomaly, colMeans(df_winter_13_anomaly[[n]][1:var_count]))
}

# Combine the month mean data into a matrix for ease of use
mean_summer_12_anomaly <- matrix(mean_summer_12_anomaly, ncol = var_count, byrow = TRUE)
mean_fall_12_anomaly   <- matrix(mean_fall_12_anomaly  , ncol = var_count, byrow = TRUE)
mean_winter_12_anomaly <- matrix(mean_winter_12_anomaly, ncol = var_count, byrow = TRUE)
mean_spring_13_anomaly <- matrix(mean_spring_13_anomaly, ncol = var_count, byrow = TRUE)
mean_summer_13_anomaly <- matrix(mean_summer_13_anomaly, ncol = var_count, byrow = TRUE)
mean_fall_13_anomaly   <- matrix(mean_fall_13_anomaly  , ncol = var_count, byrow = TRUE)
mean_winter_13_anomaly <- matrix(mean_winter_13_anomaly, ncol = var_count, byrow = TRUE)

# Calculate the T2 statistic after the anomaly for each season
T2_summer_12_anomaly <- apply(mean_summer_12_anomaly, 1, t2, colMeans(df_summer[1:var_count]), cov(df_summer[1:var_count]))
T2_fall_12_anomaly   <- apply(mean_fall_12_anomaly,   1, t2, colMeans(df_fall  [1:var_count]), cov(df_fall  [1:var_count]))
T2_winter_12_anomaly <- apply(mean_winter_12_anomaly, 1, t2, colMeans(df_winter[1:var_count]), cov(df_winter[1:var_count]))
T2_spring_13_anomaly <- apply(mean_spring_13_anomaly, 1, t2, colMeans(df_spring[1:var_count]), cov(df_spring[1:var_count]))
T2_summer_13_anomaly <- apply(mean_summer_13_anomaly, 1, t2, colMeans(df_summer[1:var_count]), cov(df_summer[1:var_count]))
T2_fall_13_anomaly   <- apply(mean_fall_13_anomaly,   1, t2, colMeans(df_fall  [1:var_count]), cov(df_fall  [1:var_count]))
T2_winter_13_anomaly <- apply(mean_winter_13_anomaly, 1, t2, colMeans(df_winter[1:var_count]), cov(df_winter[1:var_count]))

# Calculate the Upper Control Limit (UCL)
UCL <- quantile(c(T2_summer_12,
                  T2_fall_12,
                  T2_winter_12,
                  T2_spring_13,
                  T2_summer_13,
                  T2_fall_13,
                  T2_winter_13),
                probs = c(.95))

# Grid for plots
par(mfrow = c(2, 3))

# Plot the T2 statistics of Summer 2012
summer_12 <- c("6/12", "7/12", "8/12", "9/12")
plot(T2_summer_12, xaxt = "n", main = paste(focus_var, anomaly_type, "T2 Summer 2012"), xlab = "Month/Year", ylab = "T2", type = "o", ylim = c(min(0, T2_summer_12, T2_summer_12_anomaly), max(T2_summer_12, T2_summer_12_anomaly, UCL)), cex.lab = 0.7, cex.axis = 0.7, cex.main = 0.7)
axis(1, at = 1:4, labels = summer_12, cex.axis = 0.7)
lines(T2_summer_12_anomaly, col = "red", type = "o")
abline(h = UCL, col = "blue")
legend("right", legend = c("T2 (Original)", "T2 (Annomaly)", "UCL"), col = c("black", "red", "blue"), lty = 1, bg = "transparent", cex = 0.7)

# Box Plots of each season of Summer 2012
box_data <- Reduce(function(x, y) merge(x, y, all = TRUE), df_summer_12_anomaly)
boxplot(box_data[[focus_var]] ~ Month, data = box_data, main = paste(focus_var, anomaly_type, "Box plot Summer 2012"), ylab = focus_var, ylim = c(min(quantile(box_data[[focus_var]], probs = 0.05)), max(mean_summer_12_anomaly[, anomaly_var], quantile(box_data[[focus_var]], probs = 0.95))), cex.lab = 0.7, cex.axis = 0.7, cex.main = 0.7)
lines(mean_summer_12_anomaly[, anomaly_var], col = "red", type = "o")
legend("bottomright", legend = c("Average"), col = "red", lty = 1, pch = 1, bg = "transparent", cex = 0.7)

# Plot the T2 statistics of Fall 2012
fall_12 <- c("9/12", "10/12", "11/12", "12/12")
plot(T2_fall_12, xaxt = "n", main = paste(focus_var, anomaly_type, "T2 Fall 2012"), xlab = "Month/Year", ylab = "T2", type = "o", ylim = c(min(0, T2_fall_12, T2_fall_12_anomaly), max(T2_fall_12, T2_fall_12_anomaly, UCL)), cex.lab = 0.7, cex.axis = 0.7, cex.main = 0.7)
axis(1, at = 1:4, labels = fall_12, cex.axis = 0.7)
lines(T2_fall_12_anomaly, col = "red", type = "o")
abline(h = UCL, col = "blue")
legend("right", legend = c("T2 (Original)", "T2 (Annomaly)", "UCL"), col = c("black", "red", "blue"), lty = 1, bg = "transparent", cex = 0.7)

# Box Plots of each season of Fall 2012
box_data <- Reduce(function(x, y) merge(x, y, all = TRUE), df_fall_12_anomaly)
boxplot(box_data[[focus_var]] ~ Month, data = box_data, main = paste(focus_var, "Box plot Fall 2012"), ylab = focus_var, ylim = c(min(quantile(box_data[[focus_var]], probs = 0.05)), max(mean_fall_12_anomaly[, anomaly_var], quantile(box_data[[focus_var]], probs = 0.95))), cex.lab = 0.7, cex.axis = 0.7, cex.main = 0.7)
lines(mean_fall_12_anomaly[, anomaly_var], col = "red", type = "o")
legend("bottomright", legend = c("Average"), col = "red", lty = 1, pch = 1, bg = "transparent", cex = 0.7)

# Plot the T2 statistics of Winter 2012/13
winter_12 <- c("12/12", "1/13", "2/13", "3/13")
plot(T2_winter_12, xaxt = "n", main = paste(focus_var, anomaly_type, "T2 Winter 2012/13"), xlab = "Month/Year", ylab = "T2", type = "o", ylim = c(min(0, T2_winter_12, T2_winter_12_anomaly), max(T2_winter_12, T2_winter_12_anomaly, UCL)), cex.lab = 0.7, cex.axis = 0.7, cex.main = 0.7)
axis(1, at = 1:4, labels = winter_12, cex.axis = 0.7)
lines(T2_winter_12_anomaly, col = "red", type = "o")
abline(h = UCL, col = "blue")
legend("right", legend = c("T2 (Original)", "T2 (Annomaly)", "UCL"), col = c("black", "red", "blue"), lty = 1, bg = "transparent", cex = 0.7)

# Box Plots of each season of Winter 2012/13
box_data <- Reduce(function(x, y) merge(x, y, all = TRUE), df_winter_12_anomaly)
box_data$Month <- factor(box_data$Month, levels = c(12, 1, 2, 3))
boxplot(box_data[[focus_var]] ~ Month, data = box_data, main = paste(focus_var, anomaly_type, "Box plot Winter 2012/13"), ylab = focus_var, ylim = c(min(quantile(box_data[[focus_var]], probs = 0.05)), max(mean_winter_12_anomaly[, anomaly_var], quantile(box_data[[focus_var]], probs = 0.95))), cex.lab = 0.7, cex.axis = 0.7, cex.main = 0.7)
lines(mean_winter_12_anomaly[, anomaly_var], col = "red", type = "o")
legend("bottomright", legend = c("Average"), col = "red", lty = 1, pch = 1, bg = "transparent", cex = 0.7)
