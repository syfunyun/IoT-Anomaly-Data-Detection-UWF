# Aggregations galore! (Relative)

# Read the data
source("read_data.R")

# Load functions and parameters
source("start_5.R")

df = getData()

# List of variable names in order
var_list <- c("WHE", "RSE", "MHE", "BME", "HPE")

# each sensor data for the entire data
df_sensors = df %>% select(WHE, RSE, MHE, BME, HPE, Year, Month, Day, Hour, Minute, Week)

# CHANGE AGGREGATION FUNCTION
aggregation_function <- function(x) mean(x)

# CHANGE PARAMETERS
aggregation_block_size <- 10080
block_steps <- 1440
ratio <- aggregation_block_size / block_steps
inject_shift <- FALSE
shift_list <- c(0.01629639, 0.03771973, 0.01867676, 1.308594, 1.0000)

# Store aggregated value lists for each variable
aggregation_list <- list()

# Loop through all variables and aggregate over given block size and function
for (m in 1:length(var_list)) {
  if (inject_shift) {
    # Calculate a given factor of the standard deviation
    sd_factor <- sd(df_sensors[[m]], na.rm = FALSE) * shift_list[[m]]
    
    # Insert anomaly into the later half of the year
    # Note: the year in the data set starts with April (month 4), thus 10-12 and 1-3 are later
    df_sensors[df_sensors$Month %in% c(1:3, 10:12), focus_var] %+=% sd_factor
  }
  
  # Stores list of aggregated values
  aggregations <- double()
  
  # Loops through all 2 years given the blocks size and step above
  for (n in ceiling(ratio):(nrow(df_sensors) / block_steps)) {
    step <- n * aggregation_block_size / ratio
    var <- df_sensors[[var_list[m]]][(step - aggregation_block_size):step]
    aggregation <- aggregation_function(var)
    if (is.na(aggregation))
      aggregation <- 0
    aggregations <- c(aggregations, aggregation)
  }
  
  # Add aggregation values to the list that stores all aggregated variables
  aggregation_list <- c(aggregation_list, list(aggregations))
}

# Graph Mean Blocked Data
par(mfrow = c(1, 1))

data <- c(1:length(aggregation_list[[1]]))
aggregation_max <- Reduce("max", aggregation_list)
aggregation_min <- Reduce("min", aggregation_list)
plot(aggregation_list[[1]], xaxt = "n", main = paste("Aggregates to all other variables with Blocks of", aggregation_block_size, "and Steps of", block_steps), xlab = "Time", ylab = paste("Aggregation"), type = "l", ylim = c(min(-1, aggregation_min), max(1, aggregation_max)), cex.lab = 0.7, cex.axis = 0.7, cex.main = 0.7)
axis(1, at = data, labels = data, cex.axis = 0.7)
lines(aggregation_list[[2]], col = "blue", type = "l")
lines(aggregation_list[[3]], col = "red", type = "l")
lines(aggregation_list[[4]], col = "orange", type = "l")
lines(aggregation_list[[5]], col = "purple", type = "l")
legend("bottomright", legend = c(var_list), col = c("black", "blue", "red", "orange", "purple"), lty = 1, bg = "transparent", cex = 0.7)
