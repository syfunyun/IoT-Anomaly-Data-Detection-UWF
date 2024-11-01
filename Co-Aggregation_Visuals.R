# Aggregations galore! (Relative)

# Read the data
source("read_data.R")

# Load functions and parameters
source("start_5.R")

df = getData()

for (target_var in 1:5) {
  # CHANGE SENSOR INDEX: WHE:1, RSE:2, MHE:3, BME:4, HPE:5
  #target_var   <- 1
  var_list     <- c("WHE", "RSE", "MHE", "BME", "HPE")
  var_count    <- length(var_list)
  focus_var  <- var_list[target_var]
  
  shiftData <- FALSE
  
  # CHANGE CO-AGGREGATION FUNCTION
  co_aggregation_function <- cov
  
  inverse_vars <- (1:5)#[!(1:5) %in% target_var]
  
  # each sensor data for the entire data
  df_sensors = df %>% select(WHE, RSE, MHE, BME, HPE, Year, Month, Day, Hour, Minute, Week)
  
  co_aggregation_block_size <- nrow(df) / 24 * 3
  block_steps <- nrow(df) / 24
  ratio <- co_aggregation_block_size / block_steps
  co_aggregation_list <- list()
  
  # Calculate a given factor of the standard deviation
  anomaly_factor <- 1 # 0.01629639
  sd_factor <- sd(df_sensors[[focus_var]], na.rm = FALSE) * anomaly_factor
  if (shiftData)
    df_sensors[df_sensors$Month %in% c(1:3, 10:12), focus_var] %+=% sd_factor
  
  for (m in inverse_vars) {
    co_aggregations <- double()
    
    for (n in ceiling(ratio):(nrow(df_sensors) / block_steps)) {
      step <- n * co_aggregation_block_size / ratio
      var_1 <- df_sensors[[target_var]] [(step - co_aggregation_block_size):step]
      var_2 <- df_sensors[[var_list[m]]][(step - co_aggregation_block_size):step]
      co_aggregation <- co_aggregation_function(var_1, var_2)
      if (is.na(co_aggregation))
        co_aggregation <- 0
      co_aggregations <- c(co_aggregations, co_aggregation)
    }
    
    co_aggregation_list <- c(co_aggregation_list, list(co_aggregations))
  }
  
  # Graph Mean Blocked Data
  par(mfrow = c(1, 1))
  
  data <- c(1:length(co_aggregation_list[[1]]))
  co_aggregation_sum <- Reduce("+", co_aggregation_list)
  plot(co_aggregation_list[[1]], xaxt = "n", main = paste(focus_var, "Covariance to all other variables with Blocks of", co_aggregation_block_size, "and Steps of", block_steps), xlab = "Time", ylab = paste("Aggregation"), type = "o", ylim = c(min(-1, co_aggregation_sum), max(1, co_aggregation_sum)), cex.lab = 0.7, cex.axis = 0.7, cex.main = 0.7)
  axis(1, at = data, labels = data, cex.axis = 0.7)
  abline(h = mean(co_aggregation_list[[1]]), col = "black")
  lines(co_aggregation_list[[2]], col = "blue", type = "o")
  abline(h = mean(co_aggregation_list[[2]]), col = "blue")
  lines(co_aggregation_list[[3]], col = "red", type = "o")
  abline(h = mean(co_aggregation_list[[3]]), col = "red")
  lines(co_aggregation_list[[4]], col = "orange", type = "o")
  abline(h = mean(co_aggregation_list[[4]]), col = "orange")
  lines(co_aggregation_list[[5]], col = "purple", type = "o")
  abline(h = mean(co_aggregation_list[[5]]), col = "purple")
  legend("bottomright", legend = c(var_list[inverse_vars]), col = c("black", "blue", "red", "orange", "purple"), lty = 1, bg = "transparent", cex = 0.7)
}
