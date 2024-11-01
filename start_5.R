## For questions acohen@uwf.edu (Achraf Cohen)
### Main code applying T2 to EPA data 
###############################################
# DO NOT RUN

# Packages needed
pkg = c("readxl","mice","tidyverse","parallel")

# # Install packages if needed
# lapply(pkg, install.packages, character.only = TRUE)

# Load packages
lapply(pkg, library, character.only = TRUE)


# # Data clean up - no NAs
# rdata %>% drop_na() %>% dplyr::select(WHE,RSE,MHE,BME,HPE) -> X

# # Estimate Mean and S : known parameters
# m0 = colMeans(X_window)
# s0 = cov(X_window)

################## Functions ################

# T2 function
t2 = function(X_window, m0, s0) t(X_window - m0) %*% solve(s0) %*% (X_window - m0)

# A user-defined 'plus equals' operator for any two elements that can be added together using '+'
# -Sy Fontenot
`%+=%` = function(e1, e2) eval.parent(substitute(e1 <- e1 + e2))

# Key: anomalies should be detected and true values should be ignored.
# -Sy Fontenot
results = function(true_data, anomaly_data, predicted) {
  # Boolean list where anomalies exist when values of shared indices differ
  actual <- mapply(function(x, y) x != y, true_data, anomaly_data)
  
  # Display the confusion matrix table
  table(predicted, actual)
  
  # Return the confusion matrix based on the Key above
  confusionMatrix(as.factor(predicted), as.factor(actual))
}
