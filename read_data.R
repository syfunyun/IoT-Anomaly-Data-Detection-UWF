# Install packages
library(tidyverse)
library(data.table) # install with install.packages("data.table")
library(anytime)    # install with install.packages("anytime")

# Read the data
# df = fread("https://pages.uwf.edu/acohen/teaching/datasets/Electricity_P_5variables.csv")
df = read.csv("Electricity_P.csv")

# Transform
# Absolute time zone added so the code doesn't break when running it in different regions -Sy Fontenot
df %>% 
  mutate(Date = anytime(UNIX_TS, tz = "GMT+7")) %>%
  separate("Date", c("Year", "Month", "Day", "Hour", "Minute", "Second"), convert = T) %>%
  select(-Second) %>%
    mutate( 
      Season = case_when(
      Month %in%  9 ~ "Summer/Fall",
      Month %in%  10:11 ~ "Fall",
      Month %in%  12 ~ "Fall/Winter",
      Month %in%  1:2  ~ "Winter",
      Month %in%  3  ~ "Winter/Spring",
      Month %in%  4:5  ~ "Spring",
      Month %in%  6  ~ "Spring/Summer",
      Month %in%  7:8  ~ "Summer")) -> df

###############

# Updated getData function to label weeks 1-104 instead of 101-152 and 201-252
# -Sy Fontenot
getDataUpdated = function() {
  df["Week"] <- 0
  n_1st = 1       # row num of 4/1/2012 00:00
  for (week in 1:104) {
    df[n_1st:(n_1st + 10079), "Week"] <- week # weeks from 1-104
    n_1st <- n_1st + 10080 # add a week
  }
  
  df["MonthCount"] <- 0
  for (month in 1:12)
    df[df$Month == month, "MonthCount"] <- (month - 4) %% 12 + 1
  df[df$Year == 2013 & df$Month %in% 4:12, "MonthCount"] %+=% 12
  df[df$Year == 2014 & df$Month %in% 1:3, "MonthCount"] %+=% 12
  
  df["DayCount"] <- 0
  dayCounter <- 1
  for (d in 1:(nrow(df) / 1440)) {
    df[dayCounter:(dayCounter + 1439), "DayCount"] <- d
    dayCounter %+=% 1440
  }
  return(df)
}

# # Use df
# getData = function() {
#   n_1st = 1       # row num of 4/1/2012 00:00
#   n_2nd = 525601  # row num of 4/1/2013 00:00
#   df["Week"] <- 0
#   for (week in 1:52) {
#     df[n_1st:(n_1st+10079), "Week"] <- 100 + week     # set num of week (101 for 1st week in 1st year)
#     if (n_2nd + 10079 > 1048575) {
#       df[n_2nd:1048575, "Week"] <- 200 + week         # second year's data ends before 3/31/2014, which would create NA data
#     } else {
#       df[n_2nd:(n_2nd + 10079), "Week"] <- 200 + week # set num of week (201 for 1st week in 2nd year)
#     }
#     n_1st <- n_1st + 10080 # add a week
#     n_2nd <- n_2nd + 10080 # add a week
#   }
#   return(df)
# }