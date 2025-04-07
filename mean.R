# Load libraries
library(dplyr)

# Recall accuracy data (scaled from 0–100 to 0–1)
stationary_accuracy <- c(47.8, 54.6, 43.0, 57.5, 39.1, 31.0, 22.0, 33.5, 36.6, 43.8, 38.1, 48.0) 
walking_accuracy <- c(18.5, 25.9, 31.2, 37.6, 39.2, 31.5, 34.9, 20.25, 27.25, 17.25, 18.3, 22.9) 

# Recall time data (in seconds)
stationary_time <- c(53, 62, 54, 62, 49, 46, 48, 39, 40, 42, 46, 51)
walking_time <- c(35, 25, 45, 67, 24, 34, 63, 15, 100, 23, 45, 126)

# Combine into data frame
df <- data.frame(
  Group = rep(c("Stationary", "Walking"), each = 12),
  Accuracy = c(stationary_accuracy, walking_accuracy),
  RecallTime = c(stationary_time, walking_time)
)

# Compute group-wise mean and standard deviation
summary_table <- df %>%
  group_by(Group) %>%
  summarise(
    Mean_Accuracy = mean(Accuracy),
    SD_Accuracy = sd(Accuracy),
    Mean_RecallTime = mean(RecallTime),
    SD_RecallTime = sd(RecallTime)
  )

# Display result
print(summary_table)

