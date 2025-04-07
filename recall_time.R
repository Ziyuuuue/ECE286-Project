# Load libraries
library(ggplot2)
library(dplyr)
library(gridExtra)

# Embedded recall time data in seconds
stationary_time <- c(53, 62, 54, 52, 49, 46, 48, 39, 40, 42, 55, 51)
walking_time <- c(35, 25, 45, 67, 24, 34, 63, 15, 100, 23, 45, 126)

# Create data frame
df_time <- data.frame(
  RecallTime = c(stationary_time, walking_time),
  Group = rep(c("Stationary", "Walking"), each = 12)
)

# Summary statistics function
summary_stats <- function(data) {
  data %>%
    summarise(
      Mean = mean(RecallTime),
      Median = median(RecallTime),
      SD = sd(RecallTime),
      IQR = IQR(RecallTime),
      CI_lower = Mean - 1.96 * SD / sqrt(n()),
      CI_upper = Mean + 1.96 * SD / sqrt(n())
    )
}

# Print stats
stationary_stats <- summary_stats(filter(df_time, Group == "Stationary"))
walking_stats <- summary_stats(filter(df_time, Group == "Walking"))
print(stationary_stats)
print(walking_stats)

# Plot: Histogram with normal curve
plot_histogram_time <- function(data, group_name) {
  ggplot(data, aes(x = RecallTime)) +
    geom_histogram(aes(y = ..density..), bins = 10, fill = "#3182bd", color = "black", alpha = 0.7) +
    stat_function(fun = dnorm,
                  args = list(mean = mean(data$RecallTime), sd = sd(data$RecallTime)),
                  col = "red", size = 1) +
    ggtitle(paste("Histogram of Recall Time with Normal Distribution -", group_name)) +
    xlab("Recall Time (s)") +
    ylab("Density") +
    theme_minimal()
}

# Plot: Boxplot
plot_boxplot_time <- function(data, group_name) {
  ggplot(data, aes(x = group_name, y = RecallTime)) +
    geom_boxplot(fill = "#a6bddb", width = 0.4) +
    ggtitle(paste("Boxplot of Recall Time -", group_name)) +
    xlab("Group") +
    ylab("Recall Time (s)") +
    scale_y_continuous(limits = c(30, 70), breaks = seq(10, 70, 20)) +
    theme_minimal()
}

# Subsets
stationary_df <- filter(df_time, Group == "Stationary")
walking_df <- filter(df_time, Group == "Walking")

# Generate plots
p5 <- plot_histogram_time(stationary_df, "Stationary")
p6 <- plot_boxplot_time(stationary_df, "Stationary")
p7 <- plot_histogram_time(walking_df, "Walking")
p8 <- plot_boxplot_time(walking_df, "Walking")

# Display side by side
grid.arrange(p5, p6, ncol = 2)
grid.arrange(p7, p8, ncol = 2)

# Function to summarize distribution details
distribution_stats <- function(data_vector, group_name) {
  cat(paste("\n---", group_name, "Group ---\n"))
  cat("Median:", median(data_vector), "\n")
  cat("Interquartile Range (IQR):", IQR(data_vector), "\n")
  shapiro <- shapiro.test(data_vector)
  cat("Shapiro-Wilk Test:\n")
  cat("W =", round(shapiro$statistic, 3), ", p =", round(shapiro$p.value, 3), "\n")
}

# Apply to both groups
distribution_stats(stationary_time, "Stationary")
distribution_stats(walking_time, "Walking")

