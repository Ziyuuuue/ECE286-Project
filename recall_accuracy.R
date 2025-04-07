# Load libraries
library(ggplot2)
library(dplyr)
library(gridExtra)

# Embedded accuracy data for both groups (example values extracted from your spreadsheet)
# You can update these with full lists if needed

control_accuracy <- c(47.8, 54.6, 43.0, 57.5, 39.1, 31.0, 22.0, 33.5, 36.6, 43.8, 38.1, 48.0)   # scale to proportion
distraction_accuracy <- c(18.5, 25.9, 31.2, 37.6, 39.2, 31.5, 24.9, 20.25, 27.25, 17.25, 18.3, 22.9) 

# Create data frame
df <- data.frame(
  Accuracy = c(control_accuracy, distraction_accuracy),
  Group = rep(c("Stationary", "Walking"), each = 12)
)

# Summary statistics function
summary_stats <- function(data) {
  data %>%
    summarise(
      Mean = mean(Accuracy),
      Median = median(Accuracy),
      SD = sd(Accuracy),
      IQR = IQR(Accuracy),
      CI_lower = Mean - 1.96 * SD / sqrt(n()),
      CI_upper = Mean + 1.96 * SD / sqrt(n())
    )
}

# Print stats
control_stats <- summary_stats(filter(df, Group == "Stationary"))
distraction_stats <- summary_stats(filter(df, Group == "Walking"))
print(control_stats)
print(distraction_stats)

# Plot: Histogram with normal curve
plot_histogram <- function(data, group_name) {
  ggplot(data, aes(x = Accuracy)) +
    geom_histogram(aes(y = ..density..), bins = 10, fill = "#2c7fb8", color = "black", alpha = 0.7) +
    stat_function(fun = dnorm,
                  args = list(mean = mean(data$Accuracy), sd = sd(data$Accuracy)),
                  col = "red", size = 1) +
    ggtitle(paste("Histogram of Accuracy with Normal Distribution -", group_name)) +
    theme_minimal()
}

# Plot: Boxplot
plot_boxplot <- function(data, group_name) {
  ggplot(data, aes(x = group_name, y = Accuracy)) +
    geom_boxplot(fill = "#a1dab4", width = 0.4) +
    ggtitle(paste("Boxplot of Accuracy -", group_name)) +
    xlab("Group") +
    ylab("Accuracy") +
    scale_y_continuous(limits = c(10, 50), breaks = seq(10, 50, 10)) +
    theme_minimal()
}


# Subsets
control_df <- filter(df, Group == "Stationary")
distraction_df <- filter(df, Group == "Walking")

# Generate plots
p1 <- plot_histogram(control_df, "Stationary")
p2 <- plot_boxplot(control_df, "Stationary")
p3 <- plot_histogram(distraction_df, "Walking")
p4 <- plot_boxplot(distraction_df, "Walking")

# Display side by side
grid.arrange(p1, p2, ncol = 2)
grid.arrange(p3, p4, ncol = 2)
# Additional normality and distribution statistics for each group

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
distribution_stats(control_accuracy, "Stationary")
distribution_stats(distraction_accuracy, "Walking")

