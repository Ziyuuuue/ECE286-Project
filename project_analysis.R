# Load necessary packages
library(dplyr)

# Load the dataset
data <- read.csv("C:/Users/Lenovo/OneDrive - University of Toronto/Year Two/Stats/cleaned_data286.csv")
# View the exact column names to verify
print(names(data))

# Clean and prepare the data
data <- data %>%
  # Remove any rows with missing names
  filter(!is.na(Name), Name != "") %>%
  # Clean the Type column
  mutate(
    Type = as.factor(trimws(Type.sit..walk.)),  # Note the period at the end
    Type = droplevels(Type)
  ) %>%
  # Keep only sit and walk groups
  filter(Type %in% c("sit", "walk"))

# Create summary function
summary_stats <- function(var, group_var) {
  data %>%
    group_by(!!sym(group_var)) %>%
    summarise(
      N = n(),
      Mean = mean(!!sym(var), na.rm = TRUE),
      Median = median(!!sym(var), na.rm = TRUE),
      SD = sd(!!sym(var), na.rm = TRUE),
      Min = min(!!sym(var), na.rm = TRUE),
      Max = max(!!sym(var), na.rm = TRUE),
      IQR = IQR(!!sym(var), na.rm = TRUE),
      .groups = 'drop'
    )
}

# Descriptive statistics
cat("\n--- Correct Words in Correct Place ---\n")
print(summary_stats("Correct.words.in.Correct.Place", "Type"))

cat("\n--- Total Score ---\n")
print(summary_stats("Total.Score", "Type"))

cat("\n--- Complete Time (seconds) ---\n")
print(summary_stats("Complete.Time..s.", "Type"))


# F-tests for equal variance
cat("\n--- F-tests for Equal Variance ---\n")
cat("Correct Words in Correct Place:\n")
print(var.test(Correct.words.in.Correct.Place ~ Type, data = data))

cat("\nTotal Score:\n")
print(var.test(Total.Score ~ Type, data = data))

cat("\nComplete Time:\n")
print(var.test(Complete.Time..s. ~ Type, data = data))


cat("\n--- Overall Covariance: Recall Accuracy vs Understanding Score ---\n")

overall_cov <- cov(data$Total.Score, data$Question, use = "complete.obs")
print(overall_cov)

cat("Sit Group - Spearman:\n")
cor(data$Total.Score[data$Type == "sit"], data$Question[data$Type == "sit"], method = "spearman")
cat("Walk Group - Spearman:\n")
cor(data$Total.Score[data$Type == "walk"], data$Question[data$Type == "walk"], method = "spearman")
