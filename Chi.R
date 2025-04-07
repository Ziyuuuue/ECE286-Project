# Load data from CSV file
data <- read.csv("ESC286 Data.csv", stringsAsFactors = FALSE)

# Rename columns for clarity (adjust these names if needed)
names(data)[names(data) == "Sentence Order"] <- "Sentence.Order"
names(data)[names(data) == "Total.Score"] <- "Total.Score"
names(data)[names(data) == "Complete Time (s)"] <- "Recall.Time"

# For Recall Accuracy, we assume that the TotalScore is our overall accuracy measure.
# If a separate RecallAccuracy measure is available, replace "TotalScore" with that variable.
data$RecallAccuracy <- data$TotalScore
# Convert columns to numeric if they are not already
data$SentenceOrder <- as.numeric(as.character(data$Sentence.Order))
data$TotalScore <- as.numeric(as.character(data$Total.Score))
data$RecallTime <- as.numeric(as.character(data$Complete.Time..s.))

# 1. Chi-Square Test: Sentence Order vs. Total Score

# Categorize the continuous variables into 4 quantile-based bins
data$SentenceOrderCat <- cut(data$SentenceOrder,
                             breaks = quantile(data$SentenceOrder, probs = seq(0, 1, by = 0.25), na.rm = TRUE),
                             include.lowest = TRUE)
data$TotalScoreCat <- cut(data$TotalScore,
                          breaks = quantile(data$TotalScore, probs = seq(0, 1, by = 0.25), na.rm = TRUE),
                          include.lowest = TRUE)

# Create a contingency table for Sentence Order and Total Score categories
table_sentence_total <- table(data$SentenceOrderCat, data$TotalScoreCat)
print("Contingency Table: Sentence Order vs. Total Score")
print(table_sentence_total)

# Perform the chi-square test
chi_sentence_total <- chisq.test(table_sentence_total)
print("Chi-Square Test: Sentence Order vs. Total Score")
print(chi_sentence_total)

# 2. Chi-Square Test: Recall Accuracy vs. Recall Time

# Convert columns to numeric if they are not already
data$SentenceOrder <- as.numeric(as.character(data$Sentence.Order))
data$RecallTime <- as.numeric(as.character(data$Complete.Time..s.))

# Categorize Sentence Order and Recall Time into 4 quantile-based bins
data$SentenceOrderCat <- cut(data$SentenceOrder,
                             breaks = quantile(data$SentenceOrder, probs = seq(0, 1, by = 0.25), na.rm = TRUE),
                             include.lowest = TRUE)
data$RecallTimeCat <- cut(data$RecallTime,
                          breaks = quantile(data$RecallTime, probs = seq(0, 1, by = 0.25), na.rm = TRUE),
                          include.lowest = TRUE)

# Create a contingency table for Sentence Order and Recall Time categories
table_sentence_recall <- table(data$SentenceOrderCat, data$RecallTimeCat)
print("Contingency Table: Sentence Order vs. Recall Time")
print(table_sentence_recall)

# Perform the chi-square test
chi_sentence_recall <- chisq.test(table_sentence_recall)
print("Chi-Square Test: Sentence Order vs. Recall Time")
print(chi_sentence_recall)


