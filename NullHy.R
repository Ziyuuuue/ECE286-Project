# Set max.print to a large value
options(max.print = 1000000)

# Load the data
data <- read.csv("data.csv")

# Rename columns for convenience
names(data)[names(data) == "Type.sit..walk."] <- "Condition"
names(data)[names(data) == "Question"] <- "InterpretationScore"
names(data)[names(data) == "Memorization"] <- "MemorizationScore"

# Print the entire dataset
print(data)

# ------------------------------------------------------
# Null Hypothesis Test 1: Condition Effect on Interpretation Score
# ------------------------------------------------------

# Subset interpretation scores for each condition
sit_interp <- subset(data, Condition == "sit")$InterpretationScore
walk_interp <- subset(data, Condition == "walk")$InterpretationScore

# Check normality of the interpretation scores
print(shapiro.test(sit_interp))
print(shapiro.test(walk_interp))

# Perform Welch's t-test for interpretation scores (does not assume equal variances)
print(t.test(sit_interp, walk_interp, var.equal = FALSE))

# ------------------------------------------------------
# Null Hypothesis Test 2: Condition Effect on Memorization Score
# ------------------------------------------------------

# Subset memorization scores for each condition
sit_memor <- subset(data, Condition == "sit")$MemorizationScore
walk_memor <- subset(data, Condition == "walk")$MemorizationScore

# Check normality of the memorization scores
print(shapiro.test(sit_memor))
print(shapiro.test(walk_memor))

# Perform Welch's t-test for memorization scores (does not assume equal variances)
print(t.test(sit_memor, walk_memor, var.equal = FALSE))
