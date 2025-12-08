# Load Data
data(iris)
survey_df <- read.csv("survey.csv")

# Check structure
print(names(iris))
print(names(survey_df))

# Prepare Iris Data
iris_clean <- iris[, c("Species", "Petal.Length")]
names(iris_clean) <- c("Category", "Score")

# Prepare Survey Data
survey_clean <- survey_df[, c("State", "Perceptions.of.Electoral.Integrity.Index")]
names(survey_clean) <- c("Category", "Score")

# Convert to numeric
iris_clean$Score <- as.numeric(iris_clean$Score)
survey_clean$Score <- as.numeric(survey_clean$Score)

# Combine using rbind
combined_data <- rbind(iris_clean, survey_clean)

# Output Summary
print(head(combined_data))
print(tail(combined_data))
