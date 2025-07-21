# Load necessary packages
library(readr)
library(dplyr)

# Read the data
df <- read_csv("Updated CSV.csv")

# Remove the transformed column
df <- df[, !names(df) %in% c("coffee_d_preference")]

# Remove columns with only one unique value
df <- df[, sapply(df, function(x) length(unique(x)) > 1)]

# Convert character columns to factors
df <- df %>%
  mutate(across(where(is.character), as.factor))

# Remove factor columns with only one level
df <- df[, sapply(df, function(x) !(is.factor(x) && length(unique(x)) < 2))]

# Fit a linear model
model <- lm(Outcome ~ ., data = df)

# Display model summary
summary(model)
