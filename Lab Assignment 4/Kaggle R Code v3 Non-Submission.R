# Packages------------

library(naniar)
library(visdat)
library(dplyr)
library(tidyr)
library(MASS)

train_df <- read.csv("training.csv")

### Prepping training data---------
summary(train_df)
str(train_df)
vis_dat(train_df)

train_df <- train_df %>%
  mutate(
    n_age = case_when(
      age == "<18 years old" ~ 15,
      age == "18-24 years old" ~ 21,
      age == "25-34 years old" ~ 29.5,
      age == "35-44 years old" ~ 39.5,
      age == "45-54 years old" ~ 49.5,
      age == "55-64 years old" ~ 59.5,
      age == ">65 years old" ~ 70,
      TRUE ~ NA_real_
    ),
    n_hh_income = case_when(
      hh_income == "Under $25,000" ~ 12500,
      hh_income == "$25,000 - $49,999" ~ 37500,
      hh_income == "$50,000 - $74,999" ~ 62500,
      hh_income == "$75,000 - $99,999" ~ 87500,
      hh_income == "$100,000 - $149,999" ~ 125000,
      hh_income == "$150,000 or more" ~ 200000,
      TRUE ~ NA_real_
    ),
    
    # Missing indicators for categorical variables
    missing_abc_prefer = ifelse(is.na(abc_prefer), 1, 0),
    missing_party = ifelse(is.na(party), 1, 0),
    missing_state = ifelse(is.na(state), 1, 0),
    missing_favorite_drink = ifelse(is.na(favorite_drink), 1, 0),
    missing_add = ifelse(is.na(add), 1, 0),
    missing_prefer_kind = ifelse(is.na(prefer_kind), 1, 0),
    missing_prefer_strength = ifelse(is.na(prefer_strength), 1, 0),
    missing_prefer_roast = ifelse(is.na(prefer_roast), 1, 0),
    missing_prefer_caffeine = ifelse(is.na(prefer_caffeine), 1, 0),
    missing_gender = ifelse(is.na(gender), 1, 0),
    missing_education = ifelse(is.na(education), 1, 0),
    
    # Impute categorical variables
    n_abc_prefer = factor(replace_na(abc_prefer, "unknown")),
    n_party = factor(replace_na(party, "unknown")),
    n_state = factor(replace_na(state, "unknown")),
    n_favorite_drink = factor(replace_na(favorite_drink, "unknown")),
    n_add = factor(replace_na(add, "unknown")),
    n_prefer_kind = factor(replace_na(prefer_kind, "unknown")),
    n_prefer_strength = factor(replace_na(prefer_strength, "unknown")),
    n_prefer_roast = factor(replace_na(prefer_roast, "unknown")),
    n_prefer_caffeine = factor(replace_na(prefer_caffeine, "unknown")),
    n_gender = factor(replace_na(gender, "unknown")),
    n_education = factor(replace_na(education, "unknown")),
    n_race = factor(replace_na(ethnicity_race, "unknown")),
    n_marital = case_when(
      marital_status == "Divorced" ~ "unknown",
      marital_status == "Widowed" ~ "unknown"
    ),
    n_marital = factor(replace_na(marital_status, "unknown")),
    
    # Missing indicators for numeric variables
    missing_a_bitterness = ifelse(is.na(a_bitterness), 1, 0),
    missing_a_acidity = ifelse(is.na(a_acidity), 1, 0),
    missing_a_preference = ifelse(is.na(a_preference), 1, 0),
    
    missing_b_bitterness = ifelse(is.na(b_bitterness), 1, 0),
    missing_b_acidity = ifelse(is.na(b_acidity), 1, 0),
    missing_b_preference = ifelse(is.na(b_preference), 1, 0),
    
    missing_c_bitterness = ifelse(is.na(c_bitterness), 1, 0),
    missing_c_acidity = ifelse(is.na(c_acidity), 1, 0),
    missing_c_preference = ifelse(is.na(c_preference), 1, 0),
    
    missing_d_bitterness = ifelse(is.na(d_bitterness), 1, 0),
    missing_d_acidity = ifelse(is.na(d_acidity), 1, 0),
    
    # Impute numeric variables
    imp_a_bitterness = ifelse(is.na(a_bitterness), mean(a_bitterness, na.rm = TRUE), a_bitterness),
    imp_a_acidity = ifelse(is.na(a_acidity), mean(a_acidity, na.rm = TRUE), a_acidity),
    imp_a_preference = ifelse(is.na(a_preference), mean(a_preference, na.rm = TRUE), a_preference),
    
    imp_b_bitterness = ifelse(is.na(b_bitterness), mean(b_bitterness, na.rm = TRUE), b_bitterness),
    imp_b_acidity = ifelse(is.na(b_acidity), mean(b_acidity, na.rm = TRUE), b_acidity),
    imp_b_preference = ifelse(is.na(b_preference), mean(b_preference, na.rm = TRUE), b_preference),
    
    imp_c_bitterness = ifelse(is.na(c_bitterness), mean(c_bitterness, na.rm = TRUE), c_bitterness),
    imp_c_acidity = ifelse(is.na(c_acidity), mean(c_acidity, na.rm = TRUE), c_acidity),
    imp_c_preference = ifelse(is.na(c_preference), mean(c_preference, na.rm = TRUE), c_preference),
    
    imp_d_bitterness = ifelse(is.na(d_bitterness), mean(d_bitterness, na.rm = TRUE), d_bitterness),
    imp_d_acidity = ifelse(is.na(d_acidity), mean(d_acidity, na.rm = TRUE), d_acidity)
  )

mean_income <- mean(train_df$n_hh_income, na.rm = TRUE)
train_df$n_hh_income[is.na(train_df$n_hh_income)] <- mean_income

table(train_df$n_party)

### Building OLS model-----------------

model <- glm(outcome ~ n_age + n_hh_income +
               factor(n_abc_prefer) + factor(n_party) + factor(n_state) +
               factor(n_favorite_drink) + factor(n_add) + 
               factor(n_prefer_kind) + factor(n_prefer_strength) + 
               factor(n_prefer_roast) + factor(n_prefer_caffeine) + 
               factor(n_gender) + factor(n_education) +
               
               # Missing indicators for categorical variables
               missing_abc_prefer + missing_party + missing_state +
               missing_favorite_drink + missing_add + missing_prefer_kind +
               missing_prefer_strength + missing_prefer_roast +
               missing_prefer_caffeine + missing_gender + missing_education +
               
               # Quadratic terms for bitterness, acidity, and preference variables
               I(imp_a_bitterness^2) + I(imp_a_acidity^2) + I(imp_a_preference^2) +
               I(imp_b_bitterness^2) + I(imp_b_acidity^2) + I(imp_b_preference^2) +
               I(imp_c_bitterness^2) + I(imp_c_acidity^2) + I(imp_c_preference^2) +
               
               # Interaction terms within each letter
               imp_a_bitterness * imp_a_preference +
               imp_a_acidity * imp_a_preference +
               imp_a_bitterness * imp_a_acidity +
               
               imp_b_bitterness * imp_b_preference +
               imp_b_acidity * imp_b_preference +
               imp_b_bitterness * imp_b_acidity +
               
               imp_c_bitterness * imp_c_preference +
               imp_c_acidity * imp_c_preference +
               imp_c_bitterness * imp_c_acidity +
               
               # Interaction terms between different letters
               imp_a_bitterness * imp_b_bitterness +
               imp_a_bitterness * imp_c_bitterness +
               imp_b_bitterness * imp_c_bitterness +
               
               imp_a_acidity * imp_b_acidity +
               imp_a_acidity * imp_c_acidity +
               imp_b_acidity * imp_c_acidity +
               
               imp_a_preference * imp_b_preference +
               imp_a_preference * imp_c_preference +
               imp_b_preference * imp_c_preference +
               
               # Cross-interactions between bitterness, acidity, and preference from different letters
               imp_a_bitterness * imp_b_preference +
               imp_a_bitterness * imp_c_preference +
               imp_b_bitterness * imp_a_preference +
               imp_b_bitterness * imp_c_preference +
               imp_c_bitterness * imp_a_preference +
               imp_c_bitterness * imp_b_preference +
               
               imp_a_acidity * imp_b_preference +
               imp_a_acidity * imp_c_preference +
               imp_b_acidity * imp_a_preference +
               imp_b_acidity * imp_c_preference +
               imp_c_acidity * imp_a_preference +
               imp_c_acidity * imp_b_preference +
               
               # Missing indicators for numeric variables
               missing_a_bitterness + missing_a_acidity + missing_a_preference +
               missing_b_bitterness + missing_b_acidity + missing_b_preference +
               missing_c_bitterness + missing_c_acidity + missing_c_preference +
               missing_d_bitterness + missing_d_acidity +
               
               # Additional preference-based interaction terms
               factor(n_prefer_kind) * factor(n_favorite_drink) +
               factor(n_prefer_kind) * factor(n_prefer_caffeine) +
               factor(n_favorite_drink) * factor(n_prefer_caffeine) +
               
               imp_a_preference * factor(n_prefer_kind) +
               imp_b_preference * factor(n_prefer_kind) +
               imp_c_preference * factor(n_prefer_kind) +
               
               imp_a_preference * factor(n_favorite_drink) +
               imp_b_preference * factor(n_favorite_drink) +
               imp_c_preference * factor(n_favorite_drink) +
               
               imp_a_preference * factor(n_prefer_caffeine) +
               imp_b_preference * factor(n_prefer_caffeine) +
               imp_c_preference * factor(n_prefer_caffeine),
             data = train_df)


summary(model)

pred_out <- predict(model, newdata = train_df, type = "response")
sum(round(pred_out) == train_df$outcome)
mean(round(pred_out) == train_df$outcome)
sum(abs(pred_out - train_df$outcome))

###### Trying ordinal logit --------------

# for (var in c("n_age", "n_hh_income", "n_abc_prefer", "n_party", "n_state", 
#               "n_favorite_drink", "n_add", "n_prefer_kind", "n_prefer_strength", 
#               "n_prefer_roast", "n_prefer_caffeine", "n_gender", "n_education")) {
#   print(table(train_df_ordinal[[var]], train_df_ordinal$outcome))
# }
# 
# colSums(is.na(train_df_ordinal))


library(MASS)
library(forcats)

train_df_ordinal <- train_df

train_df_ordinal$ord_outcome <- factor(train_df_ordinal$outcome)
table(train_df_ordinal$ord_outcome)
table(train_df$outcome)


# train_df_ordinal <- train_df_ordinal %>%
#   mutate(
#     n_state = fct_lump(n_state, n = 10),  # Keep top 10 states, lump others into "Other"
#     n_favorite_drink = fct_lump(n_favorite_drink, n = 6),
#     n_party = fct_lump(n_party, n = 3),
#     n_add = fct_lump(n_add, n = 5)
#   )

sum(is.na(train_df$missing_abc_prefer))

ord_model <- polr(ord_outcome ~
                    n_age +
                    imp_a_bitterness:imp_a_preference +
                    imp_b_bitterness:imp_b_preference +
                    imp_c_bitterness:imp_c_preference +
                    imp_a_acidity:imp_a_preference +
                    imp_b_acidity:imp_b_preference +
                    imp_c_acidity:imp_c_preference +
                    imp_d_bitterness:imp_a_preference +
                    imp_d_bitterness:imp_b_preference +
                    imp_d_bitterness:imp_c_preference +
                    imp_d_acidity:imp_c_preference +
                    
                    n_race + n_marital +
                    
                    I(imp_a_bitterness^2) + I(imp_a_acidity^2) + I(imp_a_preference^2) +
                    I(imp_b_bitterness^2) + I(imp_b_acidity^2) + I(imp_b_preference^2) +
                    I(imp_c_bitterness^2) + I(imp_c_acidity^2) +
                    I(imp_d_bitterness^2) + I(imp_d_acidity^2) +
                    factor(n_abc_prefer) + factor(n_party) +
                    factor(n_prefer_kind) + factor(n_prefer_strength) +
                    factor(n_prefer_roast) + factor(n_prefer_caffeine) +
                    factor(n_gender) + factor(n_education) 
                  ,
                  data = train_df_ordinal,
                  method = "logistic", Hess = TRUE)

vif(ord_model)
summary(ord_model)
pred_out <- predict(ord_model, newdata = train_df_ordinal, type = "class")
table(pred_out)
fixed_pred_out <- as.numeric(pred_out) - 3
table(fixed_pred_out)
sum(fixed_pred_out == train_df$outcome)
mean(fixed_pred_out == train_df$outcome)
sum(abs(fixed_pred_out - train_df$outcome))
sum(abs(fixed_pred_out - train_df$outcome))/length(train_df$outcome)
vif(ord_model)




### Troubleshooting--------------
table(train_df_ordinal$ord_outcome)
pred_probs <- predict(ord_model, newdata = train_df_ordinal, type = "probs")
apply(pred_probs, 2, summary)

library(car)
vif(ord_model)

##########

numeric_vars <- train_df_ordinal_testing[, sapply(train_df_ordinal_testing, is.numeric)]
correlations <- cor(numeric_vars, train_df_ordinal_testing$outcome, use = "complete.obs")
correlations <- sort(correlations[,1], decreasing = TRUE)
print(correlations)

str(numeric_vars)
dim(numeric_vars)
sum(is.na(train_df_ordinal_testing$outcome))
complete_cases <- complete.cases(numeric_vars, train_df_ordinal_testing$outcome)
sum(complete_cases)  # Should be > 0

###########

train_df_ordinal_testing <- train_df_ordinal

# Identify numeric columns
numeric_vars <- names(train_df_ordinal_testing)[sapply(train_df_ordinal_testing, is.numeric)]
print(numeric_vars)

# Initialize a list to store correlations
cor_results <- list()

# Loop through each numeric variable and compute correlation with outcome
for (var in numeric_vars) {
  # Create a temporary dataset excluding NA values for the current variable and outcome
  temp_data <- train_df_ordinal_testing[, c(var, "outcome"), drop = FALSE]
  temp_data <- na.omit(temp_data)
  
  # Compute correlation if there are enough valid observations
  if (nrow(temp_data) > 1) {
    cor_value <- cor(temp_data[[var]], temp_data$outcome, use = "complete.obs")
    cor_results[[var]] <- cor_value
  } else {
    cor_results[[var]] <- NA  # Not enough data for correlation
  }
}

# Convert to a sorted dataframe for easy viewing
cor_results_df <- data.frame(variable = names(cor_results), correlation = unlist(cor_results))
cor_results_df <- cor_results_df[order(-abs(cor_results_df$correlation)), ]  # Sort by absolute correlation

# Display results
print(cor_results_df)



######## Improving model -----------

# model_selected <- stepAIC(model, direction = "both")
model_selected <- stepAIC(ord_model, direction = "both")
summary(model_selected)

pred_out_selected <- predict(model_selected, newdata = train_df, type = "class")
fixed_pred_out_selected <- as.numeric(pred_out) - 3
pred_rounded <- round(fixed_pred_out_selected)
mean(pred_rounded == train_df$outcome)
sum(pred_rounded == train_df$outcome)
sum(abs(fixed_pred_out_selected - train_df$outcome))
sum(abs(fixed_pred_out_selected - train_df$outcome))/length(train_df$outcome)


##### LASSO --------------
library(glmnet)

train_df_lasso <- train_df %>%
  mutate(across(where(is.character), as.factor))

train_df_lasso <- train_df_lasso %>%
  mutate(
    n_age_sq = n_age^2,
    n_hh_income_sq = n_hh_income^2,
    imp_a_bitterness_sq = imp_a_bitterness^2,
    imp_a_acidity_sq = imp_a_acidity^2,
    imp_a_preference_sq = imp_a_preference^2,
    imp_b_bitterness_sq = imp_b_bitterness^2,
    imp_b_acidity_sq = imp_b_acidity^2,
    imp_b_preference_sq = imp_b_preference^2,
    imp_c_bitterness_sq = imp_c_bitterness^2,
    imp_c_acidity_sq = imp_c_acidity^2,
    imp_c_preference_sq = imp_c_preference^2
  )

train_df_lasso <- train_df_lasso %>%
  mutate(
    imp_a_bitterness_imp_a_preference = imp_a_bitterness * imp_a_preference,
    imp_a_acidity_imp_a_preference = imp_a_acidity * imp_a_preference,
    imp_a_bitterness_imp_a_acidity = imp_a_bitterness * imp_a_acidity,
    
    imp_b_bitterness_imp_b_preference = imp_b_bitterness * imp_b_preference,
    imp_b_acidity_imp_b_preference = imp_b_acidity * imp_b_preference,
    imp_b_bitterness_imp_b_acidity = imp_b_bitterness * imp_b_acidity,
    
    imp_c_bitterness_imp_c_preference = imp_c_bitterness * imp_c_preference,
    imp_c_acidity_imp_c_preference = imp_c_acidity * imp_c_preference,
    imp_c_bitterness_imp_c_acidity = imp_c_bitterness * imp_c_acidity
  )



x <- data.matrix(train_df_lasso[, c(
  'n_age', 'n_hh_income', 'n_abc_prefer', 'n_party', 'n_state', 
  'n_favorite_drink', 'n_add', 'n_prefer_kind', 'n_prefer_strength', 
  'n_prefer_roast', 'n_prefer_caffeine', 'n_gender', 'n_education', 
  
  # Quadratic terms
  'n_age_sq', 'n_hh_income_sq', 'imp_a_bitterness_sq', 'imp_a_acidity_sq', 'imp_a_preference_sq',
  'imp_b_bitterness_sq', 'imp_b_acidity_sq', 'imp_b_preference_sq',
  'imp_c_bitterness_sq', 'imp_c_acidity_sq', 'imp_c_preference_sq',
  
  # Interaction terms
  'imp_a_bitterness_imp_a_preference', 'imp_a_acidity_imp_a_preference', 'imp_a_bitterness_imp_a_acidity',
  'imp_b_bitterness_imp_b_preference', 'imp_b_acidity_imp_b_preference', 'imp_b_bitterness_imp_b_acidity',
  'imp_c_bitterness_imp_c_preference', 'imp_c_acidity_imp_c_preference', 'imp_c_bitterness_imp_c_acidity'
)])

y <- train_df_lasso$outcome

# Run LASSO using cross-validation
lasso_cv <- cv.glmnet(x, y, alpha = 0.5, family = "multinomial")

# Get the best lambda
best_lambda <- lasso_cv$lambda.min

# Fit the final LASSO model using the best lambda
lasso_model <- glmnet(x, y, alpha = 0.5, lambda = best_lambda, family = "multinomial")

# Predict on training data
lasso_preds <- predict(lasso_model, newx = x, type = "response")

# Evaluate performance
mean(round(lasso_preds) == y)



### Prepping testing data---------

test_df <- read.csv("testing.csv")

test_df <- test_df %>%
  mutate(
    n_age = case_when(
      age == "<18 years old" ~ 15,
      age == "18-24 years old" ~ 21,
      age == "25-34 years old" ~ 29.5,
      age == "35-44 years old" ~ 39.5,
      age == "45-54 years old" ~ 49.5,
      age == "55-64 years old" ~ 59.5,
      age == ">65 years old" ~ 70,
      TRUE ~ NA_real_
    ),
    n_hh_income = case_when(
      hh_income == "Under $25,000" ~ 12500,
      hh_income == "$25,000 - $49,999" ~ 37500,
      hh_income == "$50,000 - $74,999" ~ 62500,
      hh_income == "$75,000 - $99,999" ~ 87500,
      hh_income == "$100,000 - $149,999" ~ 125000,
      hh_income == "$150,000 or more" ~ 200000,
      TRUE ~ NA_real_
    ),
    
    # Missing indicators for categorical variables
    missing_abc_prefer = ifelse(is.na(abc_prefer), 1, 0),
    missing_party = ifelse(is.na(party), 1, 0),
    missing_state = ifelse(is.na(state), 1, 0),
    missing_favorite_drink = ifelse(is.na(favorite_drink), 1, 0),
    missing_add = ifelse(is.na(add), 1, 0),
    missing_prefer_kind = ifelse(is.na(prefer_kind), 1, 0),
    missing_prefer_strength = ifelse(is.na(prefer_strength), 1, 0),
    missing_prefer_roast = ifelse(is.na(prefer_roast), 1, 0),
    missing_prefer_caffeine = ifelse(is.na(prefer_caffeine), 1, 0),
    missing_gender = ifelse(is.na(gender), 1, 0),
    missing_education = ifelse(is.na(education), 1, 0),
    
    # Impute categorical variables
    n_abc_prefer = factor(replace_na(abc_prefer, "unknown")),
    n_party = factor(replace_na(party, "unknown")),
    n_state = factor(replace_na(state, "unknown")),
    n_favorite_drink = factor(replace_na(favorite_drink, "unknown")),
    n_add = factor(replace_na(add, "unknown")),
    n_prefer_kind = factor(replace_na(prefer_kind, "unknown")),
    n_prefer_strength = factor(replace_na(prefer_strength, "unknown")),
    n_prefer_roast = factor(replace_na(prefer_roast, "unknown")),
    n_prefer_caffeine = factor(replace_na(prefer_caffeine, "unknown")),
    n_gender = factor(replace_na(gender, "unknown")),
    n_education = factor(replace_na(education, "unknown")),
    n_race = factor(replace_na(ethnicity_race, "unknown")),
    n_marital = case_when(
      marital_status == "Divorced" ~ "unknown",
      marital_status == "Widowed" ~ "unknown"
    ),
    n_marital = factor(replace_na(marital_status, "unknown")),
    
    # Missing indicators for numeric variables
    missing_a_bitterness = ifelse(is.na(a_bitterness), 1, 0),
    missing_a_acidity = ifelse(is.na(a_acidity), 1, 0),
    missing_a_preference = ifelse(is.na(a_preference), 1, 0),
    
    missing_b_bitterness = ifelse(is.na(b_bitterness), 1, 0),
    missing_b_acidity = ifelse(is.na(b_acidity), 1, 0),
    missing_b_preference = ifelse(is.na(b_preference), 1, 0),
    
    missing_c_bitterness = ifelse(is.na(c_bitterness), 1, 0),
    missing_c_acidity = ifelse(is.na(c_acidity), 1, 0),
    missing_c_preference = ifelse(is.na(c_preference), 1, 0),
    
    missing_d_bitterness = ifelse(is.na(d_bitterness), 1, 0),
    missing_d_acidity = ifelse(is.na(d_acidity), 1, 0),
    
    # Impute numeric variables
    imp_a_bitterness = ifelse(is.na(a_bitterness), mean(a_bitterness, na.rm = TRUE), a_bitterness),
    imp_a_acidity = ifelse(is.na(a_acidity), mean(a_acidity, na.rm = TRUE), a_acidity),
    imp_a_preference = ifelse(is.na(a_preference), mean(a_preference, na.rm = TRUE), a_preference),
    
    imp_b_bitterness = ifelse(is.na(b_bitterness), mean(b_bitterness, na.rm = TRUE), b_bitterness),
    imp_b_acidity = ifelse(is.na(b_acidity), mean(b_acidity, na.rm = TRUE), b_acidity),
    imp_b_preference = ifelse(is.na(b_preference), mean(b_preference, na.rm = TRUE), b_preference),
    
    imp_c_bitterness = ifelse(is.na(c_bitterness), mean(c_bitterness, na.rm = TRUE), c_bitterness),
    imp_c_acidity = ifelse(is.na(c_acidity), mean(c_acidity, na.rm = TRUE), c_acidity),
    imp_c_preference = ifelse(is.na(c_preference), mean(c_preference, na.rm = TRUE), c_preference),
    
    imp_d_bitterness = ifelse(is.na(d_bitterness), mean(d_bitterness, na.rm = TRUE), d_bitterness),
    imp_d_acidity = ifelse(is.na(d_acidity), mean(d_acidity, na.rm = TRUE), d_acidity)
  )


mean_income <- mean(test_df$n_hh_income, na.rm = TRUE)

test_df$n_hh_income[is.na(test_df$n_hh_income)] <- mean_income

### Ordinal test prep----------

# test_df <- train_df_ordinal %>%
#   mutate(
#     n_state = fct_lump(n_state, n = 10),  # Keep top 10 states, lump others into "Other"
#     n_favorite_drink = fct_lump(n_favorite_drink, n = 6),
#     n_party = fct_lump(n_party, n = 3),
#     n_add = fct_lump(n_add, n = 5)
#   )

### Second Test -----------------------------
alt_test <- read.csv("alt_test.csv")

alt_test <- alt_test %>%
  mutate(
    n_age = case_when(
      age == "<18 years old" ~ 15,
      age == "18-24 years old" ~ 21,
      age == "25-34 years old" ~ 29.5,
      age == "35-44 years old" ~ 39.5,
      age == "45-54 years old" ~ 49.5,
      age == "55-64 years old" ~ 59.5,
      age == ">65 years old" ~ 70,
      TRUE ~ NA_real_
    ),
    n_hh_income = case_when(
      hh_income == "Under $25,000" ~ 12500,
      hh_income == "$25,000 - $49,999" ~ 37500,
      hh_income == "$50,000 - $74,999" ~ 62500,
      hh_income == "$75,000 - $99,999" ~ 87500,
      hh_income == "$100,000 - $149,999" ~ 125000,
      hh_income == "$150,000 or more" ~ 200000,
      TRUE ~ NA_real_
    ),
    
    # Missing indicators for categorical variables
    missing_abc_prefer = ifelse(is.na(abc_prefer), 1, 0),
    missing_party = ifelse(is.na(party), 1, 0),
    missing_state = ifelse(is.na(state), 1, 0),
    missing_favorite_drink = ifelse(is.na(favorite_drink), 1, 0),
    missing_add = ifelse(is.na(add), 1, 0),
    missing_prefer_kind = ifelse(is.na(prefer_kind), 1, 0),
    missing_prefer_strength = ifelse(is.na(prefer_strength), 1, 0),
    missing_prefer_roast = ifelse(is.na(prefer_roast), 1, 0),
    missing_prefer_caffeine = ifelse(is.na(prefer_caffeine), 1, 0),
    missing_gender = ifelse(is.na(gender), 1, 0),
    missing_education = ifelse(is.na(education), 1, 0),
    
    
    # Impute categorical variables
    n_abc_prefer = factor(replace_na(abc_prefer, "unknown")),
    n_party = factor(replace_na(party, "unknown")),
    n_state = factor(replace_na(state, "unknown")),
    n_favorite_drink = factor(replace_na(favorite_drink, "unknown")),
    n_add = factor(replace_na(add, "unknown")),
    n_prefer_kind = factor(replace_na(prefer_kind, "unknown")),
    n_prefer_strength = factor(replace_na(prefer_strength, "unknown")),
    n_prefer_roast = factor(replace_na(prefer_roast, "unknown")),
    n_prefer_caffeine = factor(replace_na(prefer_caffeine, "unknown")),
    n_gender = factor(replace_na(gender, "unknown")),
    n_education = factor(replace_na(education, "unknown")),
    n_race = factor(replace_na(ethnicity_race, "unknown")),
    n_marital = case_when(
      marital_status == "Divorced" ~ "unknown",
      marital_status == "Widowed" ~ "unknown"
    ),
    n_marital = factor(replace_na(marital_status, "unknown")),
    
    # Missing indicators for numeric variables
    missing_a_bitterness = ifelse(is.na(a_bitterness), 1, 0),
    missing_a_acidity = ifelse(is.na(a_acidity), 1, 0),
    missing_a_preference = ifelse(is.na(a_preference), 1, 0),
    
    missing_b_bitterness = ifelse(is.na(b_bitterness), 1, 0),
    missing_b_acidity = ifelse(is.na(b_acidity), 1, 0),
    missing_b_preference = ifelse(is.na(b_preference), 1, 0),
    
    missing_c_bitterness = ifelse(is.na(c_bitterness), 1, 0),
    missing_c_acidity = ifelse(is.na(c_acidity), 1, 0),
    missing_c_preference = ifelse(is.na(c_preference), 1, 0),
    
    missing_d_bitterness = ifelse(is.na(d_bitterness), 1, 0),
    missing_d_acidity = ifelse(is.na(d_acidity), 1, 0),
    
    # Impute numeric variables
    imp_a_bitterness = ifelse(is.na(a_bitterness), mean(a_bitterness, na.rm = TRUE), a_bitterness),
    imp_a_acidity = ifelse(is.na(a_acidity), mean(a_acidity, na.rm = TRUE), a_acidity),
    imp_a_preference = ifelse(is.na(a_preference), mean(a_preference, na.rm = TRUE), a_preference),
    
    imp_b_bitterness = ifelse(is.na(b_bitterness), mean(b_bitterness, na.rm = TRUE), b_bitterness),
    imp_b_acidity = ifelse(is.na(b_acidity), mean(b_acidity, na.rm = TRUE), b_acidity),
    imp_b_preference = ifelse(is.na(b_preference), mean(b_preference, na.rm = TRUE), b_preference),
    
    imp_c_bitterness = ifelse(is.na(c_bitterness), mean(c_bitterness, na.rm = TRUE), c_bitterness),
    imp_c_acidity = ifelse(is.na(c_acidity), mean(c_acidity, na.rm = TRUE), c_acidity),
    imp_c_preference = ifelse(is.na(c_preference), mean(c_preference, na.rm = TRUE), c_preference),
    
    imp_d_bitterness = ifelse(is.na(d_bitterness), mean(d_bitterness, na.rm = TRUE), d_bitterness),
    imp_d_acidity = ifelse(is.na(d_acidity), mean(d_acidity, na.rm = TRUE), d_acidity)
  )


mean_income <- mean(alt_test$n_hh_income, na.rm = TRUE)

alt_test$n_hh_income[is.na(alt_test$n_hh_income)] <- mean_income

########## Testing data------------

#Ordinal
pred_out_testing <- predict(ord_model, newdata = test_df, type = "class")
fixed_pred_out_testing <- as.numeric(pred_out_testing) - 3
table(fixed_pred_out_testing)

### Second Test-------------------

pred_out_testing <- predict(ord_model, newdata = alt_test, type = "class")
fixed_pred_out_testing <- as.numeric(pred_out_testing) - 3
table(fixed_pred_out_testing)

sum(fixed_pred_out_testing == alt_test$outcome)
mean(fixed_pred_out_testing == alt_test$outcome)
sum(abs(fixed_pred_out_testing - alt_test$outcome))
sum(abs(fixed_pred_out_testing - alt_test$outcome))/length(alt_test$outcome)

### End -------------

#Improved model
pred_out_testing <- predict(model_selected, newdata = test_df, type = "class")

### Second Test -------------
fixed_pred_out_testing <- as.numeric(pred_out_testing) - 3
table(fixed_pred_out_testing)

sum(fixed_pred_out_testing == alt_test$outcome)
mean(fixed_pred_out_testing == alt_test$outcome)
sum(abs(fixed_pred_out_testing - alt_test$outcome))
sum(abs(fixed_pred_out_testing - alt_test$outcome))/length(alt_test$outcome)

### End-----------

submission <- data.frame(
  submission_id = test_df$submission_id,
  outcome = pred_out_testing
)



write.csv(file = "submissionv11_post_AIC_col_fixes.csv", submission, row.names = FALSE)
