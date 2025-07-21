library(naniar)
library(visdat)
library(dplyr)
library(tidyr)

train_df <- read.csv("training.csv")
test_df <- read.csv("testing.csv")

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

### Building model-----------------

model <- glm(outcome ~ n_age + n_hh_income +
               factor(n_abc_prefer) + factor(n_party) + factor(n_state) +
               factor(n_favorite_drink) + factor(n_add) + 
               factor(n_prefer_kind) + factor(n_prefer_strength) + 
               factor(n_prefer_roast) + factor(n_prefer_caffeine) + 
               factor(n_gender) + factor(n_education) +
               
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

model <- glm(outcome ~ (imp_a_bitterness + imp_a_acidity + imp_a_preference + 
                          imp_b_bitterness + imp_b_acidity + imp_b_preference + 
                          imp_c_bitterness + imp_c_acidity + imp_c_preference)^10, 
             data = train_df)
summary(model)


pred_out <- predict(model, newdata = train_df, type = "response")
sum(round(pred_out) == train_df$outcome)
mean(round(pred_out) == train_df$outcome)
sum(abs(pred_out - train_df$outcome))



######## Improving model -----------

library(MASS)
model_selected <- stepAIC(model, direction = "both")
summary(model_selected)

pred_out_selected <- predict(model_selected, newdata = train_df, type = "response")
pred_rounded <- round(pred_out_selected)
mean(pred_rounded == train_df$outcome)
sum(pred_rounded == train_df$outcome)




table(train_df$outcome) / nrow(train_df)




### Prepping training data---------

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



########## Testing data------------


pred_out_testing <- predict(model_selected, newdata = test_df, type = "response")
pred_out_testing

submission <- data.frame(
  id = test_df$submission_id,
  prediction = pred_out_testing
)

write.csv(file = "submission.csv", submission)
