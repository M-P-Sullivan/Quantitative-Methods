###### Packages------------

library(naniar)
library(visdat)
library(dplyr)
library(tidyr)
library(MASS)
library(MASS)
library(forcats)

train_df <- read.csv("training.csv")

###### Prepping training data---------

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

###### Trying ordinal logit --------------
train_df_ordinal <- train_df

train_df_ordinal$ord_outcome <- factor(train_df_ordinal$outcome)
table(train_df_ordinal$ord_outcome)
table(train_df$outcome)



sum(is.na(train_df$missing_abc_prefer))

ord_model <- polr(
  ord_outcome ~ imp_a_bitterness +
    imp_b_acidity +
    imp_c_bitterness + imp_c_acidity +
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
    imp_d_acidity:imp_a_preference +
    imp_d_acidity:imp_b_preference +
    imp_d_acidity:imp_c_preference +

    I(imp_a_bitterness^2) + I(imp_a_acidity^2) + I(imp_a_preference^2) +
    I(imp_b_bitterness^2) + I(imp_b_acidity^2) + I(imp_b_preference^2) +
    I(imp_c_bitterness^2) + I(imp_c_acidity^2) + I(imp_c_preference^2) +
    I(imp_d_bitterness^2) + I(imp_d_acidity^2) +
    factor(n_abc_prefer) + factor(n_party) +
    factor(n_prefer_kind) + factor(n_prefer_strength) +
    factor(n_prefer_roast) + factor(n_prefer_caffeine) +
    factor(n_gender) + factor(n_education),
  data = train_df_ordinal,
  method = "logistic", Hess = TRUE
)

summary(ord_model)
pred_out <- predict(ord_model, newdata = train_df_ordinal, type = "class")
table(pred_out)
fixed_pred_out <- as.numeric(pred_out) - 3
table(fixed_pred_out)
sum(fixed_pred_out == train_df$outcome)
mean(fixed_pred_out == train_df$outcome)
sum(abs(fixed_pred_out - train_df$outcome))
sum(abs(fixed_pred_out - train_df$outcome)) / length(train_df$outcome)
vif(ord_model)


# Troubleshooting
table(train_df_ordinal$ord_outcome)
pred_probs <- predict(ord_model, newdata = train_df_ordinal, type = "probs")
apply(pred_probs, 2, summary)

library(car)
vif(ord_model)


###### Improving model -----------

# model_selected <- stepAIC(model, direction = "both")
model_selected <- stepAIC(ord_model, direction = "both")
summary(model_selected)

pred_out_selected <- predict(model_selected, newdata = train_df, type = "class")
fixed_pred_out_selected <- as.numeric(pred_out) - 3
pred_rounded <- round(fixed_pred_out_selected)
mean(pred_rounded == train_df$outcome)
sum(pred_rounded == train_df$outcome)
sum(abs(fixed_pred_out_selected - train_df$outcome))
sum(abs(fixed_pred_out_selected - train_df$outcome)) / length(train_df$outcome)

###### Prepping testing data---------

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

###### Testing data------------

# Ordinal
pred_out_testing <- predict(ord_model, newdata = test_df, type = "class")
fixed_pred_out_testing <- as.numeric(pred_out_testing) - 3
table(fixed_pred_out_testing)

# Improved model
pred_out_testing <- predict(model_selected, newdata = test_df, type = "class")

submission <- data.frame(
  submission_id = test_df$submission_id,
  outcome = pred_out_testing
)



write.csv(file = "submissionv11_post_AIC_col_fixes.csv", submission, row.names = FALSE)
