library(lme4)
library(readxl)
library(ggplot2)
library(plm)
library(dplyr)

data <- read_excel("GACTT_RESULTS_ANONYMIZED_HW4.xlsx")

model <- lm(most_willing_for_cup ~ hh_income, data = data)
summary(model)

ggplot(data, aes(x = hh_income, y = most_willing_for_cup)) +
  geom_jitter(width = 1.5, height = 0.4, size = 1) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ age_cat)

ggplot(data, aes(x = hh_income, y = most_willing_for_cup)) +
  geom_smooth(method = "lm", se = FALSE) +
  geom_jitter(width = 1.5, height = 0.4, size = 1)

fe_one_way <- plm(most_willing_for_cup ~ hh_income + factor(age_cat), 
                  data = data, 
                  index = "age_cat", 
                  model = "within")
summary(fe_one_way)

age_cat_list <- unique(data$age_cat)
state_list <- unique(data$state)

agg_data <- data.frame(
  age_cat = rep(age_cat_list, length(state_list)),
  state = rep(state_list, each = length(age_cat_list))
)

agg_data <- agg_data %>%
  left_join(
    data %>%
      group_by(age_cat, state) %>%
      summarise(
        most_willing_for_cup = mean(most_willing_for_cup),
        hh_income = mean(hh_income),
        .groups = "drop"
      ),
    by = c("age_cat", "state")
  )

fe_two_way <- plm(most_willing_for_cup ~ hh_income + factor(age_cat) + factor(state), 
                  data = agg_data, 
                  index = c("age_cat", "state"), 
                  model = "within")
summary(fe_two_way)

fe_two_way <- plm(most_willing_for_cup ~ hh_income + factor(state) + factor(age_cat), 
                  data = agg_data, 
                  index = c("state", "age_cat"), 
                  model = "within")
summary(fe_two_way)


random_intercept_model <- lmer(most_willing_for_cup ~ hh_income + (1 | age_cat), data = data)
summary(random_intercept_model)

data <- data %>%
  mutate(hh_income_scaled = scale(hh_income))

random_slope_model <- lmer(most_willing_for_cup ~ hh_income_scaled + (hh_income_scaled | age_cat), data = data)
summary(random_slope_model)

data <- data %>%
  mutate(hh_income_scaled = scale(hh_income))

random_intercept_slope_model <- lmer(most_willing_for_cup ~ hh_income_scaled + (1 + hh_income_scaled | age_cat), data = data)
summary(random_intercept_slope_model)
