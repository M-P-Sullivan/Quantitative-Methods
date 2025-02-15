#install.packages("ggplot2")
#install.packages("dplyr")
#install.packages("distributions3")

library(ggplot2)
library(dplyr)
library(distributions3)

df <- readRDS("GACTT_RESULTS_ANONYMIZED_HW2.RDS")

head(df)
nrow(df)
sum(df$a_preference==-9, na.rm=TRUE)

df$a_preference[df$a_preference == -9] <- NA
df <- df[!is.na(df$a_preference), ]
head(df)
sum(is.na(df$a_preference))

df$pourover <- df$favorite_drink == "Pourover"
sum(df$pourover, na.rm=TRUE)
sum(is.na(df$pourover))
sum(is.na(df$favorite_drink))

pourover_df <- filter(df, favorite_drink == "Pourover")
other_df <- filter(df, favorite_drink != "Pourover")

nrow(df) - nrow(pourover_df) - nrow(other_df) - sum(is.na(df$favorite_drink))

t.test(pourover_df$a_preference, other_df$a_preference)
t.test(df$a_preference[df$pourover==1], df$a_preference[df$pourover==0])

table(pourover_df$a_preference)
table(other_df$a_preference)

combined_df <- data.frame(
  a_preference = c(pourover_df$a_preference, other_df$a_preference),
  group = c(rep("Pourover", nrow(pourover_df)), rep("Other", nrow(other_df)))
)

ggplot(combined_df, aes(x = group, y = a_preference, fill = group)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Boxplot of Two Groups")

ggplot(combined_df, aes(x = group, y = a_preference, fill = group)) +
  geom_violin(trim = FALSE) +
#  geom_jitter(width = 0.4, alpha = 0.15) +
  theme_minimal() +
  labs(title = "Violin Plot of Two Groups")

ggplot(combined_df, aes(x = group, y = a_preference, fill = group)) +
  stat_summary(fun = mean, geom = "bar", position = "dodge", alpha = 0.7) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2) +
  theme_minimal() +
  labs(title = "Mean Comparison with Error Bars")

#The difference is statistically significant and the people who prefer pourover do enjoy the tasting lot more than others

strong <- c(4,5)
medium <- c(3)
light <- c(1,2)

df <- mutate(df, grouped_preferred_strength = ifelse(
  preferred_strength %in% strong, "Strong",
  ifelse(preferred_strength %in% medium, "Medium",
  ifelse(preferred_strength %in% light, "Light", NA
  ))))

sum(is.na(df$grouped_preferred_strength))
grouped_strength_df <- df[!is.na(df$grouped_preferred_strength),]

anova_result <- aov(a_preference ~ grouped_preferred_strength, data = grouped_strength_df)
summary(anova_result)

ggplot(grouped_strength_df, aes(x = grouped_preferred_strength, y = a_preference, fill = grouped_preferred_strength)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "ANOVA: Group Comparisons", x = "Group", y = "Value")

df_summary <- grouped_strength_df %>%
  group_by(grouped_preferred_strength) %>%
  summarise(mean = mean(a_preference), se = sd(a_preference)/sqrt(n()))

ggplot(df_summary, aes(x = grouped_preferred_strength, y = mean, fill = grouped_preferred_strength)) +
  geom_bar(stat = "identity", alpha = 0.7) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.2) +
  theme_minimal() +
  labs(title = "ANOVA: Mean Comparisons with Error Bars", x = "Group", y = "Mean Value")

model <- lm(df$a_preference ~ df$pourover, data = df)
summary(model)

model <- lm(df$a_preference ~ df$preferred_strength, data = df)
summary(model)

my_t_test <- function(x, comparison_mean = 0, tails = 2){
  x = x[!is.na(x)]
  n = length(x)
  print(paste("n:", n))
  x_mean = mean(x)
  print(paste("x_mean:", x_mean))
  #s = sd(x)
  s = sqrt((1/(n-1))*sum((x-x_mean)^2))
  print(paste("s:", s))
  deg_free = n-1
  print(paste("df:", deg_free))
  t_stat = (x_mean - comparison_mean)/(s/sqrt(n))
  print(paste("t_stat:", t_stat))
  T_dist = StudentsT(df = deg_free)
  if (tails == 1) {
    p = cdf(T_dist, -abs(t_stat))
  }else if (tails == 2){
    p = 1 - cdf(T_dist, abs(t_stat)) + cdf(T_dist, -abs(t_stat))
  }else{
    
  }
  print(paste("p:", p))
  p
}

test <- c(1,50000000000000,3,4,-3,-6,-2,0,30,20,NA)
test_2 <- c(1,2)
t.test(test)
my_t_test(test)
t.test(test_2)
my_t_test(test_2)
