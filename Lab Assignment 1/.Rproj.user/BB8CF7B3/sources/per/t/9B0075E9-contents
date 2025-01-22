library(ggplot2)
library(dplyr)
library(openxlsx)

#Reading in and checking the coffee data
survey_df <- read.csv("GACTT_RESULTS_ANONYMIZED_HW1.csv")

print(head(survey_df,6))
print(tail(survey_df,4))
print(sapply(survey_df,class))

survey_df$party <- factor(survey_df$party, levels = c("Democrat", "Republican", "Independent", "No affiliation"))

#Plotting a histogram of cups/day by political party
ggplot(survey_df, aes(x = cups_num, fill = party)) +
  geom_histogram(position = position_dodge(width = 0.9, preserve = "single"),
                 binwidth = 1,
                 color = "black") +
  scale_x_continuous(
    breaks = seq(0, 5, by = 1),
    labels = seq(0, 5, by = 1) 
  ) +
  scale_fill_manual(
    values = c("Democrat" = "blue", 
               "Republican" = "red", 
               "Independent" = "green", 
               "No affiliation" = "orange")
  ) +
  labs(title = "Histogram of Cups of Coffee Per Day by Political Party",
       x = "Number of Cups Per Day",
       y = "Responses",
       fill = "Political Party") +
  theme_minimal()

#Reading in, cleaning, and merging geographic data
zip_df <- read.csv("zip_code_database.csv")
survey_df$zip <- as.integer(survey_df$zip)
joined_df <- left_join(survey_df,zip_df, by = "zip")

print(sum(!(joined_df$zip %in% zip_df$zip)))

#Function to calculate the mode of a column
calculate_mode <- function(x) {
  unique_x <- unique(x)
  counts <- tabulate(match(x, unique_x))
  modes <- unique_x[counts == max(counts)]
  paste(modes, collapse = ", ")
}

#Creating a new data frame with results by state
survey_state <- group_by(joined_df, joined_df$state) %>% summarize(avg_cups = mean(cups_num, na.rm=TRUE),
                                                                 preferred_method = calculate_mode(home_brew),
                                                                 pct_dem = 100*sum(party == "Democrat", na.rm = TRUE)/length(party),
                                                                 pct_rep = 100*sum(party == "Republican", na.rm =TRUE)/length(party),
                                                                 num_responses = n())
colnames(survey_state)[colnames(survey_state) == "joined_df$state"] <- "state_abr"

#Reading in and cleaning election data
election_df <- read.csv("election_2024.csv")

int_columns <- c(2,3,5,6,8,9,11)
pct_columns <- c(4,7,10)

election_df[,int_columns] <- sapply(election_df[,int_columns], function(x) as.integer(gsub(",","",x)))
election_df[,pct_columns] <- sapply(election_df[,pct_columns], function(x) (1/100)*as.numeric(gsub("%","",x)))
election_df[is.na(election_df)] <- 0

#Cleaning up the states and giving them the same labels
election_df <- election_df[!election_df$state %in% c("CD-1","CD-2","CD-3"),]

state_names <- c(
  "Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", 
  "Connecticut", "Delaware", "Florida", "Georgia", "Hawaii", "Idaho", 
  "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", 
  "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota", 
  "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", 
  "New Hampshire", "New Jersey", "New Mexico", "New York", "North Carolina", 
  "North Dakota", "Ohio", "Oklahoma", "Oregon", "Pennsylvania", 
  "Rhode Island", "South Carolina", "South Dakota", "Tennessee", 
  "Texas", "Utah", "Vermont", "Virginia", "Washington", 
  "West Virginia", "Wisconsin", "Wyoming", "District of Columbia"
)

state_abbreviations <- c(
  "AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA", "HI", "ID", 
  "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", 
  "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", 
  "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VT", 
  "VA", "WA", "WV", "WI", "WY", "DC"
)

state_abr_df <- data.frame(state = state_names, abr = state_abbreviations)

election_df$state_abr <- sapply(election_df$state, function(x) state_abr_df$abr[state_abr_df$state == x])

survey_election_df <- left_join(election_df, survey_state, by = "state_abr")

#Plotting vote share against survey results for party affiliation and for coffee consumption
ggplot(data = filter(survey_election_df, num_responses > 25), aes(x = pct_dem, y = harris_votes_share)) +
  geom_point(color = "blue", size = 2, alpha = 0.7) +
  labs(
    title = "Comparison of Surveyed Party Affiliation and Vote Share",
    x = "Percent Democrat",
    y = "Harris Vote Share",
    caption = "Only observations with >25 responses in the survey"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )

ggplot(data = filter(survey_election_df, num_responses > 25), aes(x = avg_cups, y = harris_votes_share)) +
  geom_point(color = "blue", size = 2, alpha = 0.7) + 
  labs(
    title = "Comparison of Surveyed Coffee Consumption and Vote Share",
    x = "Average Number of Cups Per Day",
    y = "Harris Vote Share",
    caption = "Only observations with >25 responses in the survey"
  ) +
  theme_minimal() + 
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )

ggplot(data = filter(survey_election_df, num_responses > 25), aes(x = avg_cups, y = trump_votes_share)) +
  geom_point(color = "red", size = 2, alpha = 0.7) +
  labs(
    title = "Comparison of Surveyed Coffee Consumption and Vote Share",
    x = "Average Number of Cups Per Day",
    y = "Trump Vote Share",
    caption = "Only observations with >25 responses in the survey"
  ) +
  theme_minimal() + 
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )

ggplot(data = filter(survey_election_df, num_responses > 25), aes(x = avg_cups, y = other_votes_share)) +
  geom_point(color = "darkgreen", size = 2, alpha = 0.7) +
  labs(
    title = "Comparison of Surveyed Coffee Consumption and Vote Share",
    x = "Average Number of Cups Per Day",
    y = "Other Vote Share",
    caption = "Only observations with >25 responses in the survey"
  ) +
  theme_minimal() + 
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )

write.xlsx(survey_election_df, file = "overview_hw1.xlsx")
