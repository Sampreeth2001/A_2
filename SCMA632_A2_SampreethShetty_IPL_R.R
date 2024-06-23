#Installing necessary libraries
install.packages('fuzzyjoin')
installed.packages('stringdist')


#Loading necassary libraries
library(dplyr)
library(readxl)
library(car)
library(fuzzyjoin)

setwd('D:\\R Studio')
df_ipl <- read.csv("IPL_ball_by_ball_updated till 2024.csv", stringsAsFactors = FALSE)
salary <- read_excel("IPL SALARIES 2024.xlsx")
print(colnames(df_ipl))

#Grouping the data
grouped_data = df_ipl %>%
  group_by(Season, `Innings.No`, Striker, Bowler) %>%
  summarise(runs_scored = sum(runs_scored), wicket_confirmation = sum(wicket_confirmation), .groups = 'drop')

print(grouped_data)

#Total runs and wickets for each year
total_runs_each_year = grouped_data %>%
  group_by(Season, Striker) %>%
  summarise(runs_scored = sum(runs_scored), .groups = 'drop')
total_wicket_each_year = grouped_data %>%
  group_by(Season, Bowler) %>%
  summarise(wicket_confirmation = sum(wicket_confirmation), .groups = 'drop')

print(total_runs_each_year)

# Function to match names with a threshold
match_names <- function(name, names_list) {
  matched <- stringdist::amatch(name, names_list, maxDist = 20)
  ifelse(is.na(matched), NA, names_list[matched])
}

# Match player names between salary and runs datasets
df_salary <- salary %>% mutate(Player = as.character(Player))
df_runs <- total_runs_each_year %>% mutate(Striker = as.character(Striker))


df_salary <- df_salary %>%
  rowwise() %>%
  mutate(Matched_Player = match_names(Player, df_runs$Striker)) %>%
  ungroup()

# Merge datasets based on matched player names
df_merged <- left_join(df_salary, df_runs, by = c("Matched_Player" = "Striker"))

# Subset data for the last three years
df_merged <- df_merged %>%
  filter(Season %in% c('2021', '2022', '2023'))

print(unique(df_merged$Season))
print(head(df_merged))


# Linear regression for runs scored
X <- df_merged$runs_scored
y <- as.numeric(df_merged$Rs)
model <- lm(y ~ X)
summary(model)

# Match player names between salary and wickets datasets
df_wickets <- total_wicket_each_year %>% mutate(Bowler = as.character(Bowler))

df_salary <- df_salary %>%
  rowwise() %>%
  mutate(Matched_Player = match_names(Player, df_wickets$Bowler)) %>%
  ungroup()

# Merge datasets based on matched player names
df_merged_wickets <- left_join(df_salary, df_wickets, by = c("Matched_Player" = "Bowler"))

# Filter for players with more than 10 wickets
df_merged_wickets <- df_merged_wickets %>%
  filter(wicket_confirmation > 10)

# Subset data for the last year
df_merged_wickets <- df_merged_wickets %>%
  filter(Season == '2022')

print(df_merged_wickets)

# Linear regression for wicket confirmation
X_wickets <- df_merged_wickets$wicket_confirmation
y_wickets <- as.numeric(df_merged_wickets$Rs)
model_wickets <- lm(y_wickets ~ X_wickets)
summary(model_wickets)