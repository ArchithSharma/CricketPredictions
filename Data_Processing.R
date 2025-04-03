library(cricketdata)
library(dplyr)
library(pROC)
library(caret)
library(glmnet)
library(ggplot2)
library(stats)
library(datasets)
set.seed(123)



batting_data <- read.csv("battinginningdata.csv")  # Replace with your batting data file name
bowling_data <- read.csv("bowlinginningdata.csv")  # Replace with your bowling data file name
match_data_file_cricinfo = read.csv("MatchData-Cricinfo.csv") # Replace with your match data file name
# 1. Data Cleaning and Preparation

country_acronyms <- c(
  "Australia" = "AUS",
  "India" = "IND",
  "South Africa" = "RSA",
  "England" = "ENG",
  "New Zealand" = "NZL",
  "West Indies" = "WI",
  "Pakistan" = "PAK",
  "Sri Lanka" = "SL",
  "Afghanistan" = "AFG",
  "Bangladesh" = "BAN",
  "Ireland" = "IRE",
  "Zimbabwe" = "ZIM"
  # Add more as needed
)
countries = c("Australia", "India", "South Africa", "England", "New Zealand", "West Indies", "Pakistan", "Sri Lanka", "Afghanistan", "Bangladesh", "Ireland", "Zimbabwe")


#Remove countries not in country list
#change BAN to Bangladesh
batting_data <- batting_data %>% mutate(Country = ifelse(Country == "BAN", "Bangladesh", Country))
bowling_data <- bowling_data %>% mutate(Country = ifelse(Country == "BAN", "Bangladesh", Country))
match_data_file_cricinfo <- match_data_file_cricinfo %>% filter(Team.1 %in% countries & Team.2 %in% countries & Winner %in% countries)
batting_data <- batting_data %>% filter(Country %in% countries & Opposition %in% countries)
bowling_data <- bowling_data %>% filter(Country %in% countries & Opposition %in% countries)
#Replace with acronym
match_data_file_cricinfo$Team.1 <- country_acronyms[match_data_file_cricinfo$Team.1]
match_data_file_cricinfo$Team.2 <- country_acronyms[match_data_file_cricinfo$Team.2]
match_data_file_cricinfo$Winner <- country_acronyms[match_data_file_cricinfo$Winner]
batting_data$Country <- country_acronyms[batting_data$Country]
batting_data$Opposition <- country_acronyms[batting_data$Opposition]
bowling_data$Country <- country_acronyms[bowling_data$Country]
bowling_data$Opposition <- country_acronyms[bowling_data$Opposition]


match_data_file_cricinfo$Date <- as.Date.character(match_data_file_cricinfo$Match.Date, format = "%b %d, %Y")
batting_data$Date <- as.Date(batting_data$Date)
bowling_data$Date <- as.Date(bowling_data$Date)

batting_data$Team = batting_data$Country
bowling_data$Team = bowling_data$Country
batting_data$Opponent = batting_data$Opposition
bowling_data$Opponent = bowling_data$Opposition

# Create a combined Team column in match_data_file_cricinfo
match_data_file_cricinfo <- match_data_file_cricinfo %>%
  mutate(Team = ifelse(Team.1 < Team.2, paste(Team.1, Team.2), paste(Team.2, Team.1)))

# Create a combined Team column in batting_data and bowling_data
batting_data <- batting_data %>%
  mutate(Team = ifelse(Country < Opposition, paste(Country, Opposition), paste(Opposition, Country)))
bowling_data <- bowling_data %>%
  mutate(Team = ifelse(Country < Opposition, paste(Country, Opposition), paste(Opposition, Country)))


# Print some info for debugging
print("Unique Dates in match_data_file_cricinfo:")
print(unique(match_data_file_cricinfo$Date))
print("Unique Dates in batting_data:")
print(unique(batting_data$Date))
print("Unique Dates in bowling_data:")
print(unique(bowling_data$Date))

print("Unique Teams in match_data_file_cricinfo:")
print(unique(match_data_file_cricinfo$Team))
print("Unique Teams in batting_data:")
print(unique(batting_data$Team))
print("Unique Teams in bowling_data:")
print(unique(bowling_data$Team))
# Join
batting_data <- left_join(batting_data, dplyr::select(match_data_file_cricinfo, ID, Date, Team), by = c("Date", "Team"))
bowling_data <- left_join(bowling_data, dplyr::select(match_data_file_cricinfo, ID, Date, Team), by = c("Date", "Team"))


# Compute Batting Impact
batting_data <- batting_data %>%
  mutate(OutPenalty = ifelse(NotOut == FALSE, 0.5, 0),
         BattingImpact = Runs * 0.125 + ((StrikeRate - 130) * 0.025) + (Fours * 0.3) + (Sixes * 0.5) - OutPenalty)

# Compute Bowling Impact
bowling_data <- bowling_data %>%
  mutate(BowlingImpact = (Wickets * 3.25) + (Maidens * 3) - (Economy - 7.4) * 0.5)



batting_impact <- batting_data %>%
  group_by(ID, Country) %>%
  summarise(TotalBattingImpact = sum(BattingImpact, na.rm = TRUE)) %>%
  ungroup()

bowling_impact <- bowling_data %>%
  group_by(ID, Country) %>%
  summarise(TotalBowlingImpact = sum(BowlingImpact, na.rm = TRUE)) %>%
  ungroup()

#make bowling impact have batting data mean and sd
bowling_impact$TotalBowlingImpact = (bowling_impact$TotalBowlingImpact)
batting_impact$TotalBattingImpact = (batting_impact$TotalBattingImpact)

# Merge Batting & Bowling Impact
team_impact <- batting_impact %>%
  full_join(bowling_impact, by = c("ID", "Country")) %>%
  mutate(TotalImpact = TotalBattingImpact + TotalBowlingImpact)

# Merge with Match Data
# Don't run line 143 on first run, but can be done afterward to reset columns
# match_data_file_cricinfo[, 10:16] <- list(NULL)
match_data_file_cricinfo <- match_data_file_cricinfo %>%
  left_join(team_impact, by = c("ID" = "ID", "Team.1" = "Country")) %>%
  rename(Impact_Team1 = TotalImpact) %>%
  left_join(team_impact, by = c("ID" = "ID", "Team.2" = "Country")) %>%
  rename(Impact_Team2 = TotalImpact)

# Compute Impact Difference & Outcome Variable
match_data_file_cricinfo <- match_data_file_cricinfo %>%
  mutate(Impact_Diff = Impact_Team1 - Impact_Team2,
         Winner_Team1 = ifelse(Winner == Team.1, 1, 0))

#print the final correlation
print(paste("Final correlation: ", cor(match_data_file_cricinfo$Impact_Diff, match_data_file_cricinfo$Winner_Team1, use = "complete.obs")))
