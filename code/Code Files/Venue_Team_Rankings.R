# Create venue impact factors
venue_factors <- player_data %>%
  group_by(Ground) %>%
  summarise(
    AvgBatImpact = mean(BattingImpact, na.rm = TRUE),
    AvgBowlImpact = mean(BowlingImpact, na.rm = TRUE)
  )

# Venue considerations
venue_factors <- venue_factors %>%
  mutate(
    #standardize
    BattingScale = (AvgBatImpact - mean(venue_factors$AvgBatImpact, na.rm = TRUE)) / sd(venue_factors$AvgBatImpact, na.rm = TRUE),
    BowlingScale = (AvgBowlImpact - mean(venue_factors$AvgBowlImpact, na.rm = TRUE)) / sd(venue_factors$AvgBowlImpact, na.rm = TRUE)
  )
battingranking = venue_factors %>%
  arrange(desc(BowlingScale - BattingScale))
bowlingranking = venue_factors %>%
  arrange(BattingScale - BowlingScale)


# Min max scale venue factors bating scale and bowling scale
venue_factors$BattingScale2 = (venue_factors$BattingScale - min(venue_factors$BattingScale)) / (max(venue_factors$BattingScale) - min(venue_factors$BattingScale))
venue_factors$BowlingScale2 = (venue_factors$BowlingScale - min(venue_factors$BowlingScale)) / (max(venue_factors$BowlingScale) - min(venue_factors$BowlingScale))
battingranking = venue_factors %>%
  arrange((BowlingScale2 - BattingScale2))
bowlingranking = venue_factors %>%
  arrange(BattingScale2 - BowlingScale2)
battingranking
bowlingranking

hist(battingranking$BattingScale2, col = "blue", main = "Venue Batting Scale Distribution")
hist(bowlingranking$BowlingScale2, col = "red")

# Plot Bowling - Batting Impact top 15 for best bowling grounds
library(ggplot2)
ggplot(bowlingranking[1:15, ], aes(x = reorder(Ground, BowlingScale2 - BattingScale2), y = BowlingScale2 - BattingScale2)) +
  geom_bar(stat = "identity", fill = "red") +
  coord_flip() +
  labs(title = "Top 15 Bowling Grounds: Bowling Impact - Batting Impact",
       x = "Venue",
       y = "Bowling - Batting Impact") +
  theme_minimal()
# Plot Batting - Bowling Impact top 15 for best batting grounds
ggplot(battingranking[1:15, ], aes(x = reorder(Ground, BattingScale2 - BowlingScale2), y = BattingScale2 - BowlingScale2)) +
  geom_bar(stat = "identity", fill = "blue") +
  coord_flip() +
  labs(title = "Top 15 Batting Grounds: Batting Impact - Bowling Impact",
       x = "Venue",
       y = "Batting - Bowling Impact") +
  theme_minimal()
  
ggplot(venue_factors, aes(x = reorder(Ground, BowlingScale2 - BattingScale2), y = BowlingScale2 - BattingScale2)) +
  geom_bar(stat = "identity", fill = "red") +
  coord_flip() +
  labs(title = "Bowling Impact - Batting Impact by Venue",
       x = "Venue",
       y = "Bowling - Batting Impact") +
  theme_minimal() +
  scale_y_continuous(limits = c(min(venue_factors$BowlingScale2 - venue_factors$BattingScale2), max(venue_factors$BowlingScale2 - venue_factors$BattingScale2)))

# Plot Batting - Bowling Impact for best batting grounds
ggplot(venue_factors, aes(x = reorder(Ground, BattingScale2 - BowlingScale2), y = BattingScale2 - BowlingScale2)) +
  geom_bar(stat = "identity", fill = "blue") +
  coord_flip() +
  labs(title = "Batting Impact - Bowling Impact by Venue",
       x = "Venue",
       y = "Batting - Bowling Impact") +
  theme_minimal()



# Print ranking of teams
team_ranking <- team_impact %>%
  group_by(Country) %>%
  summarise(TotalImpact = sum(TotalImpact, na.rm = TRUE)) %>%
  arrange(desc(TotalImpact))


team_ranking_recent <- team_impact %>%
  # keep only matches in your recent set
  #filter(ID %in% unique(recent_matches$ID)) %>%
  # join match dates so we can order matches
  left_join(match_data_file_cricinfo %>% select(ID, Date), by = "ID") %>%
  arrange(Country, desc(Date)) %>%
  group_by(Country) %>%
  # keep only the last 15 matches per team
  slice_head(n = 10) %>%
  summarise(
    TotalImpact = mean(TotalImpact, na.rm = TRUE),
    nMatches = n()
  ) %>%
  mutate(TotalImpact_perGame = TotalImpact / nMatches) %>%
  arrange(desc(TotalImpact))

print(team_ranking_recent)
# Plot team rankings
library(ggplot2)
ggplot(team_ranking_recent, aes(x = reorder(Country, TotalImpact_perGame), y = TotalImpact_perGame)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  coord_flip() +
  labs(title = "Team Rankings Based on Recent Matches",
       x = "Country",
       y = "Total Impact") +
  theme_minimal()
# Get data for Lord's
battingranking[80,]