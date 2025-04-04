venue_factors <- player_data %>%
  group_by(Ground) %>%
  summarise(
    AvgBatImpact = mean(BattingImpact, na.rm = TRUE),
    AvgBowlImpact = mean(BowlingImpact, na.rm = TRUE)
  )

#Venue considerations
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
battingranking
bowlingranking

# Min max scale venue factors bating scale and bowling scale
venue_factors$BattingScale2 = (venue_factors$BattingScale - min(venue_factors$BattingScale)) / (max(venue_factors$BattingScale) - min(venue_factors$BattingScale))
venue_factors$BowlingScale2 = (venue_factors$BowlingScale - min(venue_factors$BowlingScale)) / (max(venue_factors$BowlingScale) - min(venue_factors$BowlingScale))
battingranking = venue_factors %>%
  arrange(desc(BowlingScale2 - BattingScale2))
bowlingranking = venue_factors %>%
  arrange(BattingScale2 - BowlingScale2)
battingranking
bowlingranking
hist(battingranking$BattingScale2, col = "blue", main = "Venue Batting Scale Distribution")
hist(bowlingranking$BowlingScale2, col = "red")



# Print ranking of teams
team_ranking <- team_impact %>%
  group_by(Country) %>%
  summarise(TotalImpact = sum(TotalImpact, na.rm = TRUE)) %>%
  arrange(desc(TotalImpact))

# For the 6 months
recent_matches <- match_data_file_cricinfo %>%
  filter(Date >= Sys.Date() - 180)  # Matches in the last 6 months
team_ranking_recent <- team_impact %>%
  filter(ID %in% unique(recent_matches$ID)) %>%
  group_by(Country) %>%
  summarise(TotalImpact = mean(TotalImpact, na.rm = TRUE)) %>%
  mutate(nMatches = n()) %>%
  arrange(desc(TotalImpact))
print(team_ranking_recent)

# Plot team rankings
library(ggplot2)
ggplot(team_ranking_recent, aes(x = reorder(Country, TotalImpact), y = TotalImpact)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  coord_flip() +
  labs(title = "Team Rankings Based on Recent Matches",
       x = "Country",
       y = "Total Impact") +
  theme_minimal()

battingranking[79,]
