player_data <- full_join(batting_data, bowling_data, by = c("Player", "ID", "Ground", "Country"))



# Player Rankings based on Batting and Bowling ratings without making separate players one for batting and bowling
player_rankings <- player_data %>%
  group_by(Player, Country) %>%
  summarise(
    CareerBatImpact = sum(BattingImpact, na.rm = TRUE),
    CareerBowlImpact = sum(BowlingImpact, na.rm = TRUE),
    MatchesPlayed = n()
  ) %>%
  mutate(TotalImpact = CareerBatImpact + CareerBowlImpact) %>%
  arrange(desc(TotalImpact))
#Join player batting bowling data
player_rankings_2 = data.frame()
#loop through the player rankings unique elements and get the higher of their bat bowl impacts to use in new dataframe
for (player in unique(player_rankings$Player)){
  #if quote at the end of player name string, skip
  if(endsWith(player, " ")){
    next
  }
  
  person = player_rankings %>% filter(Player == paste(player, " ", sep = ""))
  person2 = player_rankings %>% filter(Player == player)
  # get which one of person and person2 has non zero batting impact and choose that
  if(person$CareerBowlImpact[1] == 0 & person2$CareerBowlImpact[1] == 0){
    #find which bat impact isn't 0
    if(person$CareerBatImpact[1] != 0){
      player_rankings_2 = rbind(player_rankings_2, person)
      next
    }
    else{
      player_rankings_2 = rbind(player_rankings_2, person2)
      next
    }
  }
  if(person$CareerBatImpact[1] == 0){
    #only bowling records
    person$CareerBatImpact[1] = person2$CareerBatImpact[1]
    player_rankings_2 = rbind(player_rankings_2, person)
  }
  else if(person2$CareerBowlImpact[1] == 0){
    #only batting records
    person2$CareerBowlImpact[1] = person$CareerBowlImpact[1]
    player_rankings_2 = rbind(player_rankings_2, person2)
  }
  else if(person$CareerBowlImpact[1] == 0){
    #only batting records
    person$CareerBowlImpact[1] = person2$CareerBowlImpact[1]
    player_rankings_2 = rbind(player_rankings_2, person)
  }
  else if(person2$CareerBatImpact[1] == 0){
    #only bowling records
    person2$CareerBatImpact[1] = person$CareerBatImpact[1]
    player_rankings_2 = rbind(player_rankings_2, person2)
  }
  
}
player_rankings_2$TotalImpact = player_rankings_2$CareerBatImpact + player_rankings_2$CareerBowlImpact
player_rankings_2 = player_rankings_2 %>% arrange(desc(TotalImpact))
player_rankings_2$BatImpactperGame = player_rankings_2$CareerBatImpact / player_rankings_2$MatchesPlayed
player_rankings_2$BatImpactperGame_Z = (player_rankings_2$BatImpactperGame - mean(player_rankings_2$BatImpactperGame, na.rm = TRUE)) / sd(player_rankings_2$BatImpactperGame, na.rm = TRUE)
player_rankings_2$BowlImpactperGame = player_rankings_2$CareerBowlImpact / player_rankings_2$MatchesPlayed
player_rankings_2$BowlImpactperGame_Z = (player_rankings_2$BowlImpactperGame - mean(player_rankings_2$BowlImpactperGame, na.rm = TRUE)) / sd(player_rankings_2$BowlImpactperGame, na.rm = TRUE)
player_rankings_2$TotalImpactperGame = player_rankings_2$TotalImpact / player_rankings_2$MatchesPlayed
player_rankings_2$TotalImpactperGame_Z = player_rankings_2$BatImpactperGame_Z + player_rankings_2$BowlImpactperGame_Z
player_rankings_2 = player_rankings_2 %>% arrange(desc(TotalImpactperGame)) #Model all matches
player_rankings_display = player_rankings_2 %>% filter(MatchesPlayed > 20)
bowlimpactmedian = quantile(player_rankings_display$BowlImpactperGame, 5/11)
batimpactmedian = quantile(player_rankings_display$BatImpactperGame, 3.75/11)
player_rankings_display2 = player_rankings_display %>% dplyr::filter(BowlImpactperGame > bowlimpactmedian)
allrounderranking = player_rankings_display2 %>% dplyr::filter(BatImpactperGame > batimpactmedian)
View(player_rankings_display)
View(allrounderranking)