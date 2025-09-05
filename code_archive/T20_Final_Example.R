# Predict India vs. South Africa, T20 WC Final 2024

#Get the playing XI for both teams
team1_playingxi = player_data %>% filter(ID == 2729)
team1_playingxi = team1_playingxi[1:22,]
team1_playingxi = team1_playingxi %>% filter(Country == "IND")
team1_playingxi = team1_playingxi$Player

team2_playingxi = player_data %>% filter(ID == 2729)
team2_playingxi = team2_playingxi[1:22,]
team2_playingxi = team2_playingxi %>% filter(Country == "RSA")
team2_playingxi = team2_playingxi$Player

# Get the ground buffs
ground_buffs = venue_factors %>% filter(Ground == "Bridgetown")

# Get the player ratings per match for the playing xi
team1_playingxi_ratings = player_rankings_2 %>% filter(Player %in% team1_playingxi)
team2_playingxi_ratings = player_rankings_2 %>% filter(Player %in% team2_playingxi)

# Multiply the ground buffs
# Weight it by batter rating, ie if the batter rating is higher, the ground buffs will have more impact
team1_playingxi_ratings$BatImpactperGame_2 = 0
team2_playingxi_ratings$BatImpactperGame_2 = 0
team1_playingxi_ratings$BowlImpactperGame_2 = 0
team2_playingxi_ratings$BowlImpactperGame_2 = 0
# Evaluate player performance in Team 1 according to their ratings and ground conditions
for(i in 1:nrow(team1_playingxi_ratings)){
  # Sort by batting rating
  team1_playingxi_ratings = team1_playingxi_ratings %>% arrange(desc(BatImpactperGame))
  # Divide by appropriate factor, assume only 8 batsmen will play
  if(i < 9){
    team1_avg = team1_playingxi_ratings$BatImpactperGame[i] * (1 / median(venue_factors$BattingScale2)) * ground_buffs$BattingScale2[1] * ((9-i)/(10-i))
    team1_playingxi_ratings$BatImpactperGame_2[i] = team1_avg
  }
  else {
    # Bottom batsmen don't get a buff and don't play as much
    team1_avg = team1_playingxi_ratings$BatImpactperGame[i] / 5
    team1_playingxi_ratings$BatImpactperGame_2[i] = team1_avg
  }
  
  team1_playingxi_ratings = team1_playingxi_ratings %>% arrange(desc(BowlImpactperGame))
  if(i < 7) {
    team1_avg = team1_playingxi_ratings$BowlImpactperGame[i] * (1 / median(venue_factors$BowlingScale2)) * ground_buffs$BowlingScale2[1] * ((8-i)/(9-i))
    team1_playingxi_ratings$BowlImpactperGame_2[i] = team1_avg
  }
  else {
    # Bottom bowlers don't bowl in the game
    team1_playingxi_ratings$BowlImpactperGame_2[i] = 0 
  }
  
}
# Evaluate player performance in Team 2 according to their ratings and ground conditions
for(i in 1:nrow(team2_playingxi_ratings)){
  team2_playingxi_ratings = team2_playingxi_ratings %>% arrange(desc(BatImpactperGame))
  # Divide by appropriate factor, assume only 8 batsmen will play
  if(i < 9){
    team2_avg = team2_playingxi_ratings$BatImpactperGame[i] * (1 / median(venue_factors$BattingScale2)) * ground_buffs$BattingScale2[1] * ((9-i)/(10-i))
    team2_playingxi_ratings$BatImpactperGame_2[i] = team2_avg
  }
  else{
    # Bottom batsmen don't get a buff and don't play as much
    team2_avg = team2_playingxi_ratings$BatImpactperGame[i] / 5
    team2_playingxi_ratings$BatImpactperGame_2[i] = team2_avg
  }
  
  team2_playingxi_ratings = team2_playingxi_ratings %>% arrange(desc(BowlImpactperGame))
  if(i < 7){
    team2_avg = team2_playingxi_ratings$BowlImpactperGame[i] * (1 / median(venue_factors$BowlingScale2)) * ground_buffs$BowlingScale2[1] * ((8-i)/(9-i))
    team2_playingxi_ratings$BowlImpactperGame_2[i] = team2_avg
  }
  else{
    # Bottom bowlers don't bowl in the game
    team2_playingxi_ratings$BowlImpactperGame_2[i] = 0 
  }
  
}

# Rearrange data to make sure 11 batting rankings correct
team1_batting = (team1_playingxi_ratings %>% arrange(desc(BatImpactperGame_2)))$BatImpactperGame_2
team2_batting = (team2_playingxi_ratings %>% arrange(desc(BatImpactperGame_2)))$BatImpactperGame_2
team1_bowling = (team1_playingxi_ratings %>% arrange(desc(BowlImpactperGame_2)))$BowlImpactperGame_2
team2_bowling = (team2_playingxi_ratings %>% arrange(desc(BowlImpactperGame_2)))$BowlImpactperGame_2

# Calculate win probability
ind_winprob = predict(final_model, newx = (data.frame(Batsman1_T1 = team1_batting[1],
                                    Batsman2_T1 = team1_batting[2],
                                    Batsman3_T1 = team1_batting[3],
                                    Batsman4_T1 = team1_batting[4],
                                    Batsman5_T1 = team1_batting[5],
                                    Batsman6_T1 = team1_batting[6],
                                    Batsman7_T1 = team1_batting[7], 
                                    Batsman8_T1 = team1_batting[8], 
                                    Bowler1_T1 = team1_bowling[1],
                                    Bowler2_T1 = team1_bowling[2],
                                    Bowler3_T1 = team1_bowling[3],
                                    Bowler4_T1 = team1_bowling[4],
                                    Bowler5_T1 = team1_bowling[5],
                                    Bowler6_T1 = team1_bowling[6],
                                    Batsman1_T2 = team2_batting[1],
                                    Batsman2_T2 = team2_batting[2],
                                    Batsman3_T2 = team2_batting[3],
                                    Batsman4_T2 = team2_batting[4],
                                    Batsman5_T2 = team2_batting[5],
                                    Batsman6_T2 = team2_batting[6],
                                    Batsman7_T2 = team2_batting[7],
                                    Batsman8_T2 = team2_batting[8],
                                    Bowler1_T2 = team2_bowling[1],
                                    Bowler2_T2 = team2_bowling[2],
                                    Bowler3_T2 = team2_bowling[3],
                                    Bowler4_T2 = team2_bowling[4],
                                    Bowler5_T2 = team2_bowling[5],
                                    Bowler6_T2 = team2_bowling[6]))
                                    , type = "response")
ind_winprob = ind_winprob 
print(paste("India win probability: ", round(ind_winprob, 4) * 100, "%"))


# With random effect correction based on number of matches played

# Change to IND RSA to predict the T20 World Cup Final 2024 with random effect as well, or
# any game you'd like

# Select games that can be passed into model with easy access; check Match_Data-Cricinfo file for more: 
#
# IND vs SL, 2014 T20 WC final (SL won by 6 wickets, Mirpur) Match ID 400
# AUS vs NZL, 2021 T20 WC final (AUS won by 8 wickets, Dubai (DICS)) Match ID 1428
# AFG vs WI, 2016 T20 WC match (AFG won by 6 runs, Nagpur) Match ID 552
# IND vs PAK, 2024 T20 WC match (IND won by 6 runs, New York) Match ID 2658 - This example, the model got wrong
# Most recent T20I: AUS vs RSA at Cairns (AUS won by 2 wickets, August 16 2025) Match ID 3407
# 
country1 = "IND"
country2 = "SL"
Venue = "Mirpur"
MatchID = 400

team1_playingxi = player_data %>% filter(ID == MatchID) 
team1_playingxi = team1_playingxi[1:22,]
team1_playingxi = team1_playingxi %>% filter(Country == country1)
team1_playingxi = team1_playingxi$Player

team2_playingxi = player_data %>% filter(ID == MatchID)
team2_playingxi = team2_playingxi[1:22,]
team2_playingxi = team2_playingxi %>% filter(Country == country2) 
team2_playingxi = team2_playingxi$Player

# Get the ground buffs and the player ratings per match for the playing xi
ground_buffs = venue_factors %>% filter(Ground == Venue) 
team1_playingxi_ratings = player_rankings_2 %>% filter(Player %in% team1_playingxi)
team2_playingxi_ratings = player_rankings_2 %>% filter(Player %in% team2_playingxi)

# Multiply the ground buffs
# Weight it by batter rating, ie if the batter rating is higher, the ground buffs will have more impact
team1_playingxi_ratings$BatImpactperGame_2 = 0
team2_playingxi_ratings$BatImpactperGame_2 = 0
team1_playingxi_ratings$BowlImpactperGame_2 = 0
team2_playingxi_ratings$BowlImpactperGame_2 = 0
# Simulate 20 times to get a better idea of win probability with random effects
kvec = numeric(20)
for(k in 1:20){
  for(i in 1:nrow(team1_playingxi_ratings)){
    #sort by batting rating
    team1_playingxi_ratings = team1_playingxi_ratings %>% arrange(desc(BatImpactperGame))
    #divide by appropriate factor, assume only 8 batsmen will play
    if(i < 9){
      team1_avg = team1_playingxi_ratings$BatImpactperGame[i] * (1 / median(venue_factors$BattingScale2)) * ground_buffs$BattingScale2[1] * ((9-i)/(10-i))
      team1_playingxi_ratings$BatImpactperGame_2[i] = rnorm(1, team1_avg, abs(team1_avg / team1_playingxi_ratings$MatchesPlayed[i])) # Random process
    }
    else {
      #Bottom batsmen don't get a buff and don't play as much
      team1_avg = team1_playingxi_ratings$BatImpactperGame[i] / 5
      team1_playingxi_ratings$BatImpactperGame_2[i] = rnorm(1, team1_avg, abs(team1_avg / team1_playingxi_ratings$MatchesPlayed[i])) # Random process
      #team1_playingxi_ratings$BatImpactperGame_2[i] = team1_avg
    }
    
    team1_playingxi_ratings = team1_playingxi_ratings %>% arrange(desc(BowlImpactperGame))
    if(i < 7) {
      team1_avg = team1_playingxi_ratings$BowlImpactperGame[i] * (1 / median(venue_factors$BowlingScale2)) * ground_buffs$BowlingScale2[1] * ((8-i)/(9-i))
      team1_playingxi_ratings$BowlImpactperGame_2[i] = rnorm(1, team1_avg, abs(team1_avg / team1_playingxi_ratings$MatchesPlayed[i])) # Random process
      #team1_playingxi_ratings$BowlImpactperGame_2[i] = team1_avg
    }
    else {
      #Bottom bowlers don't bowl in the game
      team1_playingxi_ratings$BowlImpactperGame_2[i] = 0 
    }
    
  }
  for(i in 1:nrow(team2_playingxi_ratings)){
    team2_playingxi_ratings = team2_playingxi_ratings %>% arrange(desc(BatImpactperGame))
    #divide by appropriate factor, assume only 8 batsmen will play
    if(i < 9){
      team2_avg = team2_playingxi_ratings$BatImpactperGame[i] * (1 / median(venue_factors$BattingScale2)) * ground_buffs$BattingScale2[1] * ((9-i)/(10-i))
      team2_playingxi_ratings$BatImpactperGame_2[i] = rnorm(1, team2_avg, abs(team2_avg / team2_playingxi_ratings$MatchesPlayed[i])) # Random process
    }
    else{
      #Bottom batsmen don't get a buff and don't play as much
      team2_avg = team2_playingxi_ratings$BatImpactperGame[i] / 5
      team2_playingxi_ratings$BatImpactperGame_2[i] = rnorm(1, team2_avg, abs(team2_avg / team2_playingxi_ratings$MatchesPlayed[i])) # Random process
    }
    
    team2_playingxi_ratings = team2_playingxi_ratings %>% arrange(desc(BowlImpactperGame))
    if(i < 7){
      team2_avg = team2_playingxi_ratings$BowlImpactperGame[i] * (1 / median(venue_factors$BowlingScale2)) * ground_buffs$BowlingScale2[1] * ((8-i)/(9-i))
      team2_playingxi_ratings$BowlImpactperGame_2[i] = rnorm(1, team2_avg, abs(team2_avg / team2_playingxi_ratings$MatchesPlayed[i])) # Random process
    }
    else{
      #Bottom bowlers don't bowl in the game
      team2_playingxi_ratings$BowlImpactperGame_2[i] = 0 
    }
    
  
}
# Rearrange data to make sure 11 batting rankings correct
team1_batting = (team1_playingxi_ratings %>% arrange(desc(BatImpactperGame_2)))$BatImpactperGame_2
team2_batting = (team2_playingxi_ratings %>% arrange(desc(BatImpactperGame_2)))$BatImpactperGame_2
team1_bowling = (team1_playingxi_ratings %>% arrange(desc(BowlImpactperGame_2)))$BowlImpactperGame_2
team2_bowling = (team2_playingxi_ratings %>% arrange(desc(BowlImpactperGame_2)))$BowlImpactperGame_2

winprob = predict(final_model, newx = (data.frame(Batsman1_T1 = team1_batting[1],
                                                      Batsman2_T1 = team1_batting[2],
                                                      Batsman3_T1 = team1_batting[3],
                                                      Batsman4_T1 = team1_batting[4],
                                                      Batsman5_T1 = team1_batting[5],
                                                      Batsman6_T1 = team1_batting[6],
                                                      Batsman7_T1 = team1_batting[7], 
                                                      Batsman8_T1 = team1_batting[8], 
                                                      Bowler1_T1 = team1_bowling[1],
                                                      Bowler2_T1 = team1_bowling[2],
                                                      Bowler3_T1 = team1_bowling[3],
                                                      Bowler4_T1 = team1_bowling[4],
                                                      Bowler5_T1 = team1_bowling[5],
                                                      Bowler6_T1 = team1_bowling[6],
                                                      Batsman1_T2 = team2_batting[1],
                                                      Batsman2_T2 = team2_batting[2],
                                                      Batsman3_T2 = team2_batting[3],
                                                      Batsman4_T2 = team2_batting[4],
                                                      Batsman5_T2 = team2_batting[5],
                                                      Batsman6_T2 = team2_batting[6],
                                                      Batsman7_T2 = team2_batting[7],
                                                      Batsman8_T2 = team2_batting[8],
                                                      Bowler1_T2 = team2_bowling[1],
                                                      Bowler2_T2 = team2_bowling[2],
                                                      Bowler3_T2 = team2_bowling[3],
                                                      Bowler4_T2 = team2_bowling[4],
                                                      Bowler5_T2 = team2_bowling[5],
                                                      Bowler6_T2 = team2_bowling[6]))
                      , type = "response")
 kvec[k] = winprob
}
kvec = kvec
print(paste(country1, "win probability: ", round(mean(kvec), 4) * 100, "%"))



