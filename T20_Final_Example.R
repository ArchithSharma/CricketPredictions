# Predict India vs. South Africa, T20 WC Final 2024

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
ind_winprob = ind_winprob * 0.5 / final_threshold  # Adjusting for the threshold
print(paste("India win probability: ", round(ind_winprob, 4) * 100, "%"))


# With random effect correction based on number of matches played

# Predict India vs. South Africa, T20 WC Final 2024
kvec = numeric(20)  # Initialize kvec for random effects

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


#multiply the ground buffs
#weight it by batter rating, ie if the batter rating is higher, the ground buffs will have more impact
team1_playingxi_ratings$BatImpactperGame_2 = 0
team2_playingxi_ratings$BatImpactperGame_2 = 0
team1_playingxi_ratings$BowlImpactperGame_2 = 0
team2_playingxi_ratings$BowlImpactperGame_2 = 0

for(k in 1:20){
for(i in 1:nrow(team1_playingxi_ratings)){
  #sort by batting rating
  team1_playingxi_ratings = team1_playingxi_ratings %>% arrange(desc(BatImpactperGame))
  #divide by appropriate factor, assume only 8 batsmen will play
  if(i < 9){
    team1_avg = team1_playingxi_ratings$BatImpactperGame[i] * (1 / median(venue_factors$BattingScale2)) * ground_buffs$BattingScale2[1] * ((9-i)/(10-i))
    team1_playingxi_ratings$BatImpactperGame_2[i] = rnorm(1, team1_avg, abs(team1_avg / team1_playingxi_ratings$MatchesPlayed[i]))
  }
  else {
    #Bottom batsmen don't get a buff and don't play as much
    team1_avg = team1_playingxi_ratings$BatImpactperGame[i] / 5
    team1_playingxi_ratings$BatImpactperGame_2[i] = rnorm(1, team1_avg, abs(team1_avg / team1_playingxi_ratings$MatchesPlayed[i]))
    #team1_playingxi_ratings$BatImpactperGame_2[i] = team1_avg
  }
  
  team1_playingxi_ratings = team1_playingxi_ratings %>% arrange(desc(BowlImpactperGame))
  if(i < 7) {
    team1_avg = team1_playingxi_ratings$BowlImpactperGame[i] * (1 / median(venue_factors$BowlingScale2)) * ground_buffs$BowlingScale2[1] * ((8-i)/(9-i))
    team1_playingxi_ratings$BowlImpactperGame_2[i] = rnorm(1, team1_avg, abs(team1_avg / team1_playingxi_ratings$MatchesPlayed[i]))
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
    team2_playingxi_ratings$BatImpactperGame_2[i] = rnorm(1, team2_avg, abs(team2_avg / team2_playingxi_ratings$MatchesPlayed[i]))
  }
  else{
    #Bottom batsmen don't get a buff and don't play as much
    team2_avg = team2_playingxi_ratings$BatImpactperGame[i] / 5
    team2_playingxi_ratings$BatImpactperGame_2[i] = rnorm(1, team2_avg, abs(team2_avg / team2_playingxi_ratings$MatchesPlayed[i]))
  }
  
  team2_playingxi_ratings = team2_playingxi_ratings %>% arrange(desc(BowlImpactperGame))
  if(i < 7){
    team2_avg = team2_playingxi_ratings$BowlImpactperGame[i] * (1 / median(venue_factors$BowlingScale2)) * ground_buffs$BowlingScale2[1] * ((8-i)/(9-i))
    team2_playingxi_ratings$BowlImpactperGame_2[i] = rnorm(1, team2_avg, abs(team2_avg / team2_playingxi_ratings$MatchesPlayed[i]))
  }
  else{
    #Bottom bowlers don't bowl in the game
    team2_playingxi_ratings$BowlImpactperGame_2[i] = 0 
  }
  
}
team1_batting = (team1_playingxi_ratings %>% arrange(desc(BatImpactperGame_2)))$BatImpactperGame_2
team2_batting = (team2_playingxi_ratings %>% arrange(desc(BatImpactperGame_2)))$BatImpactperGame_2
team1_bowling = (team1_playingxi_ratings %>% arrange(desc(BowlImpactperGame_2)))$BowlImpactperGame_2
team2_bowling = (team2_playingxi_ratings %>% arrange(desc(BowlImpactperGame_2)))$BowlImpactperGame_2
# Predict the win probability using the tuned model with random effects
ind_winprob = predict(final_model, newx = as.matrix(data.frame(Batsman1_T1 = team1_batting[1],
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
                                                      Bowler6_T2 = team2_bowling[6])), type = "response")
indwinprob = ind_winprob * 0.5 / final_threshold
kvec[k] = indwinprob  # Store the win probability for each k
}
print(paste("India win probability: ", round(ind_winprob, 4) * 100, "%"))


# Another example with unseen data in any of the data: Pak vs NZ March 26 2025

team1_playingxi = player_data %>% filter(ID == 3126)
team1_playingxi = team1_playingxi[1:22,]
team1_playingxi = team1_playingxi %>% filter(Country == "NZL")
team1_playingxi = team1_playingxi$Player

team2_playingxi = player_data %>% filter(ID == 3126)
team2_playingxi = team2_playingxi[1:22,]
team2_playingxi = team2_playingxi %>% filter(Country == "PAK")
team2_playingxi = team2_playingxi$Player

# Get the ground buffs
ground_buffs = venue_factors %>% filter(Ground == "Wellington")
# Get the player ratings per match for the playing xi
team1_playingxi_ratings = player_rankings_2 %>% filter(Player %in% team1_playingxi)
team2_playingxi_ratings = player_rankings_2 %>% filter(Player %in% team2_playingxi)

# Multiply the ground buffs
# Weight it by batter rating, ie if the batter rating is higher, the ground buffs will have more impact
team1_playingxi_ratings$BatImpactperGame_2 = 0
team2_playingxi_ratings$BatImpactperGame_2 = 0
team1_playingxi_ratings$BowlImpactperGame_2 = 0
team2_playingxi_ratings$BowlImpactperGame_2 = 0
kvec = numeric(20)
for(k in 1:20){
  for(i in 1:nrow(team1_playingxi_ratings)){
    #sort by batting rating
    team1_playingxi_ratings = team1_playingxi_ratings %>% arrange(desc(BatImpactperGame))
    #divide by appropriate factor, assume only 8 batsmen will play
    if(i < 9){
      team1_avg = team1_playingxi_ratings$BatImpactperGame[i] * (1 / median(venue_factors$BattingScale2)) * ground_buffs$BattingScale2[1] * ((9-i)/(10-i))
      team1_playingxi_ratings$BatImpactperGame_2[i] = rnorm(1, team1_avg, abs(team1_avg / team1_playingxi_ratings$MatchesPlayed[i]))
    }
    else {
      #Bottom batsmen don't get a buff and don't play as much
      team1_avg = team1_playingxi_ratings$BatImpactperGame[i] / 5
      team1_playingxi_ratings$BatImpactperGame_2[i] = rnorm(1, team1_avg, abs(team1_avg / team1_playingxi_ratings$MatchesPlayed[i]))
      #team1_playingxi_ratings$BatImpactperGame_2[i] = team1_avg
    }
    
    team1_playingxi_ratings = team1_playingxi_ratings %>% arrange(desc(BowlImpactperGame))
    if(i < 7) {
      team1_avg = team1_playingxi_ratings$BowlImpactperGame[i] * (1 / median(venue_factors$BowlingScale2)) * ground_buffs$BowlingScale2[1] * ((8-i)/(9-i))
      team1_playingxi_ratings$BowlImpactperGame_2[i] = rnorm(1, team1_avg, abs(team1_avg / team1_playingxi_ratings$MatchesPlayed[i]))
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
      team2_playingxi_ratings$BatImpactperGame_2[i] = rnorm(1, team2_avg, abs(team2_avg / team2_playingxi_ratings$MatchesPlayed[i]))
    }
    else{
      #Bottom batsmen don't get a buff and don't play as much
      team2_avg = team2_playingxi_ratings$BatImpactperGame[i] / 5
      team2_playingxi_ratings$BatImpactperGame_2[i] = rnorm(1, team2_avg, abs(team2_avg / team2_playingxi_ratings$MatchesPlayed[i]))
    }
    
    team2_playingxi_ratings = team2_playingxi_ratings %>% arrange(desc(BowlImpactperGame))
    if(i < 7){
      team2_avg = team2_playingxi_ratings$BowlImpactperGame[i] * (1 / median(venue_factors$BowlingScale2)) * ground_buffs$BowlingScale2[1] * ((8-i)/(9-i))
      team2_playingxi_ratings$BowlImpactperGame_2[i] = rnorm(1, team2_avg, abs(team2_avg / team2_playingxi_ratings$MatchesPlayed[i]))
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
 kvec[k] = ind_winprob
}
# Apply threshold correction
kvec = kvec * 0.5 / final_threshold
print(paste("Team 1 (New Zealand) win probability: ", round(mean(kvec), 4) * 100, "%"))
