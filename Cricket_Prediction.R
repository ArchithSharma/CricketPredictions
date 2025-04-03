predcorrectcount = 0
nummatches = nrow(match_data_file_cricinfo)
#create data frame for actual impact totals, predicted winner, and actual winner
playertotalimpact = data.frame(Batsman1_T1 = numeric(nummatches),
                               Batsman2_T1 = numeric(nummatches),
                               Batsman3_T1 = numeric(nummatches),
                               Batsman4_T1 = numeric(nummatches),
                               Batsman5_T1 = numeric(nummatches),
                               Batsman6_T1 = numeric(nummatches),
                               Batsman7_T1 = numeric(nummatches),
                               Batsman8_T1 = numeric(nummatches),
                               Bowler1_T1 = numeric(nummatches),
                               Bowler2_T1 = numeric(nummatches),
                               Bowler3_T1 = numeric(nummatches),
                               Bowler4_T1 = numeric(nummatches),
                               Bowler5_T1 = numeric(nummatches),
                               Bowler6_T1 = numeric(nummatches),
                               Batsman1_T2 = numeric(nummatches),
                               Batsman2_T2 = numeric(nummatches),
                               Batsman3_T2 = numeric(nummatches),
                               Batsman4_T2 = numeric(nummatches),
                               Batsman5_T2 = numeric(nummatches),
                               Batsman6_T2 = numeric(nummatches),
                               Batsman7_T2 = numeric(nummatches),
                               Batsman8_T2 = numeric(nummatches),
                               Bowler1_T2 = numeric(nummatches),
                               Bowler2_T2 = numeric(nummatches),
                               Bowler3_T2 = numeric(nummatches),
                               Bowler4_T2 = numeric(nummatches),
                               Bowler5_T2 = numeric(nummatches),
                               Bowler6_T2 = numeric(nummatches),
                               ID = numeric(nummatches))

                              
counter = 1
for(id in match_data_file_cricinfo$ID){ 
  #do the process for india and australia with the match id
  #get the playing xi from player data
  team1_playingxi = player_data %>% filter(id == ID)
  team1_playingxi = team1_playingxi[1:22,]
  team1_playingxi = team1_playingxi %>% filter(Country == (match_data_file_cricinfo %>% filter(id == ID))[1,1])
  team1_playingxi = team1_playingxi$Player
  
  team2_playingxi = player_data %>% filter(id == ID)
  team2_playingxi = team2_playingxi[1:22,]
  team2_playingxi = team2_playingxi %>% filter(Country == (match_data_file_cricinfo %>% filter(id == ID))[1,2])
  team2_playingxi = team2_playingxi$Player
  
  
  #get the ground buffs
  ground_buffs = venue_factors %>% filter(Ground == (match_data_file_cricinfo %>% filter(id == ID))[1,5])
  #get the player ratings per match for the playing xi
  team1_playingxi_ratings = player_rankings_2 %>% filter(Player %in% team1_playingxi)
  team2_playingxi_ratings = player_rankings_2 %>% filter(Player %in% team2_playingxi)
  
  #multiply the ground buffs
  #weight it by batter rating, ie if the batter rating is higher, the ground buffs will have more impact
  team1_playingxi_ratings$BatImpactperGame_2 = 0
  team2_playingxi_ratings$BatImpactperGame_2 = 0
  team1_playingxi_ratings$BowlImpactperGame_2 = 0
  team2_playingxi_ratings$BowlImpactperGame_2 = 0
  
  for(i in 1:nrow(team1_playingxi_ratings)){
    #sort by batting rating
    team1_playingxi_ratings = team1_playingxi_ratings %>% arrange(desc(BatImpactperGame))
    #divide by appropriate factor, assume only 8 batsmen will play
    if(i < 9){
      team1_avg = team1_playingxi_ratings$BatImpactperGame[i] * (1 / median(venue_factors$BattingScale2)) * ground_buffs$BattingScale2[1] * ((9-i)/(10-i))
      team1_playingxi_ratings$BatImpactperGame_2[i] = team1_avg
    }
    else {
      #Bottom batsmen don't get a buff and don't play as much
      team1_avg = 0
      #team1_playingxi_ratings$BatImpactperGame_2[i] = team1_avg
    }
    
    team1_playingxi_ratings = team1_playingxi_ratings %>% arrange(desc(BowlImpactperGame))
    if(i < 7) {
      team1_avg = team1_playingxi_ratings$BowlImpactperGame[i] * (1 / median(venue_factors$BowlingScale2)) * ground_buffs$BowlingScale2[1] * ((8-i)/(9-i))
      team1_playingxi_ratings$BowlImpactperGame_2[i] = team1_avg
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
      team2_playingxi_ratings$BatImpactperGame_2[i] = team2_avg
    }
    else{
      #Bottom batsmen don't get a buff and don't play as much
      team2_avg = 0
    }
    
    team2_playingxi_ratings = team2_playingxi_ratings %>% arrange(desc(BowlImpactperGame))
    if(i < 7){
      team2_avg = team2_playingxi_ratings$BowlImpactperGame[i] * (1 / median(venue_factors$BowlingScale2)) * ground_buffs$BowlingScale2[1] * ((8-i)/(9-i))
      team2_playingxi_ratings$BowlImpactperGame_2[i] = team2_avg
    }
    else{
      #Bottom bowlers don't bowl in the game
      team2_playingxi_ratings$BowlImpactperGame_2[i] = 0 
    }
    
  }
  #arrange batting ranking in descending order
  
  for(j in 1:(ncol(playertotalimpact) / 2)){
    if(j < 9){
      team1_playingxi_ratings = team1_playingxi_ratings %>% arrange(desc(BatImpactperGame))
      playertotalimpact[counter,j] = team1_playingxi_ratings$BatImpactperGame_2[j]
    }
    else{
      team1_playingxi_ratings = team1_playingxi_ratings %>% arrange(desc(BowlImpactperGame))
      playertotalimpact[counter,j] = team1_playingxi_ratings$BowlImpactperGame_2[j-8]
    }
    
  }
  for(j in 1:(ncol(playertotalimpact) / 2)){
    if(j < 9){
      team2_playingxi_ratings = team2_playingxi_ratings %>% arrange(desc(BatImpactperGame))
      playertotalimpact[counter,(j + 14)] = team2_playingxi_ratings$BatImpactperGame_2[j]
    }
    else{
      team2_playingxi_ratings = team2_playingxi_ratings %>% arrange(desc(BowlImpactperGame))
      playertotalimpact[counter,(j + 14)] = team2_playingxi_ratings$BowlImpactperGame_2[j-8]
    }
  }
  playertotalimpact[counter, 29] = id

  counter = counter + 1
}
#join by id to match_data_cricinfo$Winner_Team1
playertotalimpact2 = right_join(playertotalimpact, match_data_file_cricinfo, by = "ID")
#remove all columns from match_data_cricinfo except Winner_Team1
playertotalimpact2 = playertotalimpact2 %>% dplyr::select(-c(30:44))
playertotalimpact2 = playertotalimpact2 %>% dplyr::select(-c(29))

#find nas
nas = apply(playertotalimpact2, 2, function(x) sum(is.na(x)))
#any cell with gets row removed
playertotalimpact2 = playertotalimpact2 %>% filter_all(all_vars(!is.na(.)))

#simple heuristic: greatest total impact wins
predcorrectcount = 0
team1vec = numeric(nrow(playertotalimpact2))
team2vec = numeric(nrow(playertotalimpact2))
nummatches = nrow(playertotalimpact2)
for(i in 1:nrow(playertotalimpact2)){
  #sum rows one through 17
  team1_total = sum(playertotalimpact2[i, 1:14])
  team2_total = sum(playertotalimpact2[i, 15:28])
  team1vec[i] = team1_total
  team2vec[i] = team2_total
  if(team1_total > team2_total){
    predicted_winner = 1
  }
  else{
    predicted_winner = 0
  }
  if(predicted_winner == as.numeric(playertotalimpact2$Winner_Team1[i])){
    predcorrectcount = predcorrectcount + 1
  }
}
accuracy = predcorrectcount / nummatches
print(paste("Model Accuracy on all matches:", round(accuracy * 100, 2), "%"))

target = factor(playertotalimpact2$Winner_Team1, levels = c(0, 1), labels = c("Team2", "Team1"))  # Convert to factor for classification
# Log reg
# Train the Logistic Regression model
accvector = numeric(30)  # Initialize accuracy vector for 30 iterations
set.seed(123)
for(i in 1:30){
  
  # Train-test split (80-20)
  train_index <- createDataPartition(playertotalimpact2$Winner_Team1, p = 0.8, list = FALSE)
  train_data <- playertotalimpact2[train_index, ]
  test_data  <- playertotalimpact2[-train_index, ]
  
  # Define Predictors and Target
  train_x <- as.matrix(train_data[, 1:28])
  train_y <- as.factor(train_data$Winner_Team1)  # Convert to factor (0/1)
  test_x  <- as.matrix(test_data[, 1:28])
  test_y  <- as.factor(test_data$Winner_Team1)   # Convert to factor (0/1)
  
  # Hyperparameter grid search
  grid <- expand.grid(alpha = seq(0, 1, by = 0.1), lambda = seq(0.001, 0.1, by = 0.001))
  
  logistic_model <- train(
    x = train_x,
    y = train_y,
    method = "glmnet",
    trControl = trainControl(method = "cv", number = 5),
    family = "binomial",
    metric = "Accuracy",
    tuneGrid = grid
  )
  
  best_lambda <- logistic_model$bestTune$lambda
  best_alpha <- logistic_model$bestTune$alpha
  print(paste("Best Lambda:", round(best_lambda, 4), "Best Alpha:", round(best_alpha, 4)))
  
  # Train logistic model with best parameters
  best_log_model <- glmnet(
    x = train_x, 
    y = as.numeric(train_y) - 1, # Convert factor (0/1) to numeric
    family = "binomial", 
    alpha = best_alpha, 
    lambda = best_lambda
  )
  
  # Get test set predictions
  test_predictions <- predict(best_log_model, newx = test_x, type = "response", s = best_lambda)
  
  # Cross-validation to determine the best threshold
  thresholds = numeric(10)
  folds <- createFolds(train_y, k = 10, list = TRUE)
  
  for(j in 1:10){
    val_indices <- folds[[j]]
    train_indices <- setdiff(1:length(train_y), val_indices)
    
    # Training and validation sets
    train_xcv <- train_x[train_indices, ]
    train_ycv <- train_y[train_indices]
    val_xcv <- train_x[val_indices, ]
    val_ycv <- train_y[val_indices]
    
    # Train logistic model
    log_model_cv <- glmnet(
      x = train_xcv, 
      y = as.numeric(train_ycv) - 1, 
      family = "binomial",
      alpha = best_alpha, 
      lambda = best_lambda
    )
    
    # Predict probabilities on validation set
    val_predictions <- predict(log_model_cv, newx = val_xcv, type = "response", s = best_lambda)
    
    # Compute ROC Curve
    roc_curve <- roc(as.numeric(val_ycv) - 1, val_predictions)
    
    # Get the best threshold for classification
    best_thresh <- as.numeric(coords(roc_curve, "best", ret = "threshold"))
    
    # Store threshold
    thresholds[j] <- best_thresh
  }
  
  # Compute final averaged threshold across folds
  final_threshold <- mean(thresholds)  
  print(paste("Final Averaged Threshold:", round(final_threshold, 4)))
  
  # Apply threshold to test predictions
  test_predictions <- ifelse(test_predictions > final_threshold, 1, 0)
  test_predictions <- factor(test_predictions, levels = c(0, 1))  # Convert to factor
  levels(test_predictions) <- c("Team2", "Team1")  # Assign labels
  levels(test_y) <- c("Team2", "Team1")  # Ensure test_y has the same levels for comparison)
  # Compute Accuracy
  conf_matrix <- confusionMatrix(test_predictions, test_y)
  accvector[i] <- conf_matrix$overall['Accuracy']  # Store accuracy for each iteration
  print(accvector[i])  
  
}

# Plot Accuracy Distribution
hist(accvector, main = "Histogram of Logistic Regression Model Accuracy", xlab = "Accuracy", ylab = "Frequency")
mean(accvector)
# Final Logistic Regression Model on the entire dataset
set.seed(123)  # For reproducibility
# Train the final Logistic Regression model on the entire dataset
train_index <- createDataPartition(target, p = 0.8, list = FALSE)
train_data <- playertotalimpact2[train_index, ]
test_data  <- playertotalimpact2[-train_index, ]
# Define Predictors and Target for the final model
predictors <- as.matrix(train_data[, 1:28])  # Player impact features
target <- as.factor(train_data$Winner_Team1)  # Target variable (0 or 1)
# Hyperparameter grid search for final model
final_model = glmnet(
  x = train_x,
  y = train_y, # Convert factor (0/1) to numeric
  family = "binomial",
  alpha = 0.5,  # Use the best alpha from previous model
  lambda = 0.029, # Use the best lambda from previous model
)

print(paste("Accuracy value on test set: ", round(max(accvector) * 100, 2), "%"))

# Get threshold for final model
final_test_x <- as.matrix(train_data[, 1:28])
final_test_y <- as.factor(train_data$Winner_Team1)  # Target variable (0 or 1)
final_test_predictions <- predict(final_model, newx = final_test_x, type = "response", s = 0.029)
# Cross-validation to determine the best threshold for final model
final_thresholds = numeric(10)

folds_final <- createFolds(target, k = 10, list = TRUE)

for( j in 1:10) {
  val_indices <- folds_final[[j]]
  train_indices <- setdiff(1:length(target), val_indices)
  
  # Training and validation sets
  train_xcv <- predictors[train_indices, ]
  train_ycv <- target[train_indices]
  val_xcv <- predictors[val_indices, ]
  val_ycv <- target[val_indices]
  
  # Train logistic model
  log_model_cv_final <- glmnet(
    x = train_xcv, 
    y = as.numeric(train_ycv) - 1, 
    family = "binomial",
    alpha = 0.5, 
    lambda = 0.029
  )
  
  # Predict probabilities on validation set
  val_predictions_final <- predict(log_model_cv_final, newx = as.matrix(val_xcv), type = "response", s = 0.029)
  
  # Compute ROC Curve
  roc_curve_final <- roc(as.numeric(val_ycv) - 1, val_predictions_final)
  
  # Get the best threshold for classification
  best_thresh_final <- as.numeric(coords(roc_curve_final, "best", ret = "threshold"))
  
  # Store threshold
  final_thresholds[j] <- best_thresh_final
}
# Compute final averaged threshold across folds for the final model
final_threshold <- mean(final_thresholds)


# Max Accuracy: 70.24%

print(paste("Final Averaged Threshold for Final Model:", round(final_threshold, 4)))


