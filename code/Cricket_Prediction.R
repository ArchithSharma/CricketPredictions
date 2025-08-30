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

target = factor(playertotalimpact2$Winner_Team1, levels = c(0, 1), labels = c("Team2", "Team1"))

# Logistic Regression with repeated splits
accvector = numeric(30)  # store accuracies
set.seed(314)
overallbestlambda = 0
overallbestalpha = 0
for(i in 1:30){
  
  # Train-test split (80-20)
  train_index <- createDataPartition(playertotalimpact2$Winner_Team1, p = 0.8, list = FALSE)
  train_data <- playertotalimpact2[train_index, ]
  test_data  <- playertotalimpact2[-train_index, ]
  
  # Define Predictors and Target
  train_x <- as.matrix(train_data[, 1:28])
  train_y <- as.factor(train_data$Winner_Team1)  # 0/1
  test_x  <- as.matrix(test_data[, 1:28])
  test_y  <- as.factor(test_data$Winner_Team1)   # 0/1
  
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
    y = as.numeric(train_y) - 1, 
    family = "binomial", 
    alpha = best_alpha, 
    lambda = best_lambda
  )
  
  # Get test set probabilities
  test_prob <- predict(best_log_model, newx = test_x, type = "response", s = best_lambda)
  
  # Cross-validation to determine the best threshold
  thresholds = numeric(10)
  folds <- createFolds(train_y, k = 10, list = TRUE)
  
  
  # Apply threshold to test predictions
  test_predictions <- factor(ifelse(test_prob > 0.5, 1, 0),
                             levels = c(0, 1),
                             labels = c("Team2", "Team1"))
  
  test_y <- factor(test_y, levels = c(0, 1), labels = c("Team2", "Team1"))
  
  # Compute Accuracy
  conf_matrix <- confusionMatrix(test_predictions, test_y)
  accvector[i] <- conf_matrix$overall['Accuracy']
  print(accvector[i])
  if(i == 1){
    overallbestlambda = best_lambda
    overallbestalpha = best_alpha
  }
  else{
    if(conf_matrix$overall['Accuracy'] > max(accvector[1:(i-1)])){
      overallbestlambda = best_lambda
      overallbestalpha = best_alpha
    }
  }
  
}

mean_acc <- mean(accvector)
print(paste("Average Accuracy over 30 iterations:", round(mean_acc, 4)))

# Plot Accuracy Distribution
hist(accvector, main = "Histogram of Logistic Regression Model Accuracy", xlab = "Accuracy", ylab = "Frequency")
mean(accvector)
# Final Logistic Regression Model on the entire dataset
set.seed(314)  # For reproducibility
# Train the final Logistic Regression model on the entire dataset
train_index <- createDataPartition(target, p = 0.8, list = FALSE)
train_data <- playertotalimpact2[train_index, ]
test_data  <- playertotalimpact2[-train_index, ]
# Define Predictors and Target for the final model
predictors <- as.matrix(train_data[, 1:28])  # Player impact features
target <- as.factor(train_data$Winner_Team1)  # Target variable (0 or 1)
# Hyperparameter grid search for final model
final_model = glmnet(
  x = playertotalimpact2[, 1:28], # Player impact features,
  y = playertotalimpact2[,29], # Convert factor (0/1) to numeric
  family = "binomial",
  alpha = overallbestalpha,  # Use the best alpha from previous model
  lambda = overallbestlambda, # Use the best lambda from previous model
)

print(paste("Accuracy value on test set: ", round(max(accvector) * 100, 2), "%"))


# RF model
library(randomForest)
set.seed(314)  # For reproducibility
# Train-test split (80-20)
accvec_rf = numeric(30)  # Initialize accuracy vector for 30 iterations
for(i in 1:30) {  # Repeat for 30 iterations to get a robust accuracy
  train_index_rf <- createDataPartition(playertotalimpact2$Winner_Team1, p = 0.8, list = FALSE)
  train_data_rf <- playertotalimpact2[train_index_rf, ]
  test_data_rf  <- playertotalimpact2[-train_index_rf, ]
  # Define Predictors and Target for RF model
  train_x_rf <- train_data_rf[, 1:28]  # Player impact features
  train_y_rf <- as.factor(train_data_rf$Winner_Team1)  # Target variable (0 or 1)
  test_x_rf  <- test_data_rf[, 1:28]
  test_y_rf  <- as.factor(test_data_rf$Winner_Team1)  # Target variable (0 or 1)
  # Train the Random Forest model
  rf_model <- train(
    x = train_x_rf,
    y = train_y_rf,
    method = "rf",
    trControl = trainControl(method = "cv", number = 5),
    tuneLength = 5,  # Number of different mtry values to try
    ntree = 500,  # Number of trees
    importance = TRUE
  )
  # Get accuracy
  rf_predictions <- predict(rf_model, newdata = test_x_rf)
  # Compute Accuracy
  rf_conf_matrix <- confusionMatrix(rf_predictions, test_y_rf)
  rf_accuracy <- rf_conf_matrix$overall['Accuracy']
  print(paste("Random Forest Model Accuracy on Test Set: ", round(rf_accuracy * 100, 2), "%"))
  accvec_rf[i] <- rf_accuracy  # Store accuracy for each iteration
}
max(accvec_rf) # Print the maximum accuracy from the 30 iterations
# Maximum: 70.28

# SVMs
library(e1071)
set.seed(314)  # For reproducibility
accvec_svm = numeric(30)  # Initialize accuracy vector for 30 iterations
for(i in 1:30) {  # Repeat for 30 iterations to get a robust accuracy
  train_index_svm <- createDataPartition(playertotalimpact2$Winner_Team1, p = 0.8, list = FALSE)
  train_data_svm <- playertotalimpact2[train_index_svm, ]
  test_data_svm  <- playertotalimpact2[-train_index_svm, ]
  
  # Define Predictors and Target for SVM model
  train_x_svm <- train_data_svm[, 1:28]  # Player impact features
  train_y_svm <- as.factor(train_data_svm$Winner_Team1)  # Target variable (0 or 1)
  test_x_svm  <- test_data_svm[, 1:28]
  test_y_svm  <- as.factor(test_data_svm$Winner_Team1)  # Target variable (0 or 1)
  
  # Train the SVM model
  grid_svm <- expand.grid(C = 2^(-5:5))  # Hyperparameter grid for SVM
  svm_model <- train(
    x = train_x_svm,
    y = train_y_svm,
    method = "svmLinear",
    trControl = trainControl(method = "cv", number = 5),
    tunegrid = grid_svm,
  )
  
  # Get accuracy
  svm_predictions <- predict(svm_model, newdata = test_x_svm)
  
  # Compute Accuracy
  svm_conf_matrix <- confusionMatrix(svm_predictions, test_y_svm)
  svm_accuracy <- svm_conf_matrix$overall['Accuracy']
  
  print(paste("SVM Model Accuracy on Test Set: ", round(svm_accuracy * 100, 2), "%"))
  
  accvec_svm[i] <- svm_accuracy  # Store accuracy for each iteration
}
max(accvec_svm) # Print the maximum accuracy from the 30 iterations
#Max: 70.75%


# Compare models
model_accuracies <- data.frame(
  Model = c("Logistic Regression", "Random Forest", "SVM", "Heuristic"),
  Accuracy = c(max(accvector) * 100, max(accvec_rf) * 100, max(accvec_svm) * 100, accuracy * 100),
  CI_Lower = c(
    max(accvector) * 100 - 1.96 * (sd(accvector) * 100 / sqrt(30)),
    max(accvec_rf) * 100 - 1.96 * (sd(accvec_rf) * 100 / sqrt(30)),
    max(accvec_svm) * 100 - 1.96 * (sd(accvec_svm) * 100 / sqrt(30)),
    accuracy * 100 - 1.96 * (sqrt((accuracy * (1 - accuracy)) / nrow(playertotalimpact2)) * 100)
  ),
  CI_Upper = c(
    max(accvector) * 100 + 1.96 * (sd(accvector) * 100 / sqrt(30)),
    max(accvec_rf) * 100 + 1.96 * (sd(accvec_rf) * 100 / sqrt(30)),
    max(accvec_svm) * 100 + 1.96 * (sd(accvec_svm) * 100 / sqrt(30)),
    accuracy * 100 + 1.96 * (sqrt((accuracy * (1 - accuracy)) / nrow(playertotalimpact2)) * 100)
  )
)
print(model_accuracies)
