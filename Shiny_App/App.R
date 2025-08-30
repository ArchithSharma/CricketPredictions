library(shiny)
library(dplyr)
library(plotly)
library(glmnet)
library(DT)

# Load data (assuming all data files are in the 'data' folder)
players <- read.csv("data/full_playerdatabase.csv", stringsAsFactors = FALSE)
allrounders <- read.csv("data/allrounders.csv", stringsAsFactors = FALSE)
playertotalimpact2 <- read.csv("data/trainingdata.csv")
player_rankings_2 <- read.csv("data/full_playerdatabase.csv")
player_data <- read.csv("data/player_data.csv")
venue_factors <- read.csv("data/grounddata.csv")

# Model parameters
overallbestlambda <- 0.081
overallbestalpha <- 0

# Train the model
final_model <- glmnet(
  x = playertotalimpact2[, 1:28],
  y = playertotalimpact2[, 29],
  family = "binomial",
  alpha = overallbestalpha,
  lambda = overallbestlambda
)

# Team colors for win probability visualization
team_colors <- c(
  IND = "#0078d3", PAK = "#006400", WI = "#800000", ENG = "#FF0000",
  NZL = "#000000", RSA = "#90EE90", AUS = "#FFFF00", ZIM = "#FFA500",
  SL = "#800080", AFG = "#4169E1", BAN = "#006A4E", IRE = "#009A44",
  "Team 1" = "#0078d3", "Team 2" = "#FF0000"
)

# Helper functions for Tab 1 (Player Comparison)
get_player_type <- function(player_name) {
  if (player_name %in% allrounders$Player) return("All-Rounder")
  p <- players %>% filter(Player == player_name)
  if (nrow(p) == 0) return("Unknown")
  if (p$CareerBatImpact >= p$CareerBowlImpact) "Batter" else "Bowler"
}

get_player_gauges <- function(player_name) {
  p <- players %>% filter(Player == player_name)
  if (nrow(p) == 0) return(list())
  
  type <- get_player_type(player_name)
  gauges <- list()
  
  # Define groups
  batters <- players %>% filter(!(Player %in% allrounders$Player)) %>% filter(CareerBatImpact > CareerBowlImpact)
  bowlers <- players %>% filter(!(Player %in% allrounders$Player)) %>% filter(CareerBowlImpact > CareerBatImpact)
  rounders <- players %>% filter(Player %in% allrounders$Player)
  
  # Batting gauges
  if (type %in% c("Batter", "All-Rounder")) {
    if (type == "Batter") {
      career_pct <- percentile(batters$CareerBatImpact, p$CareerBatImpact)
      career_rank <- rank(-batters$CareerBatImpact)[match(player_name, batters$Player)]
      gauges[[length(gauges)+1]] <- make_gauge(career_pct, "Career Bat Impact %", career_rank)
    } else if (type == "All-Rounder") {
      career_pct <- percentile(rounders$CareerBatImpact, p$CareerBatImpact)
      career_rank <- rank(-rounders$CareerBatImpact)[match(player_name, rounders$Player)]
      gauges[[length(gauges)+1]] <- make_gauge(career_pct, "Bat Impact % (All-Rounders)", career_rank)
      
      career_pct_bat <- percentile(batters$CareerBatImpact, p$CareerBatImpact)
      career_rank_bat <- hypothetical_rank(p$CareerBatImpact, batters$CareerBatImpact)
      gauges[[length(gauges)+1]] <- make_gauge(career_pct_bat, "Bat Impact % (vs Batters)", career_rank_bat)
    }
    
    # Per-game batting
    if (p$MatchesPlayed >= 20) {
      per_game <- p$CareerBatImpact / p$MatchesPlayed
      if (type == "Batter") {
        pg_pct <- percentile(batters$CareerBatImpact / batters$MatchesPlayed, per_game)
        pg_rank <- rank(-(batters$CareerBatImpact / batters$MatchesPlayed))[match(player_name, batters$Player)]
        gauges[[length(gauges)+1]] <- make_gauge(pg_pct, "Per-Game Bat Impact %", pg_rank)
      } else if (type == "All-Rounder") {
        pg_pct_AR <- percentile(rounders$CareerBatImpact / rounders$MatchesPlayed, per_game)
        pg_rank_AR <- rank(-(rounders$CareerBatImpact / rounders$MatchesPlayed))[match(player_name, rounders$Player)]
        gauges[[length(gauges)+1]] <- make_gauge(pg_pct_AR, "Per-Game Bat Impact % (All-Rounders)", pg_rank_AR)
        
        pg_pct_bat <- percentile(batters$CareerBatImpact / batters$MatchesPlayed, per_game)
        pg_rank_bat <- hypothetical_rank(per_game, batters$CareerBatImpact / batters$MatchesPlayed)
        gauges[[length(gauges)+1]] <- make_gauge(pg_pct_bat, "Per-Game Bat Impact % (vs Batters)", pg_rank_bat)
      }
    }
  }
  
  # Bowling gauges
  if (type %in% c("Bowler", "All-Rounder")) {
    if (type == "Bowler") {
      career_pct <- percentile(bowlers$CareerBowlImpact, p$CareerBowlImpact)
      career_rank <- rank(-bowlers$CareerBowlImpact)[match(player_name, bowlers$Player)]
      gauges[[length(gauges)+1]] <- make_gauge(career_pct, "Career Bowl Impact %", career_rank)
    } else if (type == "All-Rounder") {
      career_pct <- percentile(rounders$CareerBowlImpact, p$CareerBowlImpact)
      career_rank <- rank(-rounders$CareerBowlImpact)[match(player_name, rounders$Player)]
      gauges[[length(gauges)+1]] <- make_gauge(career_pct, "Bowl Impact % (All-Rounders)", career_rank)
      
      career_pct_bowl <- percentile(bowlers$CareerBowlImpact, p$CareerBowlImpact)
      career_rank_bowl <- hypothetical_rank(p$CareerBowlImpact, bowlers$CareerBowlImpact)
      gauges[[length(gauges)+1]] <- make_gauge(career_pct_bowl, "Bowl Impact % (vs Bowlers)", career_rank_bowl)
    }
    
    # Per-game bowling
    if (p$MatchesPlayed >= 20) {
      per_game <- p$CareerBowlImpact / p$MatchesPlayed
      if (type == "Bowler") {
        pg_pct <- percentile(bowlers$CareerBowlImpact / bowlers$MatchesPlayed, per_game)
        pg_rank <- rank(-(bowlers$CareerBowlImpact / bowlers$MatchesPlayed))[match(player_name, bowlers$Player)]
        gauges[[length(gauges)+1]] <- make_gauge(pg_pct, "Per-Game Bowl Impact %", pg_rank)
      } else if (type == "All-Rounder") {
        pg_pct_AR <- percentile(rounders$CareerBowlImpact / rounders$MatchesPlayed, per_game)
        pg_rank_AR <- rank(-(rounders$CareerBowlImpact / rounders$MatchesPlayed))[match(player_name, rounders$Player)]
        gauges[[length(gauges)+1]] <- make_gauge(pg_pct_AR, "Per-Game Bowl Impact % (All-Rounders)", pg_rank_AR)
        
        pg_pct_bowl <- percentile(bowlers$CareerBowlImpact / bowlers$MatchesPlayed, per_game)
        pg_rank_bowl <- hypothetical_rank(per_game, bowlers$CareerBowlImpact / bowlers$MatchesPlayed)
        gauges[[length(gauges)+1]] <- make_gauge(pg_pct_bowl, "Per-Game Bowl Impact % (vs Bowlers)", pg_rank_bowl)
      }
    }
  }
  
  return(gauges)
}

hypothetical_rank <- function(val, group_vals) {
  combined <- c(group_vals, val)
  ranks <- rank(-combined)
  return(ranks[length(ranks)])
}

percentile <- function(x, val) ecdf(x)(val)

make_gauge <- function(value, title, rank) {
  plot_ly(
    domain = list(x = c(0, 1), y = c(0, 1)),
    value = round(value * 100, 1),
    title = list(text = paste0(title, "<br>Rank: ", rank), font = list(size = 14)),
    type = "indicator",
    mode = "gauge+number",
    gauge = list(
      axis = list(range = list(NULL, 100)),
      bar = list(color = "darkblue"),
      bgcolor = "white",
      bordercolor = "transparent"
    )
  ) %>%
    layout(margin = list(l = 20, r = 20, t = 50, b = 20))
}

# UI
ui <- fluidPage(
  titlePanel("Cricket Analysis Suite"),
  
  tabsetPanel(
    # Tab 1: Player Comparison
    tabPanel("Player Comparison",
             sidebarLayout(
               sidebarPanel(
                 selectInput("player1", "Select First Player:", choices = players$Player),
                 selectInput("player2", "Select Second Player:", choices = players$Player),
                 actionButton("compare", "Compare Players")
               ),
               mainPanel(
                 fluidRow(
                   column(
                     6,
                     div(
                       style = "height:600px; overflow-y:scroll; border:1px solid #ccc; padding:5px;",
                       uiOutput("player1UI")
                     )
                   ),
                   column(
                     6,
                     div(
                       style = "height:600px; overflow-y:scroll; border:1px solid #ccc; padding:5px;",
                       uiOutput("player2UI")
                     )
                   )
                 )
               )
             )
    ),
    
    # Tab 2: Win Probability Predictor
    tabPanel("Win Probability Predictor",
             sidebarLayout(
               sidebarPanel(
                 selectizeInput(
                   "match_choice",
                   "Search For Match (Type in searchbar, Matches 2005-25):",
                   choices = NULL,
                   selected = NULL,
                   options = list(
                     placeholder = "Start typing to search for a match...",
                     selectOnTab = FALSE,
                     selected = NULL,
                     highlight = FALSE,
                     openOnFocus = TRUE,
                     closeAfterSelect = TRUE
                   )
                 ),
                 actionButton("predict_btn", "Predict Win Probability")
               ),
               mainPanel(
                 h3(textOutput("match_info")),
                 
                 fluidRow(
                   column(5, imageOutput("flag1", height = "60px")),
                   column(2, div(style="text-align:center; font-size:20px; font-weight:bold;", "VS")),
                   column(5, imageOutput("flag2", height = "60px"))
                 ),
                 
                 br(),
                 
                 h4("Win Probability"),
                 uiOutput("winProbBar"),
                 verbatimTextOutput("winprob_text"),
                 
                 h4("Playing XI & Predicted Impact"),
                 fluidRow(
                   column(6, DTOutput("team1_table")),
                   column(6, DTOutput("team2_table"))
                 )
               )
             )
    ),
    
    # Tab 3: Custom Match Simulator
    tabPanel("Custom Match Simulator",
             sidebarLayout(
               sidebarPanel(
                 h4("Team 1"),
                 selectizeInput(
                   "team1_players",
                   "Select 11 Players for Team 1:",
                   choices = sort(unique(player_rankings_2$Player)),
                   multiple = TRUE,
                   options = list(maxItems = 11, placeholder = "Start typing to select players...")
                 ),
                 textInput("team1_name", "Team 1 Name (e.g., ENG)", value = "Team 1"),
                 
                 br(),
                 
                 h4("Team 2"),
                 selectizeInput(
                   "team2_players",
                   "Select 11 Players for Team 2:",
                   choices = sort(unique(player_rankings_2$Player)),
                   multiple = TRUE,
                   options = list(maxItems = 11, placeholder = "Start typing to select players...")
                 ),
                 textInput("team2_name", "Team 2 Name (e.g., IND)", value = "Team 2"),
                 
                 br(),
                 
                 selectizeInput(
                   "venue_choice",
                   "Select Venue:",
                   choices = sort(unique(venue_factors$Ground)),
                   options = list(placeholder = "Start typing to select a venue...")
                 ),
                 
                 actionButton("simulate_btn", "Simulate Match")
               ),
               mainPanel(
                 h3(textOutput("match_summary")),
                 
                 fluidRow(
                   column(6, align = "center", h4(textOutput("team1_prob_display"))),
                   column(6, align = "center", h4(textOutput("team2_prob_display")))
                 ),
                 
                 br(),
                 
                 uiOutput("customWinProbBar"),
                 
                 h4("Predicted Player Impacts"),
                 fluidRow(
                   column(6, DTOutput("custom_team1_table")),
                   column(6, DTOutput("custom_team2_table"))
                 )
               )
             )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Tab 1: Player Comparison Logic
  output$player1UI <- renderUI({
    req(input$compare)
    gauges <- get_player_gauges(input$player1)
    lapply(seq_along(gauges), function(i) {
      plotlyOutput(paste0("p1gauge", i), height = "300px")
    })
  })
  
  output$player2UI <- renderUI({
    req(input$compare)
    gauges <- get_player_gauges(input$player2)
    lapply(seq_along(gauges), function(i) {
      plotlyOutput(paste0("p2gauge", i), height = "300px")
    })
  })
  
  observeEvent(input$compare, {
    gauges1 <- get_player_gauges(input$player1)
    gauges2 <- get_player_gauges(input$player2)
    
    lapply(seq_along(gauges1), function(i) {
      local({
        myi <- i
        output[[paste0("p1gauge", myi)]] <- renderPlotly({ gauges1[[myi]] })
      })
    })
    
    lapply(seq_along(gauges2), function(i) {
      local({
        myi <- i
        output[[paste0("p2gauge", myi)]] <- renderPlotly({ gauges2[[myi]] })
      })
    })
  })
  
  # Tab 2: Win Probability Predictor Logic
  match_choices <- reactive({
    player_data %>%
      group_by(ID, Date.x, Ground) %>%
      summarise(Teams = paste(unique(Country), collapse = " vs "), .groups = "drop") %>%
      mutate(label = paste(Teams, "|", Date.x, "|", Ground)) %>%
      arrange(desc(ID)) %>%
      select(ID, label)
  })
  
  observe({
    updateSelectizeInput(
      session,
      "match_choice",
      choices = setNames(match_choices()$ID, match_choices()$label),
      server = TRUE,
      selected = NULL
    )
  })
  
  observeEvent(input$predict_btn, {
    req(input$match_choice)
    
    MatchID <- as.numeric(input$match_choice)
    md <- player_data %>% filter(ID == MatchID)
    
    country1 <- unique(md$Country)[1]
    country2 <- unique(md$Country)[2]
    Venue <- unique(md$Ground)
    
    team1_playingxi <- md %>% filter(Country == country1) %>% slice(1:22) %>% pull(Player)
    team2_playingxi <- md %>% filter(Country == country2) %>% slice(1:22) %>% pull(Player)
    
    ground_buffs <- venue_factors %>% filter(Ground == Venue)
    team1_playingxi_ratings <- player_rankings_2 %>% filter(Player %in% team1_playingxi) %>%
      mutate(BatImpactperGame_2 = 0, BowlImpactperGame_2 = 0)
    team2_playingxi_ratings <- player_rankings_2 %>% filter(Player %in% team2_playingxi) %>%
      mutate(BatImpactperGame_2 = 0, BowlImpactperGame_2 = 0)
    
    # Run simulation (20 repetitions)
    kvec <- numeric(20)
    for(k in 1:20){
      for(i in 1:nrow(team1_playingxi_ratings)){
        # Batting team 1
        team1_playingxi_ratings <- team1_playingxi_ratings %>% arrange(desc(BatImpactperGame))
        if(i < 9){
          avg <- team1_playingxi_ratings$BatImpactperGame[i] *
            (1 / median(venue_factors$BattingScale2)) * ground_buffs$BattingScale2[1] *
            ((9-i)/(10-i))
          team1_playingxi_ratings$BatImpactperGame_2[i] <-
            rnorm(1, avg, abs(avg / team1_playingxi_ratings$MatchesPlayed[i]))
        } else {
          avg <- team1_playingxi_ratings$BatImpactperGame[i]/5
          team1_playingxi_ratings$BatImpactperGame_2[i] <-
            rnorm(1, avg, abs(avg / team1_playingxi_ratings$MatchesPlayed[i]))
        }
        # Bowling team 1
        team1_playingxi_ratings <- team1_playingxi_ratings %>% arrange(desc(BowlImpactperGame))
        if(i < 7){
          avg <- team1_playingxi_ratings$BowlImpactperGame[i] *
            (1/median(venue_factors$BowlingScale2)) * ground_buffs$BowlingScale2[1] *
            ((8-i)/(9-i))
          team1_playingxi_ratings$BowlImpactperGame_2[i] <-
            rnorm(1, avg, abs(avg / team1_playingxi_ratings$MatchesPlayed[i]))
        } else {
          team1_playingxi_ratings$BowlImpactperGame_2[i] <- 0
        }
      }
      for(i in 1:nrow(team2_playingxi_ratings)){
        # Batting team 2
        team2_playingxi_ratings <- team2_playingxi_ratings %>% arrange(desc(BatImpactperGame))
        if(i < 9){
          avg <- team2_playingxi_ratings$BatImpactperGame[i] *
            (1 / median(venue_factors$BattingScale2)) * ground_buffs$BattingScale2[1] *
            ((9-i)/(10-i))
          team2_playingxi_ratings$BatImpactperGame_2[i] <-
            rnorm(1, avg, abs(avg / team2_playingxi_ratings$MatchesPlayed[i]))
        } else {
          avg <- team2_playingxi_ratings$BatImpactperGame[i]/5
          team2_playingxi_ratings$BatImpactperGame_2[i] <-
            rnorm(1, avg, abs(avg / team2_playingxi_ratings$MatchesPlayed[i]))
        }
        # Bowling team 2
        team2_playingxi_ratings <- team2_playingxi_ratings %>% arrange(desc(BowlImpactperGame))
        if(i < 7){
          avg <- team2_playingxi_ratings$BowlImpactperGame[i] *
            (1/median(venue_factors$BowlingScale2)) * ground_buffs$BowlingScale2[1] *
            ((8-i)/(9-i))
          team2_playingxi_ratings$BowlImpactperGame_2[i] <-
            rnorm(1, avg, abs(avg / team2_playingxi_ratings$MatchesPlayed[i]))
        } else {
          team2_playingxi_ratings$BowlImpactperGame_2[i] <- 0
        }
      }
      
      # Rearrange for prediction
      team1_batting <- (team1_playingxi_ratings %>% arrange(desc(BatImpactperGame_2)))$BatImpactperGame_2
      team2_batting <- (team2_playingxi_ratings %>% arrange(desc(BatImpactperGame_2)))$BatImpactperGame_2
      team1_bowling <- (team1_playingxi_ratings %>% arrange(desc(BowlImpactperGame_2)))$BowlImpactperGame_2
      team2_bowling <- (team2_playingxi_ratings %>% arrange(desc(BowlImpactperGame_2)))$BowlImpactperGame_2
      
      newdata <- data.frame(
        Batsman1_T1 = team1_batting[1], Batsman2_T1 = team1_batting[2], Batsman3_T1 = team1_batting[3],
        Batsman4_T1 = team1_batting[4], Batsman5_T1 = team1_batting[5], Batsman6_T1 = team1_batting[6],
        Batsman7_T1 = team1_batting[7], Batsman8_T1 = team1_batting[8],
        Bowler1_T1 = team1_bowling[1], Bowler2_T1 = team1_bowling[2], Bowler3_T1 = team1_bowling[3],
        Bowler4_T1 = team1_bowling[4], Bowler5_T1 = team1_bowling[5], Bowler6_T1 = team1_bowling[6],
        Batsman1_T2 = team2_batting[1], Batsman2_T2 = team2_batting[2], Batsman3_T2 = team2_batting[3],
        Batsman4_T2 = team2_batting[4], Batsman5_T2 = team2_batting[5], Batsman6_T2 = team2_batting[6],
        Batsman7_T2 = team2_batting[7], Batsman8_T2 = team2_batting[8],
        Bowler1_T2 = team2_bowling[1], Bowler2_T2 = team2_bowling[2], Bowler3_T2 = team2_bowling[3],
        Bowler4_T2 = team2_bowling[4], Bowler5_T2 = team2_bowling[5], Bowler6_T2 = team2_bowling[6]
      )
      
      kvec[k] <- predict(final_model, newx = as.matrix(newdata), type = "response")
    }
    
    winprob <- mean(kvec)
    team1_prob <- round(winprob * 100, 1)
    team2_prob <- round(100 - team1_prob, 1)
    
    output$match_info <- renderText({
      paste("Match:", country1, "vs", country2, "at", Venue)
    })
    
    output$flag1 <- renderImage({
      list(
        src = file.path("CountryFlags", paste0(country1, ".png")),
        contentType = "image/png",
        width = 100,
        height = 60
      )
    }, deleteFile = FALSE)
    
    output$flag2 <- renderImage({
      list(
        src = file.path("CountryFlags", paste0(country2, ".png")),
        contentType = "image/png",
        width = 100,
        height = 60
      )
    }, deleteFile = FALSE)
    
    output$winprob_text <- renderText({
      paste0(country1, " Win Probability: ", round(winprob*100,2), "%")
    })
    
    output$winProbBar <- renderUI({
      tags$div(
        style="width:100%; background-color:#ddd; height:30px; border-radius:6px; overflow:hidden; display:flex;",
        tags$div(
          style = paste0("flex:", team1_prob, "; background-color:", team_colors[country1],
                         "; color:", ifelse(country1 == "AUS", "black", "white"), "; text-align:center; line-height:30px;"),
          paste0(country1, " ", team1_prob, "%")
        ),
        tags$div(
          style = paste0("flex:", team2_prob, "; background-color:", team_colors[country2],
                         "; color:", ifelse(country2 == "AUS", "black", "white"), "; text-align:center; line-height:30px;"),
          paste0(team2_prob, "% ", country2)
        )
      )
    })
    
    output$team1_table <- renderDT({
      req(input$predict_btn)
      req(team1_playingxi_ratings)
      
      team1_playingxi_ratings %>% 
        select(Player, BatImpactperGame_2, BowlImpactperGame_2) %>% 
        mutate(
          BatImpactperGame_2 = round(BatImpactperGame_2, 2),
          BowlImpactperGame_2 = round(BowlImpactperGame_2, 2)
        ) %>% 
        rename(
          `Predicted Batting Impact` = BatImpactperGame_2,
          `Predicted Bowling Impact` = BowlImpactperGame_2
        )
    }, options = list(pageLength = 11, searching = FALSE), rownames = FALSE)
    
    output$team2_table <- renderDT({
      req(input$predict_btn)
      req(team2_playingxi_ratings)
      
      team2_playingxi_ratings %>% 
        select(Player, BatImpactperGame_2, BowlImpactperGame_2) %>% 
        mutate(
          BatImpactperGame_2 = round(BatImpactperGame_2, 2),
          BowlImpactperGame_2 = round(BowlImpactperGame_2, 2)
        ) %>% 
        rename(
          `Predicted Batting Impact` = BatImpactperGame_2,
          `Predicted Bowling Impact` = BowlImpactperGame_2
        )
    }, options = list(pageLength = 11, searching = FALSE), rownames = FALSE)
  })
  
  # Tab 3: Custom Match Simulator Logic
  observeEvent(input$simulate_btn, {
    req(input$team1_players, input$team2_players, input$venue_choice)
    
    if (length(input$team1_players) != 11 || length(input$team2_players) != 11) {
      showNotification("Please select exactly 11 players for each team.", type = "warning")
      return()
    }
    
    team1_playingxi_ratings <- player_rankings_2 %>% filter(Player %in% input$team1_players)
    team2_playingxi_ratings <- player_rankings_2 %>% filter(Player %in% input$team2_players)
    ground_buffs <- venue_factors %>% filter(Ground == input$venue_choice)
    
    team1_playingxi_ratings$BatImpactperGame_2 <- 0
    team1_playingxi_ratings$BowlImpactperGame_2 <- 0
    team2_playingxi_ratings$BatImpactperGame_2 <- 0
    team2_playingxi_ratings$BowlImpactperGame_2 <- 0
    
    kvec <- numeric(20)
    for(k in 1:20){
      for(i in 1:nrow(team1_playingxi_ratings)){
        # Batting team 1
        team1_playingxi_ratings <- team1_playingxi_ratings %>% arrange(desc(BatImpactperGame))
        if(i < 9){
          avg_bat <- team1_playingxi_ratings$BatImpactperGame[i] *
            (1 / median(venue_factors$BattingScale2)) * ground_buffs$BattingScale2[1] *
            ((9 - i) / (10 - i))
          team1_playingxi_ratings$BatImpactperGame_2[i] <-
            rnorm(1, avg_bat, abs(avg_bat / team1_playingxi_ratings$MatchesPlayed[i]))
        } else {
          avg_bat <- team1_playingxi_ratings$BatImpactperGame[i]/5
          team1_playingxi_ratings$BatImpactperGame_2[i] <-
            rnorm(1, avg_bat, abs(avg_bat / team1_playingxi_ratings$MatchesPlayed[i]))
        }
        
        # Bowling team 1
        team1_playingxi_ratings <- team1_playingxi_ratings %>% arrange(desc(BowlImpactperGame))
        if(i < 7){
          avg_bowl <- team1_playingxi_ratings$BowlImpactperGame[i] *
            (1 / median(venue_factors$BowlingScale2)) * ground_buffs$BowlingScale2[1] *
            ((8 - i) / (9 - i))
          team1_playingxi_ratings$BowlImpactperGame_2[i] <-
            rnorm(1, avg_bowl, abs(avg_bowl / team1_playingxi_ratings$MatchesPlayed[i]))
        } else {
          team1_playingxi_ratings$BowlImpactperGame_2[i] <- 0
        }
      }
      
      for(i in 1:nrow(team2_playingxi_ratings)){
        # Batting team 2
        team2_playingxi_ratings <- team2_playingxi_ratings %>% arrange(desc(BatImpactperGame))
        if(i < 9){
          avg_bat <- team2_playingxi_ratings$BatImpactperGame[i] *
            (1 / median(venue_factors$BattingScale2)) * ground_buffs$BattingScale2[1] *
            ((9 - i) / (10 - i))
          team2_playingxi_ratings$BatImpactperGame_2[i] <-
            rnorm(1, avg_bat, abs(avg_bat / team2_playingxi_ratings$MatchesPlayed[i]))
        } else {
          avg_bat <- team2_playingxi_ratings$BatImpactperGame[i]/5
          team2_playingxi_ratings$BatImpactperGame_2[i] <-
            rnorm(1, avg_bat, abs(avg_bat / team2_playingxi_ratings$MatchesPlayed[i]))
        }
        
        # Bowling team 2
        team2_playingxi_ratings <- team2_playingxi_ratings %>% arrange(desc(BowlImpactperGame))
        if(i < 7){
          avg_bowl <- team2_playingxi_ratings$BowlImpactperGame[i] *
            (1 / median(venue_factors$BowlingScale2)) * ground_buffs$BowlingScale2[1] *
            ((8 - i) / (9 - i))
          team2_playingxi_ratings$BowlImpactperGame_2[i] <-
            rnorm(1, avg_bowl, abs(avg_bowl / team2_playingxi_ratings$MatchesPlayed[i]))
        } else {
          team2_playingxi_ratings$BowlImpactperGame_2[i] <- 0
        }
      }
      
      team1_batting <- (team1_playingxi_ratings %>% arrange(desc(BatImpactperGame_2)))$BatImpactperGame_2
      team2_batting <- (team2_playingxi_ratings %>% arrange(desc(BatImpactperGame_2)))$BatImpactperGame_2
      team1_bowling <- (team1_playingxi_ratings %>% arrange(desc(BowlImpactperGame_2)))$BowlImpactperGame_2
      team2_bowling <- (team2_playingxi_ratings %>% arrange(desc(BowlImpactperGame_2)))$BowlImpactperGame_2
      
      newdata <- data.frame(
        Batsman1_T1 = team1_batting[1], Batsman2_T1 = team1_batting[2], Batsman3_T1 = team1_batting[3],
        Batsman4_T1 = team1_batting[4], Batsman5_T1 = team1_batting[5], Batsman6_T1 = team1_batting[6],
        Batsman7_T1 = team1_batting[7], Batsman8_T1 = team1_batting[8],
        Bowler1_T1 = team1_bowling[1], Bowler2_T1 = team1_bowling[2], Bowler3_T1 = team1_bowling[3],
        Bowler4_T1 = team1_bowling[4], Bowler5_T1 = team1_bowling[5], Bowler6_T1 = team1_bowling[6],
        Batsman1_T2 = team2_batting[1], Batsman2_T2 = team2_batting[2], Batsman3_T2 = team2_batting[3],
        Batsman4_T2 = team2_batting[4], Batsman5_T2 = team2_batting[5], Batsman6_T2 = team2_batting[6],
        Batsman7_T2 = team2_batting[7], Batsman8_T2 = team2_batting[8],
        Bowler1_T2 = team2_bowling[1], Bowler2_T2 = team2_bowling[2], Bowler3_T2 = team2_bowling[3],
        Bowler4_T2 = team2_bowling[4], Bowler5_T2 = team2_bowling[5], Bowler6_T2 = team2_bowling[6]
      )
      
      kvec[k] <- predict(final_model, newx = as.matrix(newdata), type = "response")
    }
    
    winprob <- mean(kvec)
    team1_prob <- round(winprob * 100, 1)
    team2_prob <- round(100 - team1_prob, 1)
    
    output$match_summary <- renderText({
      paste0(input$team1_name, " vs ", input$team2_name, " at ", input$venue_choice)
    })
    
    output$team1_prob_display <- renderText({
      paste0(input$team1_name, " Win Probability: ", team1_prob, "%")
    })
    
    output$team2_prob_display <- renderText({
      paste0(input$team2_name, " Win Probability: ", team2_prob, "%")
    })
    
    output$customWinProbBar <- renderUI({
      tags$div(
        style = "width:100%; background-color:#ddd; height:30px; border-radius:6px; overflow:hidden; display:flex;",
        tags$div(
          style = paste0("flex:", team1_prob, "; background-color:", team_colors["Team 1"], "; color:white; text-align:center; line-height:30px;"),
          paste0(input$team1_name, " ", team1_prob, "%")
        ),
        tags$div(
          style = paste0("flex:", team2_prob, "; background-color:", team_colors["Team 2"], "; color:white; text-align:center; line-height:30px;"),
          paste0(team2_prob, "% ", input$team2_name)
        )
      )
    })
    
    output$custom_team1_table <- renderDT({
      req(team1_playingxi_ratings)
      team1_playingxi_ratings %>%
        select(Player, BatImpactperGame_2, BowlImpactperGame_2) %>%
        mutate(
          BatImpactperGame_2 = round(BatImpactperGame_2, 2),
          BowlImpactperGame_2 = round(BowlImpactperGame_2, 2)
        ) %>%
        rename(
          `Predicted Batting Impact` = BatImpactperGame_2,
          `Predicted Bowling Impact` = BowlImpactperGame_2
        )
    }, options = list(pageLength = 11, searching = FALSE), rownames = FALSE)
    
    output$custom_team2_table <- renderDT({
      req(team2_playingxi_ratings)
      team2_playingxi_ratings %>%
        select(Player, BatImpactperGame_2, BowlImpactperGame_2) %>%
        mutate(
          BatImpactperGame_2 = round(BatImpactperGame_2, 2),
          BowlImpactperGame_2 = round(BowlImpactperGame_2, 2)
        ) %>%
        rename(
          `Predicted Batting Impact` = BatImpactperGame_2,
          `Predicted Bowling Impact` = BowlImpactperGame_2
        )
    }, options = list(pageLength = 11, searching = FALSE), rownames = FALSE)
  })
}

shinyApp(ui, server)