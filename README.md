# Stats & Stumps: A Novel Impact Factor for Player Comparison and Match Prediction in Cricket

## Description
This project analyzes T20I cricket match data to predict match outcomes based on venue factors and player performance metrics. It considers key batting and bowling statistics such as runs, strike rate, boundaries, wickets, maiden overs, and economy rate to assess the impact of players in different venues.

## Features
- Data-driven analysis of T20I matches
- Venue factor adjustments for performance predictions
- Player impact calculation based on batting and bowling statistics
- Rolling forecasts using statistical models
- R Markdown Version for easy reproducibility: https://anonymous.4open.science/w/CricketPredictions-8D84/
- ShinyApp for player comparison, past match model output, and future match prediction by inputting playing XI: https://anonymouscricket.shinyapps.io/Cricket-Analyzer/

## Installation
To set up and run this project:
1. **Clone the repository:**
   ```sh
   git clone https://github.com/ArchithSharma/CricketPredictions.git
   cd CricketPredictions
   ```
2. **Install dependencies (for R users):**
   ```r
   install.packages(c("cricketdata", "knitr", "lsr", "ggplot2", "dplyr", "caret", "forecast", "pROC", "glmnet", "e1071", "randomForest"))
   ```
3. **Run the [R markdown file](Markdown_Script/Stats_and_Stumps.Rmd)** inside the `/Markdown_Script` folder to perform analysis and predictions.

## Usage
- **Data Processing:** Load and preprocess match and player data.
- **Analysis & Visualization:** Generate performance insights using `ggplot2` and `dplyr`.
- **Prediction Models:** Use regression and machine learning techniques to forecast match outcomes.
- **App Utility:** Predict matches by inputting two playing XIs and a venue in the [ShinyApp](https://anonymouscricket.shinyapps.io/Cricket-Analyzer/).

## Data
The dataset includes:
- Historical T20I matches with player and venue statistics
- Batting and bowling performance metrics
- Venue-based adjustments for better prediction accuracy

## Repository Structure
```
CricketPredictions/
│── code/                  # Contains R code that was used in designing project          
│── data/                  # Includes datasets used in the project
│── Markdown_Script/       # Markdown version available on RPubs, fully reproducible
│── Shiny_App/             # Includes Source Code for Shiny App                 
│── README.md              # Project documentation
│── LICENSE                # License information

```

## License
This project is licensed under the MIT License.

## Contributors
- **Archith Sharma** *(Project Creator)*

## Contact
For questions or suggestions, open an issue on GitHub.

## Acknowledgments
I extend my gratitude to the cricket analytics community and open-source contributors for making data-driven sports analysis possible. A special thank you to Rob Hyndman at Monash University for authoring the cricketdata R package to make ESPN Crincinfo data which is used in this analysis easily accessible.
