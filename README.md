# Cricket Predictions

## Description
This project analyzes T20I cricket match data to predict match outcomes based on venue factors and player performance metrics. It considers key batting and bowling statistics such as runs, strike rate, boundaries, wickets, maiden overs, and economy rate to assess the impact of players in different venues.

## Features
- Data-driven analysis of T20I matches
- Venue factor adjustments for performance predictions
- Player impact calculation based on batting and bowling statistics
- Rolling forecasts using statistical models

## Installation
To set up and run this project:
1. **Clone the repository:**
   ```sh
   git clone https://github.com/ArchithSharma/CricketPredictions.git
   cd CricketPredictions
   ```
2. **Install dependencies (for R users):**
   ```r
   install.packages(c("ggplot2", "dplyr", "caret", "forecast", "pROC", "glmnet", "e1071", "randomForest"))
   ```
3. **Run the R scripts** inside the `/code` folder to perform analysis and predictions.

## Usage
- **Data Processing:** Load and preprocess match and player data.
- **Analysis & Visualization:** Generate performance insights using `ggplot2` and `dplyr`.
- **Prediction Models:** Use regression and machine learning techniques to forecast match outcomes.

## Data
The dataset includes:
- Historical T20I matches with player and venue statistics
- Batting and bowling performance metrics
- Venue-based adjustments for better prediction accuracy

## Repository Structure
```
CricketPredictions/
│── code/          # Contains all R scripts for analysis
│── data/          # Includes datasets used in the project
│── README.md      # Project documentation
│── LICENSE        # License information
```

## License
This project is licensed under the MIT License.

## Contributors
- **Archith Sharma** *(Project Creator)*
  
## Images of figures generated in analysis
![bat_careerimpact](https://github.com/user-attachments/assets/c2a45d61-f3be-43b9-aee6-5f4b37a2c480)
![impact_dist](https://github.com/user-attachments/assets/60f91e04-abc0-4e55-bc1b-13b6b6a66038)


## Contact
For questions or suggestions, open an issue on GitHub or reach out via email at [archithsharma@gmail.com](url).

## Acknowledgments
I extend my gratitude to the cricket analytics community and open-source contributors for making data-driven sports analysis possible. A special thank you to Rob Hyndman at Monash University for authoring the cricketdata R package to make ESPN Crincinfo data which is used in this analysis easily accessible.
