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
   install.packages(c("ggplot2", "dplyr", "caret", "forecast", "pROC", "glmnet"))
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
- 
## Images of figures generated in analysis
![team_rankings](https://github.com/user-attachments/assets/b7b2318c-fe4a-4182-a435-b11abc5e26fe)
![bat_careerimpact](https://github.com/user-attachments/assets/c2a45d61-f3be-43b9-aee6-5f4b37a2c480)
![bowl_careerimpact](https://github.com/user-attachments/assets/9d7817a2-7173-4374-a6f1-fcfc485a6eab)
![allrounder_careerimpact](https://github.com/user-attachments/assets/4d45f97b-5fec-42a9-a0b4-bb9fff8bcd23)
![allrounder_bygame](https://github.com/user-attachments/assets/bcaa02c9-8e22-4eb3-b98e-f24e673715b6)
![batting_bygame](https://github.com/user-attachments/assets/ecb2bb5b-2e02-406a-ad8c-7c2070882090)
![bowling_bygame](https://github.com/user-attachments/assets/6c8b75e8-0ad6-400c-877e-05b48ea8895a)
![Bowling_Grounds](https://github.com/user-attachments/assets/b9da078a-91a4-4eb2-bcae-5e80a05d80de)
![Batting_Grounds](https://github.com/user-attachments/assets/309b5190-1a2a-4dd7-8ba1-97f887867073)
![impact_dist](https://github.com/user-attachments/assets/60f91e04-abc0-4e55-bc1b-13b6b6a66038)


## Contact
For questions or suggestions, open an issue on GitHub or reach out via email at [archithsharma@gmail.com](url).

## Acknowledgments
I extend my gratitude to the cricket analytics community and open-source contributors for making data-driven sports analysis possible. A special thank you to Rob Hyndman at Monash University for authoring the cricketdata R package to make ESPN Crincinfo data which is used in this analysis easily accessible.
