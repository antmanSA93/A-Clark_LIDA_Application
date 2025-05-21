# NBA Travel and Fixture Congestion Analysis  
**A Demonstration of Data Science Skills for the LIDA Data Scientist Development Programme**

## Overview

This repository was created as part of my application to the Leeds Institute for Data Analytics (LIDA) Data Scientist Development Programme. It showcases my R coding proficiency and approach to best practices in data cleaning, feature engineering, modelling, and documentation.

The project uses a subset of NBA regular season data (2010–2013) to explore how travel (air distance, time zone changes) and fixture congestion (number of games played in recent time windows) affect match outcomes. Unlike previous studies that examined these factors in isolation and over short time frames, this analysis evaluates them together over multiple time windows (e.g., 3, 7, 14, 28 days), while controlling for team strength and home/away advantage.

## Key Question

> Which has a greater influence on NBA match outcomes—travel demands, fixture congestion, team strength, or Home/Away advantage?

## Project Structure

The code is split into four clearly documented scripts, which should be run in the following order:

1. **`NBA_Data_Scraping_EarlyWrangle.R`**  
   - Scrapes regular season match data from 2010–2023 using the `airball` package by Jose Fernandez (2020) - <https://github.com/josedv82/airball>.
   - Calculates travel-related variables such as air distance, time zone changes, and rest days.
   - Outputs a cleaned and filtered dataset (`NBA_data1.xlsx`).

2. **`NBA_Data_Engineering.R`**  
   - Performs extensive feature engineering:
     - Travel and fixture congestion over multiple time windows (3–28 days).
     - Rolling win percentages and team strength metrics.
     - Reformats the dataset to one row per match (home vs. away).
     - Computes difference variables between home and away teams.
   - Outputs a fully engineered dataset (`NBA_datafor_analysis.csv`) ready for modelling.

3. **`Splitting_train_test_NBA.R`**  
   - Splits the engineered dataset into training and testing subsets using a 70/30 split, stratified by team and season.
   - Removes unnecessary columns and exports two CSV files for modelling:
     - `nba_train_data.csv`
     - `nba_test_data.csv`

4. **`NBA_Random_Forest_Model.R`**  
   - Trains a Random Forest classifier to predict match outcome (win/loss).
   - Evaluates feature importance and model accuracy (~69.6%).
   - Highlights that team strength is the strongest predictor, but also visualises the relative importance of travel and congestion variables.

## Instructions

- Before running any scripts, please **set your working directory** to a folder of your choice.
- Ensure you have the required packages installed (e.g., `tidyverse`, `readxl`, `caret`, `randomForest`, `airball`, `nbastatR`).
- The scripts are well-commented and designed to run sequentially.

## Dependencies

- R (version ≥ 4.0)
- Key packages: `tidyverse`, `caret`, `randomForest`, `airball`, `nbastatR`, `readxl`, `openxlsx`, `lubridate`, `zoo`, `pROC`, `mlr`, `cvms`, `caTools`, `xgboost`, `h2o`

## Notes

- The 2019–20 season (COVID “bubble”) is excluded from the analysis due to its unique travel profile.
- This analysis represents a scaled-down version of the final study in my PhD, which included 14 full seasons.
- All data is publicly sourced and used for educational purposes only.

## Author

Anthony Clark  
PhD Candidate, Sport and Exercise Physiology  
Leeds Beckett University  
GitHub: [antmanSA93]  
Date: May 2025
