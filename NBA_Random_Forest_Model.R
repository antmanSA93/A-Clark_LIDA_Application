
# Adapted from Clive Beggs 3rd April 2024 iteration by Anthony Clark 07/04/2024
# Random forest analysis of NBA data
# Trying to get a feature importance plot (scree plot) of the variables that have the strongest influence on match outcome in the NBA


library(tidyverse)
library(caTools)
library(cvms)
library(caret)
library(pROC)
library(randomForest)
# rm(list = ls())    # Clears all variables from workspace


########## Tackling the WNBA data first ##########################
# Load data 


# About data: Data set containing 13 seasons of WNBA data, The Result, team strength (win% and season average %), 
# travel, match frequency, rest, and time-zone change metrics for team and opposition.

# NB. The random forest regression model can only have one response variable. So in this case 
# we will use 'Results' as the repsonse variable.



# Load data 
# NBAdat <- read.csv("NBA_datafor_xgboost.csv") %>%
#   mutate(Result = as.factor(Result))
Ntrain_set <- read.csv("nba_train_data.csv") %>%
  mutate(Result = as.factor(Result))
Ntest_set <- read.csv("nba_test_data.csv") %>%
  mutate(Result = as.factor(Result))


# About data: Data set containing 13 seasons of NBA data, The Result, team strength (win% and season average %), 
# travel, match frequency, rest, and time-zone change metrics for team and opposition.

# NB. The random forest regression model can only have one response variable. So in this case 
# we will use 'Result' (win/loss) as the repsonse variable.

# Select variables for inclusion in analysis - should be 90 in DF
Ntrain_set <- Ntrain_set %>% select(-X,-Performance_Tier,-S.AvgWinPct,-Opp_S.AvgWinPct,-Opp_Performance_Tier ,-PointsDiff,-Season,
                                    -Team, -Opposition, -Date, -G11, -G13, -G15, -G17, -G19, 
                                    -Opp_G11, -Opp_G13, -Opp_G15, -Opp_G17, -Opp_G19)
Ntest_set <- Ntest_set %>% select(-X,-Performance_Tier,-S.AvgWinPct,-Opp_S.AvgWinPct,-Opp_Performance_Tier ,-PointsDiff,-Season,
                                  -Team, -Opposition, -Date, -G11, -G13, -G15, -G17, -G19, 
                                  -Opp_G11, -Opp_G13, -Opp_G15, -Opp_G17, -Opp_G19)
names(Ntrain_set)
names(Ntest_set)
# Create random forest regression model

set.seed(42) # This makes the model repeatable.
(Nrf1 <- randomForest(Result ~., data=Ntrain_set, mtry=2, ntree=500, importance=TRUE))

#OOB of 30.4%, so expected accuracy of 69.6%

# Determine variable importance using Decrease in Gini impurity
importance(Nrf1) # Produces table
varImpPlot(Nrf1, n.var = 20) # Produces scree plot with top 20 variables

##### From this it is quite obvious that the team strength variables (win percentage, difference in team ranking, team ranking etc.)
#### are the primary predictors of match outcome




#Compare predictions with actual results

Nrf.pred <- predict(Nrf1, newdata = Ntest_set, type = "response") #used testing data set
view(Nrf.pred)

Npred.comp <- cbind.data.frame(Ntest_set$Result, Nrf.pred)
Npred.comp <- Npred.comp %>% 
  rename(Ntest.outcome = `Ntest_set$Result`) #renaming column to allow easy calculation
names(Npred.comp)
view(Npred.comp)

accuracy <- sum(Npred.comp$Ntest.outcome == Npred.comp$Nrf.pred) / nrow(Npred.comp) * 100
accuracy ## Accuracy of 69.57%

confusionMatrix(factor(Npred.comp$Ntest.outcome), factor(Npred.comp$Nrf.pred))


#### So at the end of the day RF predicts that most of the variance comes from team strength - which makes sense. 





