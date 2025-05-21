
## The purpose of this script is to calculate the remaining variables for the random forest analysis and to correctly format the data
## frame so that it is ready for the analysis.
#
#
## The new variables are the fixture congestion and travel variables for points in time up to 4 weeks (28 days)
## preceding each match for each team. These are for 1)The number are matches, 2) The amount of air travel distance (km),
## and 3) The number of time zones crossed by each team before each match. For example, it would be number of games (G) 
## played by each team for 3 days (G3), 5 days (G5), G7, G14, G21, and G28 before each match.
#
# In addition to the fixture congestion and travel variables, we calculate some team strength variables (e.g., team ranking, or team % at the
# time of the match), as these are important controls for the outcome of a sporting event.
#
#
## The initial data frame also had 2 observations (rows) for each match - one for the home team and one for the away.
## This would have resulted in doubling up of the data and influenced the analysis. So to fix this we rearrange the data frame
## so that there is 1 observation (row) for each match. As a result each row will contain all the team strength, fixture congestion and travel variables
## for both the Home and Away teams, which is what we need for the analysis. 
## Then finally, we also calculated "Difference variables" which are the difference between the Home and Away teams for the amount of travel and matches they
## have played. The final DF will have 110 variables in it.



# Load packages

library(dplyr)
library(readr)
library(usethis)
library(airball)
library(nbastatR)
library(devtools)
library(purrr)
library(magrittr)
library(glue)
library(openxlsx)
library(readxl)
library(tidyverse)
library(lubridate)
library(performance)
library(emmeans)


### Load data - Data is an excel file of all the NBA observations (2 rows for each match - 1 for home team, one for away)
#               with the base rest, distance, and time zone change variables derived from the NBA_travel function
#              

Ndata <- read_excel("C:/Users/clark57/OneDrive - Leeds Beckett University/Life Admin/Job applications/LIDA data science position/NBA_data1.xlsx", 
                   col_types = c("text", "numeric", "text", 
                                 "date", "text", "text", "text", "text", 
                                 "text", "numeric", "numeric", "numeric", 
                                 "numeric", "text"))  ### defining vector types for columns
                                                      ### Should be a DF of 31290 observations == 15645 matches

Ndata <- Ndata %>%
  mutate(Rest = ifelse(Rest > 15, 15, Rest)) ### making sure 1st game of season rest is 15 for all 
                                               # (just to keep it a constant number that can be filtered out later)

# made a mistake in original WNBA wrangle
# when a new season starts it will still read 1st matches as away-home, need to change that for accuracy
# (even though we will be filtering out all first matches)


Ndata <- Ndata %>%
  mutate(Distance = ifelse(Location == "Home" & Team != lag(Team), 0, Distance))

Ndata <- Ndata %>%
  mutate(TZ.change = ifelse(Location == "Home" & Team != lag(Team), 0, TZ.change))



##################################################################################################################
##########

## Data engineering script


Ndat <- Ndata %>% 
  ### add regular season 'Phase' column 
  mutate(Phase = "RS") %>%
  ungroup() %>%
  group_by(Season, Team) %>%
  mutate(Date = as.Date(Date)) %>%
  #add and clean missing dates needed for time between games
  complete(Date = seq.Date(min(Date), max(Date), by="day")) %>% 
  mutate(Phase = ifelse(is.na(Phase), "-", Phase)) %>%
  
  #calculate number of games played in different time windows
  mutate(G3 = zoo::rollapplyr(Phase == "RS", width = 3, sum, partial = T)) %>%
  mutate(G5 = zoo::rollapplyr(Phase == "RS", width = 5, sum, partial = T)) %>%
  mutate(G7 = zoo::rollapplyr(Phase == "RS", width = 7, sum, partial = T)) %>%
  mutate(G9 = zoo::rollapplyr(Phase == "RS", width = 9, sum, partial = T)) %>%
  mutate(G11 = zoo::rollapplyr(Phase == "RS", width = 11, sum, partial = T)) %>%
  mutate(G14 = zoo::rollapplyr(Phase == "RS", width = 14, sum, partial = T)) %>%
  mutate(G13 = zoo::rollapplyr(Phase == "RS", width = 13, sum, partial = T)) %>%
  mutate(G15 = zoo::rollapplyr(Phase == "RS", width = 15, sum, partial = T)) %>%
  mutate(G17 = zoo::rollapplyr(Phase == "RS", width = 17, sum, partial = T)) %>%
  mutate(G19 = zoo::rollapplyr(Phase == "RS", width = 19, sum, partial = T)) %>%
  mutate(G21 = zoo::rollapplyr(Phase == "RS", width = 21, sum, partial = T)) %>%
  mutate(G28 = zoo::rollapplyr(Phase == "RS", width = 28, sum, partial = T)) %>%
  mutate(GTotal = zoo::rollapplyr(Phase == "RS", width = n(), sum, partial = T)) %>%
  
  #calculate number of time zone changes for different time windows
  mutate(TZ.change = as.numeric(TZ.change)) %>%
  mutate(TZ.change = ifelse(is.na(TZ.change), 0, TZ.change)) %>%
  mutate(TZ.change3 = zoo::rollapplyr(TZ.change != 0, width = 3, sum, partial = T)) %>%
  mutate(TZ.change5 = zoo::rollapplyr(TZ.change != 0, width = 5, sum, partial = T)) %>%
  mutate(TZ.change7 = zoo::rollapplyr(TZ.change != 0, width = 7, sum, partial = T)) %>%
  mutate(TZ.change9 = zoo::rollapplyr(TZ.change != 0, width = 9, sum, partial = T)) %>%
  mutate(TZ.change14 = zoo::rollapplyr(TZ.change != 0, width = 14, sum, partial = T)) %>%
  mutate(TZ.change21 = zoo::rollapplyr(TZ.change != 0, width = 21, sum, partial = T)) %>%
  mutate(TZ.change28 = zoo::rollapplyr(TZ.change != 0, width = 28, sum, partial = T)) %>%
  mutate(TZ.Total = zoo::rollapplyr(TZ.change != 0, width = n(), sum, partial = T)) %>%
  mutate(TZ.change = abs(TZ.change)) %>%
  
  #calculate distance traveled for different time windows
  mutate(Distance = ifelse(is.na(Distance), 0, Distance)) %>%
  mutate(Distance3 = zoo::rollapplyr(Distance, width = 3, sum, partial = T)) %>%
  mutate(Distance5 = zoo::rollapplyr(Distance, width = 5, sum, partial = T)) %>%
  mutate(Distance7 = zoo::rollapplyr(Distance, width = 7, sum, partial = T)) %>%
  mutate(Distance9 = zoo::rollapplyr(Distance, width = 9, sum, partial = T)) %>%
  mutate(Distance14 = zoo::rollapplyr(Distance, width = 14, sum, partial = T)) %>%
  mutate(Distance21 = zoo::rollapplyr(Distance, width = 21, sum, partial = T)) %>%
  mutate(Distance28 = zoo::rollapplyr(Distance, width = 28, sum, partial = T)) %>%
  mutate(DistanceTotal = zoo::rollapplyr(Distance, width = n(), sum, partial = T)) %>%
  filter(Phase != "-") %>%
  
  #calculate winning % ########## NOTE: this is a rolling win percentage. Season average win percentage is also below 

  
  mutate(Games = row_number()) %>%
  mutate(W = zoo::rollapplyr(W.L == "W", Games - 1, sum, partial = TRUE, align = "right", fill = NA)) %>%
  mutate(win_pct =  round(W / (Games - 1), 2)) %>%    ## Rolling win%
  mutate(Total_Wins = sum(W.L == "W", na.rm = TRUE),  # Calculate total wins per season per team
         Total_Games = max(Games),                    # Get the total number of games from the maximum value in the Games column
         S.AvgWinPct = round(Total_Wins / Total_Games, 2)) %>%  # Calculate and round the season average win percentage
  
  
  #calculate winning streak
  mutate(Streak2 = sequence(rle(W.L)$lengths)) %>% 
  mutate(Streak2 = ifelse(W.L == "L", Streak2 * -1, Streak2)) %>%
  mutate(Streak1 = lag(Streak2)) %>%
  mutate(Streak = ifelse(is.na(Streak1), 0, Streak1))%>%
  select(-Streak2, -Streak1) %>%
  ungroup()  

###### Calculate season rankings
# 1st create season averages dataframe

Nseason_averages <- Ndat %>%
  group_by(Season, Team) %>%
  summarise(
    Total_Wins2 = sum(W.L == "W", na.rm = TRUE),
    Total_Games2 = n(),
    Total_PointsDiff = sum(PointsDiff, na.rm = TRUE),  # Sum points difference for each team per season
    S.AvgWinPct2 = round(Total_Wins2 / Total_Games2, 2),
    .groups = "drop"
  ) %>%
  arrange(Season, -S.AvgWinPct2, -Total_PointsDiff) %>%  # Sort by season, then win percentage and points difference
  group_by(Season) %>%
  mutate(Season.Rank = row_number()) 

# Step 2: Join the summary back to the original dataframe
Ndat <- Ndat %>%
  left_join(Nseason_averages, by = c("Season", "Team")) %>%
  mutate(Performance_Tier = case_when(
    Season.Rank >= 1 & Season.Rank <= 10 ~ "Top",
    Season.Rank >= 11 & Season.Rank <=20 ~ "Middle",
    Season.Rank >= 21 & Season.Rank <= 30 ~ "Bottom",
    TRUE ~ NA_character_  # This handles any unexpected cases, such as more or fewer than 12 teams per season
  ))



# select features of interest
Ndat <- Ndat %>% select(League,Season, Team, Opposition, Date, Location,Result = W.L, PointsDiff ,Season.Rank ,Performance_Tier ,Streak, win_pct, S.AvgWinPct,Rest, TZ.change,  ### add in points diff
                        Distance, DistanceTotal, Distance3, Distance5,Distance7, Distance9,Distance14,
                        Distance21, Distance28,  
                        GTotal,G3, G5, G7, G9, G11, G13, G14, G15, G17, G19, G21,
                        G28,
                        TZ.Total, TZ.change3, TZ.change5, TZ.change7, TZ.change9, TZ.change14,
                        TZ.change21, TZ.change28) %>%
  
  mutate(Location = ifelse(Location == "Away", 1, 0)) %>% ## Home = 0, Away = 1
  mutate(Result = ifelse(Result == "W", 1, 0))    ### Win = 1, loss = 0


# code to get the same features for the opponent team so that in can be use for analysis as well

Ndat1 <- Ndat %>%
  select(League, Season, Opposition = Team, Team = Opposition, Date, Opp_Season.Rank = Season.Rank,Opp_Performance_Tier = Performance_Tier ,
         Opp_Streak = Streak, Opp_win_pct = win_pct, Opp_S.AvgWinPct  =  S.AvgWinPct,Opp_Rest = Rest,
         Opp_TZ.change =  TZ.change, Opp_Distance =  Distance, Opp_DistanceTotal = DistanceTotal,
         Opp_Distance3 =  Distance3,Opp_Distance5  =  Distance5, Opp_Distance7 = Distance7, Opp_Distance9  =  Distance9,
         Opp_Distance14 =  Distance14, Opp_Distance21 = Distance21, Opp_Distance28  = Distance28,
         Opp_GTotal = GTotal, Opp_G3 = G3, Opp_G5 = G5, Opp_G7 = G7, Opp_G9  = G9,
         Opp_G11 = G11, Opp_G13  = G13, Opp_G14  = G14, Opp_G15 = G15, Opp_G17= G17,
         Opp_G19 = G19, Opp_G21  = G21, Opp_G28  = G28,
         Opp_TZ.Total = TZ.Total, Opp_TZ.change3  = TZ.change3, Opp_TZ.change5  = TZ.change5,
         Opp_TZ.change7 = TZ.change7, Opp_TZ.change9 = TZ.change9, Opp_TZ.change14 = TZ.change14,
         Opp_TZ.change21= TZ.change21, Opp_TZ.change28 = TZ.change28)

#join team and opponents data sets
Ndat_full <- full_join(Ndat, Ndat1) 

# Add difference between team and opposition values

Ndat_full <- Ndat_full %>% mutate(Diff_ranking = Season.Rank - Opp_Season.Rank) %>% 
  mutate(Diff_Distance = Distance - Opp_Distance) %>% mutate(Diff_Distance3 = Distance3 - Opp_Distance3) %>%
  mutate(Diff_Distance5 = Distance5 - Opp_Distance5) %>% mutate(Diff_Distance7 = Distance7 - Opp_Distance7) %>%
  mutate(Diff_Distance9 = Distance9 - Opp_Distance9) %>% mutate(Diff_Distance14 = Distance14 - Opp_Distance14) %>%
  mutate(Diff_Distance21 = Distance21 - Opp_Distance21) %>% mutate(Diff_Distance28 = Distance28 - Opp_Distance28) %>%
  mutate(Diff_DTotal = DistanceTotal - Opp_DistanceTotal) %>%
  mutate(Diff_Rest = Rest-Opp_Rest, Diff_Gtotal = GTotal - Opp_GTotal, Diff_G3 = G3 - Opp_G3,
         Diff_G5 = G5 - Opp_G5, Diff_G7 = G7 - Opp_G7, Diff_G9 = G9 - Opp_G9, Diff_G14 = G14 - Opp_G14,
         Diff_G21 = G21 - Opp_G21, Diff_G28 = G28 - Opp_G28) %>%
  mutate(Diff_TZTotal = TZ.Total - Opp_TZ.Total, Diff_TZchange = TZ.change - Opp_TZ.change,
         Diff_TZchange3 = TZ.change3 - Opp_TZ.change3,Diff_TZchange5 = TZ.change5 - Opp_TZ.change5,
         Diff_TZchange7 = TZ.change7 - Opp_TZ.change7,Diff_TZchange9 = TZ.change9 - Opp_TZ.change9,
         Diff_TZchange14 = TZ.change14 - Opp_TZ.change14,Diff_TZchange21 = TZ.change21 - Opp_TZ.change21,
         Diff_TZchange28 = TZ.change28 - Opp_TZ.change28)


## Filter to exclude 1st game of the season. Needs to be done as no telling how much rest or travel before the 1st game.

Ndat_full <- Ndat_full %>% filter(Rest != 15 & Opp_Rest != 15)

# Filter so that "Team" is the HOME team and "Opposition" is the AWAY team. Note - HOME = 0, AWAY = 1

Ndat_full <- Ndat_full %>% filter(Location == 0)  ## 15279 NBA games after being filtered for first game of the season

###################### 

write.csv(Ndat_full, "NBA_datafor_analysis.csv")


#################################################################################################################################################################################################################






