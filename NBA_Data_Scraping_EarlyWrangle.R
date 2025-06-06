## The purpose of this script is to download NBA team data  from the official NBA stats website (https://www.nba.com/stats/teams/boxscores-traditional)
## for every regular season match played by every NBA team from 2010 to 2023. From this data, additional variables were
## calculated for 1) The number of days between games, defaulting to 15 for the first game for each team. 
## 2) Estimated distance (in km) travelled for each game, 3) Time zone change in hours, 4) The route (cities) taken,
## 5) Direction of travel (E/W), 6) Whether the team returned home or not.

## To do this I utilised the nba_travel function, part of the 'airball' package developed by 
## Jose Fernandez (2020) - <https://github.com/josedv82/airball> to scrape the data and perform the calculations.
## I did slightly alter the nba_travel function so that the final metrics included Points Difference between the teams,
## as this may be a more appropriate representation of match outcome, than simply win/loss.
## The code for the altered function is below. It is a very small change and is essentially identical to Fernandez's code.

## I then selected my desired metrics for further data engineering at the next stage.
## This resulted in a data frame with the following metrics:
## League, Season.WNBA, Season, Date, Team, Opposition, Location, Match.city, W.L,PointsDiff, Rest,
## Distance, TZ.change, Route, Return.home, Direction
## Explanations below:


#'          \item{League}{A chracter string. NBA or WNBA. Will just be NBA for this.}
#'          \item{Season.WNBA}{WNBA season it would be. Should be an empty column}
#'          \item{Season}{A chracter string. The season(s) downloaded.}
#'          \item{Date}{Date object. The date of the game.}
#'          \item{Team}{A character String. The name of the team.}
#'          \item{Opponent}{A character String. The name of the opponent.}
#'          \item{Location}{A character string. Location of the game. (Home or Away)}
#'          \item{Match.city}{A character String. The name of the city in which the game is played.}          
#'          \item{W.L}{A character string. Outcome of the game. (W or L)}
#'          \item{PointsDiff}{Numeric. Final points difference at the end of the match}
#'          \item{Rest}{Numeric. The number of days between games. It defaults to 15 for the first game for each team. If the team is making a "return_home" trip it will default to NA.}
#'          \item{Distance}{Numeric. Estimated distance travelled (in miles) prior to a game.}
#'          \item{Route}{A character String. The route travelled for each game. If no travel involved it defaults to "No Travel".}
#'          \item{TZ.change}{Numeric. The time zone change in hours. If negative then West, if positive, then East}


####################################################################################################


#### Reminder to set working directory to location you want the final .xlsx file to be

##### Load packages required 
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


########## 1) Code for altered  NBA_TRAVEL function:
nba_travel_altered <- function(start_season = 2018,
                               end_season = 2020,
                               team = NULL,
                               return_home = 20,
                               phase = c("RS", "PO"),
                               flight_speed = 550){
  
  
  ## pull regular season data
  invisible(capture.output( RS <- tryCatch({
    
    suppressWarnings(
      nbastatR::game_logs(
        seasons = start_season:end_season,
        league = "NBA",
        result_types = "team",
        season_types = "Regular Season",
        nest_data = F,
        assign_to_environment = F,
        return_message = F
      )) %>% dplyr::mutate(Phase = "RS")
    
  }, error = function(cond){
    
    suppressWarnings(
      nbastatR::game_logs(
        seasons = start_season:(end_season-1),
        league = "NBA",
        result_types = "team",
        season_types = "Regular Season",
        nest_data = F,
        assign_to_environment = F,
        return_message = F
      )) %>% dplyr::mutate(Phase = "RS")
    
  }
  
  )))
  
  #pull play off data
  invisible(capture.output( PO <- tryCatch({
    
    suppressWarnings(
      nbastatR::game_logs(
        seasons = start_season:end_season,
        league = "NBA",
        result_types = "team",
        season_types = "Playoffs",
        nest_data = F,
        assign_to_environment = F,
        return_message = F
      )) %>% dplyr::mutate(Phase = "PO")
    
  }, error = function(cond){
    
    suppressWarnings(
      nbastatR::game_logs(
        seasons = start_season:(end_season-1),
        league = "NBA",
        result_types = "team",
        season_types = "Playoffs",
        nest_data = F,
        assign_to_environment = F,
        return_message = F
      )) %>% dplyr::mutate(Phase = "PO")
    
  }
  
  )))
  
  #pull future games (games that have not been played yet)
  
  future_games <- function(year = 2023, month = "april"){ #year needs to be updated to 2022
    
    year <- year
    month <- month
    url <- paste0("https://www.basketball-reference.com/leagues/NBA_", year, "_games-", month, ".html")
    webpage <- xml2::read_html(url)
    
    
    col_names <- webpage %>%
      rvest::html_nodes("table#schedule > thead > tr > th") %>%
      rvest::html_attr("data-stat")
    col_names <- c("game_id", col_names)
    
    
    dates <- webpage %>%
      rvest::html_nodes("table#schedule > tbody > tr > th") %>%
      rvest::html_text()
    dates <- dates[dates != "Playoffs"]
    
    game_id <- webpage %>%
      rvest::html_nodes("table#schedule > tbody > tr > th") %>%
      rvest::html_attr("csk")
    game_id <- game_id[!is.na(game_id)]
    
    
    data <- webpage %>%
      rvest::html_nodes("table#schedule > tbody > tr > td") %>%
      rvest::html_text() %>%
      matrix(ncol = length(col_names) - 2, byrow = TRUE)
    
    month_df <- as.data.frame(cbind(game_id, dates, data), stringsAsFactors = FALSE) %>%
      dplyr::mutate(dates = lubridate::mdy(dates))
    names(month_df) <- col_names
    
    month_h <- month_df %>% dplyr::select(Date = date_game, Team = home_team_name, Opponent = visitor_team_name) %>% dplyr::mutate(Location = "H")
    month_a <- month_df %>% dplyr::select(Date = date_game, Opponent = home_team_name, Team = visitor_team_name) %>% dplyr::mutate(Location = "A")
    
    future <- dplyr::full_join(month_h, month_a, by = c("Date", "Team", "Opponent", "Location")) %>%
      dplyr::mutate(Season = "2023-24", `W/L` = "-", Phase = "RS")
    
  }
  
  #join future games for all months (will need to add remaining months when schedule is announced
  oct <- future_games(year = 2023, month = "october")
  nov <- future_games(year = 2023, month = "november")
  dec <- future_games(year = 2023, month = "december")
  jan <- future_games(year = 2023, month = "january")
  feb <- future_games(year = 2023, month = "february")
  mar <- future_games(year = 2023, month = "march")
  apr <- future_games(year = 2023, month = "april")
  
  future <- dplyr::full_join(oct, nov, by = c("Date", "Team", "Opponent", "Location", "Season", "W/L", "Phase")) %>%
    dplyr::full_join(dec, by = c("Date", "Team", "Opponent", "Location", "Season", "W/L", "Phase")) %>%
    dplyr::full_join(jan, by = c("Date", "Team", "Opponent", "Location", "Season", "W/L", "Phase")) %>%
    dplyr::full_join(feb, by = c("Date", "Team", "Opponent", "Location", "Season", "W/L", "Phase")) %>%
    dplyr::full_join(mar, by = c("Date", "Team", "Opponent", "Location", "Season", "W/L", "Phase")) %>%
    dplyr::full_join(apr, by = c("Date", "Team", "Opponent", "Location", "Season", "W/L", "Phase")) %>%
    dplyr::arrange(Team, Date) %>%
    dplyr::filter(Date >= Sys.Date())
  
  
  #join regular season, playoffs and future games
  statlogs <- rbind(RS, PO) %>% dplyr::arrange(dateGame)
  
  #cleaning for away + home games
  away <- statlogs %>%
    dplyr::select(Season = slugSeason, Date = dateGame, Team = nameTeam, Location = locationGame, Opp = slugOpponent, TE = slugTeam, `W/L` = outcomeGame, PointsDiff = plusminusTeam, Phase) %>% ### add in points difference here
    dplyr::distinct()
  
  home <- statlogs %>%
    dplyr::select(Season = slugSeason, Date = dateGame, TeamB = nameTeam, LocationB = locationGame, TE = slugOpponent, Opp = slugTeam) %>%
    dplyr::distinct()
  
  #conditional merging. If there are future games involved join future dataset up to current date, else just pull all previous games
  if(end_season < 2023) { #change year when 2024 is released
    
    cal <- dplyr::full_join(away, home, by = c("Season", "Date", "Opp", "TE")) %>%
      dplyr::select(Season, Date, Team, Opponent = TeamB, Location, `W/L`, PointsDiff, Phase, -Opp, -TE, -LocationB) %>% ### added in PointsDiff here
      dplyr::arrange(Team, Date)
    
  } else {
    
    cal <- dplyr::full_join(away, home, by = c("Season", "Date", "Opp", "TE")) %>%
      dplyr::select(Season, Date, Team, Opponent = TeamB, Location, `W/L`, PointsDiff, Phase, -Opp, -TE, -LocationB) %>% ## Added in PointsDiff here
      dplyr::full_join(future, by = c("Season", "Date", "Team", "Opponent", "Location", "W/L", "Phase")) %>%
      dplyr::arrange(Team, Date)
    
  }
  
  #obtain city coordinates
  cities <- maps::world.cities %>%
    dplyr::group_by(name, country.etc) %>%
    dplyr::filter(pop == max(pop)) %>%
    dplyr::filter(country.etc %in% c("USA", "Canada")) %>%
    dplyr::group_by(name) %>%
    dplyr::filter(pop == max(pop)) %>%
    dplyr::ungroup()
  
  #clean a few team names so they can be used to locate city coordinates automatically
  cal1 <- cal %>%
    dplyr::mutate(Team = ifelse(Team == "LA Clippers", "Los Angeles Clippers", Team)) %>%
    dplyr::mutate(Opponent = ifelse(Opponent == "LA Clippers", "Los Angeles Clippers", Opponent)) %>%
    dplyr::mutate(name = ifelse(Location == "H", gsub("(\\D+)\\s+.*", "\\1", Team), gsub("(\\D+)\\s+.*", "\\1", Opponent))) %>%
    dplyr::mutate(name = ifelse(name == "Golden State", "San Francisco",
                                ifelse(name == "Utah", "Salt Lake City",
                                       ifelse(name == "Minnesota", "Minneapolis",
                                              ifelse(name == "Indiana", "Indianapolis",
                                                     ifelse(name == "Brooklyn", "New York",
                                                            ifelse(name == "New Jersey", "Newark",
                                                                   ifelse(name == "St. Louis", "Saint Louis",
                                                                          ifelse(name == "Ft. Wayne", "Fort Wayne",
                                                                                 ifelse(name == "Tri-Cities", "Moline",
                                                                                        ifelse(name == "Kansas City-Omaha", "Kansas City",
                                                                                               ifelse(name == "Capital", "Washington",
                                                                                                      ifelse(name == "New Orleans/Oklahoma City", "New Orleans",
                                                                                                             ifelse(name == "Portland Trail", "Portland", name)))))))))))))) %>%
    
    #correct for Toronto location in 2021
    dplyr::mutate(name = ifelse(Season == "2020-21" & Team == "Toronto Raptors" & Location == "H", "Tampa", name)) %>%
    dplyr::mutate(name = ifelse(Season == "2020-21" & Opponent == "Toronto Raptors" & Location == "A", "Tampa", name)) %>%
    
    #join team with cities
    dplyr::full_join(cities, by = "name") %>%
    dplyr::mutate(off = paste(name, country.etc)) %>%
    dplyr::filter(off != "Dallas Canada" & off != "Houston Canada") %>%
    na.omit() %>%
    dplyr::select(Season, Date, Team, Opponent, Location, City = name, `W/L`,PointsDiff ,Phase, lat, long) %>%
    dplyr::group_by(Team, Season) %>%
    dplyr::mutate(destLat = dplyr::lag(lat), destLon = dplyr::lag(long))
  
  #cleaning home cities
  calhome <- cal1 %>%
    dplyr::filter(Location == "H") %>%
    dplyr::ungroup() %>%
    dplyr::select(Team, City, lat, long) %>%
    dplyr::distinct() %>%
    dplyr::select(Team, City, destLat1 = lat, destLon1 = long)
  
  #cleaning all calendar data after joining cities + team data
  allcal <- dplyr::full_join(cal1, calhome, by = c("Team", "City")) %>%
    dplyr::group_by(Season, Team) %>%
    tidyr::fill(destLat1, .direction = "up") %>%
    tidyr::fill(destLon1, .direction = "up") %>%
    dplyr::mutate(destLat = ifelse(is.na(destLat), destLat1, destLat)) %>%
    dplyr::mutate(destLon = ifelse(is.na(destLon), destLon1, destLon)) %>%
    dplyr::select(-destLat1, -destLon1) %>%
    dplyr::group_by(Season, Team) %>%
    dplyr::mutate(Rest = abs(as.numeric(dplyr::lag(Date) - Date) + 1)) %>%
    dplyr::mutate(Rest = ifelse(is.na(Rest), 15, Rest)) %>%
    dplyr::mutate(AB = paste(Location, dplyr::lag(Location)))
  
  #completes missing date sequence
  miscal <-  allcal %>%
    dplyr::group_by(Season, Team) %>%
    tidyr::complete(Date = seq.Date(min(Date), max(Date), by = "day")) %>%
    dplyr::filter(is.na(Location)) %>%
    dplyr::select(Season, Team, Date) %>%
    dplyr::full_join(calhome, by = "Team") %>%
    dplyr::select(Season, Team, City, Date, lat = destLat1, long = destLon1)
  
  #perform rest + detination coordinates calculations (account for orlando during covid)
  cal_rest <- dplyr::full_join(allcal, miscal, by = c("Season", "Date", "Team", "City", "lat", "long")) %>%
    dplyr::arrange(Season, Team, Date) %>%
    dplyr::mutate(A_B = ifelse(Rest >= return_home & AB == "A A", "-", NA)) %>%
    dplyr::mutate(A_B = ifelse(dplyr::lead(A_B) == "-", "y", A_B)) %>%
    dplyr::filter(!is.na(Rest) | A_B == "y") %>%
    dplyr::mutate(destLat = ifelse(is.na(destLat), dplyr::lag(lat), destLat),
                  destLon = ifelse(is.na(destLon), dplyr::lag(long), destLon)) %>%
    dplyr::mutate(destLat1 = ifelse(dplyr::lag(A_B == "y"), dplyr::lag(lat), destLat),
                  destLon1 = ifelse(dplyr::lag(A_B == "y"), dplyr::lag(long), destLon)) %>%
    dplyr::mutate(destLat = ifelse(!is.na(destLat1), destLat1, destLat),
                  destLon = ifelse(!is.na(destLon1), destLon1, destLon)) %>%
    dplyr::select(-AB, -A_B, -destLat1, -destLon1) %>%
    dplyr::mutate(City = ifelse(Date > "2020-07-01" & Date < "2020-11-01", "Orlando", City)) %>%
    dplyr::mutate(lat = ifelse(Date > "2020-07-01" & Date < "2020-11-01", 28.50, lat),
                  destLat = ifelse(Date > "2020-07-01" & Date < "2020-11-01", 28.50, destLat),
                  long = ifelse(Date > "2020-07-01" & Date < "2020-11-01", -81.37, long),
                  destLon = ifelse(Date > "2020-07-01" & Date < "2020-11-01", -81.37, destLon)) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(dist = geosphere::distm(c(destLon, destLat), c(long, lat), fun = geosphere::distHaversine)) %>%
    dplyr::mutate(Distance = round(dist * 0.000621,0)) %>%
    dplyr::select(-dist) %>%
    dplyr::ungroup()
  
  home <- calhome %>%
    dplyr::select(Team, Citys = City) %>%
    dplyr::ungroup()
  
  #extract time zones for cities
  TZs <- lutz::tz_list() %>%
    dplyr::select(TZ = tz_name, is_dst, utc_offset_h) %>%
    dplyr::filter(is_dst == FALSE) %>%
    dplyr::select(-is_dst)
  
  #calculate time zone changes + other time related metrics
  shift <- cal_rest %>% dplyr::full_join(home, by = "Team") %>%
    dplyr::group_by(Season, Team) %>%
    dplyr::mutate(lagcity = ifelse(is.na(dplyr::lag(City)), "-" , dplyr::lag(City))) %>%
    dplyr::mutate(Route = ifelse(lagcity == "-", paste(Citys, dplyr::lead(lagcity), sep = " - "), paste(lagcity, dplyr::lead(lagcity), sep = " - "))) %>%
    dplyr::mutate(Route = ifelse(Distance == 0, "No Travel", Route)) %>%
    dplyr::select(-Citys, -lagcity) %>%
    dplyr::mutate(Opponent = ifelse(is.na(Opponent), "-", Opponent),
                  Location = ifelse(is.na(Location), "-", Location),
                  Location = ifelse(Location == "A", "Away",
                                    ifelse(Location == "H", "Home", "-")),
                  `Return Home` = ifelse(Location == "-", "Yes", "")) %>%
    dplyr::mutate(Month = lubridate::month(Date),
                  Week = lubridate::week(Date)) %>%
    dplyr::mutate(TZ = lutz::tz_lookup_coords(lat, long, method = "fast", warn = F)) %>%
    dplyr::full_join(TZs, by = "TZ") %>%
    dplyr::select(offset = utc_offset_h, everything()) %>%
    dplyr::ungroup()
  
  #create final data set with all parameters
  final <- shift %>%
    dplyr::filter(Location == "Home") %>%
    dplyr::select(Team, City, TZ) %>%
    dplyr::distinct() %>%
    dplyr::full_join(TZs, by = "TZ") %>%
    na.omit() %>%
    dplyr::full_join(shift, c("Team", "City", "TZ")) %>%
    dplyr::arrange(Season, Team, Date) %>%
    dplyr::group_by(Season, Team) %>%
    tidyr::fill(utc_offset_h, .direction = "up") %>%
    tidyr::fill(utc_offset_h, .direction = "down") %>%
    dplyr::mutate(`Shift (hrs)` = offset - dplyr::lag(offset)) %>%
    dplyr::mutate(`Shift (hrs)` = ifelse(Route != "No Travel" & is.na(`Shift (hrs)`), offset - utc_offset_h,
                                         ifelse(Route == "No Travel" & is.na(`Shift (hrs)`), 0, `Shift (hrs)`))) %>%
    dplyr::mutate(`Direction (E/W)` = ifelse(`Shift (hrs)` < 0, "West",
                                             ifelse(`Shift (hrs)` > 0, "East", "-"))) %>%
    dplyr::mutate(`Flight Time` = ifelse(Distance == 0, NA, Distance / (flight_speed / 3600))) %>%
    dplyr::mutate(`Flight Time` = ifelse(`Flight Time` <= 3300, 3300, `Flight Time`)) %>%
    dplyr::mutate(`Flight Time` = lubridate::duration(num = `Flight Time`, units = "seconds")) %>%
    dplyr::mutate(`Flight Time` = gsub("(?<=\\()[^()]*(?=\\))(*SKIP)(*F)|.", "", `Flight Time`, perl=T)) %>%
    dplyr::mutate(`Flight Time` = ifelse(is.na(`Flight Time`), "-", `Flight Time`)) %>%
    dplyr::select(Season, Phase, Month, Week, Date, Team, Opponent, Location, `W/L`,PointsDiff, City, Distance, Route, Rest, TZ, `Shift (hrs)`, `Flight Time`, `Direction (E/W)`, `Return Home`, Latitude = lat, Longitude = long, d.Latitude = destLat, d.Longitude = destLon) %>%
    dplyr::ungroup() %>%
    dplyr::filter(!is.na(Team)) %>%
    dplyr::mutate(Month = ifelse(Month == 1, 'Jan',
                                 ifelse(Month == 2, 'Feb',
                                        ifelse(Month == 3, 'Mar',
                                               ifelse(Month == 4, 'Apr',
                                                      ifelse(Month == 5, 'May',
                                                             ifelse(Month == 6, 'Jun',
                                                                    ifelse(Month == 7, 'Jul',
                                                                           ifelse(Month == 8, 'Aug',
                                                                                  ifelse(Month == 9, 'Sep',
                                                                                         ifelse(Month == 10, 'Oct',
                                                                                                ifelse(Month == 11, 'Nov', 'Dec')))))))))))) %>%
    
    dplyr::mutate(Notes = ifelse(Date > "2020-07-01" & Date < "2020-11-01" & `Return Home` == "Yes", "Remove", "")) %>% #for bubble
    dplyr::filter(Notes != "Remove") %>%
    dplyr::select(-Notes) %>%
    tidyr::fill(Phase, .direction = "up") %>%
    tidyr::fill(Phase, .direction = "down") %>%
    dplyr::mutate(`W/L` = ifelse(is.na(`W/L`), "-", `W/L`)) %>%
    dplyr::filter(Phase %in% phase) %>%
    dplyr::group_by(Season, Team) %>%
    dplyr::mutate(Rem = ifelse(!is.na(dplyr::lag(Date)) & dplyr::lag(Date) == Date, "1", "")) %>%
    dplyr::filter(Rem != "1") %>% dplyr::select(-Rem)
  
  #conditional return based on whether users select a team or not
  if(missing(team)) return(final)
  else return(final %>% dplyr::filter(Team %in% team))
  
}



############################################################################################################.
 # End of Function Code
##########################################################################################################


## 2) Use nba_travel_altered function to extract 2010-2023 seasons


### Make sure to set return home day to 5

##Set vroom to be big enough to pull data from nba.stats.API

Sys.setenv(VROOM_CONNECTION_SIZE = 500072) ## Important step, function fails if the vroom is not big enough

### Apply altered function. Extracting 2010 - 2014 seasons

NBA_data <- nba_travel_altered(start_season = 2010, 
                               end_season = 2023,
                               return_home = 5,
                               phase = "RS")  


# filter out 2019-2020 season (Orlando Covid Bubble)


check_bubble <- NBA_data %>% filter (Season != "2019-20")

NBA_data %>% filter(Season == "2019-20") # checking number rows in 2019-20 season
check_bubble %>% filter(Season == "2019-20") ## checking that there are no matches left from this season

rm(check_bubble)

NBA_data <- NBA_data %>% filter (Season != "2019-20")

## select Columns to align better with WNBA data. Season colname() correct
## columns to select : Season, Date, Team, Opponent, Location, City, W/L, PointsDiff ,Rest, Distance[, 1], Shift (hrs)[,1], Route [,1], Return Home, Direction (E/W)

NBA_data <- NBA_data %>% select(Season, Date, Team, Opponent, Location, City, `W/L`,PointsDiff, Rest, Distance, `Shift (hrs)`, Route, `Return Home`, `Direction (E/W)`)

# Change colnames to match


NBA_data <- NBA_data %>%
  rename(Opposition = Opponent,
         Match.city = City,
         W.L = `W/L`,
         TZ.change = `Shift (hrs)`,
         Return.home = `Return Home`,
         Direction = `Direction (E/W)`)

# change distance in miles to km
is.numeric(NBA_data$Distance)
NBA_data$Distance <- as.numeric(NBA_data$Distance)


NBA_data$Distance <- NBA_data$Distance * 1.60934
NBA_data$Distance <- round(NBA_data$Distance, 2)

# Add in columns for League and and Season.WNBA

NBA_data <- NBA_data %>% 
             mutate(League = "NBA") %>%
             mutate(Season.WNBA = NA) %>%
             select(League, Season.WNBA, Season, Date, Team, Opposition, Location, Match.city, W.L,PointsDiff, Rest,
                    Distance, TZ.change, Route)

##### Everything is looking good, so we can export this as NBA_data1

write.xlsx(NBA_data, "NBA_data1.xlsx")
