# ---------------------------------------------------------
# data functions
# Christopher Gauthier
# 
# A collection of functions for pulling in NBA data
# ---------------------------------------------------------





########## Load libraries ########## 
# ---------------------------------------------------------
library(tidyverse)
library(rvest)
library(janitor)
library(stringr)
# ---------------------------------------------------------





########## Team Data Scrape Functions ##########
# ---------------------------------------------------------
# Scrape NBA team ratings for single season from Basketball Reference
get_team_ratings_bref <- function(year) {
  
  # Scrape url
  url = paste0("https://www.basketball-reference.com/leagues/NBA_", year, "_ratings.html")
  tbl = url |>
    read_html() |>
    html_nodes("table") |>
    html_table()
  
  # Extract data
  data <- tbl[[1]] |>
    row_to_names(row_number = 1) %>%
    select (-Rk) %>%
    mutate(Team = as.character(Team) # Full Team Name
           , Conf = as.character(Conf) # Conference
           , Div = as.character(Div) # Division
           , W = as.numeric(W) # Wins
           , L = as.numeric(L) # Losses
           , `W/L%` = as.numeric(`W/L%`) # Win-Loss %
           , MOV = as.numeric(MOV) # Margin of Victory
           , ORtg = as.numeric(ORtg) # Offensive Rating
           , DRtg = as.numeric(DRtg) # Defensive Rating
           , NRtg = as.numeric(NRtg) # Net Rating
           , `MOV/A` = as.numeric(`MOV/A`) # Adjusted Margin of Victory
           , `ORtg/A` = as.numeric(`ORtg/A`), # Adjusted Offensive Rating
           , `DRtg/A` = as.numeric(`DRtg/A`), # Adjusted Defensive Rating
           , `NRtg/A` = as.numeric(`NRtg/A`)) %>%
    mutate(Tm = case_when( # Team Abbreviation
      Team == "Atlanta Hawks" ~ "ATL",
      Team == "Brooklyn Nets" ~ "BKN",
      Team == "Boston Celtics" ~ "BOS",
      Team == "Charlotte Hornets" ~ "CHA",
      Team == "Chicago Bulls" ~ "CHI",
      Team == "Cleveland Cavaliers" ~ "CLE",
      Team == "Dallas Mavericks" ~ "DAL",
      Team == "Denver Nuggets" ~ "DEN",
      Team == "Detroit Pistons" ~ "DET",
      Team == "Golden State Warriors" ~ "GS",
      Team == "Houston Rockets" ~ "HOU",
      Team == "Indiana Pacers" ~ "IND",
      Team == "Los Angeles Clippers" ~ "LAC",
      Team == "Los Angeles Lakers" ~ "LAL",
      Team == "Memphis Grizzlies" ~ "MEM",
      Team == "Miami Heat" ~ "MIA",
      Team == "Milwaukee Bucks" ~ "MIL",
      Team == "Minnesota Timberwolves" ~ "MIN",
      Team == "New Orleans Pelicans" ~ "NO",
      Team == "New York Knicks" ~ "NY",
      Team == "Oklahoma City Thunder" ~ "OKC",
      Team == "Orlando Magic" ~ "ORL",
      Team == "Philadelphia 76ers" ~ "PHI",
      Team == "Phoenix Suns" ~ "PHX",
      Team == "Portland Trail Blazers" ~ "POR",
      Team == "San Antonio Spurs" ~ "SA",
      Team == "Sacramento Kings" ~ "SAC",
      Team == "Toronto Raptors" ~ "TOR",
      Team == "Utah Jazz" ~ "UTAH",
      Team == "Washington Wizards" ~ "WSH",
      .default = Team
    ))
  
  # Return tibble
  return(data)
}

# Scrape NBA Eastern Conference standings for single season from Basketball Reference
get_ec_standings_bref <- function(year) {
  
  # Scrape url
  url = paste0("https://www.basketball-reference.com/leagues/NBA_", year, ".html")
  tbl = url |>
    read_html() |>
    html_nodes("table") |>
    html_table()
  
  # Extract data
  data <- tbl[[1]] |>
    rename(Team = `Eastern Conference`) |>
    mutate(Tm = case_when( # Team Abbreviation
      str_detect(Team, "Atlanta Hawks") ~ "ATL"
      , str_detect(Team, "Brooklyn Nets") ~ "BKN"
      , str_detect(Team, "Boston Celtics") ~ "BOS"
      , str_detect(Team, "Charlotte Hornets") ~ "CHA"
      , str_detect(Team, "Chicago Bulls") ~ "CHI"
      , str_detect(Team, "Cleveland Cavaliers") ~ "CLE"
      , str_detect(Team, "Dallas Mavericks") ~ "DAL"
      , str_detect(Team, "Denver Nuggets") ~ "DEN"
      , str_detect(Team, "Detroit Pistons") ~ "DET"
      , str_detect(Team, "Golden State Warriors") ~ "GS"
      , str_detect(Team, "Houston Rockets") ~ "HOU"
      , str_detect(Team, "Indiana Pacers") ~ "IND"
      , str_detect(Team, "Los Angeles Clippers") ~ "LAC"
      , str_detect(Team, "Los Angeles Lakers") ~ "LAL"
      , str_detect(Team, "Memphis Grizzlies") ~ "MEM"
      , str_detect(Team, "Miami Heat") ~ "MIA"
      , str_detect(Team, "Milwaukee Bucks") ~ "MIL"
      , str_detect(Team, "Minnesota Timberwolves") ~ "MIN"
      , str_detect(Team, "New Orleans Pelicans") ~ "NO"
      , str_detect(Team, "New York Knicks") ~ "NY"
      , str_detect(Team, "Oklahoma City Thunder") ~ "OKC"
      , str_detect(Team, "Orlando Magic") ~ "ORL"
      , str_detect(Team, "Philadelphia 76ers") ~ "PHI"
      , str_detect(Team, "Phoenix Suns") ~ "PHX"
      , str_detect(Team, "Portland Trail Blazers") ~ "POR"
      , str_detect(Team, "San Antonio Spurs") ~ "SA"
      , str_detect(Team, "Sacramento Kings") ~ "SAC"
      , str_detect(Team, "Toronto Raptors") ~ "TOR"
      , str_detect(Team, "Utah Jazz") ~ "UTAH"
      , str_detect(Team, "Washington Wizards") ~ "WSH"
      , .default = Team
    ))
  
  # Return tibble
  return(data)
}

# Scrape NBA Western Conference standings for single season from Basketball Reference
get_wc_standings_bref <- function(year) {
  
  # Scrape url
  url = paste0("https://www.basketball-reference.com/leagues/NBA_", year, ".html")
  tbl = url |>
    read_html() |>
    html_nodes("table") |>
    html_table()
  
  # Extract data
  data <- tbl[[2]] |>
    rename(Team = `Western Conference`) |>
    mutate(Tm = case_when( # Team Abbreviation
      str_detect(Team, "Atlanta Hawks") ~ "ATL"
      , str_detect(Team, "Brooklyn Nets") ~ "BKN"
      , str_detect(Team, "Boston Celtics") ~ "BOS"
      , str_detect(Team, "Charlotte Hornets") ~ "CHA"
      , str_detect(Team, "Chicago Bulls") ~ "CHI"
      , str_detect(Team, "Cleveland Cavaliers") ~ "CLE"
      , str_detect(Team, "Dallas Mavericks") ~ "DAL"
      , str_detect(Team, "Denver Nuggets") ~ "DEN"
      , str_detect(Team, "Detroit Pistons") ~ "DET"
      , str_detect(Team, "Golden State Warriors") ~ "GS"
      , str_detect(Team, "Houston Rockets") ~ "HOU"
      , str_detect(Team, "Indiana Pacers") ~ "IND"
      , str_detect(Team, "Los Angeles Clippers") ~ "LAC"
      , str_detect(Team, "Los Angeles Lakers") ~ "LAL"
      , str_detect(Team, "Memphis Grizzlies") ~ "MEM"
      , str_detect(Team, "Miami Heat") ~ "MIA"
      , str_detect(Team, "Milwaukee Bucks") ~ "MIL"
      , str_detect(Team, "Minnesota Timberwolves") ~ "MIN"
      , str_detect(Team, "New Orleans Pelicans") ~ "NO"
      , str_detect(Team, "New York Knicks") ~ "NY"
      , str_detect(Team, "Oklahoma City Thunder") ~ "OKC"
      , str_detect(Team, "Orlando Magic") ~ "ORL"
      , str_detect(Team, "Philadelphia 76ers") ~ "PHI"
      , str_detect(Team, "Phoenix Suns") ~ "PHX"
      , str_detect(Team, "Portland Trail Blazers") ~ "POR"
      , str_detect(Team, "San Antonio Spurs") ~ "SA"
      , str_detect(Team, "Sacramento Kings") ~ "SAC"
      , str_detect(Team, "Toronto Raptors") ~ "TOR"
      , str_detect(Team, "Utah Jazz") ~ "UTAH"
      , str_detect(Team, "Washington Wizards") ~ "WSH"
      , .default = Team
    ))
  
  # Return tibble
  return(data)
}

# Scrape NBA team per game stats for single season from Basketball Reference
get_team_pg_stats_bref <- function(year) {
  
  # Scrape url
  url = paste0("https://www.basketball-reference.com/leagues/NBA_", year, ".html")
  tbl = url |>
    read_html() |>
    html_nodes("table") |>
    html_table()
  
  # Extract data
  data <- tbl[[5]] |>
    select (-Rk) |>
    mutate(Tm = case_when( # Team Abbreviation
      str_detect(Team, "Atlanta Hawks") ~ "ATL"
      , str_detect(Team, "Brooklyn Nets") ~ "BKN"
      , str_detect(Team, "Boston Celtics") ~ "BOS"
      , str_detect(Team, "Charlotte Hornets") ~ "CHA"
      , str_detect(Team, "Chicago Bulls") ~ "CHI"
      , str_detect(Team, "Cleveland Cavaliers") ~ "CLE"
      , str_detect(Team, "Dallas Mavericks") ~ "DAL"
      , str_detect(Team, "Denver Nuggets") ~ "DEN"
      , str_detect(Team, "Detroit Pistons") ~ "DET"
      , str_detect(Team, "Golden State Warriors") ~ "GS"
      , str_detect(Team, "Houston Rockets") ~ "HOU"
      , str_detect(Team, "Indiana Pacers") ~ "IND"
      , str_detect(Team, "Los Angeles Clippers") ~ "LAC"
      , str_detect(Team, "Los Angeles Lakers") ~ "LAL"
      , str_detect(Team, "Memphis Grizzlies") ~ "MEM"
      , str_detect(Team, "Miami Heat") ~ "MIA"
      , str_detect(Team, "Milwaukee Bucks") ~ "MIL"
      , str_detect(Team, "Minnesota Timberwolves") ~ "MIN"
      , str_detect(Team, "New Orleans Pelicans") ~ "NO"
      , str_detect(Team, "New York Knicks") ~ "NY"
      , str_detect(Team, "Oklahoma City Thunder") ~ "OKC"
      , str_detect(Team, "Orlando Magic") ~ "ORL"
      , str_detect(Team, "Philadelphia 76ers") ~ "PHI"
      , str_detect(Team, "Phoenix Suns") ~ "PHX"
      , str_detect(Team, "Portland Trail Blazers") ~ "POR"
      , str_detect(Team, "San Antonio Spurs") ~ "SA"
      , str_detect(Team, "Sacramento Kings") ~ "SAC"
      , str_detect(Team, "Toronto Raptors") ~ "TOR"
      , str_detect(Team, "Utah Jazz") ~ "UTAH"
      , str_detect(Team, "Washington Wizards") ~ "WSH"
      , .default = Team
    ))
  
  # Return tibble
  return(data)
}

# Scrape NBA opponent per game stats for single season from Basketball Reference
get_opponent_pg_stats_bref <- function(year) {
  
  # Scrape url
  url = paste0("https://www.basketball-reference.com/leagues/NBA_", year, ".html")
  tbl = url |>
    read_html() |>
    html_nodes("table") |>
    html_table()
  
  # Extract data
  data <- tbl[[6]] %>%
    select (-Rk) %>%
    mutate(Tm = case_when( # Team Abbreviation
      str_detect(Team, "Atlanta Hawks") ~ "ATL"
      , str_detect(Team, "Brooklyn Nets") ~ "BKN"
      , str_detect(Team, "Boston Celtics") ~ "BOS"
      , str_detect(Team, "Charlotte Hornets") ~ "CHA"
      , str_detect(Team, "Chicago Bulls") ~ "CHI"
      , str_detect(Team, "Cleveland Cavaliers") ~ "CLE"
      , str_detect(Team, "Dallas Mavericks") ~ "DAL"
      , str_detect(Team, "Denver Nuggets") ~ "DEN"
      , str_detect(Team, "Detroit Pistons") ~ "DET"
      , str_detect(Team, "Golden State Warriors") ~ "GS"
      , str_detect(Team, "Houston Rockets") ~ "HOU"
      , str_detect(Team, "Indiana Pacers") ~ "IND"
      , str_detect(Team, "Los Angeles Clippers") ~ "LAC"
      , str_detect(Team, "Los Angeles Lakers") ~ "LAL"
      , str_detect(Team, "Memphis Grizzlies") ~ "MEM"
      , str_detect(Team, "Miami Heat") ~ "MIA"
      , str_detect(Team, "Milwaukee Bucks") ~ "MIL"
      , str_detect(Team, "Minnesota Timberwolves") ~ "MIN"
      , str_detect(Team, "New Orleans Pelicans") ~ "NO"
      , str_detect(Team, "New York Knicks") ~ "NY"
      , str_detect(Team, "Oklahoma City Thunder") ~ "OKC"
      , str_detect(Team, "Orlando Magic") ~ "ORL"
      , str_detect(Team, "Philadelphia 76ers") ~ "PHI"
      , str_detect(Team, "Phoenix Suns") ~ "PHX"
      , str_detect(Team, "Portland Trail Blazers") ~ "POR"
      , str_detect(Team, "San Antonio Spurs") ~ "SA"
      , str_detect(Team, "Sacramento Kings") ~ "SAC"
      , str_detect(Team, "Toronto Raptors") ~ "TOR"
      , str_detect(Team, "Utah Jazz") ~ "UTAH"
      , str_detect(Team, "Washington Wizards") ~ "WSH"
      , .default = Team
    ))
  
  # Return tibble
  return(data)
}

# Scrape NBA team total stats for single season from Basketball Reference
get_team_total_stats_bref <- function(year) {
  
  # Scrape url
  url = paste0("https://www.basketball-reference.com/leagues/NBA_", year, ".html")
  tbl = url |>
    read_html() |>
    html_nodes("table") |>
    html_table()
  
  # Extract data
  data <- tbl[[7]] |>
    select (-Rk) |>
    mutate(Tm = case_when( # Team Abbreviation
      str_detect(Team, "Atlanta Hawks") ~ "ATL"
      , str_detect(Team, "Brooklyn Nets") ~ "BKN"
      , str_detect(Team, "Boston Celtics") ~ "BOS"
      , str_detect(Team, "Charlotte Hornets") ~ "CHA"
      , str_detect(Team, "Chicago Bulls") ~ "CHI"
      , str_detect(Team, "Cleveland Cavaliers") ~ "CLE"
      , str_detect(Team, "Dallas Mavericks") ~ "DAL"
      , str_detect(Team, "Denver Nuggets") ~ "DEN"
      , str_detect(Team, "Detroit Pistons") ~ "DET"
      , str_detect(Team, "Golden State Warriors") ~ "GS"
      , str_detect(Team, "Houston Rockets") ~ "HOU"
      , str_detect(Team, "Indiana Pacers") ~ "IND"
      , str_detect(Team, "Los Angeles Clippers") ~ "LAC"
      , str_detect(Team, "Los Angeles Lakers") ~ "LAL"
      , str_detect(Team, "Memphis Grizzlies") ~ "MEM"
      , str_detect(Team, "Miami Heat") ~ "MIA"
      , str_detect(Team, "Milwaukee Bucks") ~ "MIL"
      , str_detect(Team, "Minnesota Timberwolves") ~ "MIN"
      , str_detect(Team, "New Orleans Pelicans") ~ "NO"
      , str_detect(Team, "New York Knicks") ~ "NY"
      , str_detect(Team, "Oklahoma City Thunder") ~ "OKC"
      , str_detect(Team, "Orlando Magic") ~ "ORL"
      , str_detect(Team, "Philadelphia 76ers") ~ "PHI"
      , str_detect(Team, "Phoenix Suns") ~ "PHX"
      , str_detect(Team, "Portland Trail Blazers") ~ "POR"
      , str_detect(Team, "San Antonio Spurs") ~ "SA"
      , str_detect(Team, "Sacramento Kings") ~ "SAC"
      , str_detect(Team, "Toronto Raptors") ~ "TOR"
      , str_detect(Team, "Utah Jazz") ~ "UTAH"
      , str_detect(Team, "Washington Wizards") ~ "WSH"
      , .default = Team
    ))
  
  # Return tibble
  return(data)
}

# Scrape NBA opponent total stats for single season from Basketball Reference
get_opponent_total_stats_bref <- function(year) {
  
  # Scrape url
  url = paste0("https://www.basketball-reference.com/leagues/NBA_", year, ".html")
  tbl = url |>
    read_html() |>
    html_nodes("table") |>
    html_table()
  
  # Extract data
  data <- tbl[[8]] |>
    select (-Rk) |>
    mutate(Tm = case_when( # Team Abbreviation
      str_detect(Team, "Atlanta Hawks") ~ "ATL"
      , str_detect(Team, "Brooklyn Nets") ~ "BKN"
      , str_detect(Team, "Boston Celtics") ~ "BOS"
      , str_detect(Team, "Charlotte Hornets") ~ "CHA"
      , str_detect(Team, "Chicago Bulls") ~ "CHI"
      , str_detect(Team, "Cleveland Cavaliers") ~ "CLE"
      , str_detect(Team, "Dallas Mavericks") ~ "DAL"
      , str_detect(Team, "Denver Nuggets") ~ "DEN"
      , str_detect(Team, "Detroit Pistons") ~ "DET"
      , str_detect(Team, "Golden State Warriors") ~ "GS"
      , str_detect(Team, "Houston Rockets") ~ "HOU"
      , str_detect(Team, "Indiana Pacers") ~ "IND"
      , str_detect(Team, "Los Angeles Clippers") ~ "LAC"
      , str_detect(Team, "Los Angeles Lakers") ~ "LAL"
      , str_detect(Team, "Memphis Grizzlies") ~ "MEM"
      , str_detect(Team, "Miami Heat") ~ "MIA"
      , str_detect(Team, "Milwaukee Bucks") ~ "MIL"
      , str_detect(Team, "Minnesota Timberwolves") ~ "MIN"
      , str_detect(Team, "New Orleans Pelicans") ~ "NO"
      , str_detect(Team, "New York Knicks") ~ "NY"
      , str_detect(Team, "Oklahoma City Thunder") ~ "OKC"
      , str_detect(Team, "Orlando Magic") ~ "ORL"
      , str_detect(Team, "Philadelphia 76ers") ~ "PHI"
      , str_detect(Team, "Phoenix Suns") ~ "PHX"
      , str_detect(Team, "Portland Trail Blazers") ~ "POR"
      , str_detect(Team, "San Antonio Spurs") ~ "SA"
      , str_detect(Team, "Sacramento Kings") ~ "SAC"
      , str_detect(Team, "Toronto Raptors") ~ "TOR"
      , str_detect(Team, "Utah Jazz") ~ "UTAH"
      , str_detect(Team, "Washington Wizards") ~ "WSH"
      , .default = Team
    ))
  
  # Return tibble
  return(data)
}

# Scrape NBA team per 100 possessions stats for single season from Basketball Reference
get_team_p100_poss_stats_bref <- function(year) {
  
  # Scrape url
  url = paste0("https://www.basketball-reference.com/leagues/NBA_", year, ".html")
  tbl = url |>
    read_html() |>
    html_nodes("table") |>
    html_table()
  
  # Extract data
  data <- tbl[[9]] %>%
    select (-Rk) %>%
    mutate(Tm = case_when( # Team Abbreviation
      str_detect(Team, "Atlanta Hawks") ~ "ATL"
      , str_detect(Team, "Brooklyn Nets") ~ "BKN"
      , str_detect(Team, "Boston Celtics") ~ "BOS"
      , str_detect(Team, "Charlotte Hornets") ~ "CHA"
      , str_detect(Team, "Chicago Bulls") ~ "CHI"
      , str_detect(Team, "Cleveland Cavaliers") ~ "CLE"
      , str_detect(Team, "Dallas Mavericks") ~ "DAL"
      , str_detect(Team, "Denver Nuggets") ~ "DEN"
      , str_detect(Team, "Detroit Pistons") ~ "DET"
      , str_detect(Team, "Golden State Warriors") ~ "GS"
      , str_detect(Team, "Houston Rockets") ~ "HOU"
      , str_detect(Team, "Indiana Pacers") ~ "IND"
      , str_detect(Team, "Los Angeles Clippers") ~ "LAC"
      , str_detect(Team, "Los Angeles Lakers") ~ "LAL"
      , str_detect(Team, "Memphis Grizzlies") ~ "MEM"
      , str_detect(Team, "Miami Heat") ~ "MIA"
      , str_detect(Team, "Milwaukee Bucks") ~ "MIL"
      , str_detect(Team, "Minnesota Timberwolves") ~ "MIN"
      , str_detect(Team, "New Orleans Pelicans") ~ "NO"
      , str_detect(Team, "New York Knicks") ~ "NY"
      , str_detect(Team, "Oklahoma City Thunder") ~ "OKC"
      , str_detect(Team, "Orlando Magic") ~ "ORL"
      , str_detect(Team, "Philadelphia 76ers") ~ "PHI"
      , str_detect(Team, "Phoenix Suns") ~ "PHX"
      , str_detect(Team, "Portland Trail Blazers") ~ "POR"
      , str_detect(Team, "San Antonio Spurs") ~ "SA"
      , str_detect(Team, "Sacramento Kings") ~ "SAC"
      , str_detect(Team, "Toronto Raptors") ~ "TOR"
      , str_detect(Team, "Utah Jazz") ~ "UTAH"
      , str_detect(Team, "Washington Wizards") ~ "WSH"
      , .default = Team
    ))
  
  # Return tibble
  return(data)
}

# Scrape NBA opponent per 100 possessions stats for single season from Basketball Reference
get_opponent_p100_poss_stats_bref <- function(year) {
  
  # Scrape url
  url = paste0("https://www.basketball-reference.com/leagues/NBA_", year, ".html")
  tbl = url |>
    read_html() |>
    html_nodes("table") |>
    html_table()
  
  # Extract data
  data <- tbl[[10]] %>%
    select (-Rk) %>%
    mutate(Tm = case_when( # Team Abbreviation
      str_detect(Team, "Atlanta Hawks") ~ "ATL"
      , str_detect(Team, "Brooklyn Nets") ~ "BKN"
      , str_detect(Team, "Boston Celtics") ~ "BOS"
      , str_detect(Team, "Charlotte Hornets") ~ "CHA"
      , str_detect(Team, "Chicago Bulls") ~ "CHI"
      , str_detect(Team, "Cleveland Cavaliers") ~ "CLE"
      , str_detect(Team, "Dallas Mavericks") ~ "DAL"
      , str_detect(Team, "Denver Nuggets") ~ "DEN"
      , str_detect(Team, "Detroit Pistons") ~ "DET"
      , str_detect(Team, "Golden State Warriors") ~ "GS"
      , str_detect(Team, "Houston Rockets") ~ "HOU"
      , str_detect(Team, "Indiana Pacers") ~ "IND"
      , str_detect(Team, "Los Angeles Clippers") ~ "LAC"
      , str_detect(Team, "Los Angeles Lakers") ~ "LAL"
      , str_detect(Team, "Memphis Grizzlies") ~ "MEM"
      , str_detect(Team, "Miami Heat") ~ "MIA"
      , str_detect(Team, "Milwaukee Bucks") ~ "MIL"
      , str_detect(Team, "Minnesota Timberwolves") ~ "MIN"
      , str_detect(Team, "New Orleans Pelicans") ~ "NO"
      , str_detect(Team, "New York Knicks") ~ "NY"
      , str_detect(Team, "Oklahoma City Thunder") ~ "OKC"
      , str_detect(Team, "Orlando Magic") ~ "ORL"
      , str_detect(Team, "Philadelphia 76ers") ~ "PHI"
      , str_detect(Team, "Phoenix Suns") ~ "PHX"
      , str_detect(Team, "Portland Trail Blazers") ~ "POR"
      , str_detect(Team, "San Antonio Spurs") ~ "SA"
      , str_detect(Team, "Sacramento Kings") ~ "SAC"
      , str_detect(Team, "Toronto Raptors") ~ "TOR"
      , str_detect(Team, "Utah Jazz") ~ "UTAH"
      , str_detect(Team, "Washington Wizards") ~ "WSH"
      , .default = Team
    ))
  
  # Return tibble
  return(data)
}

# Scrape NBA team advanced stats for single season from Basketball Reference
get_team_advanced_stats_bref <- function(year) {
  
  # Scrape url
  url = paste0("https://www.basketball-reference.com/leagues/NBA_", year, ".html")
  tbl = url |>
    read_html() |>
    html_nodes("table") |>
    html_table()
  
  # Extract data, fix column names
  df <- data.frame(tbl[[11]]) %>%
    select (-c(Var.18, Var.23, Var.28))
  colnames(df) <- c("Rk","Team", "Age", "W", "L", "PW", "PL", "MOV", "SOS", "SRS", "ORtg", "DRtg", "NRtg", "Pace", "FTr", "3PAr", "TS%",
                    "eFG%", "TOV%", "ORB%", "FT/FGA", "Opp. eFG%", "Opp. TOV%", "DRB%", "Opp. FT/FGA","Arena", "Attendance", "Attendance/G")
  
  df <- df[-1,]
  
  # Mapping teams, remove playoff *s
  df <- df %>%
    select (-Rk) %>%
    mutate(Tm = case_when(
      str_detect(Team, "Atlanta Hawks") ~ "ATL",
      str_detect(Team, "Brooklyn Nets") ~ "BKN",
      str_detect(Team, "Boston Celtics") ~ "BOS",
      str_detect(Team, "Charlotte Hornets") ~ "CHA",
      str_detect(Team, "Chicago Bulls") ~ "CHI",
      str_detect(Team, "Cleveland Cavaliers") ~ "CLE",
      str_detect(Team, "Dallas Mavericks") ~ "DAL",
      str_detect(Team, "Denver Nuggets") ~ "DEN",
      str_detect(Team, "Detroit Pistons") ~ "DET",
      str_detect(Team, "Golden State Warriors") ~ "GS",
      str_detect(Team, "Houston Rockets") ~ "HOU",
      str_detect(Team, "Indiana Pacers") ~ "IND",
      str_detect(Team, "Los Angeles Clippers") ~ "LAC",
      str_detect(Team, "Los Angeles Lakers") ~ "LAL",
      str_detect(Team, "Memphis Grizzlies") ~ "MEM",
      str_detect(Team, "Miami Heat") ~ "MIA",
      str_detect(Team, "Milwaukee Bucks") ~ "MIL",
      str_detect(Team, "Minnesota Timberwolves") ~ "MIN",
      str_detect(Team, "New Orleans Pelicans") ~ "NO",
      str_detect(Team, "New York Knicks") ~ "NY",
      str_detect(Team, "Oklahoma City Thunder") ~ "OKC",
      str_detect(Team, "Orlando Magic") ~ "ORL",
      str_detect(Team, "Philadelphia 76ers") ~ "PHI",
      str_detect(Team, "Phoenix Suns") ~ "PHX",
      str_detect(Team, "Portland Trail Blazers") ~ "POR",
      str_detect(Team, "San Antonio Spurs") ~ "SA",
      str_detect(Team, "Sacramento Kings") ~ "SAC",
      str_detect(Team, "Toronto Raptors") ~ "TOR",
      str_detect(Team, "Utah Jazz") ~ "UTAH",
      str_detect(Team, "Washington Wizards") ~ "WSH",
      .default = Team
    ))
  
  df$Team <- gsub("\\*", "", df$Team)
  
  # Adjust data types
  df <- df %>%
    mutate(Age = as.numeric(Age),
           W = as.numeric(W),
           L = as.numeric(L),
           PW = as.numeric(PW),
           PL = as.numeric(PL),
           MOV = as.numeric(MOV),
           SOS = as.numeric(SOS),
           SRS = as.numeric(SRS),
           ORtg = as.numeric(ORtg),
           DRtg = as.numeric(DRtg),
           NRtg = as.numeric(NRtg),
           Pace = as.numeric(Pace),
           FTr = as.numeric(FTr),
           `3PAr` = as.numeric(`3PAr`),
           `TS%` = as.numeric(`TS%`),
           `eFG%` = as.numeric(`eFG%`),
           `TOV%` = as.numeric(`TOV%`),
           `ORB%` = as.numeric(`ORB%`),
           `FT/FGA` = as.numeric(`FT/FGA`),
           `Opp. eFG%` = as.numeric(`Opp. eFG%`),
           `Opp. TOV%` = as.numeric(`Opp. TOV%`),
           `DRB%` = as.numeric(`DRB%`),
           `Opp. FT/FGA` = as.numeric(`Opp. FT/FGA`))
  
  # Return table
  return(df)
}
# ---------------------------------------------------------





########## Player Data ##########
# ---------------------------------------------------------
# Scrape NBA player per 100 possessions stats for single season from Basketball Reference
get_player_per_100_poss_bref <- function(year) {
  
  # Scrape url
  url = paste0("https://www.basketball-reference.com/leagues/NBA_", year, "_per_poss.html")
  tbl = url |>
    read_html() |>
    html_nodes("table") |>
    html_table()
  
  # Extract data
  df <- data.frame(tbl) |>
    filter(Rk != 'Rk') |> # Remove dividing rows
    distinct(Player, .keep_all = TRUE) |> # Remove duplicate player names
    select(-Rk, -Awards) # Remove rank and Awards columns
  
  # Return data.frame
  return(df)
}

# Scrape player per 36 minutes stats from Basketball Reference
get_player_per_36_mins_bref <- function(year) {
  
  # Scrape url
  url = paste0("https://www.basketball-reference.com/leagues/NBA_", year, "_per_minute.html")
  tbl = url |>
    read_html() |>
    html_nodes("table") |>
    html_table()
  
  # Extract data
  df <- data.frame(tbl) |>
    filter(Rk != 'Rk') |> # Remove dividing rows
    distinct(Player, .keep_all = TRUE) |> # Remove duplicate player names
    select(-Rk, -Awards) # Remove Rank and Award columns
  
  # Adjust column names
  colnames(df) <- c('Player', 'Age', 'Team', 'Pos', 'G', 'GS', 'MP', 'FG', 'FGA', 'FG%', '3P', '3PA',
                    '3P%', '2P', '2PA', '2P%', 'eFG%', 'FT', 'FTA', 'FT%', 'ORB', 'DRB', 'TRB', 'AST',
                    'STL', 'BLK', 'TOV', 'PF', 'PTS')
  
  # Return data.frame
  return(df)
}
# ---------------------------------------------------------




