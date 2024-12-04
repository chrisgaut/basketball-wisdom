# Load libraries
library(tidyverse)
library(rvest)
library(janitor)
library(stringr)

########## Team Data ##########
# Scrape team ratings from Basketball Reference
get_team_ratings_bref <- function(year) {
  # Scrape team ratings, individual season
  
  # Scrape url
  url = paste0("https://www.basketball-reference.com/leagues/NBA_", year, "_ratings.html")
  tbl = url%>%
    read_html() %>%
    html_nodes("table") %>%
    html_table()
  
  # Turn tbl into df
  df <- data.frame(tbl[[1]]) %>%
    row_to_names(row_number = 1) %>%
    select (-Rk) %>%
    mutate(`W/L%` = as.numeric(`W/L%`),
           `Margin of Victory` = as.numeric(MOV),
           `Offensive Rating` = as.numeric(ORtg),
           `Defensive Rating` = as.numeric(DRtg),
           `Net Rating` = as.numeric(NRtg),
           `Adjusted Margin of Victory` = as.numeric(`MOV/A`),
           `Adjusted Offensive Rating` = as.numeric(`ORtg/A`),
           `Adjusted Defensive Rating` = as.numeric(`DRtg/A`),
           `Adjusted Net Rating` = as.numeric(`NRtg/A`)) %>%
    mutate(Tm = case_when(
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
  
  # Return table
  return(df)
}

# Scrape Eastern Conference Standings from Basketball Reference
get_ec_standings_bref <- function(year) {
  # Scrape eastern conference standings, individual season
  
  # Scrape url
  url = paste0("https://www.basketball-reference.com/leagues/NBA_", year, ".html")
  tbl = url%>%
    read_html() %>%
    html_nodes("table") %>%
    html_table()
  
  # Turn tbl into df
  df <- data.frame(tbl[[1]]) %>%
    rename(Team = Eastern.Conference) %>%
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
  
  # Return table
  return(df)
}

# Scrape Western Conference Standings from Basketball Reference
get_wc_standings_bref <- function(year) {
  # Scrape western conference standings, individual season
  
  # Scrape url
  url = paste0("https://www.basketball-reference.com/leagues/NBA_", year, ".html")
  tbl = url%>%
    read_html() %>%
    html_nodes("table") %>%
    html_table()
  
  # Turn tbl into df
  df <- data.frame(tbl[[2]]) %>%
    rename(Team = Western.Conference) %>%
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
  
  # Return table
  return(df)
}

# Scrape Team Per Game Stats from Basketball Reference
get_team_pg_stats_bref <- function(year) {
  # Scrape team per game stats, individual season
  
  # Scrape url
  url = paste0("https://www.basketball-reference.com/leagues/NBA_", year, ".html")
  tbl = url%>%
    read_html() %>%
    html_nodes("table") %>%
    html_table()
  
  # Turn tbl into df
  df <- data.frame(tbl[[5]]) %>%
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
  
  # Return table
  return(df)
}

# Scrape Opponent Per Game Stats from Basketball Reference
get_opponent_pg_stats_bref <- function(year) {
  # Scrape opponent per game stats, individual season
  
  # Scrape url
  url = paste0("https://www.basketball-reference.com/leagues/NBA_", year, ".html")
  tbl = url%>%
    read_html() %>%
    html_nodes("table") %>%
    html_table()
  
  # Turn tbl into df
  df <- data.frame(tbl[[6]]) %>%
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
  
  # Return table
  return(df)
}

# Scrape Team Total Stats from Basketball Reference
get_team_total_stats_bref <- function(year) {
  # Scrape team total stats, individual season
  
  # Scrape url
  url = paste0("https://www.basketball-reference.com/leagues/NBA_", year, ".html")
  tbl = url%>%
    read_html() %>%
    html_nodes("table") %>%
    html_table()
  
  # Turn tbl into df
  df <- data.frame(tbl[[7]]) %>%
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
  
  # Return table
  return(df)
}

# Scrape Opponent Total Stats from Basketball Reference
get_opponent_total_stats_bref <- function(year) {
  # Scrape opponent total stats, individual season
  
  # Scrape url
  url = paste0("https://www.basketball-reference.com/leagues/NBA_", year, ".html")
  tbl = url%>%
    read_html() %>%
    html_nodes("table") %>%
    html_table()
  
  # Turn tbl into df
  df <- data.frame(tbl[[8]]) %>%
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
  
  # Return table
  return(df)
}

# Scrape Team Per 100 Possessions Stats from Basketball Reference
get_team_p100_poss_stats_bref <- function(year) {
  # Scrape team per 100 possessions stats, individual season
  
  # Scrape url
  url = paste0("https://www.basketball-reference.com/leagues/NBA_", year, ".html")
  tbl = url%>%
    read_html() %>%
    html_nodes("table") %>%
    html_table()
  
  # Turn tbl into df
  df <- data.frame(tbl[[9]]) %>%
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
  
  # Return table
  return(df)
}

# Scrape Opponent Per 100 Possessions Stats from Basketball Reference
get_opponent_p100_poss_stats_bref <- function(year) {
  # Scrape opponent per 100 possessions stats, individual season
  
  # Scrape url
  url = paste0("https://www.basketball-reference.com/leagues/NBA_", year, ".html")
  tbl = url%>%
    read_html() %>%
    html_nodes("table") %>%
    html_table()
  
  # Turn tbl into df
  df <- data.frame(tbl[[10]]) %>%
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
  
  # Return table
  return(df)
}

# Scrape Team Advanced Stats from Basketball Reference
get_team_advanced_stats_bref <- function(year) {
  # Scrape team advanced stats, individual season
  
  # Scrape url
  url = paste0("https://www.basketball-reference.com/leagues/NBA_", year, ".html")
  tbl = url%>%
    read_html() %>%
    html_nodes("table") %>%
    html_table()
  
  # Turn tbl into df, fix column names
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

########## Player Data ##########
# Scrape player per-100 stats from Basketball Reference, create percentiles
get_per_100_poss <- function(year) {
  url = paste0("https://www.basketball-reference.com/leagues/NBA_", year, "_per_poss.html")
  
  tbl = url%>%
    read_html() %>%
    html_nodes("table") %>%
    html_table()
  
  df <- data.frame(tbl) %>%
    filter(Rk != 'Rk') %>% # Remove dividing rows
    distinct(Player, .keep_all = TRUE) # Remove duplicate player names
  
  # Select metrics you want to analyze
  df_short <- df %>%
    filter(as.numeric(MP) >= 300) %>% # Minimum 300 minutes played
    select(Player, PTS, AST, FTA, TOV, X2PA, X3PA) %>%
    mutate(`PTS/100` = as.numeric(PTS),
           `AST/100` = as.numeric(AST),
           `FTA/100` = as.numeric(FTA),
           `TO/100` = as.numeric(TOV),
           `2PA/100` = as.numeric(X2PA),
           `3PA/100` = as.numeric(X3PA)) %>%
    select(Player, `PTS/100`, `AST/100`, `FTA/100`, `TO/100`, `2PA/100`, `3PA/100`)
  
  # Create percentiles
  df_percentiles <- df_short %>%
    gather(key="Stat", value = value, 2:7) %>% # gather into long form
    group_by(Stat) %>% # Statistic
    mutate(percentile=percent_rank(value)*100) # make new column
  
  return(df_percentiles)
}

# Scrape player per 36 minutes stats from Basketball Reference
get_player_per_36_stats_bref <- function(year) {
  url = paste0("https://www.basketball-reference.com/leagues/NBA_", year, "_per_minute.html")
  
  tbl = url%>%
    read_html() %>%
    html_nodes("table") %>%
    html_table()
  
  df <- data.frame(tbl) %>%
    filter(Rk != 'Rk') %>% # Remove dividing rows
    distinct(Player, .keep_all = TRUE) |> # Remove duplicate player names
    select(-Rk, -Awards) # Remove Rank and Award column
  
  # Adjust column names
  colnames(df) <- c('Player', 'Age', 'Team', 'Pos', 'G', 'GS', 'MP', 'FG', 'FGA', 'FG%', '3P', '3PA',
                    '3P%', '2P', '2PA', '2P%', 'eFG%', 'FT', 'FTA', 'FT%', 'ORB', 'DRB', 'TRB', 'AST',
                    'STL', 'BLK', 'TOV', 'PF', 'PTS')
  
  return(df)
}

