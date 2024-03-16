# Load libraries
library(tidyverse)
library(rvest)
library(janitor)

# Scrape team ratings from Basketball Reference
get_team_ratings <- function(year) {
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
           MOV = as.numeric(MOV),
           ORtg = as.numeric(ORtg),
           DRtg = as.numeric(DRtg),
           NRtg = as.numeric(NRtg),
           `MOV/A` = as.numeric(`MOV/A`),
           `ORtg/A` = as.numeric(`ORtg/A`),
           `DRtg/A` = as.numeric(`DRtg/A`),
           `NRtg/A` = as.numeric(`NRtg/A`)) %>%
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