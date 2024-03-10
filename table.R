# Load libraries
library(tidyverse)
library(nbaplotR)

source("data.R")

# League Net Ratings Table
team_ratings_table <- function(year) {
  data <- get_team_ratings(year)
  
  team_ratings_table <- data %>%
    select(Tm, ORtg, DRtg, NRtg) %>%
    gt()
  
  return(team_ratings_table)
}
