# Load libraries
library(tidyverse)
library(nbaplotR)

source("data.R")

# League Net Ratings Graph
team_ratings_graph <- function(year) {
  data <- get_team_ratings(year)
  
  median_ortg <- median(data$ORtg)
  median_drtg <- median(data$DRtg)
  
  net_ratings_plot <- ggplot(data = data, aes(x=ORtg, y=DRtg, label = Tm)) +
    geom_nba_logos(aes(team_abbr = Tm), width = 0.05) +
    labs(title="2023-24 League Ratings") + xlab("Offensive Rating") + ylab("Defensive Rating") +
    geom_hline(yintercept=median_drtg, color = "black", size = 1, linetype = 2) +
    geom_vline(xintercept=median_ortg, color = "black", size = 1, linetype = 2) +
    scale_x_continuous(limits = c(median_ortg - 10, median_ortg + 10)) +
    scale_y_continuous(limits = c(median_drtg - 10, median_drtg + 10)) +
    scale_y_reverse() + 
    theme(panel.grid = element_line(color = "grey", size = 0.75, linetype = 1),
          panel.background = element_rect(fill = "white"),
          panel.border = element_rect(colour = "grey", fill=NA, size=1),
          text=element_text(family="source-sans-pro"))
  
  return(net_ratings_plot)
}

# Player Per 100 Possession Percentile Graph
player_percentile_graph <- function(player, year) {
  data <- get_per_100_poss(year)
  
  player_data <- data %>%
    filter(Player == player)
  
  # Plot
  player_percentile_plot <- ggplot(player_data) +
    geom_col(aes(percentile, Stat), width = 0.6)  
  
  return(player_percentile_plot)
}